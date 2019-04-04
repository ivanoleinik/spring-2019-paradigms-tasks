#!/usr/bin/env python3
import sys
from io import StringIO

import pytest
import model


def test_scope():
    a = 1
    b = [2, 3]
    c = 'zoo'

    parent = model.Scope()
    parent['foo'] = a
    parent['bar'] = b
    scope = model.Scope(parent)
    son = model.Scope(scope)

    assert scope['bar'] is b
    assert parent['bar'] is b

    scope['bar'] = c

    assert scope['bar'] is c
    assert parent['bar'] is b
    assert scope['foo'] is a
    assert son['foo'] is a


def test_scope_key_error():
    scope = model.Scope()
    scope['zebra'] = 'zoo'
    with pytest.raises(KeyError):
        _ = scope['zoo']


def test_function_definition():
    operation = model.FunctionDefinition(
        'foo',
        model.Function(['a', 'b'], [])
    )
    scope = model.Scope()
    assert operation.evaluate(scope) == scope['foo']


def test_conditional_empty():
    scope = model.Scope()
    conditional = model.Conditional(model.Number(0), [])
    assert conditional.evaluate(scope) is None


def test_conditional_true():
    scope = model.Scope()
    conditional = model.Conditional(
        model.BinaryOperation(model.Number(3), '>', model.Number(2)),
        [model.Number(3)],
        [model.Number(2)]
    )
    assert conditional.evaluate(scope) == model.Number(3)


def test_conditional_false():
    scope = model.Scope()
    conditional = model.Conditional(
        model.BinaryOperation(model.Number(3), '<=', model.Number(2)),
        [model.Number(1)],
        [model.Number(0)]
    )
    assert conditional.evaluate(scope) == model.Number(0)


def test_print(capsys):
    scope = model.Scope()
    model.Print(model.Number(9)).evaluate(scope)
    out, err = capsys.readouterr()
    assert not err
    assert out == '9\n'


def test_read(monkeypatch):
    scope = model.Scope()
    monkeypatch.setattr(sys, 'stdin', StringIO('5'))
    assert model.Read('var').evaluate(scope) == model.Number(5)
    assert scope['var'] == model.Number(5)


def test_function_call():
    scope = model.Scope()
    operation = model.FunctionCall(
        model.Function(
            ['a', 'b'],
            [model.Reference('a')]
        ).evaluate(scope),
        [
            model.Number(6),
            model.Number(1)
        ]
    )
    assert operation.evaluate(scope) == model.Number(6)


def test_reference():
    scope = model.Scope()
    scope['a'] = model.Number(4)
    assert model.Reference('a').evaluate(scope) == model.Number(4)


def test_binary_operation():
    scope = model.Scope()
    a = model.Number(11)
    b = model.Number(7)
    assert model.BinaryOperation(a, '+', b).evaluate(scope) == model.Number(18)
    assert model.BinaryOperation(a, '-', b).evaluate(scope) == model.Number(4)
    assert model.BinaryOperation(a, '*', b).evaluate(scope) == model.Number(77)
    assert model.BinaryOperation(a, '/', b).evaluate(scope) == model.Number(1)
    assert model.BinaryOperation(a, '%', b).evaluate(scope) == model.Number(4)
    assert model.BinaryOperation(a,
                                 '==',
                                 b).evaluate(scope) == model.Number(0)
    assert model.BinaryOperation(a,
                                 '!=',
                                 b).evaluate(scope) == model.Number(1)
    assert model.BinaryOperation(a,
                                 '<',
                                 b).evaluate(scope) == model.Number(0)
    assert model.BinaryOperation(a,
                                 '>',
                                 b).evaluate(scope) == model.Number(1)
    assert model.BinaryOperation(a,
                                 '<=',
                                 b).evaluate(scope) == model.Number(0)
    assert model.BinaryOperation(a,
                                 '>=',
                                 b).evaluate(scope) == model.Number(1)
    assert model.BinaryOperation(a,
                                 '&&',
                                 b).evaluate(scope) != model.Number(0)
    assert model.BinaryOperation(a,
                                 '||',
                                 b).evaluate(scope) != model.Number(0)


def test_unary_operation():
    scope = model.Scope()
    a = model.Number(2)
    assert model.UnaryOperation('-', a).evaluate(scope) == model.Number(-2)
    assert model.UnaryOperation('!', a).evaluate(scope) == model.Number(0)


def test_factorial(capsys):
    scope = model.Scope()
    yat_fac = model.FunctionDefinition('fac', model.Function(['n'], [
        model.Conditional(
            model.BinaryOperation(model.Reference('n'), '==', model.Number(0)),
            [model.Number(1)],
            [
                model.BinaryOperation(
                    model.Reference('n'),
                    '*',
                    model.FunctionCall(model.Reference('fac'), [
                        model.BinaryOperation(
                            model.Reference('n'),
                            '-',
                            model.Number(1)
                        )
                    ])
                )
            ]
        )
    ]))

    yat_fac.evaluate(scope)
    model.Print(model.FunctionCall(model.Reference('fac'),
                                   [model.Number(5)])).evaluate(scope)
    out, err = capsys.readouterr()
    assert not err
    assert out == '120\n'


if __name__ == '__main__':
    pytest.main()
