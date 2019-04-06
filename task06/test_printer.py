#!/usr/bin/env python3
import pytest

import model
import printer


def test_pretty_number(capsys):
    printer.pretty_print(model.Number(239))
    out, err = capsys.readouterr()
    assert not err
    assert out == '239;\n'


def test_pretty_function_definition(capsys):
    printer.pretty_print(
        model.FunctionDefinition("foo", model.Function([], []))
    )
    out, err = capsys.readouterr()
    assert not err
    assert out == 'def foo() {\n};\n'


def test_pretty_conditional(capsys):
    printer.pretty_print(model.Conditional(model.Number(239), [], []))
    out, err = capsys.readouterr()
    assert not err
    assert out == 'if (239) {\n};\n'


def test_pretty_print(capsys):
    printer.pretty_print(model.Print(model.Number(239)))
    out, err = capsys.readouterr()
    assert not err
    assert out == 'print 239;\n'


def test_pretty_read(capsys):
    printer.pretty_print(model.Read('var'))
    out, err = capsys.readouterr()
    assert not err
    assert out == 'read var;\n'


def test_pretty_function_call(capsys):
    printer.pretty_print(
        model.FunctionCall(
            model.Reference('foo'),
            [
                model.Number(2),
                model.Number(3),
                model.Number(9)
            ]
        )
    )
    out, err = capsys.readouterr()
    assert not err
    assert out == 'foo(2, 3, 9);\n'


def test_pretty_reference(capsys):
    printer.pretty_print(model.Reference('var'))
    out, err = capsys.readouterr()
    assert not err
    assert out == 'var;\n'


def test_pretty_binary_operation(capsys):
    mul = model.BinaryOperation(model.Number(1), '*', model.Number(2))
    add = model.BinaryOperation(model.Number(3), '+', mul)
    mod = model.BinaryOperation(mul, '%', add)
    printer.pretty_print(mod)
    out, err = capsys.readouterr()
    assert not err
    assert out == '((1) * (2)) % ((3) + ((1) * (2)));\n'


def test_pretty_unary_operation(capsys):
    printer.pretty_print(model.UnaryOperation('-', model.Number(239)))
    out, err = capsys.readouterr()
    assert not err
    assert out == '-(239);\n'


def test_pretty_factorial(capsys):
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
    printer.pretty_print(yat_fac)
    out, err = capsys.readouterr()
    assert not err
    assert out == '''def fac(n) {
    if ((n) == (0)) {
        1;
    } else {
        (n) * (fac((n) - (1)));
    };
};
'''


if __name__ == "__main__":
    pytest.main()
