#!/usr/bin/env python3

import pytest

import model
import printer
import textwrap


def test_pretty_number():
    assert model.Number(239).accept(
        printer.PrettyPrinter(printer.ExpressionPrinter())) == '239;'


def test_pretty_function_definition():
    assert model.FunctionDefinition('foo', model.Function([], [])).accept(
        printer.PrettyPrinter(printer.ExpressionPrinter())) == 'def foo() {\n}'


def test_pretty_conditional():
    assert model.Conditional(model.Number(239), [], []).accept(
        printer.PrettyPrinter(printer.ExpressionPrinter())) == 'if (239) {\n}'


def test_pretty_print():
    assert model.Print(model.Number(239)).accept(
        printer.PrettyPrinter(printer.ExpressionPrinter())) == 'print 239;'


def test_pretty_read():
    assert model.Read('var').accept(
        printer.PrettyPrinter(printer.ExpressionPrinter())) == 'read var;'


def test_pretty_function_call():
    assert model.FunctionCall(
            model.Reference('foo'),
            [
                model.Number(2),
                model.Number(3),
                model.Number(9)
            ]
        ).accept(
        printer.PrettyPrinter(printer.ExpressionPrinter())) == 'foo(2, 3, 9);'


def test_pretty_reference():
    assert model.Reference('var').accept(
        printer.PrettyPrinter(printer.ExpressionPrinter())) == 'var;'


def test_pretty_binary_operation():
    mul = model.BinaryOperation(model.Number(1), '*', model.Number(2))
    add = model.BinaryOperation(model.Number(3), '+', mul)
    mod = model.BinaryOperation(mul, '%', add)
    assert (mod.accept(printer.PrettyPrinter(printer.ExpressionPrinter())) ==
            '((1 * 2) % (3 + (1 * 2)));')


def test_pretty_unary_operation():
    assert model.UnaryOperation('-', model.Number(239)).accept(
        printer.PrettyPrinter(printer.ExpressionPrinter())) == '(-239);'


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
    assert out == textwrap.dedent('''\
    def fac(n) {
        if ((n == 0)) {
            1;
        } else {
            (n * fac((n - 1)));
        }
    }
    ''')


if __name__ == '__main__':
    pytest.main()
