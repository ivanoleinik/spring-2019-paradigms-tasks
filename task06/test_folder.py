#!/usr/bin/env python3

import pytest

import model
import printer
import folder


def test_binary_operation_num_num(capsys):
    printer.pretty_print(folder.fold_constants(model.BinaryOperation(
        model.Number(239),
        '%',
        model.Number(30)
    )))
    out, err = capsys.readouterr()
    assert not err
    assert out == '29;\n'


def test_binary_operation_num_ref(capsys):
    printer.pretty_print(folder.fold_constants(model.BinaryOperation(
        model.Number(0),
        '*',
        model.Reference('var')
    )))
    out, err = capsys.readouterr()
    assert not err
    assert out == '0;\n'


def test_binary_operation_ref_num(capsys):
    printer.pretty_print(folder.fold_constants(model.BinaryOperation(
        model.Reference('var'),
        '*',
        model.Number(0)
    )))
    out, err = capsys.readouterr()
    assert not err
    assert out == '0;\n'


def test_binary_operation_ref_ref(capsys):
    printer.pretty_print(folder.fold_constants(model.BinaryOperation(
        model.Reference('var'),
        '-',
        model.Reference('var')
    )))
    out, err = capsys.readouterr()
    assert not err
    assert out == '0;\n'


def test_unary_operation(capsys):
    printer.pretty_print(folder.fold_constants(model.UnaryOperation(
        '-',
        model.Number(-239)
    )))
    out, err = capsys.readouterr()
    assert not err
    assert out == '239;\n'


def test_end_to_end(capsys):
    printer.pretty_print(folder.fold_constants(
        model.BinaryOperation(
            model.Number(10),
            '-',
            model.UnaryOperation(
                '-',
                model.BinaryOperation(
                    model.Number(3),
                    '+',
                    model.BinaryOperation(
                        model.Reference('x'),
                        '-',
                        model.Reference('x')
                    )
                )
            )
        )
    ))
    out, err = capsys.readouterr()
    assert not err
    assert out == '13;\n'
