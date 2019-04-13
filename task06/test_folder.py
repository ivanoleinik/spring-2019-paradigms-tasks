#!/usr/bin/env python3

import pytest

import model
import folder


def test_binary_operation_num_num():
    assert folder.fold_constants(model.BinaryOperation(
        model.Number(239),
        '%',
        model.Number(30)
    )) == model.Number(29)


def test_zero_multiply_by_ref():
    assert folder.fold_constants(model.BinaryOperation(
        model.Number(0),
        '*',
        model.Reference('var')
    )) == model.Number(0)


def test_ref_multiply_by_zero():
    assert folder.fold_constants(model.BinaryOperation(
        model.Reference('var'),
        '*',
        model.Number(0)
    )) == model.Number(0)


def test_ref_minus_the_same_ref():
    assert folder.fold_constants(model.BinaryOperation(
        model.Reference('var'),
        '-',
        model.Reference('var')
    )) == model.Number(0)


def test_unary_operation():
    assert folder.fold_constants(model.UnaryOperation(
        '-',
        model.Number(-239)
    )) == model.Number(239)


def test_end_to_end():
    assert folder.fold_constants(
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
    ) == model.Number(13)
