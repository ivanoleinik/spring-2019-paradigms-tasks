#!/usr/bin/env python3

import model


class ConstantFolder(model.ASTNodeVisitor):
    def visit_number(self, number):
        return model.Number(number.value)

    def visit_function(self, function):
        return model.Function(
            function.args,
            [statement.accept(self) for statement in function.body or []]
        )

    def visit_function_definition(self, function_definition):
        return model.FunctionDefinition(
            function_definition.name,
            function_definition.function.accept(self)
        )

    def visit_conditional(self, conditional):
        return model.Conditional(
            conditional.condition.accept(self),
            [statement.accept(self)
             for statement in conditional.if_true or []],
            [statement.accept(self)
             for statement in conditional.if_false or []]
        )

    def visit_print(self, print_object):
        return model.Print(print_object.expr.accept(self))

    def visit_read(self, read_object):
        return model.Read(read_object.name)

    def visit_function_call(self, function_call):
        return model.FunctionCall(
            function_call.fun_expr.accept(self),
            [arg.accept(self) for arg in function_call.args or []]
        )

    def visit_reference(self, reference):
        return model.Reference(reference.name)

    def visit_binary_operation(self, binary_operation):
        lhs = binary_operation.lhs.accept(self)
        rhs = binary_operation.rhs.accept(self)
        op = binary_operation.op

        if isinstance(lhs, model.Number) and isinstance(rhs, model.Number):
            return model.BinaryOperation(lhs, op, rhs).evaluate(model.Scope())
        if op == '*' and (
                (
                        isinstance(lhs, model.Number) and
                        lhs == model.Number(0) and
                        isinstance(rhs, model.Reference)
                ) or (
                        isinstance(rhs, model.Number) and
                        rhs == model.Number(0) and
                        isinstance(lhs, model.Reference)
                )
        ):
            return model.Number(0)
        if op == '-' and isinstance(lhs, model.Reference) \
                and isinstance(rhs, model.Reference) and lhs.name == rhs.name:
            return model.Number(0)

    def visit_unary_operation(self, unary_operation):
        expr = unary_operation.expr.accept(self)
        result = model.UnaryOperation(unary_operation.op, expr)
        if isinstance(expr, model.Number):
            result = result.evaluate(model.Scope())
        return result


def fold_constants(program):
    return program.accept(ConstantFolder())
