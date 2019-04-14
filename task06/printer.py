#!/usr/bin/env python3

import model
import textwrap


class ExpressionPrinter(model.ASTNodeVisitor):
    def visit_number(self, number):
        return str(number.value)

    def visit_function(self, function):
        raise TypeError('ExpressionPrinter must not visit Function object')

    def visit_function_definition(self, function_definition):
        raise TypeError('ExpressionPrinter must not visit '
                        'FunctionDefinition object')

    def visit_conditional(self, conditional):
        raise TypeError('ExpressionPrinter must not visit Conditional object')

    def visit_print(self, print_object):
        raise TypeError('ExpressionPrinter must not visit Print object')

    def visit_read(self, read_object):
        raise TypeError('ExpressionPrinter must not visit Read object')

    def visit_function_call(self, function_call):
        return '{}({})'.format(
            function_call.fun_expr.accept(self),
            ', '.join(arg.accept(self) for arg in function_call.args)
        )

    def visit_reference(self, reference):
        return reference.name

    def visit_binary_operation(self, binary_operation):
        return '({} {} {})'.format(
            binary_operation.lhs.accept(self),
            binary_operation.op,
            binary_operation.rhs.accept(self)
        )

    def visit_unary_operation(self, unary_operation):
        return '({}{})'.format(
            unary_operation.op,
            unary_operation.expr.accept(self)
        )


class PrettyPrinter(model.ASTNodeVisitor):
    def __init__(self, expression_printer):
        self.expression_printer = expression_printer

    def pretty_print(self, program):
        return program.accept(self)

    def visit_expression(self, expression):
        return expression.accept(self.expression_printer)

    def visit_block(self, block):
        result = ''
        for statement in block or []:
            result += statement.accept(self) + '\n'
        return textwrap.indent(result, '    ') + '}'

    def visit_number(self, number):
        return self.visit_expression(number) + ';'

    def visit_function(self, function):
        raise TypeError('PrettyPrinter must not visit Function object')

    def visit_function_definition(self, function_definition):
        result = 'def {}({}) {{\n'.format(
            function_definition.name,
            ', '.join(function_definition.function.args)
        )
        result += self.visit_block(function_definition.function.body)
        return result

    def visit_conditional(self, conditional):
        result = 'if (' + self.visit_expression(conditional.condition)
        result += ') {\n' + self.visit_block(conditional.if_true)
        if conditional.if_false:
            result += ' else {\n' + self.visit_block(conditional.if_false)
        return result

    def visit_print(self, print_object):
        return 'print ' + self.visit_expression(print_object.expr) + ';'

    def visit_read(self, read_object):
        return f'read {read_object.name};'

    def visit_function_call(self, function_call):
        return self.visit_expression(function_call) + ';'

    def visit_reference(self, reference):
        return self.visit_expression(reference) + ';'

    def visit_binary_operation(self, binary_operation):
        return self.visit_expression(binary_operation) + ';'

    def visit_unary_operation(self, unary_operation):
        return self.visit_expression(unary_operation) + ';'


def pretty_print(program):
    print(PrettyPrinter(ExpressionPrinter()).pretty_print(program))
