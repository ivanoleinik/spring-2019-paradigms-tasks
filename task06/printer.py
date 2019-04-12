#!/usr/bin/env python3

import model


def delete_semicolon(line):
    if line.endswith(';'):
        return line[:-1]
    return line


class PrettyPrinter(model.ASTNodeVisitor):
    def __init__(self):
        self.enable_indent = 1
        self.indent_depth = 0

    def pretty_print(self, program):
        self.enable_indent = 1
        self.indent_depth = 0
        return program.accept(self)

    def indent(self):
        if self.enable_indent > 0:
            return '    ' * self.indent_depth
        return ''

    def visit_number(self, number):
        return f'{self.indent()}{str(number.value)};'

    def visit_function(self, function):
        raise RuntimeError('PrettyPrinter must not visit function object')

    def visit_function_definition(self, function_definition):
        result = '{}def {}({}) {{\n'.format(
            self.indent(),
            function_definition.name,
            ', '.join(function_definition.function.args)
        )
        self.indent_depth += 1
        for statement in function_definition.function.body or []:
            result += statement.accept(self) + '\n'
        self.indent_depth -= 1
        result += '}'
        return result

    def visit_conditional(self, conditional):
        self.enable_indent -= 1
        condition_result = delete_semicolon(conditional.condition.accept(self))
        self.enable_indent += 1
        result = f'{self.indent()}if ({condition_result}) {{\n'

        self.indent_depth += 1
        for statement in conditional.if_true or []:
            result += f'{statement.accept(self)}\n'
        self.indent_depth -= 1
        result += self.indent() + '}'

        if conditional.if_false:
            result += ' else {\n'
            self.indent_depth += 1
            for statement in conditional.if_false:
                result += f'{statement.accept(self)}\n'
            self.indent_depth -= 1
            result += self.indent() + '}'
        return result

    def visit_print(self, print_object):
        self.enable_indent -= 1
        result = print_object.expr.accept(self)
        self.enable_indent += 1
        return f'{self.indent()}print {result}'

    def visit_read(self, read_object):
        return f'{self.indent()}read {read_object.name};'

    def visit_function_call(self, function_call):
        self.enable_indent -= 1
        result = []
        for arg in function_call.args:
            result.append(delete_semicolon(arg.accept(self)))
        self.enable_indent += 1
        return '{}({});'.format(
            delete_semicolon(function_call.fun_expr.accept(self)),
            ', '.join(result)
        )

    def visit_reference(self, reference):
        return f'{self.indent()}{reference.name};'

    def visit_binary_operation(self, binary_operation):
        self.enable_indent -= 1
        lhs_result = delete_semicolon(binary_operation.lhs.accept(self))
        rhs_result = delete_semicolon(binary_operation.rhs.accept(self))
        self.enable_indent += 1
        return self.indent() + \
            f'({lhs_result}) {binary_operation.op} ({rhs_result});'

    def visit_unary_operation(self, unary_operation):
        self.enable_indent -= 1
        result = delete_semicolon(unary_operation.expr.accept(self))
        self.enable_indent += 1
        return f'{self.indent()}{unary_operation.op}({result});'


def pretty_print(program):
    print(PrettyPrinter().pretty_print(program))
