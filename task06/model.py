#!/usr/bin/env python3
import abc


class Scope:
    def __init__(self, parent=None):
        self.parent = parent
        self.scope = {}

    def __getitem__(self, item):
        if item in self.scope:
            return self.scope[item]
        if self.parent:
            return self.parent[item]
        raise KeyError(item)

    def __setitem__(self, key, value):
        self.scope[key] = value


class ASTNode(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def evaluate(self, scope):
        pass


class Number(ASTNode):
    def __init__(self, value):
        assert isinstance(value, int)
        self.value = value

    def __eq__(self, other):
        return self.value == other.value

    def __hash__(self):
        return hash(self.value)

    def evaluate(self, scope):
        return self


class Function(ASTNode):
    def __init__(self, args, body):
        self.args = args
        self.body = body

    def evaluate(self, scope):
        return self


class FunctionDefinition(ASTNode):
    def __init__(self, name, function):
        self.name = name
        self.function = function

    def evaluate(self, scope):
        scope[self.name] = self.function
        return self.function


class Conditional(ASTNode):
    def __init__(self, condition, if_true, if_false=None):
        self.condition = condition
        self.if_true = if_true
        self.if_false = if_false

    def evaluate(self, scope):
        if self.condition.evaluate(scope) != Number(0):
            to_evaluate = self.if_true
        else:
            to_evaluate = self.if_false
        result = None
        for statement in to_evaluate or []:
            result = statement.evaluate(scope)
        return result


class Print(ASTNode):
    def __init__(self, expr):
        self.expr = expr

    def evaluate(self, scope):
        result = self.expr.evaluate(scope)
        print(result.value)
        return result


class Read(ASTNode):
    def __init__(self, name):
        self.name = name

    def evaluate(self, scope):
        number = Number(int(input()))
        scope[self.name] = number
        return number


class FunctionCall(ASTNode):
    def __init__(self, fun_expr, args):
        self.fun_expr = fun_expr
        self.args = args

    def evaluate(self, scope):
        function = self.fun_expr.evaluate(scope)
        call_scope = Scope(scope)
        for function_arg, self_arg in zip(function.args, self.args):
            call_scope[function_arg] = self_arg.evaluate(scope)

        result = None
        for statement in function.body:
            result = statement.evaluate(call_scope)
        return result


class Reference(ASTNode):
    def __init__(self, name):
        self.name = name

    def evaluate(self, scope):
        return scope[self.name]


class BinaryOperation(ASTNode):
    OPERATIONS = {'+': lambda x, y: x + y,
                  '-': lambda x, y: x - y,
                  '*': lambda x, y: x * y,
                  '/': lambda x, y: x // y,
                  '%': lambda x, y: x % y,
                  '==': lambda x, y: x == y,
                  '!=': lambda x, y: x != y,
                  '<': lambda x, y: x < y,
                  '>': lambda x, y: x > y,
                  '<=': lambda x, y: x <= y,
                  '>=': lambda x, y: x >= y,
                  '&&': lambda x, y: x and y,
                  '||': lambda x, y: x or y}

    def __init__(self, lhs, op, rhs):
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

    def evaluate(self, scope):
        lhs_result = self.lhs.evaluate(scope).value
        rhs_result = self.rhs.evaluate(scope).value
        return Number(int(BinaryOperation.OPERATIONS[self.op](lhs_result,
                                                              rhs_result)))


class UnaryOperation(ASTNode):
    OPERATIONS = {'-': lambda x: -x,
                  '!': lambda x: not x}

    def __init__(self, op, expr):
        self.op = op
        self.expr = expr

    def evaluate(self, scope):
        result = self.expr.evaluate(scope).value
        return Number(int(UnaryOperation.OPERATIONS[self.op](result)))
