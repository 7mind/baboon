from BaboonDefinitions.Generated.testpkg.pkg0.BaboonServiceRt import IBaboonServiceRt


class ResultOk:
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f"ResultOk({self.value!r})"


class ResultErr:
    def __init__(self, error):
        self.error = error

    def __repr__(self):
        return f"ResultErr({self.error!r})"


class ResultServiceRt(IBaboonServiceRt):
    def pure(self, value):
        return ResultOk(value)

    def fail(self, error):
        return ResultErr(error)

    def left_map(self, value, f):
        if isinstance(value, ResultErr):
            return ResultErr(f(value.error))
        return value

    def flat_map(self, value, f):
        if isinstance(value, ResultErr):
            return value
        return f(value.value)
