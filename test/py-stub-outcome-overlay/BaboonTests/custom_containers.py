from BaboonDefinitions.Generated.testpkg.pkg0.BaboonServiceRt import IBaboonServiceRt


class OutcomeSuccess:
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f"OutcomeSuccess({self.value!r})"


class OutcomeFailure:
    def __init__(self, error):
        self.error = error

    def __repr__(self):
        return f"OutcomeFailure({self.error!r})"


class OutcomeServiceRt(IBaboonServiceRt):
    def pure(self, value):
        return OutcomeSuccess(value)

    def fail(self, error):
        return OutcomeFailure(error)

    def left_map(self, value, f):
        if isinstance(value, OutcomeFailure):
            return OutcomeFailure(f(value.error))
        return value

    def flat_map(self, value, f):
        if isinstance(value, OutcomeFailure):
            return value
        return f(value.value)
