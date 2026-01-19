

class BaboonCodecException(Exception):
    @staticmethod
    def ConversionNotFound(msg: str) -> 'BaboonCodecException':
        return BaboonCodecException(f"ConversionNotFound: {msg}")

    @staticmethod
    def CodecNotFound(msg: str) -> 'BaboonCodecException':
        return BaboonCodecException(f"CodecNotFound: {msg}")

    @staticmethod
    def EncoderFailure(msg: str, cause: Exception = None) -> 'BaboonCodecException':
        exc = BaboonCodecException(f"EncoderFailure: {msg}")
        exc.__cause__ = cause
        return exc

    @staticmethod
    def DecoderFailure(msg: str, cause: Exception = None) -> 'BaboonCodecException':
        exc = BaboonCodecException(f"DecoderFailure: {msg}")
        exc.__cause__ = cause
        return exc

    @staticmethod
    def ConverterFailure(msg: str, cause: Exception = None) -> 'BaboonCodecException':
        exc = BaboonCodecException(f"ConverterFailure: {msg}")
        exc.__cause__ = cause
        return exc