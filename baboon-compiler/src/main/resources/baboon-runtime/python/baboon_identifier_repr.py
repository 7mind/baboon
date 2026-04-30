# Runtime helpers for the `id` toString / parseRepr machinery defined in
# docs/spec/identifier-repr.md. The Python backend (PR-57d) uses these helpers
# from emitted code; conformance to the spec is the contract.
#
# Mirrors the JVM, Rust, Swift, TS, and Dart helpers in API and behaviour.
# Result type: `BaboonEither` (matching project convention from
# `baboon_service_wiring.py`). Errors are human-readable strings — same content
# as sibling backends — so test assertions can match on substrings across
# languages.

import datetime
import re
import uuid
from typing import Optional

from .baboon_service_wiring import BaboonEither, BaboonLeft, BaboonRight


# Numeric char-code constants. Kept defensively numeric to avoid any future
# template-escape surprises if these helpers were ever templated.
_BS = 0x5C  # '\\'
_HSH = 0x23  # '#'
_COL = 0x3A  # ':'
_OBR = 0x7B  # '{'
_CBR = 0x7D  # '}'

_BSCH = chr(_BS)
_HSHCH = chr(_HSH)
_COLCH = chr(_COL)
_OBRCH = chr(_OBR)
_CBRCH = chr(_CBR)


def _left(s: str) -> BaboonEither:
    return BaboonLeft(s)


def _right(v) -> BaboonEither:
    return BaboonRight(v)


def escape_str(s: str) -> str:
    """Backslash-escape the 5 metacharacters per spec §4.2."""
    out = []
    for ch in s:
        c = ord(ch)
        if c == _BS or c == _HSH or c == _COL or c == _OBR or c == _CBR:
            out.append(_BSCH)
        out.append(ch)
    return "".join(out)


def bytes_to_hex(b: bytes) -> str:
    """Lowercase hex, no separators, per spec §3 / §4.4."""
    return b.hex()


def tsu_to_string(dt: datetime.datetime) -> str:
    """Render a `tsu` per spec §3: RFC 3339 UTC, milliseconds, trailing `Z`,
    exactly 24 characters."""
    if dt.tzinfo is not None:
        u = dt.astimezone(datetime.timezone.utc).replace(tzinfo=None)
    else:
        u = dt
    ms = u.microsecond // 1000
    return (
        f"{u.year:04d}-{u.month:02d}-{u.day:02d}T"
        f"{u.hour:02d}:{u.minute:02d}:{u.second:02d}.{ms:03d}Z"
    )


def tso_to_string(dt: datetime.datetime) -> str:
    """Render a `tso` per spec §3: RFC 3339 with explicit `±HH:MM` offset,
    milliseconds, exactly 29 characters. NEVER emits `Z` shorthand."""
    if dt.tzinfo is None:
        # No tzinfo: treat as UTC offset 0.
        offset_sec = 0
        local = dt
    else:
        utcoff = dt.utcoffset() or datetime.timedelta(0)
        offset_sec = int(utcoff.total_seconds())
        local = dt
    ms = local.microsecond // 1000
    sign = "+" if offset_sec >= 0 else "-"
    abs_off = abs(offset_sec)
    oh = abs_off // 3600
    om = (abs_off % 3600) // 60
    return (
        f"{local.year:04d}-{local.month:02d}-{local.day:02d}T"
        f"{local.hour:02d}:{local.minute:02d}:{local.second:02d}.{ms:03d}"
        f"{sign}{oh:02d}:{om:02d}"
    )


def u64_to_string(v: int) -> str:
    """Render an unsigned 64-bit value as decimal. Python `int` is arbitrary
    precision; values in the u64 range render naturally."""
    return str(v)


def bit_to_string(b: bool) -> str:
    """Render a `bit` per spec §3 — exact lowercase ASCII."""
    return "true" if b else "false"


_TSU_RE = re.compile(
    r"^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})\.(\d{3})Z$"
)
_TSO_RE = re.compile(
    r"^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})\.(\d{3})([+-])(\d{2}):(\d{2})$"
)
_UID_RE = re.compile(
    r"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
)


def parse_tsu_repr(s: str) -> BaboonEither:
    if len(s) != 24:
        return _left(f"tsu repr must be 24 chars, got {len(s)}")
    if not s.endswith("Z"):
        return _left(f"tsu repr must end with 'Z', got: {s}")
    m = _TSU_RE.match(s)
    if not m:
        return _left(f"could not parse tsu: {s}")
    try:
        dt = datetime.datetime(
            int(m.group(1)),
            int(m.group(2)),
            int(m.group(3)),
            int(m.group(4)),
            int(m.group(5)),
            int(m.group(6)),
            int(m.group(7)) * 1000,
            tzinfo=datetime.timezone.utc,
        )
        return _right(dt)
    except ValueError:
        return _left(f"could not parse tsu: {s}")


def parse_tso_repr(s: str) -> BaboonEither:
    if len(s) != 29:
        return _left(f"tso repr must be 29 chars, got {len(s)}")
    m = _TSO_RE.match(s)
    if not m:
        return _left(f"could not parse tso: {s}")
    try:
        sign = 1 if m.group(8) == "+" else -1
        oh = int(m.group(9))
        om = int(m.group(10))
        offset = datetime.timezone(
            datetime.timedelta(minutes=sign * (oh * 60 + om))
        )
        dt = datetime.datetime(
            int(m.group(1)),
            int(m.group(2)),
            int(m.group(3)),
            int(m.group(4)),
            int(m.group(5)),
            int(m.group(6)),
            int(m.group(7)) * 1000,
            tzinfo=offset,
        )
        return _right(dt)
    except ValueError:
        return _left(f"could not parse tso: {s}")


def parse_bytes_hex(s: str) -> BaboonEither:
    """Decode `bytes` from lowercase hex. Empty string is legal (empty bytes)."""
    if len(s) == 0:
        return _right(b"")
    if (len(s) & 1) != 0:
        return _left(f"odd-length hex sequence: {s}")
    for ch in s:
        c = ord(ch)
        ok = (0x30 <= c <= 0x39) or (0x61 <= c <= 0x66)
        if not ok:
            return _left(f"non-lowercase or non-hex character in: {s}")
    try:
        return _right(bytes.fromhex(s))
    except ValueError:
        return _left(f"non-lowercase or non-hex character in: {s}")


def parse_bit(s: str) -> BaboonEither:
    if s == "true":
        return _right(True)
    if s == "false":
        return _right(False)
    return _left(f"expected 'true' or 'false' but found '{s}'")


def is_canonical_uid(s: str) -> bool:
    """Lowercase canonical-form check for uid strings (spec §3 / §5.4)."""
    return bool(_UID_RE.match(s))


class BaboonIdReprCursor:
    """Cursor-based parser for parseRepr decoders. Schema-directed; the caller
    (the emitted `<TypeName>Codec.parse_repr`) drives the field sequence per
    declared type and order."""

    def __init__(self, source: str):
        self._source = source
        self._pos = 0

    @property
    def source(self) -> str:
        return self._source

    def position(self) -> int:
        return self._pos

    def at_end(self) -> bool:
        return self._pos >= len(self._source)

    def expect(self, c: str) -> BaboonEither:
        if self._pos >= len(self._source):
            return _left(f"expected '{c}' at {self._pos} but reached end of input")
        ch = self._source[self._pos]
        if ch != c:
            return _left(f"expected '{c}' at {self._pos} but found '{ch}'")
        self._pos += 1
        return _right(None)

    def expect_literal(self, lit: str) -> BaboonEither:
        if self._pos + len(lit) > len(self._source):
            return _left(
                f"expected literal '{lit}' at {self._pos} but reached end of input"
            )
        for i, ch in enumerate(lit):
            if self._source[self._pos + i] != ch:
                return _left(f"expected literal '{lit}' at {self._pos}")
        self._pos += len(lit)
        return _right(None)

    def read_until_structural(self) -> str:
        """Read until the next bare metachar in `:#{}`. Backslash is NOT a stop
        here — see read_str_field for str-field consumption."""
        start = self._pos
        while self._pos < len(self._source):
            c = ord(self._source[self._pos])
            if c == _COL or c == _HSH or c == _OBR or c == _CBR:
                break
            self._pos += 1
        return self._source[start:self._pos]

    def read_fixed(self, n: int) -> BaboonEither:
        """Consume exactly n characters as a fixed-width lexeme (tsu/tso)."""
        if self._pos + n > len(self._source):
            return _left(
                f"expected {n} chars at {self._pos} but only {len(self._source) - self._pos} remain"
            )
        out = self._source[self._pos:self._pos + n]
        self._pos += n
        return _right(out)

    def read_str_field(self) -> BaboonEither:
        """Read a `str` field value with backslash-unescaping per spec §5.5."""
        out = []
        while self._pos < len(self._source):
            c = ord(self._source[self._pos])
            if c == _COL or c == _HSH or c == _OBR or c == _CBR:
                return _right("".join(out))
            if c == _BS:
                if self._pos + 1 >= len(self._source):
                    return _left(f"trailing backslash at {self._pos}")
                nxt = ord(self._source[self._pos + 1])
                if nxt == _BS or nxt == _HSH or nxt == _COL or nxt == _OBR or nxt == _CBR:
                    out.append(self._source[self._pos + 1])
                    self._pos += 2
                else:
                    return _left(f"invalid escape at {self._pos}")
            else:
                out.append(self._source[self._pos])
                self._pos += 1
        return _right("".join(out))


def parse_header(
    cursor: BaboonIdReprCursor,
    expected_simple_name: str,
    expected_version: str,
) -> BaboonEither:
    """Validate header of an identifier-repr: `<simpleName>:<version>#`."""
    name_lit = cursor.read_until_structural()
    if name_lit != expected_simple_name:
        return _left(f"expected name '{expected_simple_name}' but found '{name_lit}'")
    r1 = cursor.expect(_COLCH)
    if isinstance(r1, BaboonLeft):
        return r1
    ver_lit = cursor.read_until_structural()
    if ver_lit != expected_version:
        return _left(f"expected version '{expected_version}' but found '{ver_lit}'")
    return cursor.expect(_HSHCH)


def parse_field_name(
    cursor: BaboonIdReprCursor,
    expected_field_name: str,
) -> BaboonEither:
    """Validate field-name segment: `<expectedFieldName>:`."""
    name = cursor.read_until_structural()
    if name != expected_field_name:
        return _left(f"expected field name '{expected_field_name}' but found '{name}'")
    return cursor.expect(_COLCH)
