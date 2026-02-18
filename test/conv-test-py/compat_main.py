import io
import json
import sys
from datetime import datetime, timezone, timedelta
from decimal import Decimal
from pathlib import Path
from uuid import UUID

from Generated.baboon_codecs import BaboonCodecContext
from Generated.baboon_runtime_shared import LEDataOutputStream, LEDataInputStream
from Generated.convtest.testpkg.AllBasicTypes import AllBasicTypes, AllBasicTypes_JsonCodec, AllBasicTypes_UEBACodec


def create_sample_data():
    return AllBasicTypes(
        vi8=42,
        vi16=1234,
        vi32=123456,
        vi64=123456789,
        vu8=200,
        vu16=50000,
        vu32=3000000000,
        vu64=10000000000,
        vf32=3.14159,
        vf64=2.718281828,
        vf128=Decimal("123456789.987654321"),
        vstr="Hello, Baboon!",
        vbstr=b'Hello Bytes',
        vuid=UUID("12345678-1234-5678-1234-567812345678"),
        vbit=True,
        vtsu=datetime(2024, 6, 15, 12, 30, 45, 123000, tzinfo=timezone.utc),
        vtso=datetime(2024, 6, 15, 14, 30, 45, 987000, tzinfo=timezone(timedelta(hours=2))),
        voptStr="optional value",
        vlstI32=[1, 2, 3, 4, 5],
        vsetStr={"apple", "banana", "cherry"},
        vmapStrI32={"one": 1, "two": 2, "three": 3},
        voptLst=["nested", "list", "values"],
        vlstOpt=[10, None, 20, 30],
        vmapLst={"numbers": [1, 2, 3], "more": [4, 5, 6]},
    )


def write_json(ctx, data, output_dir):
    json_str = AllBasicTypes_JsonCodec.instance().encode(ctx, data)
    json_file_path = Path(output_dir) / "all-basic-types.json"
    with open(json_file_path, "w", encoding="utf-8") as f:
        f.write(json_str)
    print(f"Written JSON to {json_file_path}")


def write_ueba(ctx, data, output_dir):
    memory_stream = io.BytesIO()
    ueba_writer = LEDataOutputStream(memory_stream)
    AllBasicTypes_UEBACodec.instance().encode(ctx, ueba_writer, data)
    ueba_bytes = memory_stream.getvalue()
    ueba_file_path = Path(output_dir) / "all-basic-types.ueba"
    with open(ueba_file_path, "wb") as f:
        f.write(ueba_bytes)
    print(f"Written UEBA to {ueba_file_path}")


def read_and_verify(file_path):
    ctx = BaboonCodecContext.default()
    fp = Path(file_path)

    try:
        if fp.suffix == ".json":
            with open(fp, "r", encoding="utf-8") as f:
                json_str = f.read()
            data = AllBasicTypes_JsonCodec.instance().decode(ctx, json_str)
        elif fp.suffix == ".ueba":
            with open(fp, "rb") as f:
                ueba_bytes = f.read()
            memory_stream = io.BytesIO(ueba_bytes)
            ueba_reader = LEDataInputStream(memory_stream)
            data = AllBasicTypes_UEBACodec.instance().decode(ctx, ueba_reader)
        else:
            print(f"Unknown file extension: {fp.suffix}", file=sys.stderr)
            sys.exit(1)
    except Exception as e:
        print(f"Deserialization failed: {e}", file=sys.stderr)
        sys.exit(1)

    if data.vstr != "Hello, Baboon!":
        print(f"vstr mismatch: expected 'Hello, Baboon!', got '{data.vstr}'", file=sys.stderr)
        sys.exit(1)
    if data.vi32 != 123456:
        print(f"vi32 mismatch: expected 123456, got {data.vi32}", file=sys.stderr)
        sys.exit(1)
    if not data.vbit:
        print(f"vbit mismatch: expected True, got {data.vbit}", file=sys.stderr)
        sys.exit(1)

    # Roundtrip
    try:
        if fp.suffix == ".json":
            re_encoded = AllBasicTypes_JsonCodec.instance().encode(ctx, data)
            re_decoded = AllBasicTypes_JsonCodec.instance().decode(ctx, re_encoded)
            if data != re_decoded:
                print("JSON roundtrip mismatch", file=sys.stderr)
                sys.exit(1)
        else:
            ms = io.BytesIO()
            w = LEDataOutputStream(ms)
            AllBasicTypes_UEBACodec.instance().encode(ctx, w, data)
            re_bytes = ms.getvalue()
            ms2 = io.BytesIO(re_bytes)
            r = LEDataInputStream(ms2)
            re_decoded = AllBasicTypes_UEBACodec.instance().decode(ctx, r)
            if data != re_decoded:
                print("UEBA roundtrip mismatch", file=sys.stderr)
                sys.exit(1)
    except Exception as e:
        print(f"Roundtrip failed: {e}", file=sys.stderr)
        sys.exit(1)

    print("OK")


def run_legacy():
    sample_data = create_sample_data()
    ctx = BaboonCodecContext.default()

    base_dir = Path("../../target/compat-test").resolve()
    json_dir = base_dir / "python-json"
    ueba_dir = base_dir / "python-ueba"

    json_dir.mkdir(parents=True, exist_ok=True)
    ueba_dir.mkdir(parents=True, exist_ok=True)

    write_json(ctx, sample_data, str(json_dir))
    write_ueba(ctx, sample_data, str(ueba_dir))

    print("Python serialization complete!")


if __name__ == "__main__":
    args = sys.argv[1:]
    if len(args) >= 1 and args[0] == "write":
        output_dir = args[1]
        fmt = args[2]
        Path(output_dir).mkdir(parents=True, exist_ok=True)
        sample_data = create_sample_data()
        ctx = BaboonCodecContext.default()
        if fmt == "json":
            write_json(ctx, sample_data, output_dir)
        elif fmt == "ueba":
            write_ueba(ctx, sample_data, output_dir)
        else:
            print(f"Unknown format: {fmt}", file=sys.stderr)
            sys.exit(1)
    elif len(args) >= 1 and args[0] == "read":
        read_and_verify(args[1])
    else:
        run_legacy()
