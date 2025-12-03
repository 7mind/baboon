import io
from datetime import datetime, timezone, timedelta

from decimal import Decimal
from pathlib import Path
from uuid import UUID

from Generated.convtest.testpkg.AllBasicTypes import AllBasicTypes, AllBasicTypes_JsonCodec, AllBasicTypes_UEBACodec
from Generated.baboon_codecs import BaboonCodecContext, LEDataInputStream, LEDataOutputStream


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
        vtsu=datetime(2024, 6, 15, 12, 30, 45, 123456, tzinfo=timezone.utc),
        vtso=datetime(2024, 6, 15, 14, 30, 45, 987654, tzinfo=timezone(timedelta(hours=2))),
        voptStr="optional value",
        vlstI32=[1, 2, 3, 4, 5],
        vsetStr={"apple", "banana", "cherry"},
        vmapStrI32={"one": 1, "two": 2, "three": 3},
        voptLst=["nested", "list", "values"],
        vlstOpt=[10, None, 20, 30],
        vmapLst={"numbers": [1, 2, 3], "more": [4, 5, 6]},
    )


# create sample data will all basic types
sample_data = create_sample_data()

# Create output directories - use absolute path relative to project root
base_dir = Path("../../target/compat-test").resolve()
json_dir = base_dir / "python-json"
ueba_dir = base_dir / "python-ueba"

json_dir.mkdir(parents=True, exist_ok=True)
ueba_dir.mkdir(parents=True, exist_ok=True)

# Serialize to JSON
json_str = AllBasicTypes_JsonCodec.instance().encode(sample_data)
json_file_path = json_dir / "all-basic-types.json"
with open(json_file_path, "w", encoding="utf-8") as f:
    f.write(json_str)

print(f"Written JSON to {json_file_path}")

# Serialize to UEBA
memory_stream = io.BytesIO()
ueba_writer = LEDataOutputStream(memory_stream)
AllBasicTypes_UEBACodec.instance().encode(BaboonCodecContext.default(), ueba_writer, sample_data)
ueba_bytes = memory_stream.getvalue()

ueba_file_path = ueba_dir / "all-basic-types.ueba"
with open(ueba_file_path, "wb") as f:
    f.write(ueba_bytes)

print(f"Written UEBA to {ueba_file_path}")

print("Python serialization complete!")
