package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.translator.python.PyValue.{PyModuleId, PyType}

object PyTypes {
  // baboon

  // baboon modules
  val pyBaboonSharedRuntimeModule: PyModuleId = PyModuleId(s"baboon_runtime_shared")
  val pyBaboonCodecsModule: PyModuleId        = PyModuleId(s"baboon_codecs")

  // python modules
  val pyBuiltins: PyModuleId          = PyModuleId("builtins")
  val pyUUUIDModule: PyModuleId       = PyModuleId("uuid")
  val pyDateTimeModule: PyModuleId    = PyModuleId("datetime")
  val pyTypingModule: PyModuleId      = PyModuleId("typing")
  val pyEnumModule: PyModuleId        = PyModuleId("enum")
  val pyAbcModule: PyModuleId         = PyModuleId("abc")
  val pyJson: PyModuleId              = PyModuleId("json")
  val pyFuncTools: PyModuleId         = PyModuleId("functools")
  val pyUnittest: PyModuleId          = PyModuleId("unittest")
  val pyIO: PyModuleId                = PyModuleId("io")
  val pyDecimalModule: PyModuleId     = PyModuleId("decimal")
  val pyCollections: PyModuleId       = PyModuleId("collections")

  // python external modules
  val pydantic: PyModuleId = PyModuleId("pydantic")

  // baboon meta
  val baboonMeta: PyType             = PyType(pyBaboonSharedRuntimeModule, "BaboonMeta")
  val iBaboonGenerated: PyType       = PyType(pyBaboonSharedRuntimeModule, "IBaboonGenerated")
  val iBaboonGeneratedLatest: PyType = PyType(pyBaboonSharedRuntimeModule, "IBaboonGeneratedLatest")
  val iBaboonAdtMemberMeta: PyType   = PyType(pyBaboonSharedRuntimeModule, "IBaboonAdtMemberMeta")

  // baboon codecs
  val baboonJsonCodec: PyType                  = PyType(pyBaboonCodecsModule, "BaboonJsonCodec")
  val baboonUEBACodec: PyType                  = PyType(pyBaboonCodecsModule, "BaboonUEBACodec")
  def abstractBaboonCodecs(id: String): PyType = PyType(pyBaboonCodecsModule, s"AbstractBaboon${id.capitalize}Codecs")
  val baboonCodecContext: PyType               = PyType(pyBaboonCodecsModule, "BaboonCodecContext")
  val baboonUEBACodecIndexed: PyType           = PyType(pyBaboonCodecsModule, "BaboonBinCodecIndexed")

  // baboon conversions
  val baboonAbstractConversion: PyType  = PyType(pyBaboonSharedRuntimeModule, "BaboonAbstractConversion")
  val baboonAbstractConversions: PyType = PyType(pyBaboonSharedRuntimeModule, "BaboonAbstractConversions")

  // baboon types
  val deprecated: PyType               = PyType(pyBaboonSharedRuntimeModule, "deprecated")
  val baboonFixture: PyType            = PyType(pyBaboonSharedRuntimeModule, "Fixture")
  val baboonLEDataOutputStream: PyType = PyType(pyBaboonCodecsModule, "LEDataOutputStream")
  val baboonLEDataInputStream: PyType  = PyType(pyBaboonCodecsModule, "LEDataInputStream")
  val baboonSafeList: PyType           = PyType(pyBaboonCodecsModule, "SafeList")
  val baboonSafeSet: PyType            = PyType(pyBaboonCodecsModule, "SafeSet")

  // python

  // builtins
  val pyInt          = PyType(pyBuiltins, "int")
  val pyFloat        = PyType(pyBuiltins, "float")
  val pyDecimal      = PyType(pyDecimalModule, "Decimal")
  val pyBool         = PyType(pyBuiltins, "bool")
  val pyStr          = PyType(pyBuiltins, "str")
  val pyType         = PyType(pyBuiltins, "type")
  val pyDict         = PyType(pyBuiltins, "dict")
  val pySet          = PyType(pyBuiltins, "set")
  val pyList         = PyType(pyBuiltins, "list")
  val pyClassMethod  = PyType(pyBuiltins, "classmethod")
  val pyStaticMethod = PyType(pyBuiltins, "staticmethod")
  val pyBytes        = PyType(pyBuiltins, "bytes")

  // unittest
  val pyTestCase = PyType(pyUnittest, "TestCase")

  // io
  val pyBytesIO = PyType(pyIO, "BytesIO")

  // functools
  val pyCache = PyType(pyFuncTools, "cache")

  // pydantic
  val pydanticField           = PyType(pydantic, "Field")
  val pydanticBaseModel       = PyType(pydantic, "BaseModel")
  val pydanticConfigDict      = PyType(pydantic, "ConfigDict")
  val pydanticModelSerializer = PyType(pydantic, "model_serializer")
  val pydanticModelValidator  = PyType(pydantic, "model_validator")

  // typing
  val pyOpt      = PyType(pyTypingModule, "Optional")
  val pyOverride = PyType(pyTypingModule, "override")
  val pyClassVar = PyType(pyTypingModule, "ClassVar")

  // json
  val pyJsonDumps = PyType(pyJson, "dumps")
  val pyJsonLoads = PyType(pyJson, "loads")

  // uuid
  val pyUuid = PyType(pyUUUIDModule, "UUID")

  // datetime
  val pyDateTime = PyType(pyDateTimeModule, "datetime")

  // enum
  val pyEnum = PyType(pyEnumModule, "Enum")

  // abc
  val pyABC: PyType    = PyType(pyAbcModule, "ABC")
  val pyAbstractMethod = PyType(pyAbcModule, "abstractmethod")

  // collections
  val pyDefaultDict: PyType = PyType(pyCollections, "defaultdict")
}
