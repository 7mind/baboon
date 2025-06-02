package io.septimalmind.baboon.translator.scl
import io.septimalmind.baboon.translator.csharp.CSTypes.baboonRuntimePkg
import io.septimalmind.baboon.translator.csharp.CSValue.CSType
import io.septimalmind.baboon.translator.scl.ScValue.{ScPackageId, ScType}
import izumi.fundamentals.collections.nonempty.NEList

object ScTypes {
  val baboonRuntimePkg: ScPackageId = ScPackageId(NEList("_root_", "baboon", "runtime", "shared"))

  val baboonTypeCodecs: ScType = ScType(baboonRuntimePkg, "BaboonTypeCodecs", fq = false)

  val iBaboonGenerated: ScType          = ScType(baboonRuntimePkg, "BaboonGenerated", fq = false)
  val iBaboonAdtMemberMeta: ScType      = ScType(baboonRuntimePkg, "BaboonAdtMemberMeta", fq = false)
  val iBaboonGeneratedLatest: ScType    = ScType(baboonRuntimePkg, "BaboonGeneratedLatest", fq = false)


  val scalaPkg: ScPackageId = ScPackageId(NEList("_root_", "scala"))

  val deprecated: ScType = ScType(scalaPkg, "deprecated", fq = false)

}
