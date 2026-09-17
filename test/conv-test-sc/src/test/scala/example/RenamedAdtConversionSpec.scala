package example

import org.scalatest.funsuite.AnyFunSuite

class RenamedAdtConversionSpec extends AnyFunSuite {
  private val conversions = new convtest.testpkg.BaboonConversions(new convtest.testpkg.RequiredConversions {})

  test("renamed parent retains branch payload") {
    val result = conversions.convertWithContext[Unit, convtest.testpkg.v1_0_0.OldAdtName, convtest.testpkg.NewAdtName](
      None,
      convtest.testpkg.v1_0_0.OldAdtName.Branch1("kept"),
    )
    assert(result == convtest.testpkg.NewAdtName.Branch1("kept"))
  }

  test("renamed branch adds optional default") {
    val result = conversions.convertWithContext[Unit, convtest.testpkg.v1_0_0.AdtBranchRename, convtest.testpkg.AdtBranchRename](
      None,
      convtest.testpkg.v1_0_0.AdtBranchRename.OldBranch("kept"),
    )
    assert(result == convtest.testpkg.AdtBranchRename.NewBranch("kept", None))
  }

  test("namespace-renamed ADT branches use qualified target types") {
    val result = conversions.convertWithContext[Unit, convtest.testpkg.v1_0_0.abs.core.OldAbsAdt, convtest.testpkg.abs.core.deeper.NewAbsAdt](
      None,
      convtest.testpkg.v1_0_0.abs.core.OldAbsAdt.BranchAbs("kept"),
    )
    assert(result == convtest.testpkg.abs.core.deeper.NewAbsAdt.BranchAbs("kept"))
  }
}
