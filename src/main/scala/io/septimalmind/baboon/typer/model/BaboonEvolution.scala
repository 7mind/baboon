package io.septimalmind.baboon.typer.model

case class BaboonEvolution(pkg: Pkg,
                           latest: Version,
                           diffs: Map[Version, BaboonDiff])

case class BaboonDiff()