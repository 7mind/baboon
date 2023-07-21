package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NonEmptyMap

case class BaboonLineage(pkg: Pkg, versions: NonEmptyMap[Version, Domain])
