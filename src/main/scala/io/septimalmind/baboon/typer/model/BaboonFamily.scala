package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NonEmptyMap

case class BaboonFamily(domains: NonEmptyMap[Pkg, BaboonLineage])
