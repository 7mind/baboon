package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.translator.dart.DtTypes.*
import io.septimalmind.baboon.typer.model.{TypeId, TypeRef}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

// `bytes` is a `Uint8List`, whose `==` is identity: maps keyed by it and sets of it are built with
// content equality so a lookup by an equal fresh `Uint8List` finds the entry.
private[dart] object DtCollectionBuilders {
  private def isBytes(tpe: TypeRef): Boolean = tpe == TypeRef.Scalar(TypeId.Builtins.bytes)

  def mapFromEntries(key: TypeRef, entries: TextTree[DtValue]): TextTree[DtValue] =
    if (isBytes(key)) q"$baboonBytesKeyedMap($entries)" else q"Map.fromEntries($entries)"

  def setFrom(element: TypeRef, elements: TextTree[DtValue]): TextTree[DtValue] =
    if (isBytes(element)) q"$baboonBytesSet($elements)" else q"$elements.toSet()"

  def emptyMap(key: TypeRef, keyType: TextTree[DtValue], valueType: TextTree[DtValue]): TextTree[DtValue] =
    if (isBytes(key)) q"$baboonBytesKeyedMap(<MapEntry<$keyType, $valueType>>[])" else q"<$keyType, $valueType>{}"

  def emptySet(element: TypeRef, elementType: TextTree[DtValue]): TextTree[DtValue] =
    if (isBytes(element)) q"$baboonBytesSet(<$elementType>[])" else q"<$elementType>{}"
}
