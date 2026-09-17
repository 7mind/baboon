package io.septimalmind.baboon.translator.schema

import io.circe.Json

object JsonSchema {
  def nullable(inner: Json): Json = Json.obj("oneOf" -> Json.arr(inner, Json.obj("type" -> Json.fromString("null"))))
  def array(items: Json): Json = Json.obj("type" -> Json.fromString("array"), "items" -> items)
  def uniqueArray(items: Json): Json = array(items).mapObject(_.add("uniqueItems", Json.True))

  def objectMap(value: Json, propertyNames: Option[Json]): Json = {
    val base = Json.obj("type" -> Json.fromString("object"), "additionalProperties" -> value)
    propertyNames.fold(base)(names => base.mapObject(_.add("propertyNames", names)))
  }

  def entryMap(key: Json, value: Json): Json = array(Json.obj(
    "type" -> Json.fromString("object"),
    "required" -> Json.arr(Json.fromString("key"), Json.fromString("value")),
    "properties" -> Json.obj("key" -> key, "value" -> value),
  ))
}
