package java.time

class OffsetDateTime {
  def isBefore(value: OffsetDateTime): Boolean = ???

}
class OffsetTime {
  def isBefore(value: OffsetTime): Boolean = ???

}
class LocalTime {
  def isBefore(value: LocalTime): Boolean = ???

}

class LocalDate {
  def isBefore(value: java.time.chrono.ChronoLocalDate): Boolean = ???
}
class LocalDateTime {
  def isBefore(value: LocalDateTime): Boolean                           = ???
  def isBefore(value: java.time.chrono.ChronoLocalDate): Boolean        = ???
  def isBefore(value: java.time.chrono.ChronoLocalDateTime[?]): Boolean = ???

}
class Instant {
  def isBefore(value: Instant): Boolean = ???

}
