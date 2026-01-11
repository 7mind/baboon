package io.septimalmind.baboon.lsp.features

import org.scalatest.wordspec.AnyWordSpec

final class CamelCaseMatcherTest extends AnyWordSpec {

  "CamelCaseMatcher" should {
    "match exact strings" in {
      assert(CamelCaseMatcher.matches("PaymentState", "PaymentState"))
      assert(CamelCaseMatcher.matches("i32", "i32"))
    }

    "match prefix (case-insensitive)" in {
      assert(CamelCaseMatcher.matches("pay", "PaymentState"))
      assert(CamelCaseMatcher.matches("Pay", "PaymentState"))
      assert(CamelCaseMatcher.matches("payment", "PaymentState"))
      assert(CamelCaseMatcher.matches("Payment", "PaymentState"))
    }

    "match camel case humps" in {
      assert(CamelCaseMatcher.matches("PS", "PaymentState"))
      assert(CamelCaseMatcher.matches("PaSta", "PaymentState"))
      assert(CamelCaseMatcher.matches("PSt", "PaymentState"))
      assert(CamelCaseMatcher.matches("PaS", "PaymentState"))
    }

    "match mixed case queries" in {
      assert(CamelCaseMatcher.matches("paySt", "PaymentState"))
      assert(CamelCaseMatcher.matches("payS", "PaymentState"))
    }

    "match with dot-separated paths" in {
      assert(CamelCaseMatcher.matches("ns.T", "ns.TestType"))
      assert(CamelCaseMatcher.matches("ns.TT", "ns.TestType"))
      assert(CamelCaseMatcher.matches("TT", "ns.TestType"))
    }

    "match with underscores" in {
      assert(CamelCaseMatcher.matches("s_n", "some_name"))
      assert(CamelCaseMatcher.matches("SN", "Some_Name"))
    }

    "reject non-matching queries" in {
      assert(!CamelCaseMatcher.matches("XYZ", "PaymentState"))
      assert(!CamelCaseMatcher.matches("SP", "PaymentState"))
      assert(!CamelCaseMatcher.matches("StatePayment", "PaymentState"))
      assert(!CamelCaseMatcher.matches("Paymment", "PaymentState"))
    }

    "handle empty strings" in {
      assert(CamelCaseMatcher.matches("", "PaymentState"))
      assert(!CamelCaseMatcher.matches("P", ""))
      assert(CamelCaseMatcher.matches("", ""))
    }

    "match multi-hump types" in {
      assert(CamelCaseMatcher.matches("UCSRB", "UserConfigServiceResponseBody"))
      assert(CamelCaseMatcher.matches("UConf", "UserConfigServiceResponseBody"))
      assert(CamelCaseMatcher.matches("UCon", "UserConfigServiceResponseBody"))
    }
  }
}
