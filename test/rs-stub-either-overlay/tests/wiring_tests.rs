use baboon_rs_stub::baboon_runtime::{BaboonCodecContext, BaboonBinEncode, BaboonBinDecode};
use baboon_rs_stub::baboon_service_wiring::{BaboonMethodId, BaboonWiringError, JsonMuxer, UebaMuxer};
use baboon_rs_stub::testpkg::pkg0::baboon_service_rt::BaboonServiceRtDefault;
use baboon_rs_stub::testpkg::pkg0::i1::I1;
use baboon_rs_stub::testpkg::pkg0::i1::testcall::input::In as I1_testCall_in;
use baboon_rs_stub::testpkg::pkg0::i1::testcall::out::Out as I1_testCall_out;
use baboon_rs_stub::testpkg::pkg0::i1::testcall::err::Err as I1_testCall_err;
use baboon_rs_stub::testpkg::pkg0::i2::I2;
use baboon_rs_stub::testpkg::pkg0::i2::noerrcall::input::In as I2_noErrCall_in;
use baboon_rs_stub::testpkg::pkg0::i2::noerrcall::out::Out as I2_noErrCall_out;
use baboon_rs_stub::testpkg::pkg0::t7_empty::T7_Empty;
use baboon_rs_stub::testpkg::pkg0::i1_wiring::{invoke_json_i1, invoke_ueba_i1, I1JsonService, I1UebaService};
use baboon_rs_stub::testpkg::pkg0::i2_wiring::{invoke_json_i2, invoke_ueba_i2, I2JsonService, I2UebaService};

struct MockI1;
impl I1 for MockI1 {
    fn test_call(&self, _arg: I1_testCall_in) -> Result<I1_testCall_out, I1_testCall_err> {
        Ok(I1_testCall_out { i00: 42 })
    }
    fn test_call2(&self, _arg: T7_Empty) -> Result<T7_Empty, T7_Empty> {
        Ok(T7_Empty {})
    }
}

struct FailingI1;
impl I1 for FailingI1 {
    fn test_call(&self, _arg: I1_testCall_in) -> Result<I1_testCall_out, I1_testCall_err> {
        Err(I1_testCall_err { msg: "domain error".to_string() })
    }
    fn test_call2(&self, _arg: T7_Empty) -> Result<T7_Empty, T7_Empty> {
        Err(T7_Empty {})
    }
}

struct MockI2;
impl I2 for MockI2 {
    fn no_err_call(&self, arg: I2_noErrCall_in) -> I2_noErrCall_out {
        I2_noErrCall_out { result: format!("result_{}", arg.value) }
    }
}

#[test]
fn i1_json_test_call_success() {
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "testCall".to_string() };
    let input_json = serde_json::to_string(&I1_testCall_in {}).unwrap();
    let rt = BaboonServiceRtDefault;
    let ctx = BaboonCodecContext::Default;
    let result = invoke_json_i1(&method, &input_json, &MockI1, &rt, &ctx);
    assert!(result.is_ok());
    let decoded: I1_testCall_out = serde_json::from_str(&result.unwrap()).unwrap();
    assert_eq!(decoded.i00, 42);
}

#[test]
fn i1_json_test_call2_success() {
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "testCall2".to_string() };
    let input_json = serde_json::to_string(&T7_Empty {}).unwrap();
    let rt = BaboonServiceRtDefault;
    let ctx = BaboonCodecContext::Default;
    let result = invoke_json_i1(&method, &input_json, &MockI1, &rt, &ctx);
    assert!(result.is_ok());
}

#[test]
fn i1_json_domain_error() {
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "testCall".to_string() };
    let input_json = serde_json::to_string(&I1_testCall_in {}).unwrap();
    let rt = BaboonServiceRtDefault;
    let ctx = BaboonCodecContext::Default;
    let result = invoke_json_i1(&method, &input_json, &FailingI1, &rt, &ctx);
    assert!(result.is_err());
}

#[test]
fn i1_json_unknown_method() {
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "nonexistent".to_string() };
    let rt = BaboonServiceRtDefault;
    let ctx = BaboonCodecContext::Default;
    let result = invoke_json_i1(&method, "{}", &MockI1, &rt, &ctx);
    assert!(result.is_err());
}

#[test]
fn i1_json_bad_input() {
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "testCall".to_string() };
    let rt = BaboonServiceRtDefault;
    let ctx = BaboonCodecContext::Default;
    let result = invoke_json_i1(&method, "not valid json!!", &MockI1, &rt, &ctx);
    assert!(result.is_err());
}

#[test]
fn i1_ueba_test_call_success() {
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "testCall".to_string() };
    let ctx = BaboonCodecContext::Default;
    let rt = BaboonServiceRtDefault;
    let mut writer = Vec::new();
    BaboonBinEncode::encode_ueba(&I1_testCall_in {}, &ctx, &mut writer).unwrap();
    let result = invoke_ueba_i1(&method, &writer, &MockI1, &rt, &ctx);
    assert!(result.is_ok());
    let mut cursor = std::io::Cursor::new(result.unwrap());
    let decoded: I1_testCall_out = BaboonBinDecode::decode_ueba(&ctx, &mut cursor).unwrap();
    assert_eq!(decoded.i00, 42);
}

#[test]
fn i1_ueba_unknown_method() {
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "nonexistent".to_string() };
    let rt = BaboonServiceRtDefault;
    let ctx = BaboonCodecContext::Default;
    let result = invoke_ueba_i1(&method, &[], &MockI1, &rt, &ctx);
    assert!(result.is_err());
}

#[test]
fn i2_json_no_err_call_success() {
    let method = BaboonMethodId { service_name: "I2".to_string(), method_name: "noErrCall".to_string() };
    let input_json = serde_json::to_string(&I2_noErrCall_in { value: 123 }).unwrap();
    let rt = BaboonServiceRtDefault;
    let ctx = BaboonCodecContext::Default;
    let result = invoke_json_i2(&method, &input_json, &MockI2, &rt, &ctx);
    assert!(result.is_ok());
    let decoded: I2_noErrCall_out = serde_json::from_str(&result.unwrap()).unwrap();
    assert_eq!(decoded.result, "result_123");
}

#[test]
fn i2_ueba_no_err_call_success() {
    let method = BaboonMethodId { service_name: "I2".to_string(), method_name: "noErrCall".to_string() };
    let ctx = BaboonCodecContext::Default;
    let rt = BaboonServiceRtDefault;
    let mut writer = Vec::new();
    BaboonBinEncode::encode_ueba(&I2_noErrCall_in { value: 456 }, &ctx, &mut writer).unwrap();
    let result = invoke_ueba_i2(&method, &writer, &MockI2, &rt, &ctx);
    assert!(result.is_ok());
    let mut cursor = std::io::Cursor::new(result.unwrap());
    let decoded: I2_noErrCall_out = BaboonBinDecode::decode_ueba(&ctx, &mut cursor).unwrap();
    assert_eq!(decoded.result, "result_456");
}

// ==================== Cross-domain Muxer ====================
// A single muxer composes the I1 (errors mode) and I2 (no-err mode)
// services and routes each call by method.service_name. The outer Result
// is the muxer's routing result; the inner is the service's own result.

type JsonR = Result<String, BaboonWiringError>;
type UebaR = Result<Vec<u8>, BaboonWiringError>;

fn new_json_muxer() -> JsonMuxer<JsonR> {
    JsonMuxer::<JsonR>::new()
        .with(Box::new(I1JsonService::new(MockI1, BaboonServiceRtDefault))).unwrap()
        .with(Box::new(I2JsonService::new(MockI2, BaboonServiceRtDefault))).unwrap()
}

fn new_ueba_muxer() -> UebaMuxer<UebaR> {
    UebaMuxer::<UebaR>::new()
        .with(Box::new(I1UebaService::new(MockI1, BaboonServiceRtDefault))).unwrap()
        .with(Box::new(I2UebaService::new(MockI2, BaboonServiceRtDefault))).unwrap()
}

#[test]
fn json_muxer_routes_to_i1() {
    let ctx = BaboonCodecContext::Default;
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "testCall".to_string() };
    let input_json = serde_json::to_string(&I1_testCall_in {}).unwrap();
    let routed = new_json_muxer().invoke(&method, &input_json, &ctx).expect("routing");
    let decoded: I1_testCall_out = serde_json::from_str(&routed.unwrap()).unwrap();
    assert_eq!(decoded.i00, 42);
}

#[test]
fn json_muxer_routes_to_i2() {
    let ctx = BaboonCodecContext::Default;
    let method = BaboonMethodId { service_name: "I2".to_string(), method_name: "noErrCall".to_string() };
    let input_json = serde_json::to_string(&I2_noErrCall_in { value: 123 }).unwrap();
    let routed = new_json_muxer().invoke(&method, &input_json, &ctx).expect("routing");
    let decoded: I2_noErrCall_out = serde_json::from_str(&routed.unwrap()).unwrap();
    assert_eq!(decoded.result, "result_123");
}

#[test]
fn json_muxer_no_matching_service() {
    let ctx = BaboonCodecContext::Default;
    let method = BaboonMethodId { service_name: "Nonexistent".to_string(), method_name: "x".to_string() };
    let routed = new_json_muxer().invoke(&method, "{}", &ctx);
    assert!(matches!(routed, Err(BaboonWiringError::NoMatchingService(_))));
}

#[test]
fn json_muxer_duplicate_service() {
    let dup = JsonMuxer::<JsonR>::new()
        .with(Box::new(I1JsonService::new(MockI1, BaboonServiceRtDefault))).unwrap()
        .with(Box::new(I1JsonService::new(MockI1, BaboonServiceRtDefault)));
    assert!(matches!(dup, Err(BaboonWiringError::DuplicateService(_))));
}

#[test]
fn ueba_muxer_routes_to_i1() {
    let ctx = BaboonCodecContext::Default;
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "testCall".to_string() };
    let mut writer = Vec::new();
    BaboonBinEncode::encode_ueba(&I1_testCall_in {}, &ctx, &mut writer).unwrap();
    let routed = new_ueba_muxer().invoke(&method, &writer, &ctx).expect("routing");
    let mut cursor = std::io::Cursor::new(routed.unwrap());
    let decoded: I1_testCall_out = BaboonBinDecode::decode_ueba(&ctx, &mut cursor).unwrap();
    assert_eq!(decoded.i00, 42);
}

#[test]
fn ueba_muxer_routes_to_i2() {
    let ctx = BaboonCodecContext::Default;
    let method = BaboonMethodId { service_name: "I2".to_string(), method_name: "noErrCall".to_string() };
    let mut writer = Vec::new();
    BaboonBinEncode::encode_ueba(&I2_noErrCall_in { value: 456 }, &ctx, &mut writer).unwrap();
    let routed = new_ueba_muxer().invoke(&method, &writer, &ctx).expect("routing");
    let mut cursor = std::io::Cursor::new(routed.unwrap());
    let decoded: I2_noErrCall_out = BaboonBinDecode::decode_ueba(&ctx, &mut cursor).unwrap();
    assert_eq!(decoded.result, "result_456");
}

#[test]
fn ueba_muxer_no_matching_service() {
    let ctx = BaboonCodecContext::Default;
    let method = BaboonMethodId { service_name: "Nonexistent".to_string(), method_name: "x".to_string() };
    let routed = new_ueba_muxer().invoke(&method, &[], &ctx);
    assert!(matches!(routed, Err(BaboonWiringError::NoMatchingService(_))));
}
