use baboon_rs_stub::baboon_runtime::{BaboonCodecContext, BaboonBinEncode, BaboonBinDecode};
use baboon_rs_stub::baboon_service_wiring::{BaboonMethodId, BaboonWiringError};
use baboon_rs_stub::custom_containers::{MyResult, ResultServiceRt};
use baboon_rs_stub::testpkg::pkg0::i1::I1;
use baboon_rs_stub::testpkg::pkg0::i1::testcall::r#in::In as I1_testCall_in;
use baboon_rs_stub::testpkg::pkg0::i1::testcall::out::Out as I1_testCall_out;
use baboon_rs_stub::testpkg::pkg0::i1::testcall::err::Err as I1_testCall_err;
use baboon_rs_stub::testpkg::pkg0::i2::I2;
use baboon_rs_stub::testpkg::pkg0::i2::noerrcall::r#in::In as I2_noErrCall_in;
use baboon_rs_stub::testpkg::pkg0::i2::noerrcall::out::Out as I2_noErrCall_out;
use baboon_rs_stub::testpkg::pkg0::t7_empty::T7_Empty;
use baboon_rs_stub::testpkg::pkg0::{invoke_json_I1, invoke_ueba_I1, invoke_json_I2, invoke_ueba_I2};

struct MockI1;
impl I1 for MockI1 {
    fn test_call(&self, _arg: I1_testCall_in) -> MyResult<I1_testCall_out, I1_testCall_err> {
        MyResult::Success(I1_testCall_out { i00: 42 })
    }
    fn test_call2(&self, _arg: T7_Empty) -> MyResult<T7_Empty, T7_Empty> {
        MyResult::Success(T7_Empty {})
    }
}

struct FailingI1;
impl I1 for FailingI1 {
    fn test_call(&self, _arg: I1_testCall_in) -> MyResult<I1_testCall_out, I1_testCall_err> {
        MyResult::Failure(I1_testCall_err { msg: "domain error".to_string() })
    }
    fn test_call2(&self, _arg: T7_Empty) -> MyResult<T7_Empty, T7_Empty> {
        MyResult::Failure(T7_Empty {})
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
    let rt = ResultServiceRt;
    let ctx = BaboonCodecContext::Default;
    let result = invoke_json_I1(&method, &input_json, &MockI1, &rt, &ctx);
    match result {
        MyResult::Success(json_str) => {
            let decoded: I1_testCall_out = serde_json::from_str(&json_str).unwrap();
            assert_eq!(decoded.i00, 42);
        }
        MyResult::Failure(err) => panic!("Expected Success, got Failure({:?})", err),
    }
}

#[test]
fn i1_json_test_call2_success() {
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "testCall2".to_string() };
    let input_json = serde_json::to_string(&T7_Empty {}).unwrap();
    let rt = ResultServiceRt;
    let ctx = BaboonCodecContext::Default;
    let result = invoke_json_I1(&method, &input_json, &MockI1, &rt, &ctx);
    assert!(matches!(result, MyResult::Success(_)));
}

#[test]
fn i1_json_domain_error() {
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "testCall".to_string() };
    let input_json = serde_json::to_string(&I1_testCall_in {}).unwrap();
    let rt = ResultServiceRt;
    let ctx = BaboonCodecContext::Default;
    let result = invoke_json_I1(&method, &input_json, &FailingI1, &rt, &ctx);
    match result {
        MyResult::Failure(BaboonWiringError::CallFailed(_, _)) => {}
        other => panic!("Expected Failure(CallFailed), got {:?}", format_result(&other)),
    }
}

#[test]
fn i1_json_unknown_method() {
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "nonexistent".to_string() };
    let rt = ResultServiceRt;
    let ctx = BaboonCodecContext::Default;
    let result = invoke_json_I1(&method, "{}", &MockI1, &rt, &ctx);
    match result {
        MyResult::Failure(BaboonWiringError::NoMatchingMethod(_)) => {}
        other => panic!("Expected Failure(NoMatchingMethod), got {:?}", format_result(&other)),
    }
}

#[test]
fn i1_json_bad_input() {
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "testCall".to_string() };
    let rt = ResultServiceRt;
    let ctx = BaboonCodecContext::Default;
    let result = invoke_json_I1(&method, "not valid json!!", &MockI1, &rt, &ctx);
    match result {
        MyResult::Failure(BaboonWiringError::DecoderFailed(_, _)) => {}
        other => panic!("Expected Failure(DecoderFailed), got {:?}", format_result(&other)),
    }
}

#[test]
fn i1_ueba_test_call_success() {
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "testCall".to_string() };
    let ctx = BaboonCodecContext::Default;
    let rt = ResultServiceRt;
    let mut writer = Vec::new();
    BaboonBinEncode::encode_ueba(&I1_testCall_in {}, &ctx, &mut writer).unwrap();
    let result = invoke_ueba_I1(&method, &writer, &MockI1, &rt, &ctx);
    match result {
        MyResult::Success(bytes) => {
            let mut cursor = std::io::Cursor::new(bytes);
            let decoded: I1_testCall_out = BaboonBinDecode::decode_ueba(&ctx, &mut cursor).unwrap();
            assert_eq!(decoded.i00, 42);
        }
        MyResult::Failure(err) => panic!("Expected Success, got Failure({:?})", err),
    }
}

#[test]
fn i1_ueba_unknown_method() {
    let method = BaboonMethodId { service_name: "I1".to_string(), method_name: "nonexistent".to_string() };
    let rt = ResultServiceRt;
    let ctx = BaboonCodecContext::Default;
    let result = invoke_ueba_I1(&method, &[], &MockI1, &rt, &ctx);
    match result {
        MyResult::Failure(BaboonWiringError::NoMatchingMethod(_)) => {}
        other => panic!("Expected Failure(NoMatchingMethod), got {:?}", format_result(&other)),
    }
}

#[test]
fn i2_json_no_err_call_success() {
    let method = BaboonMethodId { service_name: "I2".to_string(), method_name: "noErrCall".to_string() };
    let input_json = serde_json::to_string(&I2_noErrCall_in { value: 123 }).unwrap();
    let rt = ResultServiceRt;
    let ctx = BaboonCodecContext::Default;
    let result = invoke_json_I2(&method, &input_json, &MockI2, &rt, &ctx);
    match result {
        MyResult::Success(json_str) => {
            let decoded: I2_noErrCall_out = serde_json::from_str(&json_str).unwrap();
            assert_eq!(decoded.result, "result_123");
        }
        MyResult::Failure(err) => panic!("Expected Success, got Failure({:?})", err),
    }
}

#[test]
fn i2_ueba_no_err_call_success() {
    let method = BaboonMethodId { service_name: "I2".to_string(), method_name: "noErrCall".to_string() };
    let ctx = BaboonCodecContext::Default;
    let rt = ResultServiceRt;
    let mut writer = Vec::new();
    BaboonBinEncode::encode_ueba(&I2_noErrCall_in { value: 456 }, &ctx, &mut writer).unwrap();
    let result = invoke_ueba_I2(&method, &writer, &MockI2, &rt, &ctx);
    match result {
        MyResult::Success(bytes) => {
            let mut cursor = std::io::Cursor::new(bytes);
            let decoded: I2_noErrCall_out = BaboonBinDecode::decode_ueba(&ctx, &mut cursor).unwrap();
            assert_eq!(decoded.result, "result_456");
        }
        MyResult::Failure(err) => panic!("Expected Success, got Failure({:?})", err),
    }
}

fn format_result<S, E: std::fmt::Debug>(r: &MyResult<S, E>) -> String {
    match r {
        MyResult::Success(_) => "Success(...)".to_string(),
        MyResult::Failure(e) => format!("Failure({:?})", e),
    }
}
