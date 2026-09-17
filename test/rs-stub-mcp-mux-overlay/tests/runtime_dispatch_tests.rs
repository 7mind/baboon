use baboon_rs_stub::baboon_mcp_server::*;
use baboon_rs_stub::baboon_runtime::BaboonCodecContext;
use baboon_rs_stub::baboon_service_wiring::{BaboonMethodId, BaboonWiringError};

struct Server { base: BaboonMcpServerBase }

fn invoke(method: &BaboonMethodId, data: &str, _: (), _: &BaboonCodecContext) -> Result<String, BaboonWiringError> {
    if method.method_name == "fail" { Err(BaboonWiringError::NoMatchingMethod(method.clone())) }
    else { Ok(data.to_owned()) }
}

fn server(names: &[&'static str]) -> Server {
    Server { base: BaboonMcpServerBase {
        server_info: McpServerInfo { name: "test", version: "1" },
        tools: names.iter().map(|name| McpToolEntry {
            name,
            method: BaboonMethodId { service_name: "test".to_owned(), method_name: (*name).to_owned() },
            description: Some("description"),
            input_schema: serde_json::json!({"type":"object"}),
        }).collect(),
    }}
}

impl IBaboonRoutableMcpServer<()> for Server {
    fn server_info(&self) -> &McpServerInfo { &self.base.server_info }
    fn tools(&self) -> &[McpToolEntry] { &self.base.tools }
    fn route_tool_call(&self, method: &BaboonMethodId, data: &str, ctx: (), codec_ctx: &BaboonCodecContext) -> Result<String, BaboonWiringError> {
        invoke(method, data, ctx, codec_ctx)
    }
}

fn request(method: &str, params: Option<serde_json::Value>) -> JsonRpcRequest {
    JsonRpcRequest { id: Some(serde_json::json!(1)), method: method.to_owned(), params }
}

#[test]
fn single_and_mux_dispatch_have_identical_protocol_results() {
    let single = server(&["echo", "fail"]);
    let mux = AbstractMcpMuxer::new(McpServerInfo { name: "test", version: "1" }).with(Box::new(server(&["echo", "fail"]))).unwrap();
    let mut one_session = McpSession::new();
    let mut mux_session = McpSession::new();
    let requests = [
        request("tools/list", None),
        request("tools/call", None),
        request("initialize", None),
        request("initialize", Some(serde_json::json!({"protocolVersion": MCP_PROTOCOL_VERSION}))),
        JsonRpcRequest { id: None, method: "notifications/initialized".to_owned(), params: None },
        request("tools/list", None),
        request("tools/call", None),
        request("tools/call", Some(serde_json::json!({"name":"unknown"}))),
        request("tools/call", Some(serde_json::json!({"name":"echo"}))),
        request("tools/call", Some(serde_json::json!({"name":"echo", "arguments":{"a":1}}))),
        request("tools/call", Some(serde_json::json!({"name":"fail"}))),
        request("unknown", None),
    ];
    for req in requests {
        let a = single.base.handle_request(&req, &mut one_session, (), &BaboonCodecContext::Default, &invoke);
        let b = mux.handle(&req, &mut mux_session, (), &BaboonCodecContext::Default);
        assert_eq!(serde_json::to_string(&a).unwrap(), serde_json::to_string(&b).unwrap());
        assert_eq!(one_session.initialized, mux_session.initialized);
    }
}

#[test]
fn rejected_registration_does_not_publish_partial_routes() {
    let mut mux = AbstractMcpMuxer::new(McpServerInfo { name: "test", version: "1" });
    mux.register(Box::new(server(&["original"]))).unwrap();
    assert!(matches!(mux.register(Box::new(server(&["new", "original"]))), Err(BaboonMcpWiringError::DuplicateTool(_))));
    let mut session = McpSession { initialized: true };
    let response = mux.handle(&request("tools/list", None), &mut session, (), &BaboonCodecContext::Default).unwrap();
    let names = response.result.unwrap()["tools"].as_array().unwrap().iter().map(|tool| tool["name"].as_str().unwrap().to_owned()).collect::<Vec<_>>();
    assert_eq!(names, vec!["original"]);
}
