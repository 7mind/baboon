use baboon_rs_stub::baboon_runtime::BaboonCodecContext;
use baboon_rs_stub::baboon_service_wiring::*;
use std::{cell::Cell, rc::Rc};

struct Echo(&'static str);
impl IBaboonJsonService<usize> for Echo {
    fn service_name(&self) -> &str { self.0 }
    fn invoke(&self, _: &BaboonMethodId, data: &str, _: &BaboonCodecContext) -> usize { data.len() }
}
impl IBaboonUebaService<usize> for Echo {
    fn service_name(&self) -> &str { self.0 }
    fn invoke(&self, _: &BaboonMethodId, data: &[u8], _: &BaboonCodecContext) -> usize { data.len() }
}
impl IBaboonJsonServiceCtx<Rc<Cell<usize>>, usize> for Echo {
    fn service_name(&self) -> &str { self.0 }
    fn invoke(&self, _: &BaboonMethodId, data: &str, ctx: Rc<Cell<usize>>, _: &BaboonCodecContext) -> usize {
        ctx.set(ctx.get() + data.len()); ctx.get()
    }
}
impl IBaboonUebaServiceCtx<Rc<Cell<usize>>, usize> for Echo {
    fn service_name(&self) -> &str { self.0 }
    fn invoke(&self, _: &BaboonMethodId, data: &[u8], ctx: Rc<Cell<usize>>, _: &BaboonCodecContext) -> usize {
        ctx.set(ctx.get() + data.len()); ctx.get()
    }
}
fn method(name: &str) -> BaboonMethodId { BaboonMethodId { service_name: name.to_owned(), method_name: "echo".to_owned() } }

#[test]
fn wire_formats_preserve_registration_and_lookup_errors() {
    let mut json = JsonMuxer::new().with(Box::new(Echo("b"))).unwrap().with(Box::new(Echo("a"))).unwrap();
    let mut bin = UebaMuxer::new().with(Box::new(Echo("b"))).unwrap().with(Box::new(Echo("a"))).unwrap();
    assert_eq!(json.service_names(), vec!["a", "b"]);
    assert_eq!(bin.service_names(), json.service_names());
    assert_eq!(json.register(Box::new(Echo("a"))).unwrap_err().to_string(), "DuplicateService(a)");
    assert_eq!(bin.register(Box::new(Echo("a"))).unwrap_err().to_string(), "DuplicateService(a)");
    assert_eq!(json.invoke(&method("a"), "123", &BaboonCodecContext::Default).unwrap(), 3);
    assert_eq!(bin.invoke(&method("a"), &[1, 2, 3], &BaboonCodecContext::Default).unwrap(), 3);
    assert_eq!(json.invoke(&method("missing"), "", &BaboonCodecContext::Default).unwrap_err().to_string(), "NoMatchingService(missing.echo)");
    assert_eq!(bin.invoke(&method("missing"), &[], &BaboonCodecContext::Default).unwrap_err().to_string(), "NoMatchingService(missing.echo)");
}

#[test]
fn context_muxers_accept_non_send_owned_contexts() {
    let json = JsonMuxerCtx::new().with(Box::new(Echo("a"))).unwrap();
    let bin = UebaMuxerCtx::new().with(Box::new(Echo("a"))).unwrap();
    let ctx = Rc::new(Cell::new(0));
    assert_eq!(json.invoke(&method("a"), "123", Rc::clone(&ctx), &BaboonCodecContext::Default).unwrap(), 3);
    assert_eq!(bin.invoke(&method("a"), &[1, 2], Rc::clone(&ctx), &BaboonCodecContext::Default).unwrap(), 5);
    assert_eq!(ctx.get(), 5);
}
