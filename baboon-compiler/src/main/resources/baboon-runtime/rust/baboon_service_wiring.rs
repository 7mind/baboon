use crate::baboon_runtime::BaboonCodecContext;

#[derive(Clone, Debug, PartialEq)]
pub struct BaboonMethodId {
    pub service_name: String,
    pub method_name: String,
}

#[derive(Debug)]
pub enum BaboonWiringError {
    NoMatchingMethod(BaboonMethodId),
    NoMatchingService(BaboonMethodId),
    DuplicateService(String),
    DecoderFailed(BaboonMethodId, Box<dyn std::error::Error>),
    EncoderFailed(BaboonMethodId, Box<dyn std::error::Error>),
    CallFailed(BaboonMethodId, Box<dyn std::any::Any + Send>),
}

impl std::fmt::Display for BaboonWiringError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BaboonWiringError::NoMatchingMethod(m) => write!(f, "NoMatchingMethod({}.{})", m.service_name, m.method_name),
            BaboonWiringError::NoMatchingService(m) => write!(f, "NoMatchingService({}.{})", m.service_name, m.method_name),
            BaboonWiringError::DuplicateService(name) => write!(f, "DuplicateService({})", name),
            BaboonWiringError::DecoderFailed(m, e) => write!(f, "DecoderFailed({}.{}: {})", m.service_name, m.method_name, e),
            BaboonWiringError::EncoderFailed(m, e) => write!(f, "EncoderFailed({}.{}: {})", m.service_name, m.method_name, e),
            BaboonWiringError::CallFailed(m, _) => write!(f, "CallFailed({}.{})", m.service_name, m.method_name),
        }
    }
}

impl std::error::Error for BaboonWiringError {}

// --- Service muxers ---
//
// Cross-domain composable dispatch. A muxer holds a set of services from any
// model(s) and routes an `(method, data, ctx)` call to the right one by
// `method.service_name`. The `R` trait type parameter encodes the return shape
// so the same trait supports both sync and async generated services — for
// async (`--rs-async-services=true`) the per-service wrapper instantiates
// `R = Pin<Box<dyn Future<Output = …>>>`; for sync it instantiates
// `R = <raw or container-wrapped wire type>`. The per-service wrapper structs
// emitted alongside `invoke_json_*` / `invoke_ueba_*` carry the matching
// parameterisation and bake `rt` / service-context fields at construction
// time so the runtime contract here stays codec-flavour-symmetric.
//
// Trait-object dispatch (`Box<dyn IBaboon*Service<R>>`) is used rather than
// static dispatch because the table must hold heterogeneous wrapper types
// (one per service, each with its own impl/rt/svc-ctx generics). The dyn
// indirection is one virtual call per `invoke`; the underlying encode/decode
// dominates the cost.

pub trait IBaboonJsonService<R> {
    fn service_name(&self) -> &str;
    fn invoke(&self, method: &BaboonMethodId, data: &str, ctx: &BaboonCodecContext) -> R;
}

pub trait IBaboonUebaService<R> {
    fn service_name(&self) -> &str;
    fn invoke(&self, method: &BaboonMethodId, data: &[u8], ctx: &BaboonCodecContext) -> R;
}

pub struct JsonMuxer<R> {
    table: std::collections::BTreeMap<String, Box<dyn IBaboonJsonService<R>>>,
}

impl<R> JsonMuxer<R> {
    pub fn new() -> Self {
        Self { table: std::collections::BTreeMap::new() }
    }

    pub fn register(&mut self, service: Box<dyn IBaboonJsonService<R>>) -> Result<(), BaboonWiringError> {
        let name = service.service_name().to_string();
        if self.table.contains_key(&name) {
            return Err(BaboonWiringError::DuplicateService(name));
        }
        self.table.insert(name, service);
        Ok(())
    }

    pub fn with(mut self, service: Box<dyn IBaboonJsonService<R>>) -> Result<Self, BaboonWiringError> {
        self.register(service)?;
        Ok(self)
    }

    pub fn invoke(&self, method: &BaboonMethodId, data: &str, ctx: &BaboonCodecContext) -> Result<R, BaboonWiringError> {
        match self.table.get(&method.service_name) {
            Some(svc) => Ok(svc.invoke(method, data, ctx)),
            None => Err(BaboonWiringError::NoMatchingService(method.clone())),
        }
    }

    pub fn service_names(&self) -> Vec<String> {
        self.table.keys().cloned().collect()
    }
}

impl<R> Default for JsonMuxer<R> {
    fn default() -> Self { Self::new() }
}

pub struct UebaMuxer<R> {
    table: std::collections::BTreeMap<String, Box<dyn IBaboonUebaService<R>>>,
}

impl<R> UebaMuxer<R> {
    pub fn new() -> Self {
        Self { table: std::collections::BTreeMap::new() }
    }

    pub fn register(&mut self, service: Box<dyn IBaboonUebaService<R>>) -> Result<(), BaboonWiringError> {
        let name = service.service_name().to_string();
        if self.table.contains_key(&name) {
            return Err(BaboonWiringError::DuplicateService(name));
        }
        self.table.insert(name, service);
        Ok(())
    }

    pub fn with(mut self, service: Box<dyn IBaboonUebaService<R>>) -> Result<Self, BaboonWiringError> {
        self.register(service)?;
        Ok(self)
    }

    pub fn invoke(&self, method: &BaboonMethodId, data: &[u8], ctx: &BaboonCodecContext) -> Result<R, BaboonWiringError> {
        match self.table.get(&method.service_name) {
            Some(svc) => Ok(svc.invoke(method, data, ctx)),
            None => Err(BaboonWiringError::NoMatchingService(method.clone())),
        }
    }

    pub fn service_names(&self) -> Vec<String> {
        self.table.keys().cloned().collect()
    }
}

impl<R> Default for UebaMuxer<R> {
    fn default() -> Self { Self::new() }
}

// --- Context-carrying service muxers ---
//
// Emitted alongside the context-free muxers above when a service-context mode
// (`abstract` or `type`) is active. The service context `Ctx` is supplied
// PER-INVOKE (next to the codec context) rather than baked into the wrapper at
// construction time, so a single registered service can be dispatched with a
// different context on each call. The context-free traits/muxers above are
// left untouched so `--service-context-mode none` output (and the
// service-acceptance matrix) stays byte-identical.

pub trait IBaboonJsonServiceCtx<Ctx, R> {
    fn service_name(&self) -> &str;
    fn invoke(&self, method: &BaboonMethodId, data: &str, ctx: Ctx, codec_ctx: &BaboonCodecContext) -> R;
}

pub trait IBaboonUebaServiceCtx<Ctx, R> {
    fn service_name(&self) -> &str;
    fn invoke(&self, method: &BaboonMethodId, data: &[u8], ctx: Ctx, codec_ctx: &BaboonCodecContext) -> R;
}

pub struct JsonMuxerCtx<Ctx, R> {
    table: std::collections::BTreeMap<String, Box<dyn IBaboonJsonServiceCtx<Ctx, R>>>,
}

impl<Ctx, R> JsonMuxerCtx<Ctx, R> {
    pub fn new() -> Self {
        Self { table: std::collections::BTreeMap::new() }
    }

    pub fn register(&mut self, service: Box<dyn IBaboonJsonServiceCtx<Ctx, R>>) -> Result<(), BaboonWiringError> {
        let name = service.service_name().to_string();
        if self.table.contains_key(&name) {
            return Err(BaboonWiringError::DuplicateService(name));
        }
        self.table.insert(name, service);
        Ok(())
    }

    pub fn with(mut self, service: Box<dyn IBaboonJsonServiceCtx<Ctx, R>>) -> Result<Self, BaboonWiringError> {
        self.register(service)?;
        Ok(self)
    }

    pub fn invoke(&self, method: &BaboonMethodId, data: &str, ctx: Ctx, codec_ctx: &BaboonCodecContext) -> Result<R, BaboonWiringError> {
        match self.table.get(&method.service_name) {
            Some(svc) => Ok(svc.invoke(method, data, ctx, codec_ctx)),
            None => Err(BaboonWiringError::NoMatchingService(method.clone())),
        }
    }

    pub fn service_names(&self) -> Vec<String> {
        self.table.keys().cloned().collect()
    }
}

impl<Ctx, R> Default for JsonMuxerCtx<Ctx, R> {
    fn default() -> Self { Self::new() }
}

pub struct UebaMuxerCtx<Ctx, R> {
    table: std::collections::BTreeMap<String, Box<dyn IBaboonUebaServiceCtx<Ctx, R>>>,
}

impl<Ctx, R> UebaMuxerCtx<Ctx, R> {
    pub fn new() -> Self {
        Self { table: std::collections::BTreeMap::new() }
    }

    pub fn register(&mut self, service: Box<dyn IBaboonUebaServiceCtx<Ctx, R>>) -> Result<(), BaboonWiringError> {
        let name = service.service_name().to_string();
        if self.table.contains_key(&name) {
            return Err(BaboonWiringError::DuplicateService(name));
        }
        self.table.insert(name, service);
        Ok(())
    }

    pub fn with(mut self, service: Box<dyn IBaboonUebaServiceCtx<Ctx, R>>) -> Result<Self, BaboonWiringError> {
        self.register(service)?;
        Ok(self)
    }

    pub fn invoke(&self, method: &BaboonMethodId, data: &[u8], ctx: Ctx, codec_ctx: &BaboonCodecContext) -> Result<R, BaboonWiringError> {
        match self.table.get(&method.service_name) {
            Some(svc) => Ok(svc.invoke(method, data, ctx, codec_ctx)),
            None => Err(BaboonWiringError::NoMatchingService(method.clone())),
        }
    }

    pub fn service_names(&self) -> Vec<String> {
        self.table.keys().cloned().collect()
    }
}

impl<Ctx, R> Default for UebaMuxerCtx<Ctx, R> {
    fn default() -> Self { Self::new() }
}
