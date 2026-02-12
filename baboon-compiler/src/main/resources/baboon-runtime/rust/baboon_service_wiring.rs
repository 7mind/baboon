#[derive(Clone, Debug, PartialEq)]
pub struct BaboonMethodId {
    pub service_name: String,
    pub method_name: String,
}

#[derive(Debug)]
pub enum BaboonWiringError {
    NoMatchingMethod(BaboonMethodId),
    DecoderFailed(BaboonMethodId, Box<dyn std::error::Error>),
    EncoderFailed(BaboonMethodId, Box<dyn std::error::Error>),
    CallFailed(BaboonMethodId, Box<dyn std::any::Any + Send>),
}

impl std::fmt::Display for BaboonWiringError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BaboonWiringError::NoMatchingMethod(m) => write!(f, "NoMatchingMethod({}.{})", m.service_name, m.method_name),
            BaboonWiringError::DecoderFailed(m, e) => write!(f, "DecoderFailed({}.{}: {})", m.service_name, m.method_name, e),
            BaboonWiringError::EncoderFailed(m, e) => write!(f, "EncoderFailed({}.{}: {})", m.service_name, m.method_name, e),
            BaboonWiringError::CallFailed(m, _) => write!(f, "CallFailed({}.{})", m.service_name, m.method_name),
        }
    }
}

impl std::error::Error for BaboonWiringError {}
