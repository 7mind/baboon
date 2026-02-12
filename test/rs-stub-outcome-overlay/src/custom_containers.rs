use crate::testpkg::pkg0::baboon_service_rt::IBaboonServiceRt;

pub enum Outcome<T> {
    Success(T),
    Failure(Box<dyn std::any::Any>),
}

pub struct OutcomeServiceRt;

impl IBaboonServiceRt for OutcomeServiceRt {
    fn pure<L: 'static, R>(&self, value: R) -> Outcome<R> {
        Outcome::Success(value)
    }
    fn fail<L: 'static, R>(&self, error: L) -> Outcome<R> {
        Outcome::Failure(Box::new(error))
    }
    fn left_map<A: 'static, B, C: 'static, F: FnOnce(A) -> C>(&self, value: Outcome<B>, f: F) -> Outcome<B> {
        match value {
            Outcome::Failure(e) => {
                if let Ok(a) = e.downcast::<A>() {
                    Outcome::Failure(Box::new(f(*a)))
                } else {
                    panic!("type mismatch in left_map");
                }
            }
            Outcome::Success(v) => Outcome::Success(v),
        }
    }
    fn flat_map<A: 'static, B, C, F: FnOnce(B) -> Outcome<C>>(&self, value: Outcome<B>, f: F) -> Outcome<C> {
        match value {
            Outcome::Failure(e) => Outcome::Failure(e),
            Outcome::Success(v) => f(v),
        }
    }
}
