use crate::testpkg::pkg0::baboon_service_rt::IBaboonServiceRt;

pub enum MyResult<S, E> {
    Success(S),
    Failure(E),
}

pub struct ResultServiceRt;

impl IBaboonServiceRt for ResultServiceRt {
    fn pure<L: 'static, R>(&self, value: R) -> MyResult<R, L> {
        MyResult::Success(value)
    }
    fn fail<L: 'static, R>(&self, error: L) -> MyResult<R, L> {
        MyResult::Failure(error)
    }
    fn left_map<A: 'static, B, C: 'static, F: FnOnce(A) -> C>(&self, value: MyResult<B, A>, f: F) -> MyResult<B, C> {
        match value {
            MyResult::Failure(a) => MyResult::Failure(f(a)),
            MyResult::Success(b) => MyResult::Success(b),
        }
    }
    fn flat_map<A: 'static, B, C, F: FnOnce(B) -> MyResult<C, A>>(&self, value: MyResult<B, A>, f: F) -> MyResult<C, A> {
        match value {
            MyResult::Failure(a) => MyResult::Failure(a),
            MyResult::Success(b) => f(b),
        }
    }
}
