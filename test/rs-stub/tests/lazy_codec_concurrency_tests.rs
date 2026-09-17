use baboon_rs_stub::baboon_codecs_facade::LazyCodec;
use std::sync::{Arc, Barrier};

#[test]
fn concurrent_first_access_publishes_one_shared_value() {
    let values = Arc::new((0..1000).map(|i| LazyCodec::new(move || {
        std::thread::yield_now();
        i
    })).collect::<Vec<_>>());
    let start = Arc::new(Barrier::new(8));
    let threads = (0..8).map(|_| {
        let values = Arc::clone(&values);
        let start = Arc::clone(&start);
        std::thread::spawn(move || {
            let mut failures = 0;
            for (i, value) in values.iter().enumerate() {
                start.wait();
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| value.get()));
                match result {
                    Ok(v) => assert_eq!(*v, i),
                    Err(_) => failures += 1,
                }
            }
            failures
        })
    }).collect::<Vec<_>>();
    let failures: usize = threads.into_iter().map(|thread| thread.join().unwrap()).sum();
    assert_eq!(failures, 0);
    for value in values.iter() {
        assert!(Arc::ptr_eq(&value.get(), &value.get()));
    }
}
