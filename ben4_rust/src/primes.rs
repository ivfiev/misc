use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};

pub fn work_int(finish: Arc<AtomicBool>) -> u32 {
    let mut k = 1;
    let mut count = 0;
    let mut primes: [u32; 1229];
    while !finish.load(Ordering::SeqCst) {
        primes = [0; 1229];
        primes[0] = 2;
        k = 1;
        for n in (3..10000).step_by(2) {
            for i in 0..k {
                if n % primes[i] == 0 {
                    break;
                }
                if n < primes[i] * primes[i] {
                    primes[k] = n;
                    k += 1;
                    break;
                }
            }
        }
        count += 1;
    }
    assert!(k == 1229);
    count
}

pub fn work_float(finish: Arc<AtomicBool>) -> u32 {
    let mut primes: [f32; 1229];
    let mut recips: [f32; 1229];
    let mut square: [f32; 1229];
    let mut k = 1;
    let mut count = 0;
    while !finish.load(Ordering::Relaxed) {
        primes = [0.0; 1229];
        recips = [0.0; 1229];
        square = [0.0; 1229];
        primes[0] = 2.0;
        recips[0] = 0.5;
        square[0] = 4.0;
        k = 1;
        let mut n = 3.0;
        while n < 10000.0 {
            let mut i = 0;
            while i < k {
                if (n * recips[i]).round() * primes[i] == n {
                    break;
                }
                if square[i] > n {
                    primes[k] = n;
                    recips[k] = 1.0 / n;
                    square[k] = n * n;
                    k += 1;
                    break;
                }
                i += 1;
            }
            n += 2.0;
        }
        count += 1;
    }
    assert!(k == 1229);
    count
}
