use std::{
    process,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
    thread,
    time::Duration,
};

use crate::config::{Config, Mode};

mod config;
mod primes;

fn main() {
    let args = Config::parse_args().unwrap_or_else(|err| {
        eprintln!("bad args! {err}");
        process::exit(1);
    });
    dbg!(&args);
    let flag = Arc::new(AtomicBool::new(false));
    let mut handles = vec![];
    for _ in 0..args.threads {
        let cloned = flag.clone();
        let handle = thread::spawn(move || match &args.mode {
            Mode::Int32 => primes::work_int(cloned),
            Mode::Float32 => primes::work_float(cloned),
        });
        handles.push(handle)
    }
    thread::sleep(Duration::from_secs(10));
    flag.store(true, Ordering::Relaxed);
    let mut sum = 0;
    for h in handles {
        sum += h.join().unwrap();
    }
    println!("total {sum}");
}
