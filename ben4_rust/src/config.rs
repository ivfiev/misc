use std::env;

#[derive(Debug, Clone, Copy)]
pub enum Mode {
    Int32,
    Float32,
}

#[derive(Debug, Clone, Copy)]
pub struct Config {
    pub mode: Mode,
    pub threads: u32,
}

impl Config {
    pub fn parse_args() -> Result<Config, String> {
        let mut args = env::args();
        args.next();
        let mut config = Config {
            mode: Mode::Int32,
            threads: 1,
        };
        loop {
            let flag = args.next();
            let val = args.next();
            match (flag, val) {
                (Some(f), Some(v)) => match (f.as_str(), v.as_str()) {
                    ("-m", "i") => config.mode = Mode::Int32,
                    ("-m", "f") => config.mode = Mode::Float32,
                    ("-t", ts) => match ts.parse() {
                        Ok(count) => config.threads = count,
                        Err(e) => return Err(format!("error parsing thread count {}", e)),
                    },
                    (_, _) => return Err(String::from("unknown args")),
                },
                (_, _) => break,
            }
        }
        Ok(config)
    }
}
