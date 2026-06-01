#[allow(dead_code)]
#[allow(unused_imports)]
#[allow(unused_variables)]
#[allow(non_camel_case_types)]
#[allow(non_snake_case)]
mod generated;
pub use generated::*;

mod petstore_impl;
mod server;
mod client;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let mode = args.get(1).expect("Usage: baboon-rs-service <server|client> --host <host> --port <port>");

    let mut host = "127.0.0.1".to_string();
    let mut port: u16 = 8080;
    let mut codec = client::CodecSelector::Both;

    let mut i = 2;
    while i < args.len() {
        match args[i].as_str() {
            "--host" => {
                i += 1;
                host = args[i].clone();
            }
            "--port" => {
                i += 1;
                port = args[i].parse().expect("Invalid port number");
            }
            "--codec" => {
                i += 1;
                codec = match args[i].as_str() {
                    "json" => client::CodecSelector::Json,
                    "ueba" => client::CodecSelector::Ueba,
                    "both" => client::CodecSelector::Both,
                    other => panic!("Invalid --codec value: {}. Use 'json', 'ueba', or 'both'.", other),
                };
            }
            other => panic!("Unknown argument: {}", other),
        }
        i += 1;
    }

    match mode.as_str() {
        "server" => server::start(&host, port),
        "client" => client::run(&host, port, codec),
        other => panic!("Unknown mode: {}. Use 'server' or 'client'.", other),
    }
}
