use crate::baboon_runtime::BaboonCodecContext;
use crate::baboon_service_wiring::BaboonMethodId;
use crate::petstore::api::pet_store_wiring::invoke_json_PetStore;
use crate::petstore_impl::PetStoreImpl;

pub fn start(host: &str, port: u16) {
    let addr = format!("{}:{}", host, port);
    let server = tiny_http::Server::http(&addr).expect(&format!("Failed to bind to {}", addr));
    let ctx = BaboonCodecContext::Default;
    let impl_ = PetStoreImpl::new();

    println!("Listening on {}:{}", host, port);

    for mut request in server.incoming_requests() {
        let url = request.url().to_string();
        let method = request.method().as_str().to_uppercase();

        if method == "GET" && url == "/health" {
            let response = tiny_http::Response::from_string("ok")
                .with_header("Connection: close".parse::<tiny_http::Header>().unwrap());
            request.respond(response).ok();
            continue;
        }

        if method == "POST" && url == "/reset" {
            impl_.reset();
            let response = tiny_http::Response::from_string("ok")
                .with_header("Connection: close".parse::<tiny_http::Header>().unwrap());
            request.respond(response).ok();
            continue;
        }

        if method == "POST" && url == "/shutdown" {
            let response = tiny_http::Response::from_string("ok")
                .with_header("Connection: close".parse::<tiny_http::Header>().unwrap());
            request.respond(response).ok();
            break;
        }

        if method == "POST" {
            let parts: Vec<&str> = url.trim_start_matches('/').splitn(2, '/').collect();
            if parts.len() == 2 {
                let service_name = parts[0].to_string();
                let method_name = parts[1].to_string();

                let mut body = String::new();
                request.as_reader().read_to_string(&mut body).ok();

                let method_id = BaboonMethodId {
                    service_name: service_name.clone(),
                    method_name: method_name.clone(),
                };

                let result = match service_name.as_str() {
                    "PetStore" => invoke_json_PetStore(&method_id, &body, &impl_, &ctx),
                    _ => {
                        let response = tiny_http::Response::from_string(format!("Unknown service: {}", service_name))
                            .with_status_code(404)
                            .with_header("Connection: close".parse::<tiny_http::Header>().unwrap());
                        request.respond(response).ok();
                        continue;
                    }
                };

                match result {
                    Ok(json) => {
                        let response = tiny_http::Response::from_string(json)
                            .with_header("Content-Type: application/json".parse::<tiny_http::Header>().unwrap())
                            .with_header("Connection: close".parse::<tiny_http::Header>().unwrap());
                        request.respond(response).ok();
                    }
                    Err(e) => {
                        let response = tiny_http::Response::from_string(format!("Error: {}", e))
                            .with_status_code(500)
                            .with_header("Connection: close".parse::<tiny_http::Header>().unwrap());
                        request.respond(response).ok();
                    }
                }
                continue;
            }
        }

        let response = tiny_http::Response::from_string("Not Found")
            .with_status_code(404)
            .with_header("Connection: close".parse::<tiny_http::Header>().unwrap());
        request.respond(response).ok();
    }
}
