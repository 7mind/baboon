use std::io::Read;

use crate::baboon_runtime::BaboonCodecContext;
use crate::petstore::api::pet_status::PetStatus;
use crate::petstore::api::pet_store_client::PetStoreClient;

fn post(host: &str, port: u16, path: &str, body: &str) -> String {
    let url = format!("http://{}:{}{}", host, port, path);
    let resp = ureq::post(&url)
        .set("Content-Type", "application/json")
        .send_string(body)
        .expect(&format!("POST {} failed", url));
    resp.into_string().expect("Failed to read response body")
}

fn post_bytes(host: &str, port: u16, path: &str, body: &[u8]) -> Vec<u8> {
    let url = format!("http://{}:{}{}", host, port, path);
    let resp = ureq::post(&url)
        .set("Content-Type", "application/octet-stream")
        .send_bytes(body)
        .expect(&format!("POST {} failed", url));
    let mut buf = Vec::new();
    resp.into_reader()
        .read_to_end(&mut buf)
        .expect("Failed to read response body");
    buf
}

pub fn run(host: &str, port: u16) {
    run_json(host, port);
    run_ueba(host, port);
    println!("OK");
}

fn make_client(
    host: &str,
    port: u16,
) -> PetStoreClient<
    impl Fn(&str, &str, &[u8]) -> Result<Vec<u8>, Box<dyn std::error::Error>> + '_,
    impl Fn(&str, &str, &str) -> Result<String, Box<dyn std::error::Error>> + '_,
> {
    PetStoreClient::new(
        move |service: &str, method: &str, data: &[u8]| -> Result<Vec<u8>, Box<dyn std::error::Error>> {
            Ok(post_bytes(host, port, &format!("/{}/{}", service, method), data))
        },
        move |service: &str, method: &str, data: &str| -> Result<String, Box<dyn std::error::Error>> {
            Ok(post(host, port, &format!("/{}/{}", service, method), data))
        },
        BaboonCodecContext::Default,
    )
}

fn run_json(host: &str, port: u16) {
    // Reset
    post(host, port, "/reset", "");

    let client = make_client(host, port);

    // Add Buddy
    let add_buddy_in = crate::petstore::api::petstore::addpet::input::In {
        name: "Buddy".to_string(),
        status: PetStatus::Available,
        tag: Some("dog".to_string()),
    };
    let add_buddy_out = client.add_pet_json(add_buddy_in).unwrap();
    let buddy_id = add_buddy_out.pet.id;
    assert!(
        add_buddy_out.pet.name == "Buddy",
        "expected name Buddy, got {}",
        add_buddy_out.pet.name
    );

    // Add Whiskers
    let add_whiskers_in = crate::petstore::api::petstore::addpet::input::In {
        name: "Whiskers".to_string(),
        status: PetStatus::Pending,
        tag: Some("cat".to_string()),
    };
    let add_whiskers_out = client.add_pet_json(add_whiskers_in).unwrap();
    let whiskers_id = add_whiskers_out.pet.id;
    assert!(
        add_whiskers_out.pet.name == "Whiskers",
        "expected name Whiskers, got {}",
        add_whiskers_out.pet.name
    );

    // List pets (expect 2)
    let list_in = crate::petstore::api::petstore::listpets::input::In {};
    let list_out = client.list_pets_json(list_in).unwrap();
    assert!(
        list_out.pets.len() == 2,
        "expected 2 pets, got {}",
        list_out.pets.len()
    );

    // Get Buddy
    let get_buddy_in = crate::petstore::api::petstore::getpet::input::In { id: buddy_id };
    let get_buddy_out = client.get_pet_json(get_buddy_in).unwrap();
    assert!(
        get_buddy_out.pet.name == "Buddy",
        "expected Buddy, got {}",
        get_buddy_out.pet.name
    );
    assert!(
        get_buddy_out.pet.status == PetStatus::Available,
        "expected Available, got {:?}",
        get_buddy_out.pet.status
    );
    assert!(
        get_buddy_out.pet.tag == Some("dog".to_string()),
        "expected tag dog, got {:?}",
        get_buddy_out.pet.tag
    );

    // Delete Whiskers
    let delete_in = crate::petstore::api::petstore::deletepet::input::In { id: whiskers_id };
    let delete_out = client.delete_pet_json(delete_in).unwrap();
    assert!(
        delete_out.deleted,
        "expected deleted=true, got {}",
        delete_out.deleted
    );

    // List pets again (expect 1)
    let list2_in = crate::petstore::api::petstore::listpets::input::In {};
    let list2_out = client.list_pets_json(list2_in).unwrap();
    assert!(
        list2_out.pets.len() == 1,
        "expected 1 pet, got {}",
        list2_out.pets.len()
    );
    assert!(
        list2_out.pets[0].name == "Buddy",
        "expected remaining pet Buddy, got {}",
        list2_out.pets[0].name
    );
}

// Same scenario as run_json, but driven over the generated UEBA client methods
// (bare snake_case) which serialize via the binary codec over an
// application/octet-stream transport. Results must match the JSON pass.
fn run_ueba(host: &str, port: u16) {
    // Reset
    post(host, port, "/reset", "");

    let client = make_client(host, port);

    // Add Buddy
    let add_buddy_in = crate::petstore::api::petstore::addpet::input::In {
        name: "Buddy".to_string(),
        status: PetStatus::Available,
        tag: Some("dog".to_string()),
    };
    let add_buddy_out = client.add_pet(add_buddy_in).unwrap();
    let buddy_id = add_buddy_out.pet.id;
    assert!(
        add_buddy_out.pet.name == "Buddy",
        "ueba: expected name Buddy, got {}",
        add_buddy_out.pet.name
    );

    // Add Whiskers
    let add_whiskers_in = crate::petstore::api::petstore::addpet::input::In {
        name: "Whiskers".to_string(),
        status: PetStatus::Pending,
        tag: Some("cat".to_string()),
    };
    let add_whiskers_out = client.add_pet(add_whiskers_in).unwrap();
    let whiskers_id = add_whiskers_out.pet.id;
    assert!(
        add_whiskers_out.pet.name == "Whiskers",
        "ueba: expected name Whiskers, got {}",
        add_whiskers_out.pet.name
    );

    // List pets (expect 2)
    let list_in = crate::petstore::api::petstore::listpets::input::In {};
    let list_out = client.list_pets(list_in).unwrap();
    assert!(
        list_out.pets.len() == 2,
        "ueba: expected 2 pets, got {}",
        list_out.pets.len()
    );

    // Get Buddy
    let get_buddy_in = crate::petstore::api::petstore::getpet::input::In { id: buddy_id };
    let get_buddy_out = client.get_pet(get_buddy_in).unwrap();
    assert!(
        get_buddy_out.pet.name == "Buddy",
        "ueba: expected Buddy, got {}",
        get_buddy_out.pet.name
    );
    assert!(
        get_buddy_out.pet.status == PetStatus::Available,
        "ueba: expected Available, got {:?}",
        get_buddy_out.pet.status
    );
    assert!(
        get_buddy_out.pet.tag == Some("dog".to_string()),
        "ueba: expected tag dog, got {:?}",
        get_buddy_out.pet.tag
    );

    // Delete Whiskers
    let delete_in = crate::petstore::api::petstore::deletepet::input::In { id: whiskers_id };
    let delete_out = client.delete_pet(delete_in).unwrap();
    assert!(
        delete_out.deleted,
        "ueba: expected deleted=true, got {}",
        delete_out.deleted
    );

    // List pets again (expect 1)
    let list2_in = crate::petstore::api::petstore::listpets::input::In {};
    let list2_out = client.list_pets(list2_in).unwrap();
    assert!(
        list2_out.pets.len() == 1,
        "ueba: expected 1 pet, got {}",
        list2_out.pets.len()
    );
    assert!(
        list2_out.pets[0].name == "Buddy",
        "ueba: expected remaining pet Buddy, got {}",
        list2_out.pets[0].name
    );
}
