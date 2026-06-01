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

pub fn run(host: &str, port: u16) {
    // Reset
    post(host, port, "/reset", "");

    let client = PetStoreClient::new(
        |service: &str, method: &str, data: &str| -> Result<String, Box<dyn std::error::Error>> {
            Ok(post(host, port, &format!("/{}/{}", service, method), data))
        },
    );

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

    println!("OK");
}
