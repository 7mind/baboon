use crate::petstore::api::pet_status::PetStatus;

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

    // Add Buddy
    let add_buddy_in = crate::petstore::api::petstore::addpet::r#in::In {
        name: "Buddy".to_string(),
        status: PetStatus::Available,
        tag: Some("dog".to_string()),
    };
    let add_buddy_json = serde_json::to_string(&add_buddy_in).unwrap();
    let add_buddy_resp = post(host, port, "/PetStore/addPet", &add_buddy_json);
    let add_buddy_out: crate::petstore::api::petstore::addpet::out::Out =
        serde_json::from_str(&add_buddy_resp).unwrap();
    let buddy_id = add_buddy_out.pet.id;
    assert!(
        add_buddy_out.pet.name == "Buddy",
        "expected name Buddy, got {}",
        add_buddy_out.pet.name
    );

    // Add Whiskers
    let add_whiskers_in = crate::petstore::api::petstore::addpet::r#in::In {
        name: "Whiskers".to_string(),
        status: PetStatus::Pending,
        tag: Some("cat".to_string()),
    };
    let add_whiskers_json = serde_json::to_string(&add_whiskers_in).unwrap();
    let add_whiskers_resp = post(host, port, "/PetStore/addPet", &add_whiskers_json);
    let add_whiskers_out: crate::petstore::api::petstore::addpet::out::Out =
        serde_json::from_str(&add_whiskers_resp).unwrap();
    let whiskers_id = add_whiskers_out.pet.id;
    assert!(
        add_whiskers_out.pet.name == "Whiskers",
        "expected name Whiskers, got {}",
        add_whiskers_out.pet.name
    );

    // List pets (expect 2)
    let list_in = crate::petstore::api::petstore::listpets::r#in::In {};
    let list_json = serde_json::to_string(&list_in).unwrap();
    let list_resp = post(host, port, "/PetStore/listPets", &list_json);
    let list_out: crate::petstore::api::petstore::listpets::out::Out =
        serde_json::from_str(&list_resp).unwrap();
    assert!(
        list_out.pets.len() == 2,
        "expected 2 pets, got {}",
        list_out.pets.len()
    );

    // Get Buddy
    let get_buddy_in = crate::petstore::api::petstore::getpet::r#in::In { id: buddy_id };
    let get_buddy_json = serde_json::to_string(&get_buddy_in).unwrap();
    let get_buddy_resp = post(host, port, "/PetStore/getPet", &get_buddy_json);
    let get_buddy_out: crate::petstore::api::petstore::getpet::out::Out =
        serde_json::from_str(&get_buddy_resp).unwrap();
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
    let delete_in = crate::petstore::api::petstore::deletepet::r#in::In { id: whiskers_id };
    let delete_json = serde_json::to_string(&delete_in).unwrap();
    let delete_resp = post(host, port, "/PetStore/deletePet", &delete_json);
    let delete_out: crate::petstore::api::petstore::deletepet::out::Out =
        serde_json::from_str(&delete_resp).unwrap();
    assert!(
        delete_out.deleted,
        "expected deleted=true, got {}",
        delete_out.deleted
    );

    // List pets again (expect 1)
    let list2_resp = post(host, port, "/PetStore/listPets", &list_json);
    let list2_out: crate::petstore::api::petstore::listpets::out::Out =
        serde_json::from_str(&list2_resp).unwrap();
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
