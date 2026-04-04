use std::collections::HashMap;
use std::sync::Mutex;

use crate::petstore::api::pet::Pet;
use crate::petstore::api::pet_store::PetStore;

pub struct PetStoreImpl {
    pets: Mutex<HashMap<i64, Pet>>,
    next_id: Mutex<i64>,
}

impl PetStoreImpl {
    pub fn new() -> Self {
        PetStoreImpl {
            pets: Mutex::new(HashMap::new()),
            next_id: Mutex::new(1),
        }
    }

    pub fn reset(&self) {
        let mut pets = self.pets.lock().unwrap();
        let mut next_id = self.next_id.lock().unwrap();
        pets.clear();
        *next_id = 1;
    }
}

impl PetStore for PetStoreImpl {
    fn add_pet(
        &self,
        arg: crate::petstore::api::petstore::addpet::input::In,
    ) -> crate::petstore::api::petstore::addpet::out::Out {
        let mut pets = self.pets.lock().unwrap();
        let mut next_id = self.next_id.lock().unwrap();
        let id = *next_id;
        *next_id += 1;
        let pet = Pet {
            id,
            name: arg.name,
            status: arg.status,
            tag: arg.tag,
        };
        pets.insert(id, pet.clone());
        crate::petstore::api::petstore::addpet::out::Out { pet }
    }

    fn get_pet(
        &self,
        arg: crate::petstore::api::petstore::getpet::input::In,
    ) -> crate::petstore::api::petstore::getpet::out::Out {
        let pets = self.pets.lock().unwrap();
        let pet = pets.get(&arg.id).expect(&format!("Pet with id {} not found", arg.id));
        crate::petstore::api::petstore::getpet::out::Out { pet: pet.clone() }
    }

    fn list_pets(
        &self,
        _arg: crate::petstore::api::petstore::listpets::input::In,
    ) -> crate::petstore::api::petstore::listpets::out::Out {
        let pets = self.pets.lock().unwrap();
        let mut pet_list: Vec<Pet> = pets.values().cloned().collect();
        pet_list.sort_by_key(|p| p.id);
        crate::petstore::api::petstore::listpets::out::Out { pets: pet_list }
    }

    fn delete_pet(
        &self,
        arg: crate::petstore::api::petstore::deletepet::input::In,
    ) -> crate::petstore::api::petstore::deletepet::out::Out {
        let mut pets = self.pets.lock().unwrap();
        let deleted = pets.remove(&arg.id).is_some();
        crate::petstore::api::petstore::deletepet::out::Out { deleted }
    }
}
