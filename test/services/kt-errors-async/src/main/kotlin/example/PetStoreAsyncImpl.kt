package example

import baboon.runtime.shared.Either

/**
 * Errors-mode async server implementation for the petstore-errors model.
 *
 * This attempts to implement the generated PetStore interface using:
 *   - `suspend fun` methods (async mode: --kt-async-services=true)
 *   - Either<Err, Out> return types (errors mode: --service-result-no-errors=false
 *     --service-result-type=Either --service-result-pattern="<$error,$success>")
 *
 * This FAILS to compile because KtServiceWiringTranslator emits the server
 * dispatch wiring using a plain (non-suspend) lambda for IBaboonServiceRt.flatMap:
 *
 *   rt.flatMap<...>(input) { v ->
 *     impl.addPet(v)      // ERROR: suspend call inside non-suspend lambda
 *   }
 *
 * When --kt-async-services=true, the generated PetStore interface declares
 * `suspend fun addPet(...)`, so calling it inside the non-suspend rt.flatMap
 * lambda body causes a kotlinc error:
 *   "Suspend function 'suspend fun addPet(...)' should be called only from
 *    a coroutine or another suspend function."
 *
 * This reproduces defect D26 (errors+async wiring): the same root cause as D25
 * (no-errors+async), but in the errors code path of KtServiceWiringTranslator
 * (generateErrorsJsonMethod / generateErrorsUebaMethod).
 *
 * Expected kotlinc error:
 *   Suspend function 'suspend fun addPet(arg: In): Either<Err, Out>'
 *   should be called only from a coroutine or another suspend function.
 */
class PetStoreAsyncImpl : petstore.api.PetStore {
    private val pets = mutableMapOf<Long, petstore.api.Pet>()
    private var nextId = 1L

    fun reset() {
        pets.clear()
        nextId = 1L
    }

    // These methods are both `suspend` (async mode) AND return Either<Err, Out>
    // (errors mode). The generated PetStore interface declares:
    //   suspend fun addPet(arg: petstore.api.petstore.addpet.In):
    //       Either<petstore.api.petstore.addpet.Err, petstore.api.petstore.addpet.Out>
    // The generated PetStoreWiring will then try to call impl.addPet(v) inside
    // a non-suspend rt.flatMap lambda, causing a kotlinc compile error (D26).

    override suspend fun addPet(arg: petstore.api.petstore.addpet.In): Either<petstore.api.petstore.addpet.Err, petstore.api.petstore.addpet.Out> {
        val id = nextId++
        val pet = petstore.api.Pet(
            id = id,
            name = arg.name,
            status = arg.status,
            tag = arg.tag
        )
        pets[id] = pet
        return Either.Right(petstore.api.petstore.addpet.Out(pet = pet))
    }

    override suspend fun getPet(arg: petstore.api.petstore.getpet.In): Either<petstore.api.petstore.getpet.Err, petstore.api.petstore.getpet.Out> {
        val pet = pets[arg.id]
        return if (pet != null) {
            Either.Right(petstore.api.petstore.getpet.Out(pet = pet))
        } else {
            Either.Left(petstore.api.petstore.getpet.Err(
                problem = petstore.api.PetError(code = 404, message = "Pet not found: id=${arg.id}")
            ))
        }
    }
}
