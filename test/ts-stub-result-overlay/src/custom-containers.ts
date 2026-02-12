import { IBaboonServiceRt } from "./testpkg/pkg0/baboon-service-rt";

export type Result<S, E> =
    | { readonly tag: "Success"; readonly value: S }
    | { readonly tag: "Failure"; readonly error: E };

export const ResultServiceRt: IBaboonServiceRt = {
    pure<L, R>(value: R): Result<R, L> {
        return { tag: "Success", value };
    },
    fail<L, R>(error: L): Result<R, L> {
        return { tag: "Failure", error };
    },
    leftMap<A, B, C>(value: Result<B, A>, f: (a: A) => C): Result<B, C> {
        return value.tag === "Failure"
            ? { tag: "Failure", error: f(value.error) }
            : { tag: "Success", value: value.value };
    },
    flatMap<A, B, C>(value: Result<B, A>, f: (b: B) => Result<C, A>): Result<C, A> {
        return value.tag === "Failure"
            ? { tag: "Failure", error: value.error }
            : f(value.value);
    },
};
