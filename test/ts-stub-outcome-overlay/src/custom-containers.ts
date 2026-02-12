import { IBaboonServiceRt } from "./testpkg/pkg0/baboon-service-rt";

export type Outcome<S> =
    | { readonly tag: "Success"; readonly value: S }
    | { readonly tag: "Failure"; readonly error: unknown };

export const OutcomeServiceRt: IBaboonServiceRt = {
    pure<L, R>(value: R): Outcome<R> {
        return { tag: "Success", value };
    },
    fail<L, R>(error: L): Outcome<R> {
        return { tag: "Failure", error };
    },
    leftMap<A, B, C>(value: Outcome<B>, f: (a: A) => C): Outcome<B> {
        return value.tag === "Failure"
            ? { tag: "Failure", error: f(value.error as A) }
            : { tag: "Success", value: value.value };
    },
    flatMap<A, B, C>(value: Outcome<B>, f: (b: B) => Outcome<C>): Outcome<C> {
        return value.tag === "Failure"
            ? { tag: "Failure", error: value.error }
            : f(value.value);
    },
};
