import {type BaboonEither} from '../../../BaboonSharedRuntime'

type _BaboonEither<L, R> = BaboonEither<L, R>;

export interface IBaboonServiceRt {
    pure<L, R>(value: R): BaboonEither<L, R>;
    fail<L, R>(error: L): BaboonEither<L, R>;
    leftMap<A, B, C>(value: BaboonEither<A, B>, f: (a: A) => C): BaboonEither<C, B>;
    flatMap<A, B, C>(value: BaboonEither<A, B>, f: (b: B) => BaboonEither<A, C>): BaboonEither<A, C>;
}

export const BaboonServiceRtDefault: IBaboonServiceRt = {
    pure<L, R>(value: R): BaboonEither<L, R> { return { tag: 'Right', value }; },
    fail<L, R>(error: L): BaboonEither<L, R> { return { tag: 'Left', value: error }; },
    leftMap<A, B, C>(value: BaboonEither<A, B>, f: (a: A) => C): BaboonEither<C, B> {
        return (value as { readonly tag: string }).tag === 'Left'
            ? { tag: 'Left', value: f((value as { readonly value: A }).value) }
            : value as unknown as BaboonEither<C, B>;
    },
    flatMap<A, B, C>(value: BaboonEither<A, B>, f: (b: B) => BaboonEither<A, C>): BaboonEither<A, C> {
        return (value as { readonly tag: string }).tag === 'Left'
            ? value as unknown as BaboonEither<A, C>
            : f((value as { readonly value: B }).value);
    },
};