// Type-level checks of index.d.ts, run by the mdl action `test-js-npm` with `tsc --noEmit`
// (not published). Each @ts-expect-error line must stay a type error.
import { BaboonCompiler, BaboonArchiveInput, BaboonLoadedModel } from "./index.js";

declare const bytes: Uint8Array;
declare const model: BaboonLoadedModel;

export const fromBytes: BaboonArchiveInput = { bytes };
export const fromBase64: BaboonArchiveInput = { base64: "UEsFBg==" };
// @ts-expect-error both input forms at once
export const both: BaboonArchiveInput = { bytes, base64: "UEsFBg==" };
// @ts-expect-error no input form
export const neither: BaboonArchiveInput = {};

export const loaded: Promise<BaboonLoadedModel> = BaboonCompiler.loadMany({ bytes });

export async function envelopes(): Promise<void> {
  const encoded = await BaboonCompiler.encodedEnvelopeLoaded(model, "{}", { envelopeVersion: 2, indexed: true });
  const decoded = await BaboonCompiler.decodeEnvelopeLoaded(model, encoded.data ?? bytes);
  const json: string | undefined = decoded.json;
  void json;
  // @ts-expect-error unsupported envelope version
  await BaboonCompiler.encodedEnvelopeLoaded(model, "{}", { envelopeVersion: 3, indexed: false });
  // @ts-expect-error the index mode is required
  await BaboonCompiler.encodedEnvelopeLoaded(model, "{}", { envelopeVersion: 1 });
}
