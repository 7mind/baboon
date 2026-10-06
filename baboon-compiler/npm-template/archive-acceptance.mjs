// CLI -> ZIP -> JS acceptance (mdl action `test-js-npm`). Not part of the published package:
// it needs the archive written by the native `baboon :scheme --zip-output` (BABOON_SCHEMA_ZIP)
// and copies of it rewritten by Python's zipfile, stored with directory entries and extra fields
// (BABOON_OTHER_STORED_ZIP) and deflated (BABOON_OTHER_DEFLATED_ZIP), plus archives whose schema
// includes a *.bmo entry by a root-relative path (BABOON_INCLUDE_ZIP) or one leaving the archive
// (BABOON_ESCAPING_INCLUDE_ZIP).
// The binary vectors are the cross-language goldens of docs/spec/codec-envelope.md § 2.1.4.
import test from "ava";
import { readFileSync } from "fs";

import { BaboonCompiler } from "./index.js";

function requiredFile(variable) {
  const path = process.env[variable];
  if (!path) {
    throw new Error(`${variable} must name an archive (see .mdl/defs/tests.md, action test-js-npm)`);
  }
  return new Uint8Array(readFileSync(path));
}

const cliZip = requiredFile("BABOON_SCHEMA_ZIP");
const otherStored = requiredFile("BABOON_OTHER_STORED_ZIP");
const otherDeflated = requiredFile("BABOON_OTHER_DEFLATED_ZIP");
const withInclude = requiredFile("BABOON_INCLUDE_ZIP");
const escapingInclude = requiredFile("BABOON_ESCAPING_INCLUDE_ZIP");

const hex = bytes => Array.from(bytes, b => b.toString(16).toUpperCase().padStart(2, "0")).join(" ");
const unhex = s => new Uint8Array(s.split(" ").map(b => parseInt(b, 16)));
const base64 = bytes => Buffer.from(bytes).toString("base64");

const V1C = { envelopeVersion: 1, indexed: false };
const V1I = { envelopeVersion: 1, indexed: true };
const V2C = { envelopeVersion: 2, indexed: false };
const V2I = { envelopeVersion: 2, indexed: true };

const APPEND_VAR = '{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdAppendVar","$rv":"1.0.0","$c":{"a":42,"b":"hi","t":"t"}}';
const STABLE = '{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdStable","$uv":"1.0.0","$c":{"s":"s"}}';
const ENUM_HOST = '{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdEnumHost","$c":{"e":"C"}}';
const CHAIN = '{"$mv":1,"$d":"fwde2e.chain","$v":"3.0.0","$t":"fwde2e.chain/:#ChainAppend","$rv":"1.0.0","$c":{"a":1,"b":"b","c":"c"}}';

const GOLDEN = [
  [APPEND_VAR, V1C, "01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74"],
  [APPEND_VAR, V2C, "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 02 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74"],
  [APPEND_VAR, V2I, "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 01 04 00 00 00 03 00 00 00 07 00 00 00 03 00 00 00 2A 00 00 00 02 68 69 01 01 74"],
  [STABLE, V1C, "01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73"],
  [STABLE, V2C, "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73"],
  [ENUM_HOST, V2C, "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 18 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 45 6E 75 6D 48 6F 73 74 00 02"],
  [CHAIN, V2C, "02 0C 66 77 64 65 32 65 2E 63 68 61 69 6E 05 33 2E 30 2E 30 02 05 31 2E 30 2E 30 1A 66 77 64 65 32 65 2E 63 68 61 69 6E 2F 3A 23 43 68 61 69 6E 41 70 70 65 6E 64 00 01 00 00 00 01 01 62 01 01 63"]
];
// a ForwardWritePolicy.Tolerant v1 writer puts the prefix bound into the single slot
const APPEND_VAR_V1_TOLERANT = "01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74";

async function encodeOk(t, model, json, options) {
  const result = await BaboonCompiler.encodedEnvelopeLoaded(model, json, options);
  t.true(result.success, `${json} ${JSON.stringify(options)}: ${result.error}`);
  return result.data;
}

async function decodeOk(t, model, bytes) {
  const result = await BaboonCompiler.decodeEnvelopeLoaded(model, bytes);
  t.true(result.success, `${hex(bytes)}: ${result.error}`);
  return result.json;
}

const fromBytes = BaboonCompiler.loadMany({ bytes: cliZip });
const fromBase64 = BaboonCompiler.loadMany({ base64: base64(cliZip) });

test("a CLI archive loads every domain and version, as bytes and as base64", async t => {
  for (const model of [await fromBytes, await fromBase64]) {
    const versions = new Set(BaboonCompiler.listTypes(model).map(ty => `${ty.pkg}@${ty.version}`));
    t.deepEqual(
      [...versions].sort(),
      [
        "fwde2e.chain@1.0.0", "fwde2e.chain@2.0.0", "fwde2e.chain@3.0.0",
        "fwde2e.fwd@1.0.0", "fwde2e.fwd@2.0.0",
        "zipdemo.evo@1.0.0", "zipdemo.evo@2.0.0", "zipdemo.evo@3.0.0",
        "zipdemo.revert@1.0.0", "zipdemo.revert@2.0.0", "zipdemo.revert@3.0.0",
        "zipdemo.shapes@1.0.0", "zipdemo.shapes@2.0.0"
      ]
    );
    const evo = BaboonCompiler.listTypes(model).filter(ty => ty.pkg === "zipdemo.evo").map(ty => `${ty.version}:${ty.name}`).sort();
    t.deepEqual(evo, ["1.0.0:Old", "2.0.0:Mid", "3.0.0:New"]);
  }
});

test("envelopes match the cross-language golden bytes in both directions", async t => {
  for (const model of [await fromBytes, await fromBase64]) {
    for (const [json, options, golden] of GOLDEN) {
      t.is(hex(await encodeOk(t, model, json, options)), golden);
      t.is(await decodeOk(t, model, unhex(golden)), json);
    }
    t.is(await decodeOk(t, model, unhex(APPEND_VAR_V1_TOLERANT)), APPEND_VAR);
  }
});

test("DTO, enum, ADT, branch, namespaced and foreign-carrying envelopes round-trip", async t => {
  const model = await fromBytes;
  const envelopes = [
    '{"$mv":1,"$d":"zipdemo.shapes","$v":"1.0.0","$t":"zipdemo.shapes/:#Order","$c":{"id":"6c0e3e4e-2d53-4c2a-9a39-5d3f1b0f7a11","color":"Red","price":"-9007199254740993","tags":{"opaque key":"v","":"empty"},"at":{"x":-1,"y":2147483647},"shape":{"Circle":{"r":1.5}},"label":"x"}}',
    '{"$mv":1,"$d":"zipdemo.shapes","$v":"2.0.0","$t":"zipdemo.shapes/:#Color","$uv":"1.0.0","$c":"Green"}',
    '{"$mv":1,"$d":"zipdemo.shapes","$v":"2.0.0","$t":"zipdemo.shapes/:#Shape","$uv":"1.0.0","$c":{"Circle":{"r":0.25}}}',
    '{"$mv":1,"$d":"zipdemo.shapes","$v":"1.0.0","$t":"zipdemo.shapes/[zipdemo.shapes/:#Shape]#Square","$c":{"side":7}}',
    '{"$mv":1,"$d":"zipdemo.shapes","$v":"2.0.0","$t":"zipdemo.shapes/geo#Point","$uv":"1.0.0","$c":{"x":3,"y":4}}'
  ];
  for (const json of envelopes) {
    for (const options of [V1C, V1I, V2C, V2I]) {
      t.is(await decodeOk(t, model, await encodeOk(t, model, json, options)), json, JSON.stringify(options));
    }
  }
});

test("evolution survives the archive: bounds come from the reloaded lineage, per format", async t => {
  const model = await fromBytes;
  // Order 2.0.0 renames a field: UEBA unchanged since 1.0.0 (v2 readableMin 1.0.0), JSON is not (no $uv/$rv)
  const order = '{"$mv":1,"$d":"zipdemo.shapes","$v":"2.0.0","$t":"zipdemo.shapes/:#Order","$c":{"id":"6c0e3e4e-2d53-4c2a-9a39-5d3f1b0f7a11","colour":"Green","price":"1","tags":{},"at":{"x":1,"y":2},"shape":{"Square":{"side":3}},"label":null}}';
  const v2 = await encodeOk(t, model, order, V2C);
  t.is(hex(v2.slice(0, 30)), "02 0E 7A 69 70 64 65 6D 6F 2E 73 68 61 70 65 73 05 32 2E 30 2E 30 02 05 31 2E 30 2E 30 16");
  t.is(await decodeOk(t, model, v2), order);
  // Flip changed in 2.0.0 and reverted in 3.0.0: its 3.0.0 bytes are not 1.0.0 bytes (no $uv)
  const flip = '{"$mv":1,"$d":"zipdemo.revert","$v":"3.0.0","$t":"zipdemo.revert/:#Flip","$c":{"a":1}}';
  t.is(await decodeOk(t, model, await encodeOk(t, model, flip, V1C)), flip);
});

test("conversion never migrates or silently forward-reads", async t => {
  const model = await fromBytes;
  // a 3.0.0 writer of a type unchanged since 1.0.0 converts losslessly and keeps its version
  const stable3 = STABLE.replace('"$v":"2.0.0"', '"$v":"3.0.0"');
  t.is(await decodeOk(t, model, await encodeOk(t, model, stable3, V2C)), stable3);
  const lossy = await BaboonCompiler.encodedEnvelopeLoaded(model, APPEND_VAR.replace('"$v":"2.0.0"', '"$v":"3.0.0"'), V1C);
  t.false(lossy.success);
  t.regex(lossy.error, /Refusing lossy envelope conversion: .* reaches none of them/);
  const v1Newer = await BaboonCompiler.decodeEnvelopeLoaded(model, unhex(GOLDEN[3][2].replace("05 32 2E 30 2E 30", "05 33 2E 30 2E 30")));
  t.regex(v1Newer.error, /binary v1 envelope cannot prove/);
});

test("envelope failures are explicit", async t => {
  const model = await fromBytes;
  const failures = [
    [APPEND_VAR.replace('"fwde2e.fwd/:#FwdAppendVar"', '"FwdAppendVar"'), /type 'FwdAppendVar' does not exist/],
    [APPEND_VAR.replace('"$d":"fwde2e.fwd"', '"$d":"no.such"'), /domain 'no.such' is not loaded/],
    [STABLE.replace('"$v":"2.0.0"', '"$v":"1.5.0"'), /is not loaded \(loaded: 1.0.0, 2.0.0\)/],
    [APPEND_VAR.replace('"$mv":1', '"$mv":2'), /malformed JSON envelope metadata/],
    [APPEND_VAR.replace('"$rv":"1.0.0"', '"$rv":"3.0.0"'), /readableMin <= minCompat <= version/],
    [STABLE.replace('{"s":"s"}', '{"s":"s","extra":1}'), /does not declare: extra/],
    ['{"$d":"zipdemo.shapes","$v":"1.0.0","$t":"zipdemo.shapes/:#WithOpaque","$c":{"o":"x"}}', /Foreign types without rt binding cannot be encoded/],
    ['{"$d":"zipdemo.shapes","$v":"1.0.0","$t":"zipdemo.shapes/:#WithAny","$c":{"v":{}}}', /`any`/],
    ['{"$d":"zipdemo.shapes","$v":"1.0.0","$t":"zipdemo.shapes/:#Cents","$c":"1"}', /never travel in a top-level envelope/]
  ];
  for (const [json, message] of failures) {
    const result = await BaboonCompiler.encodedEnvelopeLoaded(model, json, V1C);
    t.false(result.success, json);
    t.regex(result.error, message);
  }
  const legacy = await encodeOk(t, model, APPEND_VAR.replace('"$mv":1', '"$mv":"1"'), V1C);
  t.is(hex(legacy), GOLDEN[0][2]);
  const stable = unhex(GOLDEN[3][2]);
  for (const [bytes, message] of [
    [new Uint8Array(0), /empty input/],
    [Uint8Array.of(16, ...stable.slice(1)), /unsupported binary envelope metaVersion 16/],
    [stable.slice(0, 7), /truncated binary envelope header/],
    [unhex(GOLDEN[0][2]).slice(0, -1), /truncated payload/],
    [Uint8Array.of(...stable, 0), /unread byte\(s\)/]
  ]) {
    const result = await BaboonCompiler.decodeEnvelopeLoaded(model, bytes);
    t.false(result.success, hex(bytes));
    t.regex(result.error, message);
  }
});

test("archives from another writer load when stored, with directories and extra fields", async t => {
  const model = await BaboonCompiler.loadMany({ bytes: otherStored });
  t.is(hex(await encodeOk(t, model, STABLE, V1C)), GOLDEN[3][2]);
  await t.throwsAsync(BaboonCompiler.loadMany({ bytes: otherDeflated }), { message: /^Loading failed: .*compression method 8/ });
});

test("*.bmo entries serve includes resolved against the archive root", async t => {
  const model = await BaboonCompiler.loadMany({ bytes: withInclude });
  t.deepEqual(
    BaboonCompiler.listTypes(model).map(ty => `${ty.pkg}@${ty.version}:${ty.id}`),
    ["zipdemo.inc@1.0.0:zipdemo.inc/:#Included"]
  );
  const error = await t.throwsAsync(BaboonCompiler.loadMany({ bytes: escapingInclude }));
  t.regex(error.message, /^Loading failed: /);
  t.regex(error.message, /\.\.\/shared\/defs\.bmo/);
});

test("damaged archives are rejected", async t => {
  const corrupt = cliZip.slice();
  corrupt[100] ^= 0xFF;
  await t.throwsAsync(BaboonCompiler.loadMany({ bytes: corrupt }), { message: /^Loading failed: .*CRC-32/ });
  await t.throwsAsync(BaboonCompiler.loadMany({ bytes: cliZip.slice(0, cliZip.length - 1) }), { message: /^Loading failed: not a ZIP archive/ });
  await t.throwsAsync(BaboonCompiler.loadMany({ base64: `data:application/zip;base64,${base64(cliZip)}` }), { message: /not valid base64/ });
});
