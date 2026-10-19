#!/usr/bin/env python3
"""Synthetic multi-version model for typer/loadMany timing: VERSIONS versions of one domain, ~240 types each,
with DAG-shaped dependencies (see docs/drafts/20261019-2320-headless-sjs.md, "`loadMany` cost").

usage: gen-big-model.py <out-dir> [versions]
       baboon --model-dir <out-dir> :scheme --domains="*@*" --zip-output=big.bsm
"""
import os
import random
import sys

ENUMS = 25
FOREIGNS = 8
DTOS = 140
ADTS = 15
BRANCHES = 3
RT = ["i32", "i64", "u64", "str"]
SCALARS = ["i32", "i64", "u64", "f64", "str", "bit", "uid", "tsu"]


def field_types(rng, i):
    refs = [f"Dto{j}" for j in range(i)] or ["str"]
    pool = SCALARS + [f"Enum{e}" for e in range(ENUMS)] + [f"Ext{f}" for f in range(FOREIGNS)] + [f"Adt{a}" for a in range(min(ADTS, i // 10))]
    def one():
        k = rng.random()
        if k < 0.35:
            return rng.choice(pool)
        if k < 0.6:
            return rng.choice(refs)
        if k < 0.75:
            return f"lst[{rng.choice(refs + pool)}]"
        if k < 0.85:
            return f"opt[{rng.choice(refs + pool)}]"
        return f"map[str, {rng.choice(refs + pool)}]"
    return [one() for _ in range(rng.randint(4, 9))]


def render(version):
    out = [f'model bench.big\n\nversion "{version}.0.0"\n']
    for e in range(ENUMS):
        rng = random.Random(f"enum{e}")
        members = "\n".join(f"  M{m}" for m in range(rng.randint(3, 8)))
        out.append(f"enum Enum{e} {{\n{members}\n}}\n")
    for f in range(FOREIGNS):
        out.append(f'foreign Ext{f} {{\n  rt = {RT[f % len(RT)]}\n  cs = "System.Int64"\n  scala = "scala.Long"\n}}\n')
    dto_count = DTOS + 2 * (version - 1)
    for i in range(dto_count):
        rng = random.Random(f"dto{i}")
        types = field_types(rng, i)
        fields = [f"  f{n}: {t}" for n, t in enumerate(types)]
        fields += [f"  added{v}: opt[str]" for v in range(2, version + 1) if i % 8 == v % 8]
        body = "\n".join(fields)
        out.append(f"root data Dto{i} {{\n{body}\n}}\n")
    for a in range(ADTS):
        branches = []
        rng = random.Random(f"adt{a}")
        for b in range(BRANCHES):
            types = field_types(rng, a)
            body = "\n".join(f"    g{n}: {t}" for n, t in enumerate(types))
            branches.append(f"  data B{b} {{\n{body}\n  }}")
        out.append(f"root adt Adt{a} {{\n" + "\n".join(branches) + "\n}\n")
    return "\n".join(out)


def main():
    target = sys.argv[1]
    versions = int(sys.argv[2]) if len(sys.argv) > 2 else 8
    os.makedirs(target, exist_ok=True)
    for v in range(1, versions + 1):
        with open(os.path.join(target, f"big-{v}.baboon"), "w") as fh:
            fh.write(render(v))


main()
