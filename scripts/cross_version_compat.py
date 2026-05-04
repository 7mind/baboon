#!/usr/bin/env python3
"""
Cross-version UEBA / JSON binary compatibility check.

Builds the baboon compiler at two commits (OLD and NEW), then runs the
cross-language acceptance harness in a way that isolates *codec* changes
from *schema* changes:

    Both directions reuse the OLD checkout's models and conv-test-* test
    projects. Only the BABOON BINARY differs between writer and reader.
    This means we are asking: given the same schema, does the codec
    emitted by compiler X still produce wire bytes that the codec emitted
    by compiler Y can decode?

Two scenarios per run (selectable via --direction):

    old-to-new : OLD baboon writes blobs ; NEW baboon reads them
                 (regression test: did NEW compiler break decoding of
                  output produced by an older release?)
    new-to-old : NEW baboon writes blobs ; OLD baboon reads them
                 (regression test: did NEW compiler change wire format
                  in a way that an older release can no longer decode?)

This script is intentionally NOT wired into mdl/CI. It is a manual
diagnostic — the kind of thing you reach for when a wire-format bug is
suspected, or before tagging a release that promises wire compatibility.

Usage:
    scripts/cross_version_compat.py \
        --old <ref> --new <ref> \
        [--target <dir>] \
        [--direction {old-to-new,new-to-old,both}] \
        [--langs cs scala ...] \
        [--parallelism N] [--timeout S] \
        [--keep-worktrees]
"""

import argparse
import os
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent.parent
BABOON_BIN_RELPATH = Path("baboon-compiler/.jvm/target/graalvm-native-image/baboon")


@dataclass
class Workspace:
    """A pristine checkout of a single commit, with no .git metadata.

    We deliberately avoid `git worktree add` because the sbt-git plugin's
    bundled jgit cannot read worktree-style `.git` pointer files and fails
    project loading with NoWorkTreeException. `git archive | tar -x` gives
    us the tree without any git metadata, which sbt-git tolerates.
    """
    path: Path
    ref: str
    sha: str  # full resolved sha
    short: str  # short sha for display


def run(cmd: list, cwd: Path, env: dict | None = None, check: bool = True) -> int:
    print(f"  $ (cd {cwd}) {' '.join(map(str, cmd))}")
    full_env = {**os.environ, **(env or {})}
    proc = subprocess.run(cmd, cwd=str(cwd), env=full_env)
    if check and proc.returncode != 0:
        raise SystemExit(f"command failed (exit {proc.returncode}): {cmd}")
    return proc.returncode


def resolve_ref(ref: str) -> tuple[str, str]:
    full = subprocess.check_output(
        ["git", "rev-parse", ref], cwd=str(REPO_ROOT)
    ).decode().strip()
    short = subprocess.check_output(
        ["git", "rev-parse", "--short", ref], cwd=str(REPO_ROOT)
    ).decode().strip()
    return full, short


def extract_workspace(path: Path, ref: str) -> Workspace:
    if path.exists():
        # Defensive: don't auto-rm — refuse so the user can inspect a previous run.
        raise SystemExit(
            f"refusing to overwrite existing path {path}; "
            f"remove it manually or pass --target to a fresh dir"
        )
    full, short = resolve_ref(ref)
    print(f"[workspace] {ref} -> {short} -> {path}")
    path.mkdir(parents=True)
    # `git archive` writes a tar of the commit's tree (no git metadata) to
    # stdout; `tar -x` extracts into the destination. Submodules are not
    # included — fine for our purposes since the baboon compiler build
    # does not depend on them.
    archive = subprocess.Popen(
        ["git", "archive", "--format=tar", full],
        cwd=str(REPO_ROOT),
        stdout=subprocess.PIPE,
    )
    extract = subprocess.Popen(["tar", "-x"], cwd=str(path), stdin=archive.stdout)
    if archive.stdout is not None:
        archive.stdout.close()
    extract_rc = extract.wait()
    archive_rc = archive.wait()
    if archive_rc != 0 or extract_rc != 0:
        raise SystemExit(
            f"git archive | tar failed for {ref} "
            f"(archive rc={archive_rc}, tar rc={extract_rc})"
        )
    return Workspace(path=path, ref=ref, sha=full, short=short)


def remove_workspace(ws: Workspace):
    print(f"[workspace] removing {ws.path}")
    shutil.rmtree(str(ws.path), ignore_errors=True)


def build_baboon(ws: Workspace, out_path: Path):
    """Build baboon native-image inside the workspace, copy binary out.

    We invoke sbt directly rather than `mdl :build`. mdl runs actions in
    a managed run-dir and exposes the binary via interpolated variables
    (${action.build.binary}); when invoked from outside the mdl pipeline
    we have no way to extract that path. sbt by contrast writes
    deterministically to <workspace>/baboon-compiler/.jvm/target/...
    """
    print(f"[build] {ws.short}: sbt baboonJVM/GraalVMNativeImage/packageBin")
    run(
        ["sbt", "-batch", "baboonJVM/GraalVMNativeImage/packageBin"],
        cwd=ws.path,
    )
    src = ws.path / BABOON_BIN_RELPATH
    if not src.exists():
        raise SystemExit(f"baboon binary not found at {src} after build")
    out_path.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(str(src), str(out_path))
    os.chmod(str(out_path), 0o755)
    print(f"[build] {ws.short}: binary -> {out_path}")


def run_acceptance(
    *,
    runner_script: Path,
    baboon_bin: Path,
    source_root: Path,
    target: Path,
    phase: str,
    blobs_dir: Path | None,
    langs: list[str] | None,
    parallelism: int,
    timeout: int,
) -> int:
    target.mkdir(parents=True, exist_ok=True)
    cmd = [
        sys.executable, str(runner_script),
        "--baboon", str(baboon_bin),
        "--source-root", str(source_root),
        "--target", str(target),
        "--phase", phase,
        "--parallelism", str(parallelism),
        "--timeout", str(timeout),
    ]
    if blobs_dir is not None:
        cmd.extend(["--blobs-dir", str(blobs_dir)])
    if langs:
        cmd.append("--langs")
        cmd.extend(langs)
    print(f"[acceptance] phase={phase} target={target}")
    print(f"  $ {' '.join(cmd)}")
    proc = subprocess.run(cmd)
    return proc.returncode


def run_direction(
    *,
    label: str,
    writer_bin: Path,
    reader_bin: Path,
    source_root: Path,
    runner_script: Path,
    target_root: Path,
    langs: list[str] | None,
    parallelism: int,
    timeout: int,
) -> bool:
    """Run a single (writer -> reader) compat scenario.

    Both phases use `source_root` (the OLD checkout), so models and
    conv-test-* projects are identical on both sides. Only the baboon
    binary used for codegen differs.
    """
    print(f"\n{'#' * 70}")
    print(f"# DIRECTION: {label}")
    print(f"#   writer baboon: {writer_bin}")
    print(f"#   reader baboon: {reader_bin}")
    print(f"#   source root:   {source_root}")
    print(f"{'#' * 70}\n")

    write_target = target_root / label / "write"
    read_target = target_root / label / "read"

    # Phase 1: writer side — codegen with WRITER baboon, build, produce blobs
    rc = run_acceptance(
        runner_script=runner_script,
        baboon_bin=writer_bin,
        source_root=source_root,
        target=write_target,
        phase="write",
        blobs_dir=None,
        langs=langs,
        parallelism=parallelism,
        timeout=timeout,
    )
    if rc != 0:
        print(f"[{label}] writer phase FAILED (exit {rc}); aborting this direction")
        return False

    blobs_dir = write_target / "compat-data"
    if not blobs_dir.exists():
        print(f"[{label}] no compat-data dir at {blobs_dir} after write phase")
        return False

    # Phase 2: reader side — codegen with READER baboon over the SAME source_root,
    #          build, then read blobs produced in phase 1.
    rc = run_acceptance(
        runner_script=runner_script,
        baboon_bin=reader_bin,
        source_root=source_root,
        target=read_target,
        phase="read",
        blobs_dir=blobs_dir,
        langs=langs,
        parallelism=parallelism,
        timeout=timeout,
    )
    summary_path = read_target / "acceptance-summary.md"
    print(f"\n[{label}] read phase exit={rc}; summary: {summary_path}")
    return rc == 0


def main():
    parser = argparse.ArgumentParser(
        description="Cross-version baboon codec compatibility check.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument("--old", required=True, help="Older git ref (sha/branch/tag)")
    parser.add_argument("--new", required=True, help="Newer git ref (sha/branch/tag)")
    parser.add_argument(
        "--target",
        default=str(REPO_ROOT / "target" / "x-version"),
        help="Workspace dir (default: target/x-version)",
    )
    parser.add_argument(
        "--direction",
        choices=("old-to-new", "new-to-old", "both"),
        default="both",
    )
    parser.add_argument(
        "--langs",
        nargs="*",
        default=None,
        help="Subset of languages (cs scala python rust typescript kotlin "
             "kotlin-kmp java dart swift). Default: all.",
    )
    parser.add_argument(
        "--parallelism",
        type=int,
        default=max(1, (os.cpu_count() or 2) - 1),
    )
    parser.add_argument("--timeout", type=int, default=900)
    parser.add_argument(
        "--keep-worktrees",
        action="store_true",
        help="Don't remove worktrees on success (useful for inspection).",
    )
    args = parser.parse_args()

    target_root = Path(args.target).resolve()
    target_root.mkdir(parents=True, exist_ok=True)

    # Resolve refs early so we can fail fast on typos.
    old_full, old_short = resolve_ref(args.old)
    new_full, new_short = resolve_ref(args.new)
    if old_full == new_full:
        raise SystemExit(f"--old and --new resolve to the same commit: {old_full}")

    print(f"OLD: {args.old} -> {old_short}")
    print(f"NEW: {args.new} -> {new_short}")
    print(f"Target: {target_root}")
    print(f"Direction: {args.direction}")

    workspaces_dir = target_root / "workspaces"
    bins_dir = target_root / "binaries"
    bins_dir.mkdir(parents=True, exist_ok=True)

    old_ws_path = workspaces_dir / f"old-{old_short}"
    new_ws_path = workspaces_dir / f"new-{new_short}"

    old_ws = extract_workspace(old_ws_path, old_full)
    new_ws = extract_workspace(new_ws_path, new_full)

    old_bin = bins_dir / f"baboon-{old_short}"
    new_bin = bins_dir / f"baboon-{new_short}"

    # The runner script is always taken from the orchestrator's own checkout,
    # not from a workspace. The orchestrator and the runner are versioned
    # together; pulling the runner from a workspace would require the
    # orchestrator-aware flags (--source-root / --phase / --blobs-dir) to
    # already be committed at that ref, which they may not be.
    runner_script = REPO_ROOT / "test" / "acceptance" / "run_acceptance.py"
    if not runner_script.exists():
        raise SystemExit(f"runner script not found at {runner_script}")

    # Both writer and reader use OLD's models + OLD's conv-test-* projects.
    # This is intentional: we are isolating *codec* differences from
    # *schema* differences. See module docstring.
    source_root = old_ws.path

    overall_ok = True
    crashed = False
    try:
      try:
        # Build both binaries up front so a build break aborts before we
        # run any acceptance phases.
        build_baboon(old_ws, old_bin)
        build_baboon(new_ws, new_bin)

        if args.direction in ("old-to-new", "both"):
            ok = run_direction(
                label="old-to-new",
                writer_bin=old_bin,
                reader_bin=new_bin,
                source_root=source_root,
                runner_script=runner_script,
                target_root=target_root,
                langs=args.langs,
                parallelism=args.parallelism,
                timeout=args.timeout,
            )
            overall_ok = overall_ok and ok

        if args.direction in ("new-to-old", "both"):
            ok = run_direction(
                label="new-to-old",
                writer_bin=new_bin,
                reader_bin=old_bin,
                source_root=source_root,
                runner_script=runner_script,
                target_root=target_root,
                langs=args.langs,
                parallelism=args.parallelism,
                timeout=args.timeout,
            )
            overall_ok = overall_ok and ok

      except BaseException:
        crashed = True
        raise
    finally:
        if overall_ok and not crashed and not args.keep_worktrees:
            remove_workspace(old_ws)
            remove_workspace(new_ws)
        else:
            print(
                f"\nWorkspaces retained for inspection:\n"
                f"  {old_ws.path}\n  {new_ws.path}\n"
                f"Remove with: rm -rf <path>"
            )

    print("\n" + "=" * 70)
    print(f"OVERALL: {'PASS' if overall_ok else 'FAIL'}")
    print(f"Reports under: {target_root}/<direction>/read/acceptance-summary.md")
    print("=" * 70)
    return 0 if overall_ok else 1


if __name__ == "__main__":
    sys.exit(main())
