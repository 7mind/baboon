#!/usr/bin/env python3
"""
Baboon cross-language serialization acceptance test runner.

Tests all (format, from_lang, to_lang) triplets across 9 languages and 2 formats,
producing an HTML report with color-coded compatibility matrices.

Usage:
    python3 run_acceptance.py --baboon <path> --target <dir> [--parallelism N] [--timeout S]
"""

import argparse
import asyncio
import json
import os
import shlex
import shutil
import signal
import subprocess
import sys
import time
from dataclasses import dataclass, field
from enum import Enum
from html import escape as html_escape
from pathlib import Path
from typing import Optional


# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------

class Lang(Enum):
    CS = "cs"
    SCALA = "scala"
    PYTHON = "python"
    RUST = "rust"
    TYPESCRIPT = "typescript"
    KOTLIN = "kotlin"
    JAVA = "java"
    DART = "dart"
    SWIFT = "swift"


class Format(Enum):
    JSON = "json"
    UEBA = "ueba"


class Status(Enum):
    PASSED = "P"
    BUILD_FAILED = "B"
    SERIALIZATION_FAILED = "S"
    UNEXPECTED = "U"


LANG_DISPLAY = {
    Lang.CS: "C#",
    Lang.SCALA: "Scala",
    Lang.PYTHON: "Python",
    Lang.RUST: "Rust",
    Lang.TYPESCRIPT: "TypeScript",
    Lang.KOTLIN: "Kotlin",
    Lang.JAVA: "Java",
    Lang.DART: "Dart",
    Lang.SWIFT: "Swift",
}


@dataclass
class StepResult:
    status: Status
    duration_sec: float
    stdout: str = ""
    stderr: str = ""
    return_code: Optional[int] = None


@dataclass
class AcceptanceReport:
    codegen_result: Optional[StepResult] = None
    build_results: dict = field(default_factory=dict)
    write_results: dict = field(default_factory=dict)
    read_results: dict = field(default_factory=dict)
    total_duration_sec: float = 0.0


# ---------------------------------------------------------------------------
# Language configuration
# ---------------------------------------------------------------------------

@dataclass
class LangConfig:
    dir_name: str
    baboon_target: str
    baboon_output: str
    build_cmds: list
    write_cmd: object  # callable(abs_output_dir, format_str) -> (cmd_list, use_shell)
    read_cmd: object   # callable(abs_file_path) -> (cmd_list, use_shell)
    rsync_excludes: list = field(default_factory=list)


def _shell_cmd(shell_str: str):
    return (["bash", "-c", shell_str], False)

def _swift_shell_cmd(swift_cmd: str):
    return _shell_cmd(
        f"""
set -euo pipefail
SWIFT_BIN="$(readlink -f "$(command -v swift)")"
SWIFT_DEPS="$(nix-store -q --requisites "$SWIFT_BIN")"
SWIFT_LIBDISPATCH="$(echo "$SWIFT_DEPS" | grep 'swift-corelibs-libdispatch' | head -n1)/lib"
SWIFT_STDLIB="$(echo "$SWIFT_DEPS" | grep -E 'swift-.*-lib$' | head -n1)/lib/swift/linux"
if [ ! -d "$SWIFT_LIBDISPATCH" ] || [ ! -d "$SWIFT_STDLIB" ]; then
  echo "Swift runtime libraries are not available: SWIFT_LIBDISPATCH=$SWIFT_LIBDISPATCH SWIFT_STDLIB=$SWIFT_STDLIB" >&2
  exit 1
fi
export LD_LIBRARY_PATH="$SWIFT_LIBDISPATCH:$SWIFT_STDLIB:${{LD_LIBRARY_PATH:-}}"
{swift_cmd}
""".strip()
    )


LANG_CONFIGS: dict[Lang, LangConfig] = {
    Lang.CS: LangConfig(
        dir_name="conv-test-cs",
        baboon_target=":cs",
        baboon_output="ConvTest/Generated",
        build_cmds=[
            (["dotnet", "build", "-c", "Release", "ConvTest/ConvTest.csproj"], False),
        ],
        write_cmd=lambda d, f: (
            ["dotnet", "run", "-c", "Release", "--no-build",
             "--project", "ConvTest/ConvTest.csproj", "--", "write", d, f],
            False,
        ),
        read_cmd=lambda p: (
            ["dotnet", "run", "-c", "Release", "--no-build",
             "--project", "ConvTest/ConvTest.csproj", "--", "read", p],
            False,
        ),
        rsync_excludes=["bin", "obj", "Generated"],
    ),
    Lang.SCALA: LangConfig(
        dir_name="conv-test-sc",
        baboon_target=":scala",
        baboon_output="src/main/scala/generated-main",
        build_cmds=[
            (["sbt", "-batch", "compile"], False),
        ],
        write_cmd=lambda d, f: _shell_cmd(
            f'sbt -batch "runMain example.CompatMain write {d} {f}"'
        ),
        read_cmd=lambda p: _shell_cmd(
            f'sbt -batch "runMain example.CompatMain read {p}"'
        ),
        rsync_excludes=["target", "project/target", "generated-main"],
    ),
    Lang.PYTHON: LangConfig(
        dir_name="conv-test-py",
        baboon_target=":python",
        baboon_output="Generated",
        build_cmds=[
            (["python3", "-m", "venv", ".venv"], False),
            _shell_cmd(
                'source .venv/bin/activate && python3 -m pip install -q -r requirements.txt'
            ),
        ],
        write_cmd=lambda d, f: _shell_cmd(
            f'source .venv/bin/activate && python3 compat_main.py write {d} {f}'
        ),
        read_cmd=lambda p: _shell_cmd(
            f'source .venv/bin/activate && python3 compat_main.py read {p}'
        ),
        rsync_excludes=[".venv", "Generated", "__pycache__"],
    ),
    Lang.RUST: LangConfig(
        dir_name="conv-test-rs",
        baboon_target=":rust",
        baboon_output="src/generated",
        build_cmds=[
            (["cargo", "build", "--release"], False),
        ],
        write_cmd=lambda d, f: (
            ["cargo", "run", "--release", "--", "write", d, f],
            False,
        ),
        read_cmd=lambda p: (
            ["cargo", "run", "--release", "--", "read", p],
            False,
        ),
        rsync_excludes=["target", "generated"],
    ),
    Lang.TYPESCRIPT: LangConfig(
        dir_name="conv-test-ts",
        baboon_target=":typescript",
        baboon_output="src/generated",
        build_cmds=[
            (["npm", "install"], False),
        ],
        write_cmd=lambda d, f: (
            ["npx", "tsx", "src/compat_main.ts", "write", d, f],
            False,
        ),
        read_cmd=lambda p: (
            ["npx", "tsx", "src/compat_main.ts", "read", p],
            False,
        ),
        rsync_excludes=["node_modules", "dist", "generated"],
    ),
    Lang.KOTLIN: LangConfig(
        dir_name="conv-test-kt",
        baboon_target=":kotlin",
        baboon_output="src/main/kotlin/generated-main",
        build_cmds=[
            (["gradle", "--no-daemon", "build", "-x", "test"], False),
        ],
        write_cmd=lambda d, f: (
            ["gradle", "--no-daemon", "run", f"--args=write {d} {f}"],
            False,
        ),
        read_cmd=lambda p: (
            ["gradle", "--no-daemon", "run", f"--args=read {p}"],
            False,
        ),
        rsync_excludes=["build", ".gradle", "generated-main"],
    ),
    Lang.JAVA: LangConfig(
        dir_name="conv-test-jv",
        baboon_target=":java",
        baboon_output="src/main/java/generated-main",
        build_cmds=[
            (["mvn", "-q", "compile"], False),
        ],
        write_cmd=lambda d, f: (
            ["mvn", "-q", "exec:java", f"-Dexec.args=write {d} {f}"],
            False,
        ),
        read_cmd=lambda p: (
            ["mvn", "-q", "exec:java", f"-Dexec.args=read {p}"],
            False,
        ),
        rsync_excludes=["target", "generated-main"],
    ),
    Lang.DART: LangConfig(
        dir_name="conv-test-dt",
        baboon_target=":dart",
        baboon_output="lib/generated",
        build_cmds=[
            (["dart", "pub", "get"], False),
        ],
        write_cmd=lambda d, f: (
            ["dart", "run", "bin/compat_main.dart", "write", d, f],
            False,
        ),
        read_cmd=lambda p: (
            ["dart", "run", "bin/compat_main.dart", "read", p],
            False,
        ),
        rsync_excludes=[".dart_tool", "pubspec.lock", "generated"],
    ),
    Lang.SWIFT: LangConfig(
        dir_name="conv-test-sw",
        baboon_target=":swift",
        baboon_output="Sources/BaboonGenerated",
        build_cmds=[
            _swift_shell_cmd("swift build -c release -Xswiftc -enable-testing"),
        ],
        write_cmd=lambda d, f: _swift_shell_cmd(
            f"swift run -c release -Xswiftc -enable-testing CompatMain write {shlex.quote(d)} {shlex.quote(f)}"
        ),
        read_cmd=lambda p: _swift_shell_cmd(
            f"swift run -c release -Xswiftc -enable-testing CompatMain read {shlex.quote(p)}"
        ),
        rsync_excludes=[".build", "Sources/BaboonGenerated"],
    ),
}


# ---------------------------------------------------------------------------
# Process management
# ---------------------------------------------------------------------------

_children: list[asyncio.subprocess.Process] = []
_shutting_down = False
_lang_locks: dict = {}


def _get_lang_lock(lang: Lang) -> asyncio.Lock:
    """Per-language lock to prevent concurrent operations in the same project dir.

    SBT, Gradle, and Maven use daemon/server processes that conflict when
    multiple invocations target the same project directory concurrently.
    """
    if lang not in _lang_locks:
        _lang_locks[lang] = asyncio.Lock()
    return _lang_locks[lang]


async def run_subprocess(
    cmd: list[str],
    cwd: str,
    semaphore: asyncio.Semaphore,
    timeout: int = 300,
    use_shell: bool = False,
    env: Optional[dict] = None,
) -> StepResult:
    async with semaphore:
        return await _run_subprocess_inner(cmd, cwd, timeout, use_shell, env)


async def run_subprocess_unsemaphored(
    cmd: list[str],
    cwd: str,
    timeout: int = 300,
    use_shell: bool = False,
    env: Optional[dict] = None,
) -> StepResult:
    return await _run_subprocess_inner(cmd, cwd, timeout, use_shell, env)


async def _run_subprocess_inner(
    cmd: list[str],
    cwd: str,
    timeout: int,
    use_shell: bool,
    env: Optional[dict],
) -> StepResult:
    start = time.monotonic()
    merged_env = {**os.environ, **(env or {})}

    try:
        if use_shell:
            proc = await asyncio.create_subprocess_shell(
                cmd if isinstance(cmd, str) else " ".join(cmd),
                cwd=cwd,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                env=merged_env,
                start_new_session=True,
            )
        else:
            proc = await asyncio.create_subprocess_exec(
                *cmd,
                cwd=cwd,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                env=merged_env,
                start_new_session=True,
            )
        _children.append(proc)
        try:
            stdout_bytes, stderr_bytes = await asyncio.wait_for(
                proc.communicate(), timeout=timeout
            )
        finally:
            if proc in _children:
                _children.remove(proc)

        duration = time.monotonic() - start
        stdout = stdout_bytes.decode("utf-8", errors="replace")
        stderr = stderr_bytes.decode("utf-8", errors="replace")

        if proc.returncode == 0:
            return StepResult(Status.PASSED, duration, stdout, stderr, 0)
        elif proc.returncode == 1:
            return StepResult(Status.SERIALIZATION_FAILED, duration, stdout, stderr, 1)
        else:
            return StepResult(
                Status.UNEXPECTED, duration, stdout, stderr, proc.returncode
            )

    except asyncio.TimeoutError:
        duration = time.monotonic() - start
        try:
            os.killpg(os.getpgid(proc.pid), signal.SIGTERM)
        except (ProcessLookupError, OSError):
            pass
        try:
            await asyncio.wait_for(proc.wait(), timeout=5)
        except (asyncio.TimeoutError, ProcessLookupError):
            try:
                os.killpg(os.getpgid(proc.pid), signal.SIGKILL)
            except (ProcessLookupError, OSError):
                pass
        if proc in _children:
            _children.remove(proc)
        return StepResult(
            Status.UNEXPECTED, duration, "", f"Timed out after {timeout}s", -1
        )

    except Exception as e:
        duration = time.monotonic() - start
        return StepResult(Status.UNEXPECTED, duration, "", str(e), -1)


def terminate_all_children():
    global _shutting_down
    _shutting_down = True
    for proc in list(_children):
        try:
            os.killpg(os.getpgid(proc.pid), signal.SIGTERM)
        except (ProcessLookupError, OSError):
            pass


# ---------------------------------------------------------------------------
# Pipeline phases
# ---------------------------------------------------------------------------

def copy_projects(source_root: Path, target_dir: Path, langs: list[Lang]):
    """Copy conv-test projects and model files to target directory."""
    # Copy model files
    model_src = source_root / "test" / "conv-test"
    model_dst = target_dir / "conv-test"
    if model_dst.exists():
        shutil.rmtree(model_dst)
    shutil.copytree(str(model_src), str(model_dst))

    for lang in langs:
        config = LANG_CONFIGS[lang]
        src = source_root / "test" / config.dir_name
        dst = target_dir / config.dir_name

        if dst.exists():
            shutil.rmtree(dst)

        exclude_args = []
        for exc in config.rsync_excludes:
            exclude_args.extend(["--exclude", exc])

        subprocess.run(
            ["rsync", "-a"] + exclude_args + [str(src) + "/", str(dst) + "/"],
            check=True,
        )

    print(f"Copied {len(langs)} projects to {target_dir}")


async def run_codegen(
    baboon_bin: str, target_dir: Path, langs: list[Lang], timeout: int
) -> StepResult:
    """Run baboon compiler to generate code for all languages."""
    cmd = [baboon_bin, "--model-dir", str(target_dir / "conv-test")]

    for lang in langs:
        config = LANG_CONFIGS[lang]
        output_path = str(target_dir / config.dir_name / config.baboon_output)
        cmd.extend([config.baboon_target, "--output", output_path])

    result = await run_subprocess_unsemaphored(
        cmd, cwd=str(target_dir), timeout=timeout
    )

    if result.status == Status.PASSED:
        # Dart runtime file move
        if Lang.DART in langs:
            dart_gen = target_dir / "conv-test-dt" / "lib" / "generated" / "baboon_runtime.dart"
            dart_pkg = (
                target_dir
                / "conv-test-dt"
                / "packages"
                / "baboon_runtime"
                / "lib"
                / "baboon_runtime.dart"
            )
            if dart_gen.exists():
                shutil.move(str(dart_gen), str(dart_pkg))

    return result


async def build_language(
    lang: Lang, target_dir: Path, semaphore: asyncio.Semaphore, timeout: int
) -> StepResult:
    """Build a single language's conv-test project."""
    config = LANG_CONFIGS[lang]
    cwd = str(target_dir / config.dir_name)
    total_stdout = []
    total_stderr = []
    total_duration = 0.0

    for cmd_spec in config.build_cmds:
        cmd, use_shell = cmd_spec if isinstance(cmd_spec, tuple) else (cmd_spec, False)
        result = await run_subprocess(
            cmd, cwd=cwd, semaphore=semaphore, timeout=timeout, use_shell=use_shell
        )
        total_stdout.append(result.stdout)
        total_stderr.append(result.stderr)
        total_duration += result.duration_sec

        if result.status != Status.PASSED:
            return StepResult(
                Status.BUILD_FAILED,
                total_duration,
                "\n".join(total_stdout),
                "\n".join(total_stderr),
                result.return_code,
            )

    return StepResult(
        Status.PASSED, total_duration, "\n".join(total_stdout), "\n".join(total_stderr), 0
    )


async def write_data(
    lang: Lang,
    fmt: Format,
    target_dir: Path,
    semaphore: asyncio.Semaphore,
    timeout: int,
) -> StepResult:
    """Run serializer for a (lang, format) pair."""
    config = LANG_CONFIGS[lang]
    cwd = str(target_dir / config.dir_name)
    output_dir = str(
        (target_dir / "compat-data" / f"{lang.value}-{fmt.value}").resolve()
    )
    os.makedirs(output_dir, exist_ok=True)

    cmd, use_shell = config.write_cmd(output_dir, fmt.value)
    async with _get_lang_lock(lang):
        return await run_subprocess(
            cmd, cwd=cwd, semaphore=semaphore, timeout=timeout, use_shell=use_shell
        )


async def read_and_verify(
    fmt: Format,
    from_lang: Lang,
    to_lang: Lang,
    target_dir: Path,
    semaphore: asyncio.Semaphore,
    timeout: int,
) -> StepResult:
    """Run deserializer for a (format, from, to) triplet."""
    config = LANG_CONFIGS[to_lang]
    cwd = str(target_dir / config.dir_name)

    ext = fmt.value
    file_path = str(
        (
            target_dir
            / "compat-data"
            / f"{from_lang.value}-{fmt.value}"
            / f"all-basic-types.{ext}"
        ).resolve()
    )

    cmd, use_shell = config.read_cmd(file_path)
    async with _get_lang_lock(to_lang):
        return await run_subprocess(
            cmd, cwd=cwd, semaphore=semaphore, timeout=timeout, use_shell=use_shell
        )


# ---------------------------------------------------------------------------
# HTML report generation
# ---------------------------------------------------------------------------

STATUS_COLORS = {
    "P": "#4caf50",
    "B": "#ff9800",
    "S": "#f44336",
    "U": "#9e9e9e",
}

STATUS_LABELS = {
    "P": "Passed",
    "B": "Build Failed",
    "S": "Serialization Failed",
    "U": "Unexpected Failure",
}


def generate_html_report(report: AcceptanceReport, langs: list[Lang], target_dir: Path):
    lines = [
        "<!DOCTYPE html>",
        "<html><head>",
        "<meta charset='utf-8'>",
        "<title>Baboon Cross-Language Compatibility Report</title>",
        "<style>",
        "body { font-family: 'Segoe UI', monospace; margin: 20px; background: #fafafa; }",
        "h1 { color: #333; }",
        "h2 { color: #555; margin-top: 30px; }",
        "table { border-collapse: collapse; margin: 15px 0; }",
        "th, td { border: 1px solid #ccc; padding: 8px 12px; text-align: center; min-width: 50px; }",
        "th { background: #eee; font-weight: bold; }",
        ".cell-P { background: #4caf50; color: white; font-weight: bold; }",
        ".cell-B { background: #ff9800; color: white; font-weight: bold; }",
        ".cell-S { background: #f44336; color: white; font-weight: bold; }",
        ".cell-U { background: #9e9e9e; color: white; font-weight: bold; }",
        "td.cell-P:hover, td.cell-B:hover, td.cell-S:hover, td.cell-U:hover { opacity: 0.8; cursor: pointer; }",
        ".summary { display: flex; gap: 20px; margin: 10px 0; }",
        ".summary-item { padding: 8px 16px; border-radius: 4px; color: white; font-weight: bold; }",
        ".log-section { display: none; margin: 5px 0 15px 0; background: #f0f0f0; padding: 10px; ",
        "  border: 1px solid #ddd; border-radius: 4px; white-space: pre-wrap; max-height: 400px; ",
        "  overflow: auto; font-family: monospace; font-size: 12px; }",
        ".legend { margin: 10px 0; }",
        ".legend span { display: inline-block; padding: 4px 10px; margin-right: 8px; border-radius: 3px; color: white; font-weight: bold; font-size: 12px; }",
        "</style>",
        "</head><body>",
        "<h1>Baboon Cross-Language Compatibility Report</h1>",
        f"<p>Generated: {time.strftime('%Y-%m-%d %H:%M:%S')}</p>",
        f"<p>Total duration: {report.total_duration_sec:.1f}s</p>",
    ]

    # Legend
    lines.append("<div class='legend'>")
    for code, label in STATUS_LABELS.items():
        color = STATUS_COLORS[code]
        lines.append(f"<span style='background:{color}'>{code} = {label}</span>")
    lines.append("</div>")

    # Summary counts
    total = len(report.read_results)
    counts = {s: 0 for s in Status}
    for r in report.read_results.values():
        counts[r.status] += 1
    # Count cells that didn't get a read result (build/write failures)
    expected_total = len(langs) * len(langs) * len(Format)
    missing = expected_total - total
    for fmt in Format:
        for from_lang in langs:
            for to_lang in langs:
                key = (fmt, from_lang, to_lang)
                if key not in report.read_results:
                    br_from = report.build_results.get(from_lang)
                    br_to = report.build_results.get(to_lang)
                    if (br_from and br_from.status != Status.PASSED) or (
                        br_to and br_to.status != Status.PASSED
                    ):
                        counts[Status.BUILD_FAILED] += 1
                    else:
                        counts[Status.SERIALIZATION_FAILED] += 1

    lines.append("<div class='summary'>")
    for status in Status:
        color = STATUS_COLORS[status.value]
        lines.append(
            f"<div class='summary-item' style='background:{color}'>"
            f"{status.value}: {counts[status]}/{expected_total}</div>"
        )
    lines.append("</div>")

    # Build status
    lines.append("<h2>Build Status</h2>")
    lines.append("<table><tr><th>Language</th><th>Status</th><th>Duration</th></tr>")
    for lang in langs:
        r = report.build_results.get(lang)
        if r:
            status = r.status.value
            if r.status == Status.BUILD_FAILED:
                s = "B"
            elif r.status == Status.PASSED:
                s = "P"
            else:
                s = "U"
            duration = f"{r.duration_sec:.1f}s"
        else:
            s = "U"
            duration = "-"
        lines.append(
            f"<tr><td>{LANG_DISPLAY[lang]}</td>"
            f"<td class='cell-{s}'>{s}</td>"
            f"<td>{duration}</td></tr>"
        )
    lines.append("</table>")

    # Codegen status
    if report.codegen_result:
        r = report.codegen_result
        s = "P" if r.status == Status.PASSED else "U"
        lines.append(f"<p>Code generation: <span class='cell-{s}' style='padding: 2px 8px'>{s}</span> ({r.duration_sec:.1f}s)</p>")

    # Matrices
    for fmt in Format:
        lines.append(f"<h2>{fmt.value.upper()} Compatibility Matrix</h2>")
        lines.append("<p><em>Rows = serializer (FROM), Columns = deserializer (TO)</em></p>")
        lines.append("<table>")

        # Header
        lines.append("<tr><th>FROM \\ TO</th>")
        for to_lang in langs:
            lines.append(f"<th>{LANG_DISPLAY[to_lang]}</th>")
        lines.append("</tr>")

        # Rows
        for from_lang in langs:
            lines.append(f"<tr><th>{LANG_DISPLAY[from_lang]}</th>")
            for to_lang in langs:
                key = (fmt, from_lang, to_lang)
                r = report.read_results.get(key)
                if r:
                    s = r.status.value
                else:
                    br_from = report.build_results.get(from_lang)
                    br_to = report.build_results.get(to_lang)
                    if (br_from and br_from.status != Status.PASSED) or (
                        br_to and br_to.status != Status.PASSED
                    ):
                        s = "B"
                    else:
                        wr = report.write_results.get((from_lang, fmt))
                        if wr and wr.status != Status.PASSED:
                            s = "S"
                        else:
                            s = "U"
                cell_id = f"{fmt.value}-{from_lang.value}-{to_lang.value}"
                lines.append(
                    f"<td class='cell-{s}' id='{cell_id}' "
                    f"onclick=\"toggleLog('{cell_id}-log')\">{s}</td>"
                )
            lines.append("</tr>")
        lines.append("</table>")

    # JavaScript
    lines.append("<script>")
    lines.append("function toggleLog(id) {")
    lines.append("  var el = document.getElementById(id);")
    lines.append("  if (el) el.style.display = el.style.display === 'block' ? 'none' : 'block';")
    lines.append("}")
    lines.append("</script>")

    # Log sections for failures
    lines.append("<h2>Failure Details</h2>")
    lines.append("<p><em>Click a failed cell above to show/hide its log.</em></p>")

    # Build failure logs
    for lang in langs:
        r = report.build_results.get(lang)
        if r and r.status != Status.PASSED:
            lid = f"build-{lang.value}-log"
            lines.append(f"<div id='{lid}' class='log-section' style='display:block'>")
            lines.append(f"<strong>Build failure: {LANG_DISPLAY[lang]}</strong><br/>")
            lines.append(f"Exit code: {r.return_code}<br/>")
            lines.append(f"<strong>stdout:</strong>\n{html_escape(r.stdout[-2000:])}\n")
            lines.append(f"<strong>stderr:</strong>\n{html_escape(r.stderr[-2000:])}\n")
            lines.append("</div>")

    # Write failure logs
    for (lang, fmt), r in report.write_results.items():
        if r.status != Status.PASSED:
            lid = f"write-{lang.value}-{fmt.value}-log"
            lines.append(f"<div id='{lid}' class='log-section' style='display:block'>")
            lines.append(f"<strong>Write failure: {LANG_DISPLAY[lang]} {fmt.value.upper()}</strong><br/>")
            lines.append(f"Exit code: {r.return_code}<br/>")
            lines.append(f"<strong>stdout:</strong>\n{html_escape(r.stdout[-2000:])}\n")
            lines.append(f"<strong>stderr:</strong>\n{html_escape(r.stderr[-2000:])}\n")
            lines.append("</div>")

    # Read failure logs
    for key, r in report.read_results.items():
        if r.status != Status.PASSED:
            fmt, from_lang, to_lang = key
            cell_id = f"{fmt.value}-{from_lang.value}-{to_lang.value}"
            lines.append(f"<div id='{cell_id}-log' class='log-section'>")
            lines.append(
                f"<strong>{LANG_DISPLAY[from_lang]} -> {LANG_DISPLAY[to_lang]} "
                f"({fmt.value.upper()})</strong><br/>"
            )
            lines.append(f"Exit code: {r.return_code}<br/>")
            lines.append(f"Duration: {r.duration_sec:.1f}s<br/>")
            lines.append(f"<strong>stdout:</strong>\n{html_escape(r.stdout[-2000:])}\n")
            lines.append(f"<strong>stderr:</strong>\n{html_escape(r.stderr[-2000:])}\n")
            lines.append("</div>")

    lines.append("</body></html>")

    report_path = target_dir / "acceptance-report.html"
    report_path.write_text("\n".join(lines), encoding="utf-8")
    print(f"HTML report: {report_path}")


def _resolve_cell_status(
    report: AcceptanceReport, fmt: Format, from_lang: Lang, to_lang: Lang,
) -> str:
    key = (fmt, from_lang, to_lang)
    r = report.read_results.get(key)
    if r:
        return r.status.value
    br_from = report.build_results.get(from_lang)
    br_to = report.build_results.get(to_lang)
    if (br_from and br_from.status != Status.PASSED) or (
        br_to and br_to.status != Status.PASSED
    ):
        return "B"
    wr = report.write_results.get((from_lang, fmt))
    if wr and wr.status != Status.PASSED:
        return "S"
    return "U"


def generate_markdown_summary(
    report: AcceptanceReport, langs: list[Lang], target_dir: Path,
):
    status_emoji = {"P": ":white_check_mark:", "B": ":warning:", "S": ":x:", "U": ":grey_question:"}
    lines: list[str] = []

    expected_total = len(langs) * len(langs) * len(Format)
    counts: dict[str, int] = {"P": 0, "B": 0, "S": 0, "U": 0}
    for fmt in Format:
        for from_lang in langs:
            for to_lang in langs:
                counts[_resolve_cell_status(report, fmt, from_lang, to_lang)] += 1

    all_passed = counts["P"] == expected_total

    if all_passed:
        lines.append(f"## :white_check_mark: Acceptance Tests Passed ({expected_total}/{expected_total})")
    else:
        lines.append(f"## :x: Acceptance Tests: {counts['P']}/{expected_total} passed")

    lines.append("")
    lines.append(f"Duration: {report.total_duration_sec:.1f}s")
    lines.append("")
    lines.append(
        f"| | Passed | Build Failed | Serde Failed | Unexpected |"
    )
    lines.append(f"|---|---|---|---|---|")
    lines.append(
        f"| Count | {counts['P']} | {counts['B']} | {counts['S']} | {counts['U']} |"
    )
    lines.append("")

    for fmt in Format:
        lines.append(f"### {fmt.value.upper()} Compatibility Matrix")
        lines.append("")
        lines.append("*Rows = serializer (FROM), Columns = deserializer (TO)*")
        lines.append("")

        header = "| FROM \\ TO | " + " | ".join(LANG_DISPLAY[l] for l in langs) + " |"
        sep = "|---|" + "|".join("---" for _ in langs) + "|"
        lines.append(header)
        lines.append(sep)

        for from_lang in langs:
            cells = []
            for to_lang in langs:
                s = _resolve_cell_status(report, fmt, from_lang, to_lang)
                cells.append(status_emoji.get(s, s))
            lines.append(
                f"| **{LANG_DISPLAY[from_lang]}** | " + " | ".join(cells) + " |"
            )
        lines.append("")

    # Failure details
    failures = []
    for fmt in Format:
        for from_lang in langs:
            for to_lang in langs:
                s = _resolve_cell_status(report, fmt, from_lang, to_lang)
                if s != "P":
                    key = (fmt, from_lang, to_lang)
                    r = report.read_results.get(key)
                    detail = ""
                    if r and r.stderr:
                        detail = r.stderr.strip().split("\n")[-1][:120]
                    failures.append(
                        f"- **{LANG_DISPLAY[from_lang]} -> {LANG_DISPLAY[to_lang]}** "
                        f"({fmt.value.upper()}): {s}"
                        + (f" — `{detail}`" if detail else "")
                    )

    if failures:
        lines.append("<details>")
        lines.append(f"<summary>{len(failures)} failures (click to expand)</summary>")
        lines.append("")
        for f in failures:
            lines.append(f)
        lines.append("")
        lines.append("</details>")
        lines.append("")

    summary_path = target_dir / "acceptance-summary.md"
    summary_path.write_text("\n".join(lines), encoding="utf-8")
    print(f"Markdown summary: {summary_path}")


def write_json_results(report: AcceptanceReport, langs: list[Lang], target_dir: Path):
    results = {
        "total_duration_sec": report.total_duration_sec,
        "codegen": {
            "status": report.codegen_result.status.value if report.codegen_result else "U",
            "duration": report.codegen_result.duration_sec if report.codegen_result else 0,
        },
        "builds": {},
        "writes": {},
        "matrix": {},
    }
    for lang, r in report.build_results.items():
        results["builds"][lang.value] = {
            "status": r.status.value,
            "duration": r.duration_sec,
        }
    for (lang, fmt), r in report.write_results.items():
        results["writes"][f"{lang.value}/{fmt.value}"] = {
            "status": r.status.value,
            "duration": r.duration_sec,
        }
    for key, r in report.read_results.items():
        fmt, from_lang, to_lang = key
        results["matrix"][f"{fmt.value}/{from_lang.value}/{to_lang.value}"] = {
            "status": r.status.value,
            "duration": r.duration_sec,
        }

    # Fill in missing matrix entries
    for fmt in Format:
        for from_lang in langs:
            for to_lang in langs:
                k = f"{fmt.value}/{from_lang.value}/{to_lang.value}"
                if k not in results["matrix"]:
                    br_from = report.build_results.get(from_lang)
                    br_to = report.build_results.get(to_lang)
                    if (br_from and br_from.status != Status.PASSED) or (
                        br_to and br_to.status != Status.PASSED
                    ):
                        results["matrix"][k] = {"status": "B", "duration": 0}
                    else:
                        results["matrix"][k] = {"status": "U", "duration": 0}

    results_path = target_dir / "acceptance-results.json"
    results_path.write_text(json.dumps(results, indent=2), encoding="utf-8")
    print(f"JSON results: {results_path}")


# ---------------------------------------------------------------------------
# Report output
# ---------------------------------------------------------------------------

def write_all_reports(report: AcceptanceReport, langs: list[Lang], target_dir: Path):
    generate_html_report(report, langs, target_dir)
    generate_markdown_summary(report, langs, target_dir)
    write_json_results(report, langs, target_dir)


# ---------------------------------------------------------------------------
# Console output
# ---------------------------------------------------------------------------

def print_phase(name: str):
    print(f"\n{'='*60}")
    print(f"  {name}")
    print(f"{'='*60}")


def print_step(lang: Lang, action: str, result: StepResult):
    icon = {
        Status.PASSED: "OK",
        Status.BUILD_FAILED: "FAIL",
        Status.SERIALIZATION_FAILED: "FAIL",
        Status.UNEXPECTED: "ERR",
    }[result.status]
    print(f"  [{icon}] {LANG_DISPLAY[lang]} {action} ({result.duration_sec:.1f}s)")
    if result.status != Status.PASSED and result.stderr:
        for line in result.stderr.strip().split("\n")[-5:]:
            print(f"       {line}")


def print_summary(report: AcceptanceReport, langs: list[Lang]):
    total = len(langs) * len(langs) * len(Format)
    counts = {s: 0 for s in Status}

    for fmt in Format:
        for from_lang in langs:
            for to_lang in langs:
                key = (fmt, from_lang, to_lang)
                r = report.read_results.get(key)
                if r:
                    counts[r.status] += 1
                else:
                    br_from = report.build_results.get(from_lang)
                    br_to = report.build_results.get(to_lang)
                    if (br_from and br_from.status != Status.PASSED) or (
                        br_to and br_to.status != Status.PASSED
                    ):
                        counts[Status.BUILD_FAILED] += 1
                    else:
                        counts[Status.SERIALIZATION_FAILED] += 1

    print(f"\n{'='*60}")
    print(f"  SUMMARY ({report.total_duration_sec:.1f}s total)")
    print(f"{'='*60}")
    print(f"  Passed:       {counts[Status.PASSED]}/{total}")
    print(f"  Build failed: {counts[Status.BUILD_FAILED]}/{total}")
    print(f"  Serde failed: {counts[Status.SERIALIZATION_FAILED]}/{total}")
    print(f"  Unexpected:   {counts[Status.UNEXPECTED]}/{total}")

    all_passed = counts[Status.PASSED] == total
    print(f"\n  Result: {'ALL PASSED' if all_passed else 'FAILURES DETECTED'}")
    return all_passed


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

async def async_main():
    parser = argparse.ArgumentParser(
        description="Baboon cross-language serialization acceptance tests"
    )
    parser.add_argument("--baboon", required=True, help="Path to baboon native-image binary")
    parser.add_argument("--target", required=True, help="Target directory for artifacts")
    parser.add_argument(
        "--parallelism",
        type=int,
        default=max(1, (os.cpu_count() or 2) - 1),
        help="Parallel task limit (default: nproc - 1)",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=600,
        help="Per-step timeout in seconds (default: 600)",
    )
    parser.add_argument(
        "--langs",
        nargs="*",
        default=None,
        help="Subset of languages to test (default: all)",
    )
    args = parser.parse_args()

    baboon_bin = str(Path(args.baboon).resolve())
    target_dir = Path(args.target).resolve()
    target_dir.mkdir(parents=True, exist_ok=True)

    # Determine source root (where test/ directory lives)
    source_root = Path(__file__).resolve().parent.parent.parent

    if args.langs:
        lang_names = {l.value for l in Lang}
        for name in args.langs:
            if name not in lang_names:
                print(f"Unknown language: {name}. Available: {sorted(lang_names)}")
                return 1
        langs = [l for l in Lang if l.value in args.langs]
    else:
        langs = list(Lang)

    semaphore = asyncio.Semaphore(args.parallelism)
    report = AcceptanceReport()
    start_time = time.monotonic()

    # Install signal handlers
    loop = asyncio.get_event_loop()
    for sig in (signal.SIGINT, signal.SIGTERM):
        loop.add_signal_handler(sig, terminate_all_children)

    print(f"Baboon acceptance test suite")
    print(f"  Binary:      {baboon_bin}")
    print(f"  Target:      {target_dir}")
    print(f"  Parallelism: {args.parallelism}")
    print(f"  Languages:   {', '.join(LANG_DISPLAY[l] for l in langs)}")
    print(f"  Formats:     {', '.join(f.value for f in Format)}")
    print(f"  Triplets:    {len(langs) * len(langs) * len(Format)}")

    # Phase 1: Copy projects
    print_phase("Phase 1: Copy projects")
    try:
        copy_projects(source_root, target_dir, langs)
    except Exception as e:
        print(f"  FATAL: Failed to copy projects: {e}")
        return 1

    if _shutting_down:
        return 130

    # Phase 2: Code generation
    print_phase("Phase 2: Code generation")
    report.codegen_result = await run_codegen(
        baboon_bin, target_dir, langs, args.timeout
    )
    if report.codegen_result.status != Status.PASSED:
        print(f"  FATAL: Code generation failed (exit {report.codegen_result.return_code})")
        if report.codegen_result.stderr:
            for line in report.codegen_result.stderr.strip().split("\n")[-10:]:
                print(f"    {line}")
        report.total_duration_sec = time.monotonic() - start_time
        write_all_reports(report, langs, target_dir)
        return 1
    print(f"  OK ({report.codegen_result.duration_sec:.1f}s)")

    if _shutting_down:
        return 130

    # Phase 3: Build all languages
    print_phase("Phase 3: Build projects")
    build_tasks = {
        lang: asyncio.create_task(build_language(lang, target_dir, semaphore, args.timeout))
        for lang in langs
    }
    build_results = {}
    for lang, task in build_tasks.items():
        build_results[lang] = await task
        report.build_results[lang] = build_results[lang]
        print_step(lang, "build", build_results[lang])

    if _shutting_down:
        report.total_duration_sec = time.monotonic() - start_time
        write_all_reports(report, langs, target_dir)
        return 130

    # Phase 4: Serialize
    print_phase("Phase 4: Serialize")
    write_tasks = {}
    for lang in langs:
        if report.build_results[lang].status != Status.PASSED:
            continue
        for fmt in Format:
            write_tasks[(lang, fmt)] = asyncio.create_task(
                write_data(lang, fmt, target_dir, semaphore, args.timeout)
            )

    for (lang, fmt), task in write_tasks.items():
        result = await task
        report.write_results[(lang, fmt)] = result
        print_step(lang, f"write {fmt.value}", result)

    if _shutting_down:
        report.total_duration_sec = time.monotonic() - start_time
        write_all_reports(report, langs, target_dir)
        return 130

    # Phase 5: Deserialize and verify
    print_phase("Phase 5: Deserialize and verify")
    read_tasks = {}
    for fmt in Format:
        for from_lang in langs:
            for to_lang in langs:
                # Skip if from failed to build or write
                br_from = report.build_results.get(from_lang)
                br_to = report.build_results.get(to_lang)
                if not br_from or br_from.status != Status.PASSED:
                    continue
                if not br_to or br_to.status != Status.PASSED:
                    continue
                wr = report.write_results.get((from_lang, fmt))
                if not wr or wr.status != Status.PASSED:
                    continue

                key = (fmt, from_lang, to_lang)
                read_tasks[key] = asyncio.create_task(
                    read_and_verify(
                        fmt, from_lang, to_lang, target_dir, semaphore, args.timeout
                    )
                )

    completed = 0
    total_reads = len(read_tasks)
    for key, task in read_tasks.items():
        result = await task
        report.read_results[key] = result
        completed += 1
        fmt, from_lang, to_lang = key
        if result.status != Status.PASSED:
            print(
                f"  [{result.status.value}] {LANG_DISPLAY[from_lang]} -> "
                f"{LANG_DISPLAY[to_lang]} ({fmt.value}) "
                f"[{completed}/{total_reads}]"
            )
            if result.stderr:
                for line in result.stderr.strip().split("\n")[-3:]:
                    print(f"       {line}")
        elif completed % 16 == 0 or completed == total_reads:
            print(f"  Progress: {completed}/{total_reads}")

    # Phase 6: Report
    report.total_duration_sec = time.monotonic() - start_time

    print_phase("Phase 6: Generate report")
    write_all_reports(report, langs, target_dir)

    all_passed = print_summary(report, langs)
    return 0 if all_passed else 1


def main():
    try:
        exit_code = asyncio.run(async_main())
    except KeyboardInterrupt:
        terminate_all_children()
        print("\nInterrupted.")
        exit_code = 130
    sys.exit(exit_code)


if __name__ == "__main__":
    main()
