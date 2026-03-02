#!/usr/bin/env python3
"""
Baboon cross-language service RPC acceptance test runner.

Tests all (server_lang, client_lang) pairs across 9 languages,
producing an HTML report with a color-coded NxN compatibility matrix.

Usage:
    python3 run_service_acceptance.py --baboon <path> --target <dir> [--parallelism N] [--timeout S]
"""

import argparse
import asyncio
import json
import os
import shutil
import signal
import socket
import subprocess
import sys
import time
import urllib.error
import urllib.request
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


class Status(Enum):
    PASSED = "P"
    BUILD_FAILED = "B"
    SERVER_FAILED = "S"
    CLIENT_FAILED = "C"
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
class ServiceReport:
    codegen_result: Optional[StepResult] = None
    build_results: dict = field(default_factory=dict)
    matrix_results: dict = field(default_factory=dict)  # (server_lang, client_lang) -> StepResult
    total_duration_sec: float = 0.0


# ---------------------------------------------------------------------------
# Language configuration
# ---------------------------------------------------------------------------

@dataclass
class ServiceLangConfig:
    dir_name: str
    baboon_target: str
    baboon_output: str
    baboon_extra_flags: list
    build_cmds: list
    server_cmd: object  # callable(host, port) -> (cmd_list_or_str, use_shell)
    client_cmd: object  # callable(host, port) -> (cmd_list_or_str, use_shell)
    rsync_excludes: list = field(default_factory=list)


def _shell_cmd(shell_str: str):
    return (shell_str, True)


SWIFT_WRAPPER = str(
    (Path(__file__).resolve().parent.parent.parent / "scripts" / "swift-xcode.sh")
)


SVC_FLAGS = ["--service-result-no-errors=true", "--generate-json-codecs-by-default=true"]


LANG_CONFIGS: dict[Lang, ServiceLangConfig] = {
    Lang.TYPESCRIPT: ServiceLangConfig(
        dir_name="ts",
        baboon_target=":typescript",
        baboon_output="src/generated",
        baboon_extra_flags=SVC_FLAGS,
        build_cmds=[
            (["npm", "install"], False),
        ],
        server_cmd=lambda h, p: (["npx", "tsx", "src/main.ts", "server", "--host", h, "--port", str(p)], False),
        client_cmd=lambda h, p: (["npx", "tsx", "src/main.ts", "client", "--host", h, "--port", str(p)], False),
        rsync_excludes=["node_modules", "dist", "generated"],
    ),
    Lang.PYTHON: ServiceLangConfig(
        dir_name="py",
        baboon_target=":python",
        baboon_output="Generated",
        baboon_extra_flags=SVC_FLAGS,
        build_cmds=[
            (["python3", "-m", "venv", ".venv"], False),
            _shell_cmd("source .venv/bin/activate && python3 -m pip install -q -r requirements.txt"),
        ],
        server_cmd=lambda h, p: _shell_cmd(
            f"source .venv/bin/activate && python3 main.py server --host {h} --port {p}"
        ),
        client_cmd=lambda h, p: _shell_cmd(
            f"source .venv/bin/activate && python3 main.py client --host {h} --port {p}"
        ),
        rsync_excludes=[".venv", "Generated", "__pycache__"],
    ),
    Lang.JAVA: ServiceLangConfig(
        dir_name="jv",
        baboon_target=":java",
        baboon_output="src/main/java/generated",
        baboon_extra_flags=SVC_FLAGS,
        build_cmds=[
            (["mvn", "-q", "compile"], False),
        ],
        server_cmd=lambda h, p: (
            ["mvn", "-q", "exec:java", f"-Dexec.args=server --host {h} --port {p}"],
            False,
        ),
        client_cmd=lambda h, p: (
            ["mvn", "-q", "exec:java", f"-Dexec.args=client --host {h} --port {p}"],
            False,
        ),
        rsync_excludes=["target", "generated"],
    ),
    Lang.KOTLIN: ServiceLangConfig(
        dir_name="kt",
        baboon_target=":kotlin",
        baboon_output="src/main/kotlin/generated",
        baboon_extra_flags=SVC_FLAGS,
        build_cmds=[
            (["gradle", "--no-daemon", "build", "-x", "test"], False),
        ],
        server_cmd=lambda h, p: (
            ["gradle", "--no-daemon", "run", f"--args=server --host {h} --port {p}"],
            False,
        ),
        client_cmd=lambda h, p: (
            ["gradle", "--no-daemon", "run", f"--args=client --host {h} --port {p}"],
            False,
        ),
        rsync_excludes=["build", ".gradle", "generated"],
    ),
    Lang.SCALA: ServiceLangConfig(
        dir_name="sc",
        baboon_target=":scala",
        baboon_output="src/main/scala/generated",
        baboon_extra_flags=SVC_FLAGS,
        build_cmds=[
            (["sbt", "-batch", "compile"], False),
        ],
        server_cmd=lambda h, p: _shell_cmd(
            f'sbt -batch "runMain example.Main server --host {h} --port {p}"'
        ),
        client_cmd=lambda h, p: _shell_cmd(
            f'sbt -batch "runMain example.Main client --host {h} --port {p}"'
        ),
        rsync_excludes=["target", "project/target", "generated"],
    ),
    Lang.CS: ServiceLangConfig(
        dir_name="cs",
        baboon_target=":cs",
        baboon_output="ServiceTest/Generated",
        baboon_extra_flags=SVC_FLAGS,
        build_cmds=[
            (["dotnet", "build", "-c", "Release", "ServiceTest/ServiceTest.csproj"], False),
        ],
        server_cmd=lambda h, p: (
            ["dotnet", "run", "-c", "Release", "--no-build",
             "--project", "ServiceTest/ServiceTest.csproj", "--",
             "server", "--host", h, "--port", str(p)],
            False,
        ),
        client_cmd=lambda h, p: (
            ["dotnet", "run", "-c", "Release", "--no-build",
             "--project", "ServiceTest/ServiceTest.csproj", "--",
             "client", "--host", h, "--port", str(p)],
            False,
        ),
        rsync_excludes=["bin", "obj", "Generated"],
    ),
    Lang.RUST: ServiceLangConfig(
        dir_name="rs",
        baboon_target=":rust",
        baboon_output="src/generated",
        baboon_extra_flags=SVC_FLAGS,
        build_cmds=[
            (["cargo", "build", "--release"], False),
        ],
        server_cmd=lambda h, p: (
            ["cargo", "run", "--release", "--", "server", "--host", h, "--port", str(p)],
            False,
        ),
        client_cmd=lambda h, p: (
            ["cargo", "run", "--release", "--", "client", "--host", h, "--port", str(p)],
            False,
        ),
        rsync_excludes=["target", "generated"],
    ),
    Lang.DART: ServiceLangConfig(
        dir_name="dt",
        baboon_target=":dart",
        baboon_output="lib/generated",
        baboon_extra_flags=SVC_FLAGS,
        build_cmds=[
            (["dart", "pub", "get"], False),
        ],
        server_cmd=lambda h, p: (
            ["dart", "run", "bin/main.dart", "server", "--host", h, "--port", str(p)],
            False,
        ),
        client_cmd=lambda h, p: (
            ["dart", "run", "bin/main.dart", "client", "--host", h, "--port", str(p)],
            False,
        ),
        rsync_excludes=[".dart_tool", "pubspec.lock", "generated"],
    ),
    Lang.SWIFT: ServiceLangConfig(
        dir_name="sw",
        baboon_target=":swift",
        baboon_output="Sources/Generated",
        baboon_extra_flags=SVC_FLAGS,
        build_cmds=[
            ([SWIFT_WRAPPER, ".", "build"], False),
        ],
        server_cmd=lambda h, p: (
            [SWIFT_WRAPPER, ".", "run", "ServiceMain", "server", "--host", h, "--port", str(p)],
            False,
        ),
        client_cmd=lambda h, p: (
            [SWIFT_WRAPPER, ".", "run", "ServiceMain", "client", "--host", h, "--port", str(p)],
            False,
        ),
        rsync_excludes=[".build", "Generated"],
    ),
}


# ---------------------------------------------------------------------------
# Process management
# ---------------------------------------------------------------------------

_children: list[asyncio.subprocess.Process] = []
_shutting_down = False
_lang_locks: dict = {}
_self_pair_client_dirs: dict[Lang, str] = {}

BASE_PORT = 18100


def _get_lang_lock(lang: Lang) -> asyncio.Lock:
    if lang not in _lang_locks:
        _lang_locks[lang] = asyncio.Lock()
    return _lang_locks[lang]


def _allocate_free_port(host: str) -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind((host, 0))
        s.listen(1)
        return int(s.getsockname()[1])


async def run_subprocess(
    cmd,
    cwd: str,
    timeout: int = 300,
    use_shell: bool = False,
    env: Optional[dict] = None,
) -> StepResult:
    return await _run_subprocess_inner(cmd, cwd, timeout, use_shell, env)


async def _run_subprocess_inner(
    cmd,
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
                *(cmd if isinstance(cmd, list) else [cmd]),
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
        else:
            return StepResult(
                Status.CLIENT_FAILED, duration, stdout, stderr, proc.returncode
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


async def start_server_process(
    cmd,
    cwd: str,
    use_shell: bool,
    host: str,
    port: int,
    startup_timeout: int = 60,
) -> tuple[Optional[asyncio.subprocess.Process], str, str]:
    """Start a server process and wait for /health to respond."""
    merged_env = {**os.environ}

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
                *(cmd if isinstance(cmd, list) else [cmd]),
                cwd=cwd,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                env=merged_env,
                start_new_session=True,
            )
        _children.append(proc)

        health_url = f"http://{host}:{port}/health"
        deadline = time.monotonic() + startup_timeout
        while time.monotonic() < deadline:
            # Check if process has exited
            if proc.returncode is not None:
                stdout = (await proc.stdout.read()).decode("utf-8", errors="replace") if proc.stdout else ""
                stderr = (await proc.stderr.read()).decode("utf-8", errors="replace") if proc.stderr else ""
                if proc in _children:
                    _children.remove(proc)
                return None, stdout, stderr

            try:
                req = urllib.request.Request(health_url, method="GET")
                with urllib.request.urlopen(req, timeout=2) as resp:
                    if resp.status == 200:
                        return proc, "", ""
            except (urllib.error.URLError, OSError, TimeoutError):
                pass

            await asyncio.sleep(0.5)

        # Timed out waiting for health
        await stop_server_process(proc, host, port)
        return None, "", f"Server did not become healthy within {startup_timeout}s"

    except Exception as e:
        return None, "", str(e)


def _post_control(host: str, port: int, path: str, timeout_sec: int = 5) -> bool:
    url = f"http://{host}:{port}{path}"
    request = urllib.request.Request(url, data=b"", method="POST")
    try:
        with urllib.request.urlopen(request, timeout=timeout_sec) as response:
            return response.status == 200
    except (urllib.error.URLError, OSError, TimeoutError):
        return False


async def stop_server_process(proc: asyncio.subprocess.Process, host: str, port: int):
    """Stop a server process gracefully."""
    if proc in _children:
        _children.remove(proc)

    _post_control(host, port, "/shutdown", timeout_sec=3)

    try:
        await asyncio.wait_for(proc.wait(), timeout=8)
        return
    except (asyncio.TimeoutError, ProcessLookupError):
        pass

    try:
        os.killpg(os.getpgid(proc.pid), signal.SIGTERM)
    except (ProcessLookupError, OSError):
        pass
    try:
        await asyncio.wait_for(proc.wait(), timeout=10)
    except (asyncio.TimeoutError, ProcessLookupError):
        try:
            os.killpg(os.getpgid(proc.pid), signal.SIGKILL)
        except (ProcessLookupError, OSError):
            pass
    try:
        await asyncio.wait_for(proc.wait(), timeout=5)
    except (asyncio.TimeoutError, ProcessLookupError):
        pass


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
    """Copy service projects and model file to target directory."""
    # Copy model file
    model_src = source_root / "test" / "services" / "petstore.baboon"
    model_dst = target_dir / "petstore.baboon"
    shutil.copy2(str(model_src), str(model_dst))

    for lang in langs:
        config = LANG_CONFIGS[lang]
        src = source_root / "test" / "services" / config.dir_name
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
    cmd = [baboon_bin, "--model-dir", str(target_dir / "petstore.baboon")]

    for lang in langs:
        config = LANG_CONFIGS[lang]
        output_path = str(target_dir / config.dir_name / config.baboon_output)
        cmd.append(config.baboon_target)
        cmd.extend(["--output", output_path])
        cmd.extend(config.baboon_extra_flags)

    result = await run_subprocess(cmd, cwd=str(target_dir), timeout=timeout)

    if result.status == Status.PASSED:
        # Dart runtime file move
        if Lang.DART in langs:
            dart_gen = target_dir / "dt" / "lib" / "generated" / "baboon_runtime.dart"
            dart_pkg = (
                target_dir / "dt" / "packages" / "baboon_runtime" / "lib" / "baboon_runtime.dart"
            )
            if dart_gen.exists():
                dart_pkg.parent.mkdir(parents=True, exist_ok=True)
                shutil.move(str(dart_gen), str(dart_pkg))

        apply_codegen_patches(target_dir, langs)

    return result


def apply_codegen_patches(target_dir: Path, langs: list[Lang]):
    """Apply post-generation patches for known codegen issues."""

    if Lang.RUST in langs:
        _patch_rust_module_structure(target_dir / "rs")

    if Lang.KOTLIN in langs:
        _patch_kotlin_missing_imports(target_dir / "kt")

    if Lang.SCALA in langs:
        _patch_scala_missing_imports(target_dir / "sc")


def _patch_rust_module_structure(rs_dir: Path):
    """Rename generated lib.rs to mod.rs for submodule usage."""
    lib_rs = rs_dir / "src" / "generated" / "lib.rs"
    mod_rs = rs_dir / "src" / "generated" / "mod.rs"
    if lib_rs.exists() and not mod_rs.exists():
        lib_rs.rename(mod_rs)


def _patch_kotlin_missing_imports(kt_dir: Path):
    """Add missing parent-package imports to Kotlin service method type files.

    The Kotlin code generator doesn't add imports for types from parent packages
    in service method sub-packages (e.g., petstore.api.petstore.addpet references
    PetStatus from petstore.api without importing it).
    """
    gen_dir = kt_dir / "src" / "main" / "kotlin" / "generated"
    if not gen_dir.exists():
        return

    for kt_file in gen_dir.rglob("*.kt"):
        content = kt_file.read_text(encoding="utf-8")
        lines = content.split("\n")
        if not lines:
            continue

        pkg_line = lines[0] if lines[0].startswith("package ") else ""
        if not pkg_line:
            continue

        pkg = pkg_line.replace("package ", "").strip()
        parts = pkg.split(".")

        # Only patch files in service method sub-packages (depth > 2, e.g., petstore.api.petstore.addpet)
        if len(parts) <= 2:
            continue

        parent_pkg = ".".join(parts[:2])
        parent_import = f"import {parent_pkg}.*"

        if parent_import in content:
            continue

        # Insert wildcard import after the package declaration
        new_lines = [lines[0], "", parent_import] + lines[1:]
        kt_file.write_text("\n".join(new_lines), encoding="utf-8")


def _patch_scala_missing_imports(sc_dir: Path):
    """Add missing parent-package imports to Scala service method type files.

    The Scala code generator doesn't add _root_ prefix for types from parent
    packages in service method sub-packages.
    """
    gen_dir = sc_dir / "src" / "main" / "scala" / "generated"
    if not gen_dir.exists():
        return

    import re

    for sc_file in gen_dir.rglob("*.scala"):
        content = sc_file.read_text(encoding="utf-8")

        # Find package declarations like "package petstore.api.petstore.addpet {"
        pkg_match = re.search(r'package\s+([\w.]+)\s*\{', content)
        if not pkg_match:
            continue

        pkg = pkg_match.group(1)
        parts = pkg.split(".")

        if len(parts) <= 2:
            continue

        parent_pkg = ".".join(parts[:2])

        # Add _root_ prefix import at the top of the file (before package declaration)
        parent_import = f"import _root_.{parent_pkg}._"
        if parent_import not in content:
            # Insert import inside the package block to avoid package resolution issues.
            content = content.replace(
                f"package {pkg} {{",
                f"package {pkg} {{\n  {parent_import}"
            )

        # Rewrite nested-package references to absolute paths in type positions.
        content = re.sub(
            r'([:=\[(,]\s*)petstore\.api\.',
            r'\1_root_.petstore.api.',
            content,
        )

        sc_file.write_text(content, encoding="utf-8")

    # Patch wiring file specifically
    _patch_scala_wiring(gen_dir)


def _patch_scala_wiring(gen_dir: Path):
    """Fix wiring file references that get shadowed by sub-packages."""
    import re

    for wiring_file in gen_dir.rglob("*Wiring*.scala"):
        content = wiring_file.read_text(encoding="utf-8")

        # In package petstore.api, references to petstore.api.petstore.X
        # resolve petstore as sub-package. Add _root_ prefix.
        # Match patterns like: petstore.api.petstore.addpet.In_JsonCodec
        content = re.sub(
            r'(?<!\.)(?<![_])petstore\.api\.petstore\.',
            '_root_.petstore.api.petstore.',
            content
        )

        content = content.replace(
            "val json = Json.fromString(data).fold(throw _, identity)\n",
            "",
        )
        content = content.replace(
            ".mkString(\", \")",
            ".toString",
        )

        wiring_file.write_text(content, encoding="utf-8")


async def build_language(
    lang: Lang, target_dir: Path, semaphore: asyncio.Semaphore, timeout: int
) -> StepResult:
    """Build a single language's service project."""
    config = LANG_CONFIGS[lang]
    cwd = str(target_dir / config.dir_name)
    total_stdout = []
    total_stderr = []
    total_duration = 0.0

    for cmd_spec in config.build_cmds:
        if isinstance(cmd_spec, tuple):
            cmd, use_shell = cmd_spec
        else:
            cmd, use_shell = cmd_spec, False

        async with semaphore:
            result = await run_subprocess(
                cmd, cwd=cwd, timeout=timeout, use_shell=use_shell
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


async def run_server_client_pair(
    server_lang: Lang,
    client_lang: Lang,
    target_dir: Path,
    host: str,
    port: int,
    server_proc: asyncio.subprocess.Process,
    timeout: int,
) -> StepResult:
    """Run a client against a running server."""
    config = LANG_CONFIGS[client_lang]
    cwd = str(target_dir / config.dir_name)
    if server_lang == client_lang and client_lang not in (Lang.SWIFT, Lang.TYPESCRIPT):
        shadow = _self_pair_client_dirs.get(client_lang)
        if shadow is None:
            src = target_dir / config.dir_name
            dst = target_dir / f"{config.dir_name}-client-selfpair"
            if dst.exists():
                shutil.rmtree(dst)
            shutil.copytree(src, dst)
            shadow = str(dst)
            _self_pair_client_dirs[client_lang] = shadow
        cwd = shadow

    # Reset server state first
    try:
        reset_url = f"http://{host}:{port}/reset"
        req = urllib.request.Request(reset_url, data=b"", method="POST")
        with urllib.request.urlopen(req, timeout=10) as resp:
            if resp.status != 200:
                return StepResult(
                    Status.SERVER_FAILED, 0, "", "Reset returned non-200", resp.status
                )
    except Exception as e:
        return StepResult(Status.SERVER_FAILED, 0, "", f"Reset failed: {e}", -1)

    # Run client
    cmd, use_shell = config.client_cmd(host, str(port))
    async with _get_lang_lock(client_lang):
        result = await run_subprocess(cmd, cwd=cwd, timeout=timeout, use_shell=use_shell)

    if result.status == Status.PASSED:
        return result
    else:
        return StepResult(
            Status.CLIENT_FAILED,
            result.duration_sec,
            result.stdout,
            result.stderr,
            result.return_code,
        )


# ---------------------------------------------------------------------------
# HTML report generation
# ---------------------------------------------------------------------------

STATUS_COLORS = {
    "P": "#4caf50",
    "B": "#ff9800",
    "S": "#e91e63",
    "C": "#f44336",
    "U": "#9e9e9e",
}

STATUS_LABELS = {
    "P": "Passed",
    "B": "Build Failed",
    "S": "Server Failed",
    "C": "Client Failed",
    "U": "Unexpected",
}


def generate_html_report(report: ServiceReport, langs: list[Lang], target_dir: Path):
    lines = [
        "<!DOCTYPE html>",
        "<html><head>",
        "<meta charset='utf-8'>",
        "<title>Baboon Service RPC Compatibility Report</title>",
        "<style>",
        "body { font-family: 'Segoe UI', monospace; margin: 20px; background: #fafafa; }",
        "h1 { color: #333; }",
        "h2 { color: #555; margin-top: 30px; }",
        "table { border-collapse: collapse; margin: 15px 0; }",
        "th, td { border: 1px solid #ccc; padding: 8px 12px; text-align: center; min-width: 50px; }",
        "th { background: #eee; font-weight: bold; }",
        ".cell-P { background: #4caf50; color: white; font-weight: bold; }",
        ".cell-B { background: #ff9800; color: white; font-weight: bold; }",
        ".cell-S { background: #e91e63; color: white; font-weight: bold; }",
        ".cell-C { background: #f44336; color: white; font-weight: bold; }",
        ".cell-U { background: #9e9e9e; color: white; font-weight: bold; }",
        "td.cell-P:hover, td.cell-B:hover, td.cell-S:hover, td.cell-C:hover, td.cell-U:hover { opacity: 0.8; cursor: pointer; }",
        ".summary { display: flex; gap: 20px; margin: 10px 0; }",
        ".summary-item { padding: 8px 16px; border-radius: 4px; color: white; font-weight: bold; }",
        ".log-section { display: none; margin: 5px 0 15px 0; background: #f0f0f0; padding: 10px; ",
        "  border: 1px solid #ddd; border-radius: 4px; white-space: pre-wrap; max-height: 400px; ",
        "  overflow: auto; font-family: monospace; font-size: 12px; }",
        ".legend { margin: 10px 0; }",
        ".legend span { display: inline-block; padding: 4px 10px; margin-right: 8px; border-radius: 3px; color: white; font-weight: bold; font-size: 12px; }",
        "</style>",
        "</head><body>",
        "<h1>Baboon Service RPC Compatibility Report</h1>",
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
    expected_total = len(langs) * len(langs)
    counts = {s: 0 for s in Status}
    for server_lang in langs:
        for client_lang in langs:
            key = (server_lang, client_lang)
            r = report.matrix_results.get(key)
            if r:
                counts[r.status] += 1
            else:
                br_server = report.build_results.get(server_lang)
                br_client = report.build_results.get(client_lang)
                if (br_server and br_server.status != Status.PASSED) or (
                    br_client and br_client.status != Status.PASSED
                ):
                    counts[Status.BUILD_FAILED] += 1
                else:
                    counts[Status.UNEXPECTED] += 1

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
            s = "B" if r.status == Status.BUILD_FAILED else ("P" if r.status == Status.PASSED else "U")
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

    # Matrix
    lines.append("<h2>Service RPC Compatibility Matrix</h2>")
    lines.append("<p><em>Rows = Server language, Columns = Client language</em></p>")
    lines.append("<table>")

    # Header
    lines.append("<tr><th>Server \\ Client</th>")
    for client_lang in langs:
        lines.append(f"<th>{LANG_DISPLAY[client_lang]}</th>")
    lines.append("</tr>")

    # Rows
    for server_lang in langs:
        lines.append(f"<tr><th>{LANG_DISPLAY[server_lang]}</th>")
        for client_lang in langs:
            key = (server_lang, client_lang)
            r = report.matrix_results.get(key)
            if r:
                s = r.status.value
            else:
                br_server = report.build_results.get(server_lang)
                br_client = report.build_results.get(client_lang)
                if (br_server and br_server.status != Status.PASSED) or (
                    br_client and br_client.status != Status.PASSED
                ):
                    s = "B"
                else:
                    s = "U"
            cell_id = f"{server_lang.value}-{client_lang.value}"
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

    # Failure details
    lines.append("<h2>Failure Details</h2>")
    lines.append("<p><em>Click a failed cell above to show/hide its log.</em></p>")

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

    for key, r in report.matrix_results.items():
        if r.status != Status.PASSED:
            server_lang, client_lang = key
            cell_id = f"{server_lang.value}-{client_lang.value}"
            lines.append(f"<div id='{cell_id}-log' class='log-section'>")
            lines.append(
                f"<strong>Server: {LANG_DISPLAY[server_lang]}, "
                f"Client: {LANG_DISPLAY[client_lang]}</strong><br/>"
            )
            lines.append(f"Status: {r.status.value}<br/>")
            lines.append(f"Exit code: {r.return_code}<br/>")
            lines.append(f"Duration: {r.duration_sec:.1f}s<br/>")
            lines.append(f"<strong>stdout:</strong>\n{html_escape(r.stdout[-2000:])}\n")
            lines.append(f"<strong>stderr:</strong>\n{html_escape(r.stderr[-2000:])}\n")
            lines.append("</div>")

    lines.append("</body></html>")

    report_path = target_dir / "service-report.html"
    report_path.write_text("\n".join(lines), encoding="utf-8")
    print(f"HTML report: {report_path}")


def _resolve_cell_status(
    report: ServiceReport, server_lang: Lang, client_lang: Lang,
) -> str:
    key = (server_lang, client_lang)
    r = report.matrix_results.get(key)
    if r:
        return r.status.value
    br_server = report.build_results.get(server_lang)
    br_client = report.build_results.get(client_lang)
    if (br_server and br_server.status != Status.PASSED) or (
        br_client and br_client.status != Status.PASSED
    ):
        return "B"
    return "U"


def generate_markdown_summary(
    report: ServiceReport, langs: list[Lang], target_dir: Path,
):
    status_emoji = {"P": ":white_check_mark:", "B": ":warning:", "S": ":no_entry:", "C": ":x:", "U": ":grey_question:"}
    lines: list[str] = []

    expected_total = len(langs) * len(langs)
    counts: dict[str, int] = {"P": 0, "B": 0, "S": 0, "C": 0, "U": 0}
    for server_lang in langs:
        for client_lang in langs:
            counts[_resolve_cell_status(report, server_lang, client_lang)] += 1

    all_passed = counts["P"] == expected_total

    if all_passed:
        lines.append(f"## :white_check_mark: Service Tests Passed ({expected_total}/{expected_total})")
    else:
        lines.append(f"## :x: Service Tests: {counts['P']}/{expected_total} passed")

    lines.append("")
    lines.append(f"Duration: {report.total_duration_sec:.1f}s")
    lines.append("")
    lines.append("| | Passed | Build Failed | Server Failed | Client Failed | Unexpected |")
    lines.append("|---|---|---|---|---|---|")
    lines.append(
        f"| Count | {counts['P']} | {counts['B']} | {counts['S']} | {counts['C']} | {counts['U']} |"
    )
    lines.append("")

    lines.append("### Service RPC Compatibility Matrix")
    lines.append("")
    lines.append("*Rows = Server, Columns = Client*")
    lines.append("")

    header = "| Server \\ Client | " + " | ".join(LANG_DISPLAY[l] for l in langs) + " |"
    sep = "|---|" + "|".join("---" for _ in langs) + "|"
    lines.append(header)
    lines.append(sep)

    for server_lang in langs:
        cells = []
        for client_lang in langs:
            s = _resolve_cell_status(report, server_lang, client_lang)
            cells.append(status_emoji.get(s, s))
        lines.append(
            f"| **{LANG_DISPLAY[server_lang]}** | " + " | ".join(cells) + " |"
        )
    lines.append("")

    # Failure details
    failures = []
    for server_lang in langs:
        for client_lang in langs:
            s = _resolve_cell_status(report, server_lang, client_lang)
            if s != "P":
                key = (server_lang, client_lang)
                r = report.matrix_results.get(key)
                detail = ""
                if r and r.stderr:
                    detail = r.stderr.strip().split("\n")[-1][:120]
                failures.append(
                    f"- **{LANG_DISPLAY[server_lang]} server / {LANG_DISPLAY[client_lang]} client**: {s}"
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

    summary_path = target_dir / "service-summary.md"
    summary_path.write_text("\n".join(lines), encoding="utf-8")
    print(f"Markdown summary: {summary_path}")


def write_json_results(report: ServiceReport, langs: list[Lang], target_dir: Path):
    results = {
        "total_duration_sec": report.total_duration_sec,
        "codegen": {
            "status": report.codegen_result.status.value if report.codegen_result else "U",
            "duration": report.codegen_result.duration_sec if report.codegen_result else 0,
        },
        "builds": {},
        "matrix": {},
    }
    for lang, r in report.build_results.items():
        results["builds"][lang.value] = {
            "status": r.status.value,
            "duration": r.duration_sec,
        }
    for key, r in report.matrix_results.items():
        server_lang, client_lang = key
        results["matrix"][f"{server_lang.value}/{client_lang.value}"] = {
            "status": r.status.value,
            "duration": r.duration_sec,
        }

    for server_lang in langs:
        for client_lang in langs:
            k = f"{server_lang.value}/{client_lang.value}"
            if k not in results["matrix"]:
                br_server = report.build_results.get(server_lang)
                br_client = report.build_results.get(client_lang)
                if (br_server and br_server.status != Status.PASSED) or (
                    br_client and br_client.status != Status.PASSED
                ):
                    results["matrix"][k] = {"status": "B", "duration": 0}
                else:
                    results["matrix"][k] = {"status": "U", "duration": 0}

    results_path = target_dir / "service-results.json"
    results_path.write_text(json.dumps(results, indent=2), encoding="utf-8")
    print(f"JSON results: {results_path}")


def write_all_reports(report: ServiceReport, langs: list[Lang], target_dir: Path):
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
        Status.SERVER_FAILED: "FAIL",
        Status.CLIENT_FAILED: "FAIL",
        Status.UNEXPECTED: "ERR",
    }[result.status]
    print(f"  [{icon}] {LANG_DISPLAY[lang]} {action} ({result.duration_sec:.1f}s)")
    if result.status != Status.PASSED and result.stderr:
        for line in result.stderr.strip().split("\n")[-5:]:
            print(f"       {line}")


def print_summary(report: ServiceReport, langs: list[Lang]):
    total = len(langs) * len(langs)
    counts = {s: 0 for s in Status}

    for server_lang in langs:
        for client_lang in langs:
            key = (server_lang, client_lang)
            r = report.matrix_results.get(key)
            if r:
                counts[r.status] += 1
            else:
                br_server = report.build_results.get(server_lang)
                br_client = report.build_results.get(client_lang)
                if (br_server and br_server.status != Status.PASSED) or (
                    br_client and br_client.status != Status.PASSED
                ):
                    counts[Status.BUILD_FAILED] += 1
                else:
                    counts[Status.UNEXPECTED] += 1

    print(f"\n{'='*60}")
    print(f"  SUMMARY ({report.total_duration_sec:.1f}s total)")
    print(f"{'='*60}")
    print(f"  Passed:        {counts[Status.PASSED]}/{total}")
    print(f"  Build failed:  {counts[Status.BUILD_FAILED]}/{total}")
    print(f"  Server failed: {counts[Status.SERVER_FAILED]}/{total}")
    print(f"  Client failed: {counts[Status.CLIENT_FAILED]}/{total}")
    print(f"  Unexpected:    {counts[Status.UNEXPECTED]}/{total}")

    all_passed = counts[Status.PASSED] == total
    print(f"\n  Result: {'ALL PASSED' if all_passed else 'FAILURES DETECTED'}")
    return all_passed


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

async def async_main():
    parser = argparse.ArgumentParser(
        description="Baboon cross-language service RPC acceptance tests"
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
    parser.add_argument(
        "--host",
        default="127.0.0.1",
        help="Host for server binding (default: 127.0.0.1)",
    )
    args = parser.parse_args()

    baboon_bin = str(Path(args.baboon).resolve())
    target_dir = Path(args.target).resolve()
    target_dir.mkdir(parents=True, exist_ok=True)

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
    report = ServiceReport()
    start_time = time.monotonic()
    host = args.host

    # Install signal handlers
    loop = asyncio.get_event_loop()
    for sig in (signal.SIGINT, signal.SIGTERM):
        loop.add_signal_handler(sig, terminate_all_children)

    print(f"Baboon service RPC acceptance test suite")
    print(f"  Binary:      {baboon_bin}")
    print(f"  Target:      {target_dir}")
    print(f"  Parallelism: {args.parallelism}")
    print(f"  Languages:   {', '.join(LANG_DISPLAY[l] for l in langs)}")
    print(f"  Matrix:      {len(langs)}x{len(langs)} = {len(langs) * len(langs)} pairs")

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

    # Phase 3: Build all languages in parallel
    print_phase("Phase 3: Build projects")
    build_tasks = {
        lang: asyncio.create_task(build_language(lang, target_dir, semaphore, args.timeout))
        for lang in langs
    }
    for lang, task in build_tasks.items():
        report.build_results[lang] = await task
        print_step(lang, "build", report.build_results[lang])

    if _shutting_down:
        report.total_duration_sec = time.monotonic() - start_time
        write_all_reports(report, langs, target_dir)
        return 130

    # Phase 4: Run NxN matrix (server × client)
    print_phase("Phase 4: Server/client matrix test")

    buildable_langs = [l for l in langs if report.build_results.get(l, StepResult(Status.UNEXPECTED, 0)).status == Status.PASSED]

    for server_lang in buildable_langs:
        port = _allocate_free_port(host)
        server_config = LANG_CONFIGS[server_lang]
        server_cwd = str(target_dir / server_config.dir_name)

        print(f"\n  Starting {LANG_DISPLAY[server_lang]} server on {host}:{port}...")

        cmd, use_shell = server_config.server_cmd(host, str(port))

        async with _get_lang_lock(server_lang):
            server_proc, srv_stdout, srv_stderr = await start_server_process(
                cmd, server_cwd, use_shell, host, port, startup_timeout=120
            )

        if server_proc is None:
            print(f"  [FAIL] {LANG_DISPLAY[server_lang]} server failed to start")
            if srv_stderr:
                for line in srv_stderr.strip().split("\n")[-5:]:
                    print(f"       {line}")
            for client_lang in buildable_langs:
                report.matrix_results[(server_lang, client_lang)] = StepResult(
                    Status.SERVER_FAILED, 0, srv_stdout, srv_stderr, -1
                )
            continue

        print(f"  [OK] {LANG_DISPLAY[server_lang]} server started on {host}:{port}")

        # Run all clients against this server
        for client_lang in buildable_langs:
            if _shutting_down:
                break

            result = await run_server_client_pair(
                server_lang, client_lang, target_dir, host, port,
                server_proc, args.timeout,
            )
            report.matrix_results[(server_lang, client_lang)] = result

            status_icon = "OK" if result.status == Status.PASSED else "FAIL"
            print(
                f"    [{status_icon}] {LANG_DISPLAY[server_lang]} server <- "
                f"{LANG_DISPLAY[client_lang]} client ({result.duration_sec:.1f}s)"
            )
            if result.status != Status.PASSED and result.stderr:
                for line in result.stderr.strip().split("\n")[-3:]:
                    print(f"         {line}")

        # Stop server
        await stop_server_process(server_proc, host, port)
        print(f"  {LANG_DISPLAY[server_lang]} server stopped")

        if _shutting_down:
            break

    # Phase 5: Report
    report.total_duration_sec = time.monotonic() - start_time

    print_phase("Phase 5: Generate report")
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
