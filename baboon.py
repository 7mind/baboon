#!/usr/bin/env python3

import http.client
import json
import os
import platform
import sys
import urllib
import zipfile


def get_xdg_path(env_var, default):
    return os.getenv(env_var, os.path.expanduser(default))


def download_bytes(url_str):
    url = urllib.parse.urlparse(url_str)
    conn = http.client.HTTPSConnection(url.hostname)
    req = url.path
    if url.query:
        req = "%s?%s" % (url.path, url.query)
    conn.request("GET", req, headers={"User-Agent": "python"})
    rsp = conn.getresponse()

    if rsp.status != 200 and rsp.status != 302:
        print("%s status code while downloading: %s" % (rsp.status, url_str))
        exit(1)

    redirect = rsp.getheader('Location')
    if redirect:
        return download_bytes(redirect)
    else:
        data1 = rsp.read()
    conn.close()
    return data1


def zip_extract(zip_file, target_file):
    with zipfile.ZipFile(zip_file, 'r') as zip_ref:
        source_file = zip_ref.filelist[0]
        bytes = zip_ref.read(source_file)
        file_write(target_file, bytes)


def file_write(target_file, bytes):
    with open(target_file, "bw+") as f:
        f.write(bytes)


if __name__ == "__main__":
    do_update = False
    version = None
    cache_dir = None
    suffix = None
    artifact = None

    # parse args `-u` parameter
    args = sys.argv[1:]
    if args and args[0] == '-u':
        do_update = True
        args = args[1:]

    # detect baboon version and upgrade it if requested
    if do_update:
        with open("baboon.json", "w+") as f:
            bytes = download_bytes("https://api.github.com/repos/7mind/baboon/releases/latest")
            version = json.loads(bytes)["name"]
            f.write(json.dumps({"baboon-version": version}))
    else:
        with open("baboon.json") as f:
            data = json.loads(f.read())
            version = data['baboon-version']

    # detect baboon artifact name for current platform
    osname = platform.system()
    if os.name == 'posix':
        defaultcachedir = None
        if osname == 'Darwin':
            defaultcachedir = os.path.expanduser('~/.cache')
            artifact = "baboon-macos-aarch64-14" if platform.processor() == "arm" else "baboon-macos-amd64-13"
        elif osname == 'Linux':
            defaultcachedir = os.path.expanduser('~/.cache')
            artifact = "baboon-linux-aarch64" if platform.processor() == "arm" else "baboon-linux-amd64"
        cache_dir = get_xdg_path('XDG_CACHE_HOME', defaultcachedir)
    elif os.name == 'nt':
        suffix = ".exe"
        artifact = "baboon-windows-amd64"
        cache_dir = os.path.expanduser('~/AppData/Local')

    # check cache dir
    assert (os.path.exists(cache_dir))
    baboon_cache = os.path.join(cache_dir, "baboon")
    if not os.path.exists(baboon_cache):
        os.makedirs(baboon_cache)

    # prepare cache
    baboon_path = os.path.join(baboon_cache, "baboon-%s" % version)
    baboon_zip_path = baboon_path + ".zip"
    baboon_exec_path = baboon_path + (suffix if suffix else "")

    # download baboon executable
    if not os.path.exists(baboon_path):
        download_uri = "https://github.com/7mind/baboon/releases/download/%s/%s" % (version, artifact + ".zip")
        print("Downloading Baboon: %s" % download_uri)
        zip_bytes = download_bytes(download_uri)
        file_write(baboon_zip_path, zip_bytes)
        zip_extract(baboon_zip_path, baboon_exec_path)

    # chmod +x
    if os.name != "nt":
        import stat

        st = os.stat(baboon_path)
        os.chmod(baboon_path, st.st_mode | stat.S_IEXEC)

    # run baboon executable
    baboon_args = [baboon_exec_path] + args
    os.execv(baboon_exec_path, baboon_args)
