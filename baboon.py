#!/usr/bin/env python3

import sys
import os
import platform
import datetime
import json
import http.client
import urllib

def get_xdg_path(env_var, default):
    return os.getenv(env_var, os.path.expanduser(default))

def download(path):
    u = urllib.parse.urlparse(path)
    conn = http.client.HTTPSConnection(u.hostname)
    req = u.path
    if u.query:
        req = "%s?%s" % (u.path, u.query)
    conn.request("GET", req, headers={"User-Agent": "python"})
    r1 = conn.getresponse()
    loc = r1.getheader('Location')
    if loc:
        return download(loc)
    else:
        data1 = r1.read()
    conn.close()
    return data1

if __name__ == "__main__":
    args = sys.argv[1:]
    do_update = False
    if args and args[0] == '-u':
        do_update = True
        args = args[1:]

    version = None
    if do_update:
        with open("baboon.json", "w+") as f:
            data1 = download("https://api.github.com/repos/7mind/baboon/releases/latest")
            version = json.loads(data1)["name"]
            f.write(json.dumps({"baboon-version": version}))
    else:
        with open("baboon.json") as f:
            data = json.loads(f.read())
            version = data['baboon-version']
    osname = platform.system()

    cache_dir = None
    suffix = None
    artifact = None

    if os.name == 'posix':
        defaultcachedir = None
        if osname == 'Darwin':
            defaultcachedir = os.path.expanduser('~/.cache')
            artifact = "baboon-mac-x64"
        elif osname == 'Linux':
            defaultcachedir = os.path.expanduser('~/.cache')
            artifact = "baboon-linux-x64"
        cache_dir = get_xdg_path('XDG_CACHE_HOME', defaultcachedir)
    elif os.name == 'nt':
        suffix = ".exe"
        artifact = "baboon-x64.exe"
        cache_dir = os.path.expanduser('~/AppData/Local')

    assert(os.path.exists(cache_dir))
    baboon_cache = os.path.join(cache_dir, "baboon")
    if not os.path.exists(baboon_cache):
        os.makedirs(baboon_cache)

    baboon_path = os.path.join(baboon_cache, "baboon-%s" % version)
    if suffix:
        baboon_path = baboon_path + suffix

    if not os.path.exists(baboon_path) or do_update:
        data1 = download("https://github.com/7mind/baboon/releases/download/%s/%s" % (version, artifact))
        with open(baboon_path, "bw+") as f:
            f.write(data1)

    if os.name != "nt":
        import stat
        st = os.stat(baboon_path)
        os.chmod(baboon_path, st.st_mode | stat.S_IEXEC)

    os.execv(baboon_path, [baboon_path] + args)
