#!/usr/bin/env python3
"""Snapshot the CRAN incoming queue for the packages on the dashboard.

Crawls https://cran.r-project.org/incoming/ (the same listing cransays
reads), keeps submissions of packages in docs/status.json, and writes
docs/queue.json.  Standard library only, so the hourly workflow needs no
R setup.  The file is rewritten only when the entries change, so an
unchanged queue makes no commit.
"""

import json
import os
import re
import sys
import urllib.request
from datetime import datetime, timezone

BASE = "https://cran.r-project.org/incoming/"
STATUS = "docs/status.json"
OUT = "docs/queue.json"

# Apache autoindex rows, plain or table style:
#   <a href="name">name</a></td><td align="right">2026-10-19 21:03 ...
ROW = re.compile(r'<a href="([^"?/][^"]*)">[^<]*</a>(?:\s|<[^>]*>)*(\d{4}-\d{2}-\d{2} \d{2}:\d{2})')
TARBALL = re.compile(r"^([A-Za-z][A-Za-z0-9.]*)_([0-9][0-9.\-]*)\.tar\.gz$")


def fetch(url):
    with urllib.request.urlopen(url, timeout=60) as r:
        return r.read().decode("utf-8", "replace")


def parse(html):
    """[(name, modified)] for one listing; directories end in '/'."""
    return ROW.findall(html)


def crawl(url=BASE, path=(), depth=0, get=fetch):
    """Submissions under url, recursing into reviewer subfolders."""
    out = []
    for name, modified in parse(get(url)):
        if name.endswith("/"):
            if depth < 2:
                out += crawl(url + name, path + (name.rstrip("/"),), depth + 1, get)
            continue
        m = TARBALL.match(name)
        if m and path:
            out.append({"package": m.group(1), "version": m.group(2),
                        "folder": path[0], "subfolder": "/".join(path[1:]),
                        "modified": modified,
                        "url": BASE + "".join(p + "/" for p in path)})
    return out


def main():
    with open(STATUS) as f:
        mine = {p["package"] for p in json.load(f)["packages"]}
    entries = sorted((e for e in crawl() if e["package"] in mine),
                     key=lambda e: (e["package"], e["folder"], e["subfolder"]))
    old = None
    if os.path.exists(OUT):
        with open(OUT) as f:
            old = json.load(f).get("entries")
    if entries == old:
        print("queue unchanged (%d entries)" % len(entries))
        return
    now = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")
    with open(OUT, "w") as f:
        json.dump({"changed": now, "source": BASE, "entries": entries}, f, indent=2)
        f.write("\n")
    print("queue changed: %d entries" % len(entries))


if __name__ == "__main__":
    sys.exit(main())
