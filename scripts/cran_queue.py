#!/usr/bin/env python3
"""Snapshot the CRAN incoming queue for the packages on the dashboard.

Crawls https://cran.r-project.org/incoming/ (the same listing cransays
reads), keeps submissions of packages in docs/status.json, and writes
docs/queue.json.  Submissions of packages not yet on the dashboard (new
packages) are matched by the Maintainer email in their DESCRIPTION
against maintainer_emails in config.yml; each tarball is read once and
the answer cached in .cache/ (kept between runs by actions/cache).  Standard library only, so the hourly workflow needs no
R setup.  The file is rewritten only when the entries change, so an
unchanged queue makes no commit.
"""

import json
import os
import re
import sys
import tarfile
import urllib.request
from datetime import datetime, timezone

BASE = "https://cran.r-project.org/incoming/"
STATUS = "docs/status.json"
OUT = "docs/queue.json"
CONFIG = "config.yml"
CACHE = ".cache/queue_maintainers.json"
MAX_READS = 300   # tarballs opened per run; the rest wait for the next run

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


def config_emails(path=CONFIG):
    """maintainer_emails from config.yml, without a yaml parser."""
    with open(path) as f:
        block = re.search(r"^maintainer_emails:\s*\n((?:[ \t]+-.*\n?)+)", f.read(), re.M)
    if not block:
        return set()
    return {e.strip().strip("'\"").lower() for e in re.findall(r"-\s*(\S+)", block.group(1))}


def maintainer(url, package):
    """Maintainer email from a submitted tarball, reading only up to its DESCRIPTION."""
    with urllib.request.urlopen(url, timeout=60) as r:
        with tarfile.open(fileobj=r, mode="r|gz") as tar:
            for m in tar:
                if m.name == package + "/DESCRIPTION":
                    text = tar.extractfile(m).read().decode("utf-8", "replace")
                    hit = re.search(r"^Maintainer:.*?<([^>]+)>", text, re.M | re.S)
                    return hit.group(1).strip().lower() if hit else ""
    return ""


def by_maintainer(entries, emails, cache, read=maintainer):
    """Entries whose tarball names one of emails as Maintainer; fills cache."""
    out, reads = [], 0
    for e in entries:
        key = "%s%s_%s.tar.gz" % (e["url"], e["package"], e["version"])
        if key not in cache:
            if reads >= MAX_READS:
                continue
            reads += 1
            try:
                cache[key] = read(key, e["package"])
            except Exception as err:   # moved or half-uploaded: retry next run
                print("could not read %s: %s" % (key, err))
                continue
        if cache[key] in emails:
            out.append(dict(e, new=True))
    return out


def main():
    with open(STATUS) as f:
        mine = {p["package"] for p in json.load(f)["packages"]}
    queue = crawl()
    cache = {}
    if os.path.exists(CACHE):
        with open(CACHE) as f:
            cache = json.load(f)
    emails = config_emails()
    new = by_maintainer([e for e in queue if e["package"] not in mine], emails, cache) if emails else []
    # forget tarballs that have left the queue
    live = {"%s%s_%s.tar.gz" % (e["url"], e["package"], e["version"]) for e in queue}
    os.makedirs(os.path.dirname(CACHE), exist_ok=True)
    with open(CACHE, "w") as f:
        json.dump({k: v for k, v in cache.items() if k in live}, f)
    print("queue: %d submissions, %d new of mine" % (len(queue), len(new)))
    entries = sorted([e for e in queue if e["package"] in mine] + new,
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
