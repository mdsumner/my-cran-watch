#!/usr/bin/env python3
"""Snapshot the CRAN incoming queue for the packages on the dashboard.

Crawls https://cran.r-project.org/incoming/ (the same listing cransays
reads), keeps submissions of packages in docs/status.json, and writes
docs/queue.json.  Each submission keeps the date it was first seen, and
one that leaves the queue is kept for EXIT_DAYS as an exit: archived when
its tarball is in incoming/archive, published when CRAN's PACKAGES lists
that version, otherwise "left" until one of those shows up.  Submissions of packages not yet on the dashboard (new
packages) are matched by the Maintainer email in their DESCRIPTION
against maintainer_emails in config.yml; each tarball is read once and
the answer cached in .cache/ (kept between runs by actions/cache).  Standard library only, so the queue workflow needs no
R setup.  The file is rewritten only when the entries change, so an
unchanged queue makes no commit.
"""

import json
import os
import re
import sys
import tarfile
import urllib.request
from datetime import datetime, timedelta, timezone

BASE = "https://cran.r-project.org/incoming/"
STATUS = "docs/status.json"
OUT = "docs/queue.json"
CONFIG = "config.yml"
CACHE = ".cache/queue_maintainers.json"
PACKAGES = "https://cran.r-project.org/src/contrib/PACKAGES"
SKIP = {"archive"}
EXIT_DAYS = 7
MAX_READS = 600   # tarballs opened per run; the rest wait for the next run

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
                print("::warning::could not read %s: %s" % (key, err))
                continue
        if cache[key] in emails:
            out.append(dict(e, new=True))
    return out


def cran_versions(get=fetch):
    """{package: version} from CRAN's PACKAGES file."""
    return dict(re.findall(r"^Package: (\S+)\nVersion: (\S+)", get(PACKAGES), re.M))


def exits(old, current, archive, now, versions=cran_versions):
    """Submissions in old (entries and exits) that are no longer in current.

    Outcomes settle to archived or published; a "left" exit is looked up
    again each run.  Exits older than EXIT_DAYS, and those of packages
    with a current submission, are dropped.
    """
    live = {(e["package"], e["version"]) for e in current}
    queued = {e["package"] for e in current}
    gone = [{"package": e["package"], "version": e["version"], "folder": e["folder"],
             "first_seen": e.get("first_seen", e["modified"]), "left": now, "outcome": "left"}
            for e in old.get("entries", []) if (e["package"], e["version"]) not in live]
    out, cran = [], None
    for x in old.get("exits", []) + gone:
        if x["package"] in queued or (x["package"], x["version"]) in live:
            continue
        if x["left"][:10] < days_ago(now, EXIT_DAYS):
            continue
        if x["outcome"] == "left":
            if (x["package"], x["version"]) in archive:
                x = dict(x, outcome="archived")
            else:
                if cran is None:
                    try:
                        cran = versions()
                    except Exception as err:
                        print("::warning::could not read CRAN PACKAGES: %s" % err)
                        cran = {}
                if cran.get(x["package"]) == x["version"]:
                    x = dict(x, outcome="published")
        out.append(x)
    return sorted(out, key=lambda x: (x["package"], x["version"]))


def days_ago(now, n):
    """Date n days before now (a "YYYY-MM-DD HH:MM UTC" stamp), as YYYY-MM-DD."""
    t = datetime.strptime(now[:10], "%Y-%m-%d").replace(tzinfo=timezone.utc)
    return (t - timedelta(days=n)).strftime("%Y-%m-%d")


def main():
    with open(STATUS) as f:
        mine = {p["package"] for p in json.load(f)["packages"]}
    # archive holds submissions that have left the queue (withdrawn,
    # rejected, superseded): not news, and ~300 tarballs to read
    listed = crawl()
    queue = [e for e in listed if e["folder"] not in SKIP]
    archive = {(e["package"], e["version"]) for e in listed if e["folder"] == "archive"}
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
    folders = {}
    for e in queue:
        folders[e["folder"]] = folders.get(e["folder"], 0) + 1
    unread = sum(1 for e in queue if e["package"] not in mine and
                 "%s%s_%s.tar.gz" % (e["url"], e["package"], e["version"]) not in cache)
    # notices show on the run page, so a run can be checked without its log
    print("::notice::queue: %d submissions (%s); %d maintainers known, %d unread; new of mine: %s" % (
        len(queue), ", ".join("%s %d" % kv for kv in sorted(folders.items())),
        len(live & cache.keys()), unread,
        ", ".join("%s %s (%s)" % (e["package"], e["version"], e["folder"]) for e in new) or "none"))
    now = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")
    old = {}
    if os.path.exists(OUT):
        with open(OUT) as f:
            old = json.load(f)
    # a submission keeps its first-seen date as it moves between folders;
    # one seen for the first time takes its file date, which is when it
    # reached incoming (runs are at most half a day apart)
    seen = {(e["package"], e["version"]): e.get("first_seen", e["modified"])
            for e in old.get("entries", [])}
    entries = sorted([dict(e, first_seen=seen.get((e["package"], e["version"]), e["modified"]))
                      for e in [e for e in queue if e["package"] in mine] + new],
                     key=lambda e: (e["package"], e["folder"], e["subfolder"]))
    left = exits(old, entries, archive, now)
    if left:
        print("::notice::queue: left the queue: %s" % ", ".join(
            "%s %s (%s)" % (x["package"], x["version"], x["outcome"]) for x in left))
    if entries == old.get("entries") and left == old.get("exits", []):
        print("queue unchanged (%d entries)" % len(entries))
        return
    with open(OUT, "w") as f:
        json.dump({"changed": now, "source": BASE, "entries": entries, "exits": left}, f, indent=2)
        f.write("\n")
    print("queue changed: %d entries, %d exits" % (len(entries), len(left)))


if __name__ == "__main__":
    sys.exit(main())
