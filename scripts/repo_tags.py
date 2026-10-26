#!/usr/bin/env python3
"""Match each package's current CRAN version to a tag in its GitHub repo.

For every package in docs/status.json with a repo (the same repo its name
links to on the dashboard), lists the repo's tags through the GitHub API
and looks for one naming the CRAN version: v1.2.3, 1.2.3, pkg-1.2.3 and
the like, with "-" and "." treated alike (CRAN's 1.2-3 matches v1.2.3).
The version comes from docs/published.json when it is newer than
status.json, so a release shows up as untagged within half a day.  A
matched tag links to its GitHub release when there is one.  The newest
version-like tag is kept too, to show a tag CRAN does not have yet.

Writes docs/tags.json, rewritten only when an answer changes, so a quiet
day makes no commit.  Dashboard only: nothing here opens an issue or
pushes a tag (R/tag.R does the tagging, when WATCH_PAT is set).
Standard library only; needs GH_TOKEN (GITHUB_TOKEN in Actions reads
public repos, 1000 requests an hour).
"""

import json
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime, timezone

STATUS = "docs/status.json"
PUBLISHED = "docs/published.json"
OUT = "docs/tags.json"
API = "https://api.github.com"

# a tag that is a version, after an optional prefix of letters and
# separators: v1.2.3, V1.2-3, pkg-1.2.3, pkg_v1.2.3, release/1.2.3
TAG = re.compile(r"^(?:[A-Za-z][A-Za-z._/@-]*?)?(\d+(?:[.\-]\d+)*)$")


def get(url):
    """(json, next url) for one GitHub API page; raises HTTPError."""
    req = urllib.request.Request(url, headers={"Accept": "application/vnd.github+json",
                                               "X-GitHub-Api-Version": "2022-11-28"})
    token = os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN")
    if token:
        req.add_header("Authorization", "Bearer " + token)
    with urllib.request.urlopen(req, timeout=60) as r:
        link = r.headers.get("Link") or ""
        m = re.search(r'<([^>]+)>;\s*rel="next"', link)
        return json.loads(r.read().decode("utf-8")), (m.group(1) if m else None)


def tags(repo, get=get):
    """Tag names in repo, every page."""
    url, out = "%s/repos/%s/git/matching-refs/tags/" % (API, repo), []
    while url:
        page, url = get(url)
        out += [r["ref"][len("refs/tags/"):] for r in page if r["ref"].startswith("refs/tags/")]
    return out


def release(repo, tag, get=get):
    """html_url of the GitHub release for tag, or None."""
    try:
        rel, _ = get("%s/repos/%s/releases/tags/%s" % (API, repo, urllib.parse.quote(tag, safe="")))
        return rel.get("html_url")
    except urllib.error.HTTPError as err:
        if err.code == 404:
            return None
        raise


def version_of(tag):
    """The version a tag names, "." for "-", or None."""
    m = TAG.match(tag)
    return m.group(1).replace("-", ".") if m else None


def key(v):
    return tuple(int(x) for x in v.split("."))


def match(version, names):
    """(tag for version or None, newest version-like tag or None)."""
    want = version.replace("-", ".")
    hits = [t for t in names if version_of(t) == want]
    # prefer the usual spellings when a repo has more than one
    for t in ("v" + version, version):
        if t in hits:
            hits = [t]
    # dated tags (2024.01.05 and the like) are not versions to compare with
    vers = [t for t in names if version_of(t) and key(version_of(t))[0] < 1000]
    newest = max(vers, key=lambda t: key(version_of(t))) if vers else None
    return (hits[0] if hits else None), newest


def check(pkg, repo, version, get=get):
    """One package's entry for tags.json."""
    out = {"repo": repo, "version": version}
    try:
        names = tags(repo, get)
    except urllib.error.HTTPError as err:
        out["error"] = "repo not found" if err.code == 404 else "GitHub API %d" % err.code
        return out
    tag, newest = match(version, names)
    out["tag"] = tag
    if tag:
        rel = release(repo, tag, get)
        out["url"] = rel or "https://github.com/%s/tree/%s" % (repo, urllib.parse.quote(tag, safe=""))
        out["release"] = bool(rel)
    else:
        out["url"] = "https://github.com/%s/tags" % repo
    if newest and newest != tag and key(version_of(newest)) > key(version.replace("-", ".")):
        out["newer"] = newest
    out["tags"] = len(names)
    return out


def packages():
    """[(package, repo, version)] for packages with a repo, versions from
    published.json when it is ahead of status.json."""
    with open(STATUS) as f:
        status = json.load(f)["packages"]
    pub = {}
    if os.path.exists(PUBLISHED):
        with open(PUBLISHED) as f:
            pub = json.load(f).get("packages", {})
    out = []
    for p in status:
        if not p.get("repo"):
            continue
        version, date = p.get("version"), p.get("published") or ""
        c = pub.get(p["package"])
        if c and c.get("published", "") > date:
            version = c["version"]
        if version:
            out.append((p["package"], p["repo"], version))
    return out


def main():
    if not (os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN")):
        print("::warning::tags: no GH_TOKEN; unauthenticated GitHub API allows 60 requests an hour")
    now = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")
    old = {}
    if os.path.exists(OUT):
        with open(OUT) as f:
            old = json.load(f).get("packages", {})
    pkgs = {}
    for pkg, repo, version in packages():
        try:
            pkgs[pkg] = check(pkg, repo, version)
        except Exception as err:
            # rate limit or network: keep the last answer rather than flap
            print("::warning::tags: %s (%s): %s" % (pkg, repo, err))
            if pkg in old:
                pkgs[pkg] = old[pkg]
    missing = sorted("%s %s" % (k, v["version"]) for k, v in pkgs.items()
                     if "error" not in v and not v.get("tag"))
    errors = sorted("%s (%s)" % (k, v["error"]) for k, v in pkgs.items() if "error" in v)
    print("::notice::tags: %d of %d CRAN versions tagged; untagged: %s%s" % (
        sum(1 for v in pkgs.values() if v.get("tag")), len(pkgs), ", ".join(missing) or "none",
        "; errors: " + ", ".join(errors) if errors else ""))
    if pkgs == old:
        print("tags unchanged")
        return
    with open(OUT, "w") as f:
        json.dump({"checked": now, "packages": dict(sorted(pkgs.items()))}, f, indent=2)
        f.write("\n")


if __name__ == "__main__":
    sys.exit(main())
