#!/usr/bin/env python3
"""Is each dashboard package on conda-forge, or on its way there?

For every package in docs/status.json, asks anaconda.org for the
conda-forge package r-<name, lowercased> (one request each, no token
needed) and keeps its highest version, compared with the CRAN version
("-" and "_" alike: conda spells CRAN's 1.6-0 as 1.6_0).  Packages not
there are looked for among the open pull requests of
conda-forge/staged-recipes (one paged listing for all of them, about a
dozen requests), matched on "r-<name>" as a whole word in the PR title
or branch name; a PR that names its recipe some other way is missed.

Writes docs/conda.json, rewritten only when an answer changes, so a quiet
day makes no commit.  Dashboard only: nothing here opens an issue or a
PR.  Standard library only; GH_TOKEN (GITHUB_TOKEN in Actions) for the
GitHub listing.
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
OUT = "docs/conda.json"
ANACONDA = "https://api.anaconda.org/package/conda-forge/"
PULLS = "https://api.github.com/repos/conda-forge/staged-recipes/pulls?state=open&per_page=100"


def fetch(url, github=False):
    """(json, next url); raises HTTPError."""
    req = urllib.request.Request(url, headers={"Accept": "application/json"})
    if github:
        req.add_header("Accept", "application/vnd.github+json")
        req.add_header("X-GitHub-Api-Version", "2022-11-28")
        token = os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN")
        if token:
            req.add_header("Authorization", "Bearer " + token)
    with urllib.request.urlopen(req, timeout=60) as r:
        m = re.search(r'<([^>]+)>;\s*rel="next"', r.headers.get("Link") or "")
        return json.loads(r.read().decode("utf-8")), (m.group(1) if m else None)


def conda_name(pkg):
    return "r-" + pkg.lower()


def norm(v):
    return re.sub(r"[-_]", ".", v or "")


def key(v):
    return tuple(int(x) if x.isdigit() else 0 for x in norm(v).split("."))


def on_conda(pkg, fetch=fetch):
    """anaconda.org's record for r-<pkg>, or None when there is none."""
    try:
        d, _ = fetch(ANACONDA + urllib.parse.quote(conda_name(pkg)))
        return d
    except urllib.error.HTTPError as err:
        if err.code == 404:
            return None
        raise


def open_prs(fetch=fetch):
    """Every open staged-recipes PR, the fields we use."""
    url, out = PULLS, []
    while url:
        page, url = fetch(url, github=True)
        out += [{"number": p["number"], "title": p.get("title") or "", "ref": (p.get("head") or {}).get("ref") or "",
                 "url": p["html_url"], "user": (p.get("user") or {}).get("login"),
                 "draft": bool(p.get("draft")), "created": (p.get("created_at") or "")[:10]} for p in page]
    return out


def find_prs(pkg, prs):
    """The open PRs naming r-<pkg> in their title or branch."""
    # whole word: r-sf is not r-sfheaders, r-brand is not r-brand.yml
    pat = re.compile(r"(?<![\w.-])" + re.escape(conda_name(pkg)) + r"(?![\w.])", re.I)
    return [p for p in prs if pat.search(p["title"]) or pat.search(p["ref"])]


def check(pkg, version, prs, fetch=fetch):
    """One package's entry for conda.json."""
    name = conda_name(pkg)
    out = {"name": name, "version": version}
    d = on_conda(pkg, fetch)
    if d:
        # not latest_version: anaconda.org reports r-sf's as 0.8_0 with 1.1_3 uploaded
        vs = d.get("versions") or []
        cv = max(vs, key=key) if vs else d.get("latest_version")
        out.update(conda=cv, url="https://github.com/conda-forge/%s-feedstock" % name,
                   behind=bool(cv and version and key(cv) < key(version)))
        return out
    out["conda"] = None
    out["prs"] = [{k: p[k] for k in ("number", "url", "user", "draft", "created")} for p in find_prs(pkg, prs)]
    return out


def packages():
    """[(package, version)], versions from published.json when it is ahead."""
    with open(STATUS) as f:
        status = json.load(f)["packages"]
    pub = {}
    if os.path.exists(PUBLISHED):
        with open(PUBLISHED) as f:
            pub = json.load(f).get("packages", {})
    out = []
    for p in status:
        version, date = p.get("version"), p.get("published") or ""
        c = pub.get(p["package"])
        if c and c.get("published", "") > date:
            version = c["version"]
        out.append((p["package"], version))
    return out


def main():
    now = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")
    old = {}
    if os.path.exists(OUT):
        with open(OUT) as f:
            old = json.load(f).get("packages", {})
    try:
        prs = open_prs()
    except Exception as err:
        # without the listing an absent package's PR state is unknown: keep the old answers
        print("::warning::conda: staged-recipes listing failed: %s" % err)
        prs = None
    pkgs = {}
    for pkg, version in packages():
        try:
            if prs is None and pkg in old and not old[pkg].get("conda"):
                pkgs[pkg] = old[pkg]
                continue
            pkgs[pkg] = check(pkg, version, prs or [])
        except Exception as err:
            # rate limit or network: keep the last answer rather than flap
            print("::warning::conda: %s: %s" % (pkg, err))
            if pkg in old:
                pkgs[pkg] = old[pkg]
    absent = sorted(k for k, v in pkgs.items() if not v.get("conda"))
    staged = sorted("%s (#%s)" % (k, v["prs"][0]["number"]) for k, v in pkgs.items() if v.get("prs"))
    behind = sorted("%s %s<%s" % (k, v["conda"], v["version"]) for k, v in pkgs.items() if v.get("behind"))
    print("::notice::conda: %d of %d on conda-forge; in staged-recipes: %s; absent: %s; behind CRAN: %s" % (
        len(pkgs) - len(absent), len(pkgs), ", ".join(staged) or "none",
        ", ".join(a for a in absent if not pkgs[a].get("prs")) or "none", ", ".join(behind) or "none"))
    if pkgs == old:
        print("conda unchanged")
        return
    with open(OUT, "w") as f:
        json.dump({"checked": now, "packages": dict(sorted(pkgs.items()))}, f, indent=2)
        f.write("\n")


if __name__ == "__main__":
    sys.exit(main())
