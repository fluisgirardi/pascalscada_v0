#!/usr/bin/env python3
"""Publishes docs/site/*.md to www.pascalscada.com (WordPress + qTranslate-XT).

Each page listed in docs/site/pages.json has one Markdown file per language
(docs/site/en/<slug>.md and docs/site/pb/<slug>.md). Both are converted to
HTML and merged into a single qTranslate-XT string:

    [:en]<english html>[:pb]<portuguese html>[:]

which is sent as the page content through the WordPress REST API. Pages that
already have an "id" in the manifest are updated in place; pages with
"id": null are created (as drafts) and the new id is written back to the
manifest. Pages with a "menu" entry are also added to the site's
Documentation menu (WP REST menu-items endpoint) right after the sibling named
in "after"; the created item id is stored in "menu_item".

Local images referenced as img/<file> are uploaded to the Media Library once
and the resulting URL is cached in docs/site/media.json.

Credentials come from the environment only (never from the repository):

    export WP_USER='login'
    export WP_APP_PASSWORD='xxxx xxxx xxxx xxxx xxxx xxxx'   # WP Application Password

Usage:
    tools/publish_docs.py --dry-run [slug ...]      render to docs/site/build/ only
    tools/publish_docs.py --test slug               publish as a NEW draft "TEST: <title>", verify both languages
    tools/publish_docs.py slug [slug ...]           update/create the real pages
    tools/publish_docs.py --all                     every page whose Markdown exists
    tools/publish_docs.py --verify slug             fetch the live page in both languages and report
    tools/publish_docs.py --menu slug               only add/reposition the page's menu item
    tools/publish_docs.py --import slug [...]       reverse path: download the live page in both
                                                    languages and write docs/site/<lang>/<slug>.md
                                                    (existing .md files are kept unless --force)
    tools/publish_docs.py --history slug            list the WordPress revisions of the page (needs credentials)
    tools/publish_docs.py --import --revision ID slug   import the text stored in that revision instead of the live page
"""
import argparse
import base64
import html
import json
import os
import re
import sys
from pathlib import Path

import markdown
import requests

ROOT = Path(__file__).resolve().parent.parent
SITE = ROOT / "docs" / "site"
MANIFEST = SITE / "pages.json"
MEDIA_CACHE = SITE / "media.json"
BUILD = SITE / "build"

WP_URL = os.environ.get("WP_URL", "https://www.pascalscada.com").rstrip("/")
API = WP_URL + "/wp-json/wp/v2"

MD_EXTENSIONS = ["tables", "fenced_code", "attr_list", "sane_lists"]


# --------------------------------------------------------------------------- helpers
def die(msg):
    print("error:", msg, file=sys.stderr)
    sys.exit(1)


def auth_headers():
    user = os.environ.get("WP_USER")
    pwd = os.environ.get("WP_APP_PASSWORD")
    if not user or not pwd:
        die("WP_USER and WP_APP_PASSWORD must be set in the environment")
    token = base64.b64encode(f"{user}:{pwd}".encode()).decode()
    return {"Authorization": "Basic " + token}


def load_manifest():
    return json.loads(MANIFEST.read_text(encoding="utf-8"))


def save_manifest(m):
    MANIFEST.write_text(json.dumps(m, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def load_media_cache():
    if MEDIA_CACHE.exists():
        return json.loads(MEDIA_CACHE.read_text(encoding="utf-8"))
    return {}


def save_media_cache(c):
    MEDIA_CACHE.write_text(json.dumps(c, ensure_ascii=False, indent=2, sort_keys=True) + "\n", encoding="utf-8")


CODE_BLOCK_RE = re.compile(r'<pre><code class="language-(\w+)">(.*?)\n?</code></pre>', re.S)


def code_blocks_to_shortcode(html_text):
    """Turns <pre><code class="language-X">...</code></pre> (from a ```X fence) into
    the site's own [X tabsize="2"]...[/X] syntax-highlighter shortcode, unescaping
    the HTML entities the fenced-code renderer added."""

    def repl(m):
        lang, code = m.group(1), html.unescape(m.group(2))
        return f'[{lang} tabsize="2"]{code}[/{lang}]'

    return CODE_BLOCK_RE.sub(repl, html_text)


def md_to_html(text):
    rendered = markdown.markdown(text, extensions=MD_EXTENSIONS, output_format="html")
    return code_blocks_to_shortcode(rendered)


IMG_RE = re.compile(r'(<img\b[^>]*\bsrc=")img/([^"]+)(")')


def resolve_images(html_text, cache, upload):
    """Replaces img/<file> references with Media Library URLs, uploading when needed."""

    def repl(m):
        name = m.group(2)
        if name not in cache:
            path = SITE / "img" / name
            if not path.exists():
                die(f"image not found: {path}")
            if not upload:
                return m.group(1) + f"img/{name}" + m.group(3)
            cache[name] = upload_media(path)
            save_media_cache(cache)
        return m.group(1) + cache[name] + m.group(3)

    return IMG_RE.sub(repl, html_text)


def upload_media(path):
    print(f"  uploading {path.name} to the Media Library")
    with open(path, "rb") as fh:
        r = requests.post(
            API + "/media",
            headers={
                **auth_headers(),
                "Content-Disposition": f'attachment; filename="{path.name}"',
                "Content-Type": "image/png" if path.suffix == ".png" else "application/octet-stream",
            },
            data=fh.read(),
            timeout=60,
        )
    if r.status_code not in (200, 201):
        die(f"media upload failed ({r.status_code}): {r.text[:300]}")
    return r.json()["source_url"]


def qtx(parts):
    """Builds the qTranslate-XT multilingual string from {lang: text}."""
    return "".join(f"[:{lang}]{text}" for lang, text in parts.items()) + "[:]"


def link_examples(md, base):
    """`examples/<name>` in backticks -> link to the example folder on GitHub."""
    if not base:
        return md
    return re.sub(r"`examples/([A-Za-z0-9_\-]+)((?:/[^`]*)?)`",
                  lambda m: f"[`examples/{m.group(1)}{m.group(2)}`]({base}{m.group(1)})", md)


def render_page(page, langs, cache, upload, examples_base=None):
    """Returns (title_qtx, content_qtx, per-language html) or None if Markdown is missing."""
    htmls = {}
    for lang in langs:
        src = SITE / lang / f"{page['slug']}.md"
        if not src.exists():
            return None
        md = link_examples(src.read_text(encoding="utf-8"), examples_base)
        htmls[lang] = resolve_images(md_to_html(md), cache, upload)
    title = qtx({lang: page["title"][lang] for lang in langs})
    return title, qtx(htmls), htmls


# --------------------------------------------------------------------------- REST
def wp_get(path, **params):
    r = requests.get(API + path, headers=auth_headers(), params=params, timeout=60)
    if r.status_code != 200:
        die(f"GET {path} failed ({r.status_code}): {r.text[:300]}")
    return r.json()


def wp_post(path, payload):
    r = requests.post(API + path, headers={**auth_headers(), "Content-Type": "application/json"},
                      data=json.dumps(payload), timeout=60)
    if r.status_code not in (200, 201):
        die(f"POST {path} failed ({r.status_code}): {r.text[:500]}")
    return r.json()


def find_page_by_slug(slug):
    """Returns the existing page with this slug (any status), or None."""
    found = wp_get("/pages", slug=slug, status="publish,draft,pending,private,future", context="edit", per_page=1)
    return found[0] if found else None


def find_menu_item_for_page(page_id, parent_item):
    """Returns the menu item that already points to this page under parent_item, or None."""
    items = wp_get("/menu-items", per_page=100, context="edit")
    for i in items:
        if i["object"] == "page" and i["object_id"] == page_id and i["parent"] == parent_item:
            return i
    return None


def strip_tags(s):
    return html.unescape(re.sub(r"<[^>]+>", " ", s))


def verify(page_id, expected_htmls):
    """Fetches the stored raw content and the public rendering in each language."""
    raw = wp_get(f"/pages/{page_id}", context="edit")["content"]["raw"]
    ok = True
    for lang, expected in expected_htmls.items():
        marker = f"[:{lang}]"
        if marker not in raw:
            print(f"  FAIL: stored content has no {marker} block")
            ok = False
            continue
        # compare a distinctive fragment: the first heading text of that language
        m = re.search(r"<h\d[^>]*>(.*?)</h\d>", expected)
        probe = strip_tags(m.group(1)).strip() if m else strip_tags(expected)[:60].strip()
        block = raw.split(marker, 1)[1].split("[:", 1)[0]
        print(f"  {lang}: stored block {len(block)} chars, probe '{probe}': {'ok' if probe in strip_tags(block) else 'MISSING'}")
        ok = ok and probe in strip_tags(block)
    return ok


# --------------------------------------------------------------------------- import
def fetch_rendered(page_id, lang):
    """Public rendered HTML of a page in one language (no credentials needed).
    Drafts are not public: those are fetched authenticated, from the raw multilingual content."""
    prefix = "" if lang == "en" else f"/{lang}"
    r = requests.get(f"{WP_URL}{prefix}/wp-json/wp/v2/pages/{page_id}", params={"_fields": "content,title"}, timeout=60)
    if r.status_code in (401, 403, 404):
        print(f"  page {page_id} is not public ({r.status_code}), fetching it authenticated")
        d = wp_get(f"/pages/{page_id}", context="edit")
        parts = split_qtx(d["content"]["raw"])
        titles = split_qtx(d["title"]["raw"])
        if lang not in parts:
            die(f"page {page_id} has no [:{lang}] block (has: {', '.join(parts)})")
        return titles.get(lang, d["title"]["raw"]), parts[lang]
    if r.status_code != 200:
        die(f"GET page {page_id} ({lang}) failed ({r.status_code})")
    d = r.json()
    return d["title"]["rendered"], d["content"]["rendered"]


def split_qtx(raw):
    """Splits a qTranslate-XT string ([:en]...[:pb]...[:]) into {lang: text}."""
    parts = {}
    for m in re.finditer(r"\[:([a-z]{2})\](.*?)(?=\[:[a-z]{2}\]|\[:\]|$)", raw, re.S):
        parts[m.group(1)] = m.group(2)
    return parts or {"en": raw}


def list_revisions(page_id):
    revs = wp_get(f"/pages/{page_id}/revisions", context="edit", per_page=100)
    for r in revs:
        parts = split_qtx(r["content"]["raw"])
        sizes = ", ".join(f"{lang} {len(strip_tags(t).strip())} chars" for lang, t in sorted(parts.items()))
        print(f"  revision {r['id']:>6}  {r['modified'][:16]}  {sizes}")
    if not revs:
        print("  no revisions stored")


def fetch_revision(page_id, revision_id, lang):
    r = wp_get(f"/pages/{page_id}/revisions/{revision_id}", context="edit")
    parts = split_qtx(r["content"]["raw"])
    if lang not in parts:
        die(f"revision {revision_id} has no [:{lang}] block (has: {', '.join(parts)})")
    return r["title"]["raw"], parts[lang]


def html_to_markdown(html_text):
    """WordPress HTML -> Markdown in the conventions of docs/site (##### headings, anchors kept)."""
    import html2text
    from bs4 import BeautifulSoup

    soup = BeautifulSoup(html_text, "lxml")
    # drop WP lazy-load noise on images, keep only src/alt
    for img in soup.find_all("img"):
        for attr in list(img.attrs):
            if attr not in ("src", "alt"):
                del img[attr]
    # literal < and > in the text (e.g. "tty<number>") would be read back as HTML tags:
    # keep them as entities, which Markdown passes through untouched
    from bs4 import NavigableString
    for node in list(soup.find_all(string=True)):
        if node.parent.name in ("script", "style", "table"):
            continue
        if "<" in node or ">" in node:
            node.replace_with(NavigableString(node.replace("<", "\x00lt\x00").replace(">", "\x00gt\x00")))
    # <strong><b>x</b></strong> would become ****x****: unwrap nested emphasis
    for outer, inner in (("strong", "b"), ("b", "strong"), ("em", "i"), ("i", "em")):
        for tag in soup.find_all(outer):
            if len(tag.contents) == 1 and getattr(tag.contents[0], "name", None) == inner:
                tag.contents[0].unwrap()
    # headings with an id become "text {#id}" so the anchor survives the round trip
    for h in soup.find_all(re.compile(r"^h[1-6]$")):
        if h.get("id"):
            h.append(" {#" + h["id"] + "}")
            del h["id"]
    # tables with multi-line cells do not survive as Markdown tables: keep them as raw HTML
    # blocks (Markdown passes them through untouched), stripped of inline styling
    tables = []
    for t in soup.find_all("table"):
        for el in t.find_all(True):
            for attr in list(el.attrs):
                if attr in ("style", "width", "height", "class", "border", "cellpadding", "cellspacing"):
                    del el[attr]
        tables.append(str(t))
        placeholder = soup.new_tag("p")
        placeholder.string = f"@@TABLE{len(tables) - 1}@@"
        t.replace_with(placeholder)
    conv = html2text.HTML2Text()
    conv.body_width = 0          # no hard wrapping
    conv.ignore_images = False
    conv.protect_links = True
    conv.mark_code = True
    conv.single_line_break = False
    md = conv.handle(str(soup))
    md = re.sub(r"\[code\]\n?", "```\n", md)
    md = re.sub(r"\n?\[/code\]", "\n```", md)
    # [pascal]...[/pascal] shortcodes of a highlighter plugin no longer installed -> fenced code
    def fence(m):
        code = re.sub(r"[ \t]+\n", "\n", m.group(2)).strip("\n")
        return f"\n```{m.group(1)}\n{code}\n```\n"
    md = re.sub(r"\[(pascal|delphi|code)\](.*?)\[/\1\]", fence, md, flags=re.S)
    md = md.replace("\x00lt\x00", "&lt;").replace("\x00gt\x00", "&gt;")
    md = re.sub(r"@@TABLE(\d+)@@", lambda m: "\n" + tables[int(m.group(1))] + "\n", md)
    md = re.sub(r"\n{3,}", "\n\n", md).strip() + "\n"
    return md


def import_page(page, langs, force, revision=None):
    for lang in langs:
        dst = SITE / lang / f"{page['slug']}.md"
        if dst.exists() and not force:
            print(f"  {lang}: {dst.relative_to(ROOT)} exists, kept (use --force to overwrite)")
            continue
        if revision:
            title, html_text = fetch_revision(page["id"], revision, lang)
        else:
            title, html_text = fetch_rendered(page["id"], lang)
        md = html_to_markdown(html_text)
        dst.write_text(md, encoding="utf-8")
        print(f"  {lang}: wrote {dst.relative_to(ROOT)} ({len(md)} chars) — title on site: {html.unescape(title)}")


# --------------------------------------------------------------------------- menu
def menu_siblings(parent_item):
    """Menu items under parent_item, sorted by menu_order."""
    items = wp_get("/menu-items", menus=None, per_page=100, context="edit")
    return sorted((i for i in items if i["parent"] == parent_item), key=lambda i: i["menu_order"])


def ensure_menu_item(page, manifest):
    """Creates the page's menu item after its 'after' sibling, renumbering the siblings."""
    spec = page.get("menu")
    if not spec or page["id"] is None:
        return
    by_slug = {p["slug"]: p for p in manifest["pages"]}
    after = by_slug.get(spec["after"])
    after_item = after.get("menu_item") if after else None
    siblings = menu_siblings(spec["parent"])
    menu_id = siblings[0]["menus"] if siblings else wp_get(f"/menu-items/{spec['parent']}", context="edit")["menus"]

    # desired order: existing siblings, with this page inserted after 'after'
    order = [i for i in siblings if i["id"] != page.get("menu_item")]
    pos = len(order)
    for k, i in enumerate(order):
        if i["id"] == after_item:
            pos = k + 1
            break
    if after_item is None and after and after.get("menu") and after.get("menu_item") is None:
        print(f"  menu: sibling '{spec['after']}' has no menu item yet, appending at the end")

    payload = {
        "title": qtx(page["title"]),   # qTranslate-XT filters menu labels too
        "type": "post_type", "object": "page", "object_id": page["id"],
        "menus": menu_id, "parent": spec["parent"], "status": "publish",
        "menu_order": pos + 1,
    }
    if not page.get("menu_item"):
        existing = find_menu_item_for_page(page["id"], spec["parent"])
        if existing:
            page["menu_item"] = existing["id"]
            save_manifest(manifest)
            print(f"  menu: item {existing['id']} already points to this page, reusing it")
    if page.get("menu_item"):
        item = wp_post(f"/menu-items/{page['menu_item']}", payload)
        print(f"  menu: updated item {item['id']} (order {pos + 1})")
    else:
        item = wp_post("/menu-items", payload)
        page["menu_item"] = item["id"]
        save_manifest(manifest)
        print(f"  menu: created item {item['id']} under parent {spec['parent']} (order {pos + 1})")

    # renumber the siblings that come after it
    for k, i in enumerate(order[pos:], start=pos + 2):
        if i["menu_order"] != k:
            wp_post(f"/menu-items/{i['id']}", {"menu_order": k})


# --------------------------------------------------------------------------- main
def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("slugs", nargs="*")
    ap.add_argument("--all", action="store_true", help="every page whose Markdown exists")
    ap.add_argument("--dry-run", action="store_true", help="render HTML to docs/site/build/ and stop")
    ap.add_argument("--test", action="store_true", help="publish to a new draft page 'TEST: <title>' instead of the real one")
    ap.add_argument("--verify", action="store_true", help="only fetch the live page and check both languages")
    ap.add_argument("--status", default=None, help="page status for new pages (default: draft)")
    ap.add_argument("--menu", action="store_true", help="only add/reposition the page's Documentation menu item")
    ap.add_argument("--import", dest="do_import", action="store_true", help="download the live page (both languages) into docs/site/<lang>/<slug>.md")
    ap.add_argument("--force", action="store_true", help="with --import: overwrite existing .md files")
    ap.add_argument("--history", action="store_true", help="list the WordPress revisions of the page (needs credentials)")
    ap.add_argument("--revision", type=int, default=None, help="with --import: take the text from this revision id")
    args = ap.parse_args()

    manifest = load_manifest()
    langs = manifest["languages"]
    pages = manifest["pages"]
    if args.all and args.do_import:
        selected = [p for p in pages if p["id"] is not None]
    elif args.all:
        selected = [p for p in pages if all((SITE / l / f"{p['slug']}.md").exists() for l in langs)]
    else:
        if not args.slugs:
            ap.error("give at least one slug, or --all")
        by_slug = {p["slug"]: p for p in pages}
        missing = [s for s in args.slugs if s not in by_slug]
        if missing:
            die(f"slug(s) not in manifest: {', '.join(missing)}")
        selected = [by_slug[s] for s in args.slugs]

    cache = load_media_cache()
    upload = not args.dry_run

    for page in selected:
        print(f"== {page['slug']} (id {page['id']})")
        if args.history:
            if page["id"] is None:
                print("  not on the site yet")
            else:
                list_revisions(page["id"])
            continue
        if args.do_import:
            if page["id"] is None:
                print("  not on the site yet, nothing to import")
            else:
                import_page(page, langs, args.force, args.revision)
            continue
        if args.menu:
            if not page.get("menu"):
                print("  no 'menu' entry in pages.json for this page (add {\"parent\": <item id>, \"after\": \"<sibling slug>\"})")
            else:
                ensure_menu_item(page, manifest)
            continue
        rendered = render_page(page, langs, cache, upload, manifest.get("examples_base"))
        if rendered is None:
            print("  skipped: Markdown missing for one of the languages")
            continue
        title, content, htmls = rendered

        if args.dry_run:
            BUILD.mkdir(exist_ok=True)
            for lang, h in htmls.items():
                out = BUILD / f"{page['slug']}.{lang}.html"
                out.write_text(h, encoding="utf-8")
                print(f"  wrote {out.relative_to(ROOT)} ({len(h)} chars)")
            (BUILD / f"{page['slug']}.qtx.html").write_text(content, encoding="utf-8")
            continue

        if args.verify:
            if page["id"] is None:
                print("  no id yet, nothing to verify")
            else:
                verify(page["id"], htmls)
            continue

        if args.test:
            test_title = qtx({l: "TEST: " + page["title"][l] for l in langs})
            existing = find_page_by_slug("test-" + page["slug"])
            if existing:
                created = wp_post(f"/pages/{existing['id']}", {"title": test_title, "content": content})
                print(f"  updated TEST draft id {created['id']}: {created['link']}")
            else:
                created = wp_post("/pages", {"title": test_title, "content": content, "status": "draft",
                                             "slug": "test-" + page["slug"]})
                print(f"  created TEST draft id {created['id']}: {created['link']}")
            print("  verifying stored content:")
            verify(created["id"], htmls)
            print(f"  delete it afterwards from WP Admin (or keep it to preview). Preview: {WP_URL}/?page_id={created['id']}&preview=true")
            continue

        payload = {"title": title, "content": content}
        if page["id"] is None:
            existing = find_page_by_slug(page["slug"])
            if existing:
                page["id"] = existing["id"]
                save_manifest(manifest)
                print(f"  page with slug '{page['slug']}' already exists (id {existing['id']}, {existing['status']}), updating it")
        if page["id"] is None:
            payload["slug"] = page["slug"]
            payload["status"] = args.status or "draft"
            created = wp_post("/pages", payload)
            page["id"] = created["id"]
            save_manifest(manifest)
            print(f"  created page id {created['id']} ({payload['status']}): {created['link']}")
        else:
            if args.status:
                payload["status"] = args.status
            updated = wp_post(f"/pages/{page['id']}", payload)
            print(f"  updated: {updated['link']}")
        if page.get("menu") and not page.get("menu_item"):
            ensure_menu_item(page, manifest)
        verify(page["id"], htmls)


if __name__ == "__main__":
    main()
