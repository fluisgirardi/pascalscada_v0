#!/usr/bin/env python3
"""Publishes a bilingual blog post to www.pascalscada.com (WordPress + qTranslate-XT).

Companion to publish_docs.py (pages), for one-off posts under
docs/site/posts/<slug>.<lang>.md. Merges the en/pb Markdown into the
qTranslate-XT [:en]...[:pb]...[:] form and creates or updates a WordPress
post via the REST API.

Credentials come from the environment only (same as publish_docs.py):

    export WP_USER='login'
    export WP_APP_PASSWORD='xxxx xxxx xxxx xxxx xxxx xxxx'

Usage:
    tools/publish_post.py --dry-run <slug>              render only, to docs/site/build/
    tools/publish_post.py <slug> --title "PB title" --title-en "EN title" [--status draft|publish] [--id <post_id>]

If --id is given (or the slug already matches an existing post found via the
REST API), the post is updated in place instead of creating a new one.
"""
import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from publish_docs import ROOT, SITE, BUILD, API, md_to_html, qtx, wp_get, wp_post, die  # noqa: E402

POSTS_DIR = SITE / "posts"


def find_post_by_slug(slug):
    found = wp_get("/posts", slug=slug, status="publish,draft,pending,private,future", context="edit", per_page=1)
    return found[0] if found else None


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("slug")
    ap.add_argument("--title-pb", default=None, help="pt-br title (required unless --dry-run)")
    ap.add_argument("--title-en", default=None, help="English title (required unless --dry-run)")
    ap.add_argument("--status", default="draft", help="status for a new post (default: draft)")
    ap.add_argument("--id", type=int, default=None, help="update this post id instead of looking it up / creating")
    ap.add_argument("--dry-run", action="store_true", help="render HTML to docs/site/build/ and stop")
    args = ap.parse_args()

    langs = ["en", "pb"]
    htmls = {}
    for lang in langs:
        src = POSTS_DIR / f"{args.slug}.{lang}.md"
        if not src.exists():
            die(f"missing {src}")
        htmls[lang] = md_to_html(src.read_text(encoding="utf-8"))
    content = qtx(htmls)

    if args.dry_run:
        BUILD.mkdir(exist_ok=True)
        for lang, h in htmls.items():
            out = BUILD / f"post-{args.slug}.{lang}.html"
            out.write_text(h, encoding="utf-8")
            print(f"wrote {out.relative_to(ROOT)} ({len(h)} chars)")
        (BUILD / f"post-{args.slug}.qtx.html").write_text(content, encoding="utf-8")
        return

    if not args.title_pb or not args.title_en:
        die("--title-pb and --title-en are required to publish (not needed for --dry-run)")
    title = qtx({"en": args.title_en, "pb": args.title_pb})

    post_id = args.id
    if post_id is None:
        existing = find_post_by_slug(args.slug)
        if existing:
            post_id = existing["id"]
            print(f"post with slug '{args.slug}' already exists (id {post_id}, {existing['status']}), updating it")

    payload = {"title": title, "content": content}
    if post_id is None:
        payload["slug"] = args.slug
        payload["status"] = args.status
        created = wp_post("/posts", payload)
        print(f"created post id {created['id']} ({args.status}): {created['link']}")
    else:
        updated = wp_post(f"/posts/{post_id}", payload)
        print(f"updated: {updated['link']}")


if __name__ == "__main__":
    main()
