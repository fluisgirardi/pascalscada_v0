# Site documentation source (www.pascalscada.com)

Source of the "How to use" pages published on www.pascalscada.com. The site
runs WordPress with qTranslate-XT, so every page exists in two languages:
`en` (English) and `pb` (Portuguese, served under `/pb/`).

```
docs/site/
  pages.json      manifest: slug, WordPress page id, title per language
  media.json      cache of local images already uploaded to the Media Library
  en/<slug>.md    English text of the page
  pb/<slug>.md    Portuguese text of the page
  img/            images referenced as img/<file> from the Markdown
  build/          --dry-run output (ignored by git)
```

Conventions for the Markdown:

* Section titles are `#####` (`<h5>`), to match the pages written by hand in
  the site's classic editor.
* Give component sections an anchor so other pages can link to them:
  `##### TPLCTagNumber {#TPLCTagNumber}` → `/tags/#TPLCTagNumber`.
* Internal links are site-relative: `/scale-processors/` in `en`,
  `/pb/scale-processors/` in `pb`.
* Component icons already in the Media Library are referenced by their
  `http://www.pascalscada.com/wp-content/uploads/...` URL; new ones go in
  `img/` (copied from `artwork/24x24png/`) and are uploaded on first publish.

Publishing (`tools/publish_docs.py`, needs `python3-markdown` and `requests`):

```
export WP_USER='login'
export WP_APP_PASSWORD='xxxx xxxx xxxx xxxx xxxx xxxx'   # WP Admin → Users → Profile → Application Passwords

tools/publish_docs.py --dry-run tags     # render only, inspect docs/site/build/
tools/publish_docs.py --test tags        # publish to a NEW draft "TEST: Tags", check both languages
tools/publish_docs.py tags               # update the real page (id from pages.json)
tools/publish_docs.py --all              # every page that has both .md files
tools/publish_docs.py --verify tags      # re-check the stored content of a live page
```

New pages (`"id": null` in the manifest) are created as drafts and their id is
written back to `pages.json`; add them to the site menu from WP Admin.
