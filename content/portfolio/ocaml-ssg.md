---
title: OCaml Static Site Generator
date: 2026-03-11
tags:
  - ocaml
  - web
---

I built this to learn OCaml and to have something to host my writing and projects on. No framework, no template engine — just a small pipeline that walks a folder of Markdown files and spits out a static site.

[View on GitHub](https://github.com/moulindavid/ocaml-website)

It parses YAML frontmatter, converts Markdown to HTML via `cmarkit`, and generates type-safe HTML using `tyxml` — which means if you mess up the structure of a page, it won't compile. Turns out that's a nice property to have.

It also generates an RSS feed, handles blog posts, portfolio entries, and an art section, and copies static assets into the output directory. The whole thing is a few hundred lines of OCaml across four files.
