---
layout: index
title: Scholdoc | a fork of Pandoc that understands ScholarlyMarkdown
---

### Converts [ScholarlyMarkdown][scholmd] documents into HTML5, LaTeX, or Docx

**Current stable version:** 0.1.3

**Development build status** [![build status][scholarly-devel-travisimage]][travis_stat]  
**Stable build status** [![build status][scholarly-travisimage]][travis_stat]

**Scholdoc** is a command-line utility that converts
[ScholarlyMarkdown][scholmd] documents into the HTML5, LaTeX, and Docx (OOML)
formats. It is intended to facilitate academic writing in a cross-platform,
semantic-aware, plaintext format that can be quickly used in modern publishing
pipelines.

You can test the HTML and LaTeX output of Scholdoc on small (limited to 10,000
characters) [ScholarlyMarkdown][scholmd] snippets using the online
[Dingus][dingus].

Scholdoc is implemented as fork of [Pandoc][pandoc], and mostly retains the
same user interface (including the custom [template][pandocTemplate] and
[filter][pandocFilters] system). It essentially understands a new input format
`markdown_scholarly` (implemented in the markdown reader a superset of
`markdown_pandoc` features), and limits itself to HTML5/LaTeX/Docx output.
Scholdoc defaults to `standalone` output and has its own [custom
templates][scholdoc-templates] to ensure output compatibility with
[ScholarlyMarkdown][scholmd].

See the [Pandoc Guide][pandocReadme] for more about Pandoc, its usage, and the
Markdown dialect that Pandoc (and hence Scholdoc) [understands][pandocMarkdown].

Scholdoc is currently up to date with [Pandoc][pandoc] version 1.13.1 (commit 8b60d430)

### Installing Scholdoc

#### Via Homebrew (OSX only)

On OSX, the easiest way to obtain Scholdoc is from the official
[Homebrew][Homebrew] [tap][homebrew-scholdoc]. First make sure you have
[Homebrew][Homebrew] set-up correctly on your system, and that running `brew
doctor` gives you no serious warnings. After that, run the following:

    brew tap timtylin/scholdoc
    brew update
    brew install scholdoc scholdoc-citeproc

To upgrade to the latest release, just run

    brew update
    brew upgrade scholdoc scholdoc-citeproc

#### Via Hackage (all operating systems)

Scholdoc is written in pure Haskell. It requires the [GHC] compiler and the
[cabal-install] build system. The easiest way to get it on all platforms is by
installing the [Haskell platform] for your operating system. Please make sure
you are using GHC version 7.4 or above.

If you are using Ubuntu, Herbert V. Riedel have conveniently provided a [PPA of
pre-compiled GHC and cabal-install][hvr-PPA] for recent Ubuntu systems. Here's
an example of how to get recommended versions of [GHC] and [cabal-install]
using `apt-get`

    sudo add-apt-repository ppa:hvr/ghc
    sudo apt-get update && apt-get install ghc-7.8.3 cabal-install-1.20

Once you have GHC and `cabal-install` on your system, run the following

    cabal update
    cabal install scholdoc
    cabal install scholdoc-citeproc

To upgrade to the latest release of Scholdoc, just run the above three commands
again.

### HTML output

***Important:*** *A ScholarlyMarkdown [core CSS][corecss] is required for proper
formatting of most HTML files output by Scholdoc.*

Scholdoc's HTML output is strictly limited to HTML5 due to its enhanced
semantic capabilities (such as the `figure` and `figcaption` element), and
relies on some CSS3 features for layout (mostly for multi-image figures with
subcaptions). It adheres to a fairly straightforward [schema][html-schema]. No
formatting information is written to the HTML by Scholdoc, so a
ScholarlyMarkdown [core CSS][corecss] is required for bare minimum proper
formatting. You can also write your own CSS that target the schema.

By default, the `html` output format generates a complete (but bare-bones)
HTML5 document that can be used immediately. To have Scholdoc generate just the
bare content (everything inside [`scholmd-content`][html-schema-content]), use
the `html_bodyonly` output format. By default, Scholdoc will always include
proper [MathJax] settings for the way [ScholarlyMarkdown][scholmd] prescribes
math content in HTML.

### Docx output

The Docx writer currently isn't fully functional yet. It does not yet output structures specific to ScholarlyMarkdown (such as figures).

[scholmd]: http://scholarlymarkdown.com
[scholdoc]: https://github.com/timtylin/scholdoc
[scholdoc-types]: https://github.com/timtylin/scholdoc-types
[texmath]: https://github.com/jgm/texmath
[pandoc]: http://johnmacfarlane.net/pandoc/
[pandocReadme]: http://johnmacfarlane.net/pandoc/README.html
[pandocMarkdown]: http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown
[pandocTemplate]: http://johnmacfarlane.net/pandoc/README.html#templates
[pandocFilters]: https://github.com/jgm/pandocfilters
[pandocWriters]: http://johnmacfarlane.net/pandoc/README.html#custom-writers
[pandoc-types]: https://github.com/jgm/pandoc-types
[travis_stat]: https://travis-ci.org/timtylin/scholdoc
[scholarly-devel-travisimage]: https://travis-ci.org/timtylin/scholdoc.svg?branch=master
[scholarly-travisimage]: https://travis-ci.org/timtylin/scholdoc.svg?branch=stable
[scholdoc-templates]: https://github.com/timtylin/scholdoc-templates
[html-schema]: http://scholarlymarkdown.com/Scholarly-Markdown-HTML-Schema.html
[html-schema-content]: http://scholarlymarkdown.com/Scholarly-Markdown-HTML-Schema.html#content
[corecss]: http://scholarlymarkdown.com/scholdoc-distribution/css/core/scholmd-core-latest.css
[mathjax]: http://www.mathjax.org
[GHC]: http://www.haskell.org/ghc/
[Haskell platform]: http://hackage.haskell.org/platform/
[cabal-install]: http://hackage.haskell.org/trac/hackage/wiki/CabalInstall
[zip-archive]: http://hackage.haskell.org/package/zip-archive
[highlighting-kate]: http://hackage.haskell.org/package/highlighting-kate
[blaze-html]: http://hackage.haskell.org/package/blaze-html
[Cabal User's Guide]: http://www.haskell.org/cabal/release/latest/doc/users-guide/builders.html#setup-configure-paths
[Homebrew]: http://brew.sh
[hvr-PPA]: https://launchpad.net/~hvr/+archive/ubuntu/ghc
[homebrew-scholdoc]: https://github.com/timtylin/homebrew-scholdoc/

