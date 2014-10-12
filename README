Scholdoc
========

### Converts [ScholarlyMarkdown][scholmd] documents into HTML5/LaTeX/Docx

**Current stable version:** 0.1.3

**Development build status** [![build status][scholarly-devel-travisimage]][travis_stat]  
**Stable build status** [![build status][scholarly-travisimage]][travis_stat]

**Scholdoc** is a command-line utility that converts
[ScholarlyMarkdown][scholmd] documents into the HTML5, LaTeX, and Docx (OOML)
formats. It is intended to facilitate academic writing in a cross-platform,
semantic-aware, plaintext format that can be quickly used in modern publishing
pipelines.

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

Scholdoc is currently up to date with [Pandoc][pandoc] version 1.13.1


### HTML output

***Important:** A ScholarlyMarkdown [core CSS][corecss] is required for proper
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
[travis_stat]: https://travis-ci.org/timtylin/scholdoc-texmath
[scholarly-devel-travisimage]: https://travis-ci.org/timtylin/scholdoc.svg?branch=scholarly-devel
[scholarly-travisimage]: https://travis-ci.org/timtylin/scholdoc.svg?branch=scholarly
[scholdoc-templates]: https://github.com/timtylin/scholdoc-templates
[html-schema]: http://scholarlymarkdown.com/Scholarly-Markdown-HTML-Schema.html
[html-schema-content]: http://scholarlymarkdown.com/Scholarly-Markdown-HTML-Schema.html#content
[corecss]: http://scholarlymarkdown.com/scholdoc-distribution/css/core/scholmd-core-latest.css
[mathjax]: http://www.mathjax.org
