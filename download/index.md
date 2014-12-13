---
layout: index
title: Download Scholdoc
---

### Getting Scholdoc

Scholdoc is written in Haskell and must be compiled to native binary executables before running. Below you can find the latest pre-compiled binaries for various operating systems.

Some of the distributions may be more tested than others. If a particular executable do not work for your platform (but you think it should), feel free to file a bug on the [project GitHub page][scholdoc].

Due to resource limitations, pre-built Scholdoc is currently only targeting x86_64 architecture on the following platforms. If you feel that 32-bit architecture or some particular platform is important to you, we welcome offers to help provide alternative builds.

#### Windows (Vista or later)

An installer for Windows is available for download [here][windows-latest]

#### OS X (10.7 or later)

On OS X, the easiest way to obtain Scholdoc is from the official
[Homebrew][Homebrew] [tap][homebrew-scholdoc]. First make sure you have
[Homebrew][Homebrew] set-up correctly on your system, and that running `brew
doctor` gives you no serious warnings. After that, run the following:

    brew tap timtylin/scholdoc
    brew update
    brew install scholdoc scholdoc-citeproc

To upgrade to the latest release, just run

    brew update
    brew upgrade scholdoc scholdoc-citeproc

#### Linux

A generic `x86_64` binary build for Linux is supplied [here][linux-libgmp5-latest]. This version links dynamically against [GMP][gmp] version 5, which is typically named `libgmp.so.10` on most machines. Building was done on a Ubuntu 14.04 machine, although most modern distros should work if GMP 5 exists in the linking path.


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
[dingus]: http://scholarlymarkdown.com/dingus/
[gmp]: https://gmplib.org

[windows-latest]: http://scholarlymarkdown.com/scholdoc-distribution/windows/scholdoc-0.1.3-alpha-windows.msi
[linux-libgmp5-latest]: http://scholarlymarkdown.com/scholdoc-distribution/linux/scholdoc-0.1.3-ubuntu14.tgz
