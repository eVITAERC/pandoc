Contributing to Scholdoc
========================

Official Scholdoc repository address on GitHub:

https://github.com/timtylin/scholdoc

Had a sudden inspiration?
-------------------------

If you have an awesome idea for Scholdoc, great! You should probably discuss it on the ScholarlyMarkdown discussion forum, so others will have a chance to weigh in.

If your idea is related to the syntax, design, or convention prescribed by ScholarlyMarkdown itself, and concerns changes that would affect all implementations, please talk about it in the Syntax category:

http://forum.scholarlymarkdown.com/category/syntax

If you have suggestions concerning the *behavior* of Scholdoc itself, such as default settings or command-line options, use the Scholdoc category instead:

http://forum.scholarlymarkdown.com/category/implementations/scholdoc


Have you found a bug?
---------------------

If you feel confident that a bug is directly related to ScholarlyMarkdown features, you can file a report in the Scholdoc issue tracker:

https://github.com/timtylin/scholdoc/issues
 
Before submitting a bug report, please search all issued (both open *and* closed) to make sure it is not a duplicate issue.

Your report should give detailed instructions for how to reproduce the problem,
including

  * the exact command line used
  * the exact input used
  * the output received
  * the output you expected instead

A small test case (just a few lines) is ideal.  If your input is large,
try to whittle it down to the minimum necessary to illustrate the problem.

If the issue does not reflect an obvious programming bug, but instead an
undesired behavior, the policy is to aggressively close the issue and move the
discussion to the forum instead (see above). The goal is to ensure that
important discussion concerning potential improvements do not get lost in the
bug tracker.

Any bugs that are also deemed to impact Pandoc itself will be referred to
Pandoc's issue tracker. The official policy is to merge in the fixes only after
it has been made in Pandoc.

The README file will always describe the latest version of improvement to Pandoc
that have been merged into Scholdoc. This information will also be reflected in the changelog.

Pull requests
-------------

Pull requests without an associated issue ticket or discussion on the forum are welcome for bugs and trivial fixes (such as documentation).

All other pull request will be considered, but not necessarily followed-up on. It is very possible that you will be asked to discuss the pull request first on the forum.

All pull requests are expected to meet the [guidelines laid out for Pandoc][pandoc-pr-guidelines].

Technical details
-----------------

All bug fixes and pull requests are requested to provide appropriate addition to the test cases. For information on running tests, as well as the general library structure of Scholdoc, please refer to the [contribution guide for Pandoc][pandoc-contrib-tech].


[pandoc-pr-guidelines]: http://johnmacfarlane.net/pandoc/CONTRIBUTING.html#patches-and-pull-requests
[pandoc-contrib-tech]: http://johnmacfarlane.net/pandoc/CONTRIBUTING.html#tests
