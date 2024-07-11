
# morphdown

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/morphdown)](https://CRAN.R-project.org/package=morphdown)
[![R-CMD-check](https://github.com/ricardo-semiao/morphdown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ricardo-semiao/morphdown/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The morphdown R package is being developed to provide a way to
programmatically change the structure of R Markdown files. For example,
to turn a ‘full text’ file into a summarized slide presentation.

The package turns raw .Rdm files into a R list of sections and blocks,
which can then be targeted individually by editing functions.

The morphing becomes much more declarative and documented than manually
adapting the files, increasing speed, but most important,becoming more
robust to changes in the source material.

I created this tool to turn the chapters of my book
[RFCD](https://ricardo-semiao.github.io/rfcd/) into slide presentations
for the trainings I administer.

**Disclaimer:** this package is in the early most stage of life. It
hasn’t been thoroughly tested and can present several bugs. I don’t
recommend using it for large-scale projects, yet.

Please report any problems in my email (below), or as a github issue.
Thank you!

Author: Ricardo Semião e Castro (ricardo.semiao@outlook).

## Installation

You can install the development version of morphdown like so:

``` r
# install.packages("devtools")
devtools::install_github("ricardo-semiao/morphdown")
```

## Example

The basic workflow of the package is as below:

- First, one splits the source file into a more interpretable R list.
- Then, the user defines a plan, choosing the sections he wants to keep
  and how they are to be edited.
- Each section is comprised of blocks, which the user edits with one of
  the editing functions (`e` or `div`). The user can also add new lines
  of markdown.

A mock representation of such workflow is presented below.

``` r
sections <- split_sections("path_to_your_file.Rmd")

plan <- plan_doc(
  sections,
  s2 = plan_sec(
    add_cur_head(2),
    b1 = e(),
    b2 = e(1:3, "\n"),
    b3 = e(subs = "\n"),
  ),
  s3 = plan_sec(
    add_cur_head(2),
    b1 = e(),
    b3 = e(subs = "\n"),
  ),
  s4 = plan_sec(
    add_cur_head(2)
    b1 = div(),
    add_cur_head(2),
    b3 = e(),
    b4 = e(),
    add_cur_head(2),
    b7 = e(2),
    b8 = div(),
  )
)
```
