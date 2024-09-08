
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

The package turns raw .Rdm files into a R list of sections and their
blocks, which can then be targeted individually by editing functions.

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

- First, one splits the source file into a more interpretable R list,
  using `split_sections()`.
- Then, the user defines a plan, specifying, for each section and each
  block, how they are to be edited.
- Each section is comprised of blocks, which the user edits with one of
  the editing functions (`e` or `div`). The user can also add new lines
  of markdown.

A mock representation of such workflow is presented below.

Consider the exemplary .Rmd file (as a string) below:

```` scroll-code
# Title

This is a markdown file.

## Section A

We have some text here. This will be recognized as a single-line text expression,
and split into clauses. Thus, it should be edited with the `e()` function. The
user can choose to select only some clauses with `keep = c(1, 3, 4)`. The user
can also add line breaks with `adds = '\\n'`.

- We also have a list.
- This list will be split as a single block.
- And the user can select which items (lines) to keep.

### Blocks

We can count lv3 headers as sections or not, by controlling the `sec_lv` argument.
Regardless, it is saved as a 'headx' block, which can be manipulated by
`add_subhead()`.

:::{.result}
The same is true for markdown blocks.

The user can choose to ignore this line, the third of the block, with `keep = -3`.
:::


``` r
Code blocks are also recognized as a single block.

Note the use of `breaks`.
```

and lastly   tables 
------------ ----------------
are also     blocks          
whose rows   can be ignored  
------------ ----------------
````

Assume that `original` is the path to such file, or the file as a
string. Then, we can split the sections:

``` r
library(morphdown)

sections <- split_sections(original, sec_lv = 2)
sections
```

``` scroll-code
## $s1
## $s1$head1
## [1] "# Title"
## 
## $s1$empty1
## [1] ""
## 
## $s1$b1
## [1] "This is a markdown file."
## 
## $s1$empty2
## [1] ""
## 
## 
## $s2
## $s2$head1
## [1] "## Section A"
## 
## $s2$empty1
## [1] ""
## 
## $s2$b1
## [1] "We have some text here."                                                          
## [2] "This will be recognized as a single-line text expression, and split into clauses."
## [3] "Thus, it should be edited with the `e()` function."                               
## [4] "The user can choose to select only some clauses with `keep = c(1, 3, 4)`."        
## [5] "The user can also add line breaks with `adds = '\\n'`."                           
## 
## $s2$empty2
## [1] ""
## 
## $s2$b2
## [1] "- We also have a list."                                
## [2] "- This list will be split as a single block."          
## [3] "- And the user can select which items (lines) to keep."
## 
## $s2$empty3
## [1] ""
## 
## $s2$head2
## [1] "### Blocks"
## 
## $s2$empty4
## [1] ""
## 
## $s2$b3
## [1] "We can count lv3 headers as sections or not, by controlling the `sec_lv` argument."
## [2] "Regardless, it is saved as a 'headx' block, which can be manipulated by"           
## [3] "`add_subhead()`."                                                                  
## 
## $s2$empty5
## [1] ""
## 
## $s2$b4
## [1] ":::{.result}"                                                                      
## [2] "The same is true for markdown blocks."                                             
## [3] ""                                                                                  
## [4] "The user can choose to ignore this line, the third of the block, with `keep = -3`."
## [5] ":::"                                                                               
## 
## $s2$empty6
## [1] ""
## 
## $s2$b5
## [1] "```{r}"                                            
## [2] "Code blocks are also recognized as a single block."
## [3] ""                                                  
## [4] "Note the use of `breaks`."                         
## [5] "```"                                               
## 
## $s2$empty7
## [1] ""
## 
## $s2$b6
## [1] "and lastly   tables "          "------------ ----------------"
## [3] "are also     blocks          " "whose rows   can be ignored  "
## [5] "------------ ----------------"
## 
## $s2$empty8
## [1] ""
```

Now, we can use this organization of the document to create our morphing
plan:

``` r
result <- morph_doc(
  sections,
  end = "lb", #set the default value of `end` for `e()` and `div()`
  head_lv = 1, #default value of `head_lv` for `morph_sec` and `add_cur_head()`
  s1 = morph_sec(
    #no head1 argument, so header is leaved as is, with a level equals `head_lv`
    b1 = e() #get the text expression unaltered
  ),
  s2 = morph_sec(
    head_lv = 1, #alter the level of the section header
    end = "br", #set a different default value of `sep` only for this section
    b1 = e(c(1, 3, 4), adds = "\n"),
    b2 = div(2:3),
    head2 = add_subhead(n = 3),
    #no b3 argument, such that it is ignored. The same can be done with sections
    b4 = div(-3),
    b5 = div(breaks = 2, sep = "I can add things here"),
    b6 = div(-3)
  )
)

cat(result)
```

``` scroll-code
## # Title
## 
## This is a markdown file.
## 
## 
## # Section A
## 
## We have some text here.
##  Thus, it should be edited with the `e()` function.
##  The user can choose to select only some clauses with `keep = c(1, 3, 4)`.
## 
## <br>
## 
## 
## - This list will be split as a single block.
## - And the user can select which items (lines) to keep.
## 
## <br>
## 
## 
## ### Section A - Blocks
## :::{.result}
## The same is true for markdown blocks.
## The user can choose to ignore this line, the third of the block, with `keep = -3`.
## :::
## 
## <br>
## 
## 
## ```{r}
## Code blocks are also recognized as a single block.
## I can add things here
## 
## Note the use of `breaks`.
## ```
## 
## <br>
## 
## 
## and lastly   tables 
## ------------ ----------------
## whose rows   can be ignored  
## ------------ ----------------
## 
## <br>
```

One could save the result to a variable, and write it to any file with
`writeLines`.
