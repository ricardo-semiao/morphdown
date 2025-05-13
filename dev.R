
# Setup ------------------------------------------------------------------------

library(devtools)
library(pkgdown)
#library(vdiffr) for graphic tests

use_package_min <- function(pkgs = NULL, type = "Imports") {
  if (is.null(pkgs)) return(invisible(NULL))
  min_versions <- purrr::map(pkgs, ~ gsub("([0-9]+)\\.[0-9]+\\.[0-9]+", "\\1.0.0", packageVersion(.x)))
  purrr::walk2(pkgs, min_versions, ~ use_package(.x, type = type, min_version = .y))
}



# Informal testing -------------------------------------------------------------

load_all() #reload()
document()
#mathjaxr::preview_rd("path", type = "html", verbose = TRUE)



# Updating ---------------------------------------------------------------------

load_all()
document() #check_man()

test()
run_examples()

build_readme()
build_vignettes()

check() #check_dir = "personal/"
install(upgrade = FALSE)

unlink("docs", recursive = TRUE)
build_site(preview = FALSE)
build_manual(path = "personal/")



# Creating Files ---------------------------------------------------------------

# Creating
if (FALSE) {
  use_r("")
  use_test("empty_test") #use_test_helper()
  use_vignette("") #use_article("name")
  #use_tutorial()
  #use_data_raw(); use_data()
}

# Styling
if (FALSE) {
  styling_report <- styler::style_pkg(
    scope = "line_breaks",
    style = tidyverse_style,
    strict = TRUE
  )
  spell_check()
  lintr::lint_package(path = ".")
}

# Versioning
if (FALSE) {
  #use_version(); use_dev_version()
  #use_cran_comments(); use_revdep()
}


# Package setup ----------------------------------------------------------------

# General
if (FALSE) {
  # Creating:
  available::available("")
  create_package("")
  use_git()

  # Main documentation:
  use_namespace()
  use_description(fields = list("Authors@R" = utils::person(
    "Ricardo Semião", email = "ricardo.semiao@outlook.com", role = c("aut", "cre")
  )))
  use_github_links()
  use_mit_license() #also other licenses
  #use_citation(); use_tidy_contributing()

  # Other documentation:
  use_package_doc()
  use_readme_rmd()
  use_news_md()
  #use_logo()
  use_cran_badge() #also use_badge and others
  use_lifecycle_badge("experimental")

  # Site:
  use_pkgdown()
  use_github_pages(branch = git_default_branch(), path = "/docs")
  #use_pkgdown_github_pages() #for the gh-pages branch scheme

  # Tests:
  use_testthat(3)
  use_spell_check()
  use_github_action("check-standard")
  #use_github_action("test-coverage") #also pr-commands

  # Others:
  use_roxygen_md()
  #use_lifecycle()

  # Mathjax:
  #add to imports
  #add `RdMacros: mathjaxr` and `BuildManual: TRUE` in DESCRIPTION
  #add `@importFrom mathjaxr preview_rd` somewhere

  # Set up .onLoad with rlang::on_load
}

# Package imports
depends <- c()
imports <- c("magrittr", "glue", "rlang", "purrr", "stringr")
suggests <- c()

if (FALSE) {
  use_package_min(depends, "Depends")
  use_package_min(imports, "Imports")
  use_package_min(suggests, "Suggests")
  use_package("R", type = "Depends", min_version = "4.3")
  use_tidy_description()
}

# Function imports
use_import_from("glue", "glue")
use_import_from("magrittr", "%<>%")
use_import_from("magrittr", "%>%")
use_import_from("rlang", "%||%")
