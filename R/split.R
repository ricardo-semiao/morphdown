#' Split Rmd Into Sections and Blocks
#'
#' @name splitting
#'
#' @description
#' This function takes a path to an R Markdown (.Rmd) file or a string vector of
#' text from an Rmd file, cleans it by replacing specified patterns, and then
#' splits it into sections and blocks. The result makes it easy to refer to each
#' section/block with the plan functions of the package.
#'
#' @param x A character vector containing the path to an Rmd file or the actual
#'  text from an Rmd file.
#' @param sec_level An integer specifying the maximum heading size (number)
#'  of "#" to be considered. Sub-sections lower than that won't be split.
#' @param replacements_pat A named vector where names are patterns to match in
#'  the document's text and values are the replacements for those patterns.
#'  Passed to the `pattern` argument of \link[stringr]{str_replace_all}.
#'  Defaults to no substitution.
#' @param split_args A list of 'splits' to separate the sections and blocks.
#'  See the source code of `morphdown:::get_split_args` for the default splits.
#'
#' @return A named list of character vectors. Each vector is a section named
#'  `s*`. Each section has blocks named `b*`. Use those names to refer to each
#'  in the plan functions.
#'
#' @export
split_sections <- function(
    x, sec_level = 3,
    replacements_pat = NULL, split_args = NULL,
    clauses_sep  = ".:;!?") {
  doc_raw <- if (is_path(x)) readLines(x) else x
  doc_raw <- c(doc_raw, "")

  pat <- glue("((?<=[{clauses_sep}]))(?<!\\{{\\.)")

  if (!rlang::is_null(replacements_pat)) {
    doc_raw <- stringr::str_replace_all(doc_raw, replacements_pat)
  }

  a = apply_splits(doc_raw, split_args, sec_level, clauses_pat = pat) #%>%
  b = split_after(a, pat = glue("^#{{1,{sec_level}}} "), final = FALSE) #%>% #separate sections
  create_sections_names(b)
}


# Apply an Ordered Succession of Splits to Character Vector
#' @keywords internal
apply_splits <- function(doc_raw, split_args, sec_level, clauses_pat) {
  split_args <- split_args %||% get_split_args(sec_level, clauses_pat)
  splits <- list(structure(doc_raw, is_final = FALSE))

  for (args in split_args) {
    for (i in seq_along(splits)) {
      if (!attr(splits[[i]], "is_final")) {
        splits[[i]] <- do.call(args[[1]], c(x = list(splits[[i]]), args[-1]))
      }
    }
    splits <- purrr::list_flatten(splits)
  }

  splits
}


#' Split Character Vector By Punctuation
#' @keywords internal
split_clauses <- function(x, pat) {
  x %>%
    stringr::str_split_1(pat) %>%
    stringr::str_trim() %>%
    stringr::str_subset("^.+$")
}


# Helper functions:

# Create pretty names for the sections and blocks
#' @keywords internal
create_sections_names <- function(sections) {
  purrr::map(sections, function(sec) {
    is_head <- grepl("^#", sec)
    is_empty <- grepl("^$", sec)
    names(sec)[is_head] %<>% {paste0("head", seq_along(.))}
    names(sec)[is_empty] %<>% {paste0("empty", seq_along(.))}
    names(sec)[!(is_head | is_empty)] %<>% {paste0("b", seq_along(.))}
    sec
  }) %>%
    purrr::set_names(paste0("s", seq_along(.)))
}

# Function to get the standard `split_args`
#' @keywords internal
get_split_args <- function(sec_level, clauses_pat) {
  list(
    list(split_after, #separate by headers
      pat = glue("^#{{1,{sec_level}}} "),
      final = FALSE
    ),
    list(split_between_12, #separate code and md blocks
      pat = c("^(:{1,3}|`{3}) *\\{.*\\}$", "^(:{1,3}|`{3}) *$"),
      final = TRUE
    ),
    list(split_between_11, #separate md tables
      pat = "(^[-|+][-|+:= ]+$)|^(\\|.+\\| *)+$",
      border = c(1, 0),
      final = TRUE
    ),
    list(split_on_repeated, #separate lists
      pat = "^[ \t]*([-+*] )|([0-9]+\\. )",
      final = TRUE
    ),
    list(split_between_12, #separate raw html blocks
      pat = c("^<.+>", "</.+>$"),
      final = TRUE
    ),
    list(split_after, #finally, separate each remaining line (match everything)
      pat = "^",
      final = FALSE
    ),
    list( #lastly, separate each clause
      function(x, pat) {
        if (length(x) == 1 && !(x == "" || grepl("^#", x))) {
          split_clauses(x, pat)
        } else {
          x
        }
      },
      pat = clauses_pat
    )
  )
}
