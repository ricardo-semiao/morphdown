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
#' @param str_pattern A named vector where names are patterns to match in
#'  the document's text and values are the replacements for those patterns.
#'  Passed to the `pattern` argument of \link[stringr]{str_replace_all}.
#'  Defaults to no substitution.
#' @param split_sec_lim An integer specifying the maximum heading size (number)
#'  of "#" to be considered. Sub-sections lower than that won't be split.
#' @param split_args A list of 'splits' to separate the sections and blocks.
#'  See the source code of `morphdown:::apply_splits` for the default splits,
#'  and `morphdown:::split_match` to see how they work.
#'
#' @return A named list of character vectors. Each vector is a section named
#'  `s*`. Each section has blocks named `b*`. Use those names to refer to each
#'  in the plan functions.
#'
#' @export
split_sections <- function(
    x,
    str_pattern = NULL,
    split_sec_lim = 3, split_args = NULL) {
  doc_raw <- if (is_path(x)) readLines(x) else x

  if (!rlang::is_null(str_pattern)) {
    doc_raw <- stringr::str_replace_all(doc_raw, str_pattern)
  }

  apply_splits(doc_raw, split_sec_lim, split_args) %>%
    split_match(paste0("^#{1,", split_sec_lim, "} "), FALSE, FALSE) %>%
    purrr::map(function(sec) {
      is_head <- grepl("^#", sec)
      is_empty <- grepl("^$", sec)
      names(sec)[is_head] %<>% {paste0("head", seq_along(.))}
      names(sec)[is_empty] %<>% {paste0("empty", seq_along(.))}
      names(sec)[!(is_head | is_empty)] %<>% {paste0("b", seq_along(.))}
      sec
    }) %>%
    purrr::set_names(paste0("s", seq_along(.)))
}

# Split Character Vector Into List Based on Match
#' @keywords internal
split_match <- function(x, pattern, group, final, equal = FALSE) {
  get_final <- \(x) if (final) grepl(pattern[[1]], x[[1]]) else FALSE

  match <- if (!equal && length(pattern) == 1) {
    if (is.na(pattern)) rep(TRUE, length(x)) else grepl(pattern, x)
  } else {
    open <- grepl(pattern[[1]], x)
    if (equal) {
      close <- open
      close[cumsum(close) %% 2 != 0] <- 0
      open[cumsum(open) %% 2 == 0] <- 0
    } else {
      close <- grepl(pattern[[2]], x)
    }
    cumsum(open - c(0, close[-length(close)])) > 0
  }

  match_fct <- if (group) abs(c(1, diff(match))) else match
  split(x, cumsum(match_fct)) %>%
    purrr::map(~structure(.x, is_final = get_final(.x)))
}


# Apply an Ordered Succession of Splits to Character Vector
#' @keywords internal
apply_splits <- function(doc_raw, split_sec_lim, split_args) {
  patterns <- list(
    paste0("^#{1,", split_sec_lim, "} "),
    c("^(:{1,3}|`{3}) *\\{.*\\}$", "^(:{1,3}|`{3}) *$"),
    "^-{2}[- ]*$",
    "^[ \t]*([-+*] )|([0-9]+\\. )"
  )

  split_args <- split_args %||% list(
    list(pat = patterns[[1]], grp = FALSE, final = FALSE, equal = FALSE),
    list(pat = patterns[[2]], grp = TRUE, final = TRUE, equal = FALSE),
    list(pat = patterns[[3]], grp = TRUE, final = TRUE, equal = TRUE),
    list(pat = patterns[[4]], grp = TRUE, final = TRUE, equal = FALSE),
    list(pat = NA, grp = FALSE, final = TRUE, equal = FALSE)
  )

  splits <- list(structure(doc_raw, is_final = FALSE))
  for (args in split_args) {
    for (i in seq_along(splits)) {
      if (!attr(splits[[i]], "is_final")) {
        splits[[i]] <- split_match(splits[[i]], args$pat, args$grp, args$final, args$equal)
      }
    }
    splits <- purrr::list_flatten(splits)
  }
  purrr::map_if(splits, ~length(.x) == 1 && !(.x == "" || grepl("^#", .x)), split_clauses)
}

#' Split Character Vector By Punctuation
#' @keywords internal
split_clauses <- function(x, pattern = "((?<=[\\.:;!?]))(?<!\\{\\.)") {
  x %>%
  stringr::str_split_1(pattern) %>%
  stringr::str_trim() %>%
  stringr::str_subset("^.+$")
}
