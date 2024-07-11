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
    split_match("^#", FALSE, FALSE) %>%
    purrr::map(function(sec) {
      is_head <- grepl("^#", sec)
      is_empty <- grepl("^$", sec)
      names(sec)[is_head] <- "head"
      names(sec)[is_empty] %<>% {paste0("empty", seq_along(.))}
      names(sec)[!(is_head | is_empty)] %<>% {paste0("b", seq_along(.))}
      sec
    }) %>%
    purrr::set_names(paste0("s", seq_along(.)))
}

# Split Character Vector Into List Based on Match
#' @keywords internal
split_match <- function(x, pattern, group, final) {
  match <- if (length(pattern) == 1) {
    if (is.na(pattern)) rep(TRUE, length(x)) else grepl(pattern, x)
  } else {
    open <- grepl(pattern[[1]], x)
    close <- grepl(pattern[[2]], x)
    cumsum(open - c(0, close[-length(close)])) > 0
  }

  match_fct <- if (group) abs(c(1, diff(match))) else match
  get_final <- \(x) if (final) grepl(pattern[[1]], x[[1]]) else FALSE
  split(x, cumsum(match_fct)) %>%
    purrr::map(~structure(.x, is_final = get_final(.x)))
}


# Apply an Ordered Succession of Splits to Character Vector
#' @keywords internal
apply_splits <- function(doc_raw, split_sec_lim, split_args) {
  split_div_pat <- c(
    paste0("^(:{", split_sec_lim, ",}|`{3}) *\\{.*\\}$"),
    paste0("^(:{", split_sec_lim, ",}|`{3}) *$")
  )
  if (rlang::is_null(split_args)) {
    split_args <- list(
      list(pat = "^#", grp = FALSE, final = FALSE),
      list(pat = split_div_pat, grp = TRUE, final = TRUE),
      list(pat = "^[ \t]*([-+] )|([0-9]+\\. )", grp = TRUE, final = TRUE),
      list(pat = NA, grp = FALSE, final = TRUE)
    )
  }

  splits <- list(structure(doc_raw, is_final = FALSE))
  for (args in split_args) {
    for (i in seq_along(splits)) {
      if (!attr(splits[[i]], "is_final")) {
        splits[[i]] <- split_match(splits[[i]], args$pat, args$grp, args$final)
      }
    }
    splits <- purrr::list_flatten(splits)
  }
  splits
}

#' Split Character Vector By Punctuation
#' @keywords internal
split_clauses <- function(x, pattern = "(?<=[\\.;!?])") {
  x %>%
  stringr::str_split_1(pattern) %>%
  stringr::str_trim() %>%
  stringr::str_subset("^.+$")
}
