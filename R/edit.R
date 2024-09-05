#' Edit Blocks
#'
#' @name editing
#' @rdname editing
#'
#' @description
#' These functions are assigned to blocks inside `plan_sec(`b* = fun(...)`)`.
#'  See \link[morphdown]{planning}. `e` and `div` edit existing blocks, the rest
#'  adds new markdown content.
#'
#' - `e` is used on single-lines of text, that have been split into clauses.
#' - `div` is used on multi-line blocks of text, that have been split by lines.
#' - `add_cur_head` adds the heading of the current section, with the specified
#'  level.
#' - `add_custom` adds any custom text.
#' - `add_pause` adds `'. . .'` and `add_br` adds `'<br>'`.
#'
#' @param keep Indexes of the clauses (for `e`) or lines (for `div`) to keep.
#'  Defaults to all (`TRUE`).
#' @param end Character specifying what to add at the end of the block:
#'  - `'pbr'` for `add_pause()` (the default);
#'  - `'p'` for `add_pause(FALSE)`;
#'  - `'br'` for `add_br()`;
#'  - `'lb'` for `''` (an empty line);
#'  - `'none'` for `NULL` (nothing).
#' @param breaks Indexes to insert `sep` after.
#' @param adds,subs Character vector specifying additions or substitutions
#'  (respectively) on the final punctuation of each clause in `keep`. Defaults
#'  to none (`NULL`).
#' @param sep What to add after the indexes specified by `breaks`.
#' @param sep_n Add `sep` after the break + `sep_n[1]` elements. `sep_n[2]`
#'  is for before the break (when `sep_fragment = TRUE`).
#' @param sep_fragment Instead of `sep`, should the elements specified by
#'  `breaks` be encapsulated in a ".fragment" markdown block? (for revealjs).
#' @param x The custom text to add.
#' @param n The heading level to use. Set to `NA` to ommit. Set to `NULL` to
#'  use the existing heading level.
#' @param br Should a `'<br>'` be added after the pause?
#'
#' @export
e <- function(
    keep = TRUE, end = "pbr", breaks = NULL,
    adds = NULL, subs = NULL,
    modify = \(x) x,
    sep = add_pause(), sep_n = c(1, 1)) {
  msec_env <- rlang::caller_env()

  kept <- msec_env$sections[[msec_env$sec]][[msec_env$block]][keep]
  kept <- subs_and_adds(kept, adds, subs)

  if (!rlang::is_null(breaks)) {
    kept <- append_vec(kept, sep, breaks, sep_n)
  }

  paste0(paste0(modify(kept), collapse = ""), get_end(end)) #rejoin clauses
}


#' @rdname editing
#' @export
div <- function(
    keep = TRUE, end = "pbr", breaks = NULL,
    adds = NULL, subs = NULL,
    modify = \(x) x,
    sep = add_pause(), sep_n = c(1, 1), sep_fragment = FALSE) {
  msec_env <- rlang::caller_env()

  kept <- msec_env$sections[[msec_env$sec]][[msec_env$block]][keep]
  kept <- subs_and_adds(kept, adds, subs)

  if (!rlang::is_null(breaks)) {
    #add .fragment (pause) encapsuling each break
    values <- if (sep_fragment) c(":::::\n", "\n:::::{.fragment}") else sep
    kept <- append_vec(kept, values, breaks, n = sep_n)
  }

  paste0(paste0(modify(kept), collapse = "\n"), get_end(end))
}


#' @rdname editing
#' @export
add_cur_head <- function(n = 2) {
  msec_env <- rlang::caller_env()
  cur_head <- msec_env$sections[[msec_env$sec]]$head1

  if (rlang::is_na(n)) {
    ""
  } else if (rlang::is_null(n)) {
    paste0(cur_head, "\n")
  } else {
    paste0(stringr::str_replace(cur_head, "^#+", strrep("#", n)), "\n")
  }
}

#' @rdname editing
#' @export
add_custom <- function(x, end = "pbr") {
  paste0(x, get_end(end))
}

#' @rdname editing
#' @export
add_pause <- function(br = TRUE, trailing = TRUE) {
  base <- paste0(". . .\n\n", if (br) add_br(FALSE))
  if (trailing) paste0("\n\n", base) else base
}

#' @rdname editing
#' @export
add_br <- function(trailing = TRUE) {
  base <- c("<br>\n\n")
  if (trailing) paste0("\n\n", base) else base
}


# Helper functions:

# Additions and substitutions to each character element
#' @keywords internal
subs_and_adds <- function(kept, adds, subs) {
  if (!rlang::is_null(adds)) {
    n <- length(kept)
    kept <- c(purrr::map2_chr(kept[-n], adds, ~paste0(.x, .y)), kept[n])
  }

  if (!rlang::is_null(subs)) {
    kept <- purrr::map2_chr(kept, subs, ~stringr::str_replace(.x, ".$", .y))
  }

  kept
}
