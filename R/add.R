#' Additions to the document
#'
#' @name adding
#' @rdname adding
#'
#' Functions to add content to de document:
#' - `add_cur_head` adds the heading of the current section, with the specified
#'  level. `add_subhead` is passed to a 'headx' block, to add it to the doc,
#'  with a new heading level, and possibly accompanying the section header..
#' - `add_custom` adds any custom text.
#' - `add_pause` adds `'. . .'`, `add_br` adds `'<br>'`, and `add_lb` adds
#'  `'\n\n'`.
#'
#' @param x The custom text to add.
#' @param n The heading level to use. Set to `NA` to ommit. Set to `NULL` to
#'  use the existing heading level.
#' @param br Should a `'<br>'` be added after the pause?
#'
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

#' @rdname adding
#' @export
add_subhead <- function(n = 2, sub = TRUE, sep = " - ") {
  msec_env <- rlang::caller_env()
  sec <- msec_env$sections[[msec_env$sec]]

  if (sub) {
    paste0(
      stringr::str_replace(sec$head1, "^#+", strrep("#", n)),
      sep,
      stringr::str_remove(sec[[msec_env$block]], "^(#+) ")
    )
  } else {
    stringr::str_replace(sec[[msec_env$block]], "^(#+)", strrep('#', n))
  }
}

#' @rdname adding
#' @export
add_custom <- function(x, end = "pbr") {
  paste0(x, get_end(end))
}

#' @rdname adding
#' @export
add_pause <- function(br = TRUE, trailing = TRUE) {
  base <- paste0(". . .\n\n", if (br) add_br(FALSE))
  if (trailing) paste0("\n\n", base) else base
}

#' @rdname adding
#' @export
add_br <- function(trailing = TRUE) {
  base <- c("<br>\n\n")
  if (trailing) paste0("\n\n", base) else base
}

#' @rdname adding
#' @export
add_lb <- function() {
  c("\n\n")
}


# Helper functions:
get_end <- function(end, ...) {
  list(
    "pbr" = add_pause(...),
    "p" = add_pause(FALSE, ...),
    "br" = add_br(...),
    "lb" = add_lb(),
    "none" = NULL
  )[[end]]
}
