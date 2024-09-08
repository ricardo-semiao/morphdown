#' Additions to the document
#'
#' @name adding
#' @rdname adding
#'
#' @description
#' Functions to add content to de document:
#' - `add_cur_head()` adds the heading of the current section, with the
#'  specified level, and customized text. `add_subhead` is passed to a 'headx'
#'  block, to add it to the doc,
#'  with a new heading level, and possibly accompanying the section header..
#' - `add_custom()` adds any custom text.
#' - `add_pause()` adds `'. . .'`, `add_br()` adds `'<br>'`, and `add_lb()` adds
#'  `'\n\n'`. `get_end()` is a wrapper to choose between them.
#'
#' @param n The heading level to use. Set to `NA` to ommit. Set to `NULL` (the
#'  default) to use the value of `head_lv`.
#' @param x The custom text to add.
#' @param sep If `sub = TRUE`, the separator to use between the section headers.
#' @param sub Should the header be added to the current section header, or be a
#'  standalone header?
#' @param br Should a `'<br>'` be added after the pause?
#' @param trailing Should an empty line be added before?
#' @param end The type of end to add. See `get_end()`.
#' @param ... Arguments to pass to the `add_*` functions.
#'
#' @export
add_cur_head <- function(n = NULL, x = "") {
  msec_env <- rlang::caller_env()

  n <- n %||% msec_env$head_lv
  cur_head <- msec_env$sections[[msec_env$sec]]$head1

  if (rlang::is_na(n)) {
    ""
  } else if (rlang::is_null(n)) {
    paste0(cur_head, x, "\n")
  } else {
    paste0(stringr::str_replace(cur_head, "^#+", strrep("#", n)), x, "\n")
  }
}

#' @rdname adding
#' @export
add_subhead <- function(n = NULL, sep = " - ", x = "", sub = TRUE) {
  msec_env <- rlang::caller_env()

  n <- n %||% msec_env$head_lv
  sec <- msec_env$sections[[msec_env$sec]]

  if (sub) {
    paste0(
      stringr::str_replace(sec$head1, "^#+", strrep("#", n)),
      sep,
      stringr::str_remove(sec[[msec_env$block]], "^(#+) "),
      x
    )
  } else {
    paste0(
      stringr::str_replace(sec[[msec_env$block]], "^(#+)", strrep('#', n)),
      x
    )
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

#' @rdname adding
#' @export
get_end <- function(end, ...) {
  list(
    "pbr" = add_pause(...),
    "p" = add_pause(FALSE, ...),
    "br" = add_br(...),
    "lb" = add_lb(),
    "none" = NULL
  )[[end]]
}
