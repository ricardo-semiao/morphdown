#' Edit Blocks
#'
#' @name editing
#' @rdname editing
#'
#' @description
#' These functions are assigned to blocks inside `plan_sec(b* = fun(...))`.
#'  See \link[morphdown]{morphing}. The functions `e()` and `div()` edit
#'  existing blocks:
#'
#' - `e()` is used on single-lines of text, that have been split into clauses.
#' - `div()` is used on multi-line blocks of text, that have been split by
#'  lines.
#'
#' @param keep Indexes of the clauses (for `e()`) or lines (for `div()`) to keep.
#'  Defaults to all (`NULL`).
#' @param end Character specifying what to add at the end of the block, passed
#'  to `get_end()`. The default value is whatever was passed to the
#'  `morph_sec()`'s `end` argument. Possible values are:
#'  - `'pbr'` for `add_pause()` (the default);
#'  - `'p'` for `add_pause(FALSE)`;
#'  - `'br'` for `add_br()`;
#'  - `'lb'` for `''` (an empty line);
#'  - `'none'` for `NULL` (nothing).
#' @param breaks Indexes to insert `sep` after. If `sep_fragment = TRUE`, it
#'  should be a list of indexes to surround with '.fragment' markdown blocks.
#' @param adds,subs Character vector specifying additions or substitutions
#'  (respectively) on the final punctuation of each clause in `keep`. Defaults
#'  to none (`NULL`).
#' @param modify Function to to the block, after the base manipulations.
#' @param sep What to add after the indexes specified by `breaks`.
#' @param sep_n Add `sep` after the break + `sep_n[1]` elements. `sep_n[2]`
#'  is for before the break (when `sep_fragment = TRUE`).
#' @param sep_fragment Instead of `sep`, should the elements specified by
#'  `breaks` be encapsulated in a ".fragment" markdown block? (for revealjs).
#'
#' @export
e <- function(
    keep = NULL, end = NULL, breaks = NULL,
    adds = NULL, subs = NULL,
    modify = \(x) x,
    sep = add_pause(), sep_n = 1) {
  msec_env <- rlang::caller_env()

  original <- msec_env$sections[[msec_env$sec]][[msec_env$block]]
  keep <- keep %||% seq_along(original)
  end <- end %||% msec_env$end

  kept <- original[keep]
  kept <- subs_and_adds(kept, adds, subs)

  if (!rlang::is_null(breaks)) {
    breaks <- match(breaks, if (is.integer(keep)) keep else which(keep))
    kept <- vec_append(kept, breaks, sep, sep_n)
  }

  paste0(paste0(modify(kept), collapse = " "), get_end(end)) #rejoin clauses
}


#' @rdname editing
#' @export
div <- function(
    keep = NULL, end = NULL, breaks = NULL,
    adds = NULL, subs = NULL,
    modify = \(x) x,
    sep = add_pause(), sep_n = NULL, sep_fragment = FALSE) {
  msec_env <- rlang::caller_env()
  sep_n <- sep_n %||% if (sep_fragment) c(1, 1) else 1

  original <- msec_env$sections[[msec_env$sec]][[msec_env$block]]
  keep <- keep %||% seq_along(original)
  end <- end %||% msec_env$end

  kept <- original[keep]
  kept <- subs_and_adds(kept, adds, subs)

  if (!rlang::is_null(breaks)) {
    breaks <- if (rlang::is_list(breaks)) {
      purrr::map(breaks, ~ match(.x, if (is.integer(keep)) keep else which(keep)))
    } else {
      match(breaks, if (is.integer(keep)) keep else which(keep))
    }
    if (sep_fragment) { #add .fragment (pause) encapsuling each break
      kept <- vec_surround(kept, breaks, c(":::::\n", "\n:::::{.fragment}"), sep_n)
    } else {
      kept <- vec_append(kept, breaks, sep, sep_n)
    }
  }

  paste0(paste0(modify(kept), collapse = "\n"), get_end(end))
}


# Helper functions:

#' Additions and substitutions to each character element
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
