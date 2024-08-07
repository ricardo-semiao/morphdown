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
#' - `e` is used on single element blocks ('text expressions'). It splits the
#'  expression into clauses, and allows the user to select and alter each.
#' - `div` is used on multi-line blocks.
#' - `add_cur_head` adds the heading of the current section, with the specified
#'  level.
#' - `add_pause` adds `'. . .'` and `add_br` adds `'<br>'`.
#' - `add_custom` adds any custom text.
#'
#' @param cs Integer vector of clauses (for `e`) or lines (for `div`) to keep.
#'  Defaults to all.
#' @param end Character specifying what to add at the end of the block: `'pbr'`
#'  for `add_pause()` (the default); `'p'` for `add_pause(FALSE)`, and `'br'`
#'  for `add_br()`.
#' @param breaks Indexes to insert `sep`.
#' @param adds,subs Character vector specifying additions or substitutions 
#'  (respectively) on the final punctuation of each clause in `cs`. Defaults to
#'  none.
#' @param pat The pattern to break expressions into clauses. Defaults to
#'  `"(?<=[\\.:;!?])"`.
#' @param sep What to add after the indexes specified by `breaks`.
#' @param x The custom text to add.
#' @param n The heading level to use.
#' @param br Should a `'<br>'` be added after the pause?
#'
#' @export
e <- function(
    cs = NULL, end = "pbr", breaks = NULL,
    adds = NULL, subs = NULL,
    pat = "(?<=[\\.:;!?])(?<!\\{\\.)", sep = add_pause()) {
  ind <- rlang::caller_env()

  clauses <- split_clauses(ind$sections[[ind$sec]][[ind$block]], pat)
  cs <- cs %||% seq_along(clauses)
  kept <- clauses[cs]

  if (!rlang::is_null(adds)) {
    n <- length(kept)
    kept <- c(purrr::map2_chr(kept[-n], adds, ~paste0(.x, .y)), kept[n])
  }

  if (!rlang::is_null(subs)) {
    kept <- purrr::map2_chr(kept, subs, ~stringr::str_replace(.x, ".$", .y))
  }

  if (!rlang::is_null(breaks)) {
    kept <- append_vec(kept, c("", sep), breaks)
  }

  c(kept, "", get_end(end))
}


#' @rdname editing
#' @export
div <- function(
    cs = TRUE, end = "p", breaks = NULL,
    sep = add_pause()) {
  ind <- rlang::caller_env()

  kept <- ind$sections[[ind$sec]][[ind$block]][cs]

  if (!rlang::is_null(breaks)) {
    kept <- append_vec(kept, c("", sep), breaks)
  }

  c(kept, "", get_end(end))
}


#' @rdname editing
#' @export
add_cur_head <- function(n = NULL) {
  ind <- rlang::caller_env()
  cur_head <- ind$sections[[ind$sec]]$head
  if (!rlang::is_null(n)) {
    cur_head <- stringr::str_replace(cur_head, "^#+", strrep("#", n))
  }

  c(cur_head, "")
}

#' @rdname editing
#' @export
add_custom <- function(x, end = "pbr") {
  c(x, "", get_end(end))
}

#' @rdname editing
#' @export
add_pause <- function(br = TRUE) {
  c(". . .", "", if (br) add_br())
}

#' @rdname editing
#' @export
add_br <- function() {
  c("<br>", "")
}
