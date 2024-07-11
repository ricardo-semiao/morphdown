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
#' - `div` is used on multi-element blocks. Currently, it returns the block
#'  as is.
#' - `add_cur_head` adds the heading of the current section, with the specified
#'  level.
#'
#' @param cs Integer vector of clauses to keep. Defaults to all.
#' @param subs Character vector specifying substitutions for the final
#'  punctuation on each clause in `cs`. Defaults to none.
#' @param pat The pattern to break expressions into clauses. Defaults to
#'  `"(?<=[\\.:;!?])"`.
#' @param n The heading level to use.
#' @export
e <- function(cs = NULL, subs = NULL, pat = NULL) {
  ind <- rlang::caller_env()
  pat <- pat %||% "(?<=[\\.;!?])"

  clauses <- split_clauses(ind$sections[[ind$sec]][[ind$block]], pat)
  cs <- cs %||% seq_along(clauses)
  kept <- clauses[cs]

  if (!rlang::is_null(subs)) {
    kept <- purrr::map2_chr(kept, subs, ~stringr::str_replace(.x, ".$", .y))
  }

  c(kept, "")
}


#' @rdname editing
#' @export
div <- function() {
  ind <- rlang::caller_env()
  c(ind$sections[[ind$sec]][[ind$block]], "")
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
