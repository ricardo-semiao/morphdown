#' Plan Document Morphing
#'
#' @name planning
#' @rdname planning
#'
#' @description
#' These functions are used to programmatically alter the Rmd, once it has been
#' split (see \link[morphdown]{splitting}).
#'
#' In `plan_doc`, global changes can be defined. Then, the user must supply one
#' `s* = plan_sec(...)` arguments, where `*` corresponds to the section
#' number, for each section he wants to keep.
#'
#' In `plan_sec`, local section changes can be defined. Then, the user must
#' supply one `b* = fun(...)` to each block he wants to keep. There is also the
#' possibility to add new lines of markdown with unnamed calls. See the
#' available block editing functions in \link[morphdown]{editing}.
#'
#' @param sections A list of sections as returned by `split_sections`.
#' @param level_add An integer value to adjust the header level of all sections.
#'  For instance, a `level_add = 1` transform "# Header" into "## Header".
#'  Negative values decrease the header value.
#' @param head_level An integer value to set the heading level of the current
#'  section header.
#' @param ... `s* = plan_sec(...)` arguments for `plan_doc`, or `b* = fun(...)`
#'  arguments for `plan_sec`.
#'
#' @return For `plan_doc`, a character vector containing the processed document.
#'  `plan_sec` should not be used outside `plan_doc`.
#' @export
plan_doc <- function(sections, level_add = 0, ...) {
  s_plans <- rlang::enexprs(...)

  result_doc <- list()
  for (s in seq_along(s_plans)) {
    sec <- names(s_plans)[s]
    sections[[s]]$head <- head_rmv(sections[[s]]$head, level_add)
    result_doc[[s]] <- eval(s_plans[[s]])
  }

  raw <- paste0(unlist(result_doc), collapse = "\n")
  final <- gsub("(\\. \\. \\.)(\\n)*(<br>)?(\\n)*#", "#", raw)
}


#' @rdname planning
#' @export
plan_sec <- function(head_level = NULL, ...) {
  b_plans <- rlang::enexprs(...)
  sec <- rlang::caller_env()$sec
  sections <- rlang::caller_env()$sections

  result_sec <- list()
  for (b in seq_along(b_plans)) {
    block <- names(b_plans)[b]
    result_sec[[b]] <- eval(b_plans[[b]])
  }

  c(add_cur_head(head_level), "", result_sec)
}
