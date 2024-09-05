#' Morph documents
#'
#' @name planning
#' @rdname planning
#'
#' @description
#' These functions are used to programmatically alter the Rmd, once it has been
#' split (see \link[morphdown]{splitting}).
#'
#' In `morph_doc`, global changes can be defined. Then, the user must supply one
#' `s* = morph_sec(...)` arguments, where `*` corresponds to the section
#' number, for each section he wants to keep.
#'
#' In `morph_sec`, local section changes can be defined. Then, the user must
#' supply one `b* = fun(...)` to each block he wants to keep. There is also the
#' possibility to add new lines of markdown with unnamed calls. See the
#' available block editing functions in \link[morphdown]{editing}.
#'
#' @param sections A list of sections as returned by `split_sections`.
#' @param level_add An integer value to adjust the header level of all sections.
#'  For instance, a `level_add = 1` transform "# Header" into "## Header".
#'  Negative values decrease the header value.
#' @param head_lv An integer value to set the heading level of the current
#'  section header. The default value for `morph_sec` is whatever was passed to
#'  the `morph_doc`'s argument.
#' @param ... `s* = morph_sec(...)` arguments for `morph_doc`, or
#'  `b* = fun(...)` arguments for `morph_sec`.
#'
#' @return For `morph_doc`, a character vector containing the morphed
#'  document. `morph_sec` should not be used outside `morph_doc`.
#'
#' @export
morph_doc <- function(sections, ..., head_lv = 2) {
  s_plans <- rlang::enexprs(...)

  result_doc <- list()
  for (s in seq_along(s_plans)) {
    sec <- names(s_plans)[s] #passed to morph_sec
    result_doc[[s]] <- eval(s_plans[[s]])
  }

  unlist(result_doc) %>%
    paste0(collapse = "\n") %>%
    gsub("(\\. \\. \\.)(\\n)*(<br>)?(\\n)*#", "#", .) #remove pauses before headers
}


#' @rdname planning
#' @export
morph_sec <- function(..., head_lv = NULL) {
  mdoc_env <- rlang::caller_env()
  sections <- rlang::caller_env()$sections #passed to editing functions
  sec <- rlang::caller_env()$sec #passed to editing functions
  head_lv <- head_lv %||% mdoc_env$head_lv

  b_plans <- rlang::enexprs(...)
  if (rlang::is_null(b_plans$head1)) {
    b_plans <- c(list(head1 = add_cur_head(head_lv)), b_plans)
  }

  result_sec <- list()
  for (b in seq_along(b_plans)) {
    block <- names(b_plans)[b] #passed to editing functions
    result_sec[[b]] <- eval(b_plans[[b]])
  }

  result_sec
}
