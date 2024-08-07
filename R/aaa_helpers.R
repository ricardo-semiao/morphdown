utils::globalVariables(".") # Remove CRAN note towards the magrittr dot

head_rmv <- function(x, n) {
  stringr::str_replace(x, paste0("#{", n,"}"), "")
}

is_path <- function(x) {
  length(x) == 1 && grepl("/.+\\.[Rr][Mm][Dd]$", x)
}

append_vec <- function(x, value, afters) {
  val <- c(as.list(x), purrr::map(afters, ~value))
  id  <- c(seq_along(x), afters + 0.5)
  unlist(val[order(id)])
}

get_end <- function(end) {
  list(
    "pbr" = add_pause(),
    "p" = add_pause(FALSE),
    "br" = add_br()
  )[[end]]
}
