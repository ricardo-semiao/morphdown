utils::globalVariables(".") # Remove CRAN note towards the magrittr dot

adjust_head_level <- function(x, n) {
  stringr::str_replace(x, paste0("#{", n,"}"), "")
}

is_path <- function(x) {
  length(x) == 1 && grepl("/.+\\.[Rr][Mm][Dd]$", x)
}

vec_append <- function(x, after, value, n = 1) {
  id <- seq_along(x)
  x <- c(as.list(x), purrr::map(after, ~value))
  id  <- c(id, after + 0.5 + n - 1)
  unlist(x[order(id)])
}

vec_surround <- function(x, after, value, n = c(1, 1)) {
  id <- seq_along(x)
  k <- length(after)

  after <- if (rlang::is_list(after)) {
    purrr::transpose(after)
  } else {
    list(after, after)
  }

  for (i in 1:2) {
    x <- c(as.list(x), purrr::map(1:k, ~value[[i]]))
    id  <- c(id, after[[i]] + `if`(i == 1, 1, -1) * (0.5 + n[[i]] - 1))
  }

  unlist(x[order(id)])
}
