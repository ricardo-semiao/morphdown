utils::globalVariables(".") # Remove CRAN note towards the magrittr dot

adjust_head_level <- function(x, n) {
  stringr::str_replace(x, paste0("#{", n,"}"), "")
}

is_path <- function(x) {
  length(x) == 1 && grepl("/.+\\.[Rr][Mm][Dd]$", x)
}

append_vec <- function(x, values, afters, n = c(1, 1)) {
  id <- seq_along(x)
  for (i in seq_along(values)) { #if values has two elements, add before and after
    x <- c(as.list(x), purrr::map(afters, ~values[[i]]))
    id  <- c(id, afters + `if`(i == 1, 1, -1) * (0.5 + n[[i]] - 1))
  }

  unlist(x[order(id)])
}

get_end <- function(end) {
  list(
    "pbr" = add_pause(),
    "p" = add_pause(FALSE),
    "br" = add_br(),
    "lb" = "\n\n",
    "none" = NULL
  )[[end]]
}


