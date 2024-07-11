utils::globalVariables(".") # Remove CRAN note towards the magrittr dot

head_rmv <- function(x, n) {
  stringr::str_replace(x, paste0("#{", n,"}"), "")
}

is_path <- function(x) {
  length(x) == 1 && grepl("/.+\\.[Rr][Mm][Dd]$", x)
}
