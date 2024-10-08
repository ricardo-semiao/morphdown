#' Split character vector after match
#' @keywords internal
split_after <- function(x, pat, final, border = 0) {
  indexes <- grep(pat, x)
  if (length(indexes) == 0) return(x)

  indexes <- index_add_border(indexes, border, "before")
  groups_from_indexes(x, indexes, final, single = TRUE)
}

#' Split character vector between opening (1) and closing (2) matches
#' @keywords internal
split_between_12 <- function(x, pat, border = c(0, 0), final) {
  indexes <- list(grep(pat[[1]], x), grep(pat[[2]], x))
  if (length(indexes[[1]]) == 0) return(x)

  indexes <- index_add_border(indexes, border, "both")
  groups_from_indexes(x, indexes, final, single = FALSE)
}

#' Split character vector between opening (1) and closing (1) matches
#' @keywords internal
split_between_11 <- function(x, pat, border = c(0, 0), final) {
  indexes <- grep(pat, x)
  if (length(indexes) == 0) return(x)

  closings <- seq_along(indexes) %% 2 == 0
  indexes <- list(indexes[!closings], indexes[closings])

  indexes <- index_add_border(indexes, border, "both")
  groups_from_indexes(x, indexes, final, single = FALSE)
}

#' Split character vector on groups of repeated matches
#' @keywords internal
split_on_repeated <- function(x, pat, border = c(0, 0), final) {
  matches <- grep(pat, x)
  if (length(matches) == 0) return(x)

  indexes <- list(
    matches[c(TRUE, which(c(FALSE, diff(matches) > 1)))],
    matches[c(which(c(FALSE, (diff(matches) > 1)[-1], TRUE)))]
  )

  indexes <- index_add_border(indexes, border, "both")
  groups_from_indexes(x, indexes, final, single = FALSE)
}

#' Split character vector on a single match
#' @keywords internal
split_on_single <- function(x, pat, final) {
  indexes <- grep(pat, x)
  if (length(indexes) == 0) return(x)

  indexes <- c(indexes, indexes + 1) %>% `[`(. <= length(x))

  groups_from_indexes(x, indexes, final, single = TRUE)
}


# Helper functions:

#' Update the index of a match to `n` places before and/or after the match
#' @keywords internal
index_add_border <- function(x, border, type) {
  if (type == "before") {
    purrr::map_int(x, ~.x - border)
  } else if (type == "after") {
    purrr::map_int(x, ~.x + border)
  } else if (type == "both") {
    list(
      purrr::map_int(x[[1]], ~.x - border[[1]]),
      purrr::map_int(x[[2]], ~.x + border[[2]])
    )
  }
}

#' Get a factor vector of 'group ids' from binary vector of 'regions'
#' @keywords internal
groups_from_indexes <- function(x, indexes, final, single) {
  matches <- integer(length(x))

  if (!single) {
    matches[indexes[[1]]] <- 1
    matches[indexes[[2]]] <- -1
    frontier <- c(1, diff(cumsum(matches) > 0))
    ends <- which(frontier == -1)
    frontier[ends] <- 0
    frontier[ends + 1] <- -1
    groups <- cumsum(abs(frontier))
  } else {
    matches[indexes] <- 1
    groups <- cumsum(matches) + 1
  }

  splits <- split(x, groups)

  if (!rlang::is_null(final)) {
    splits %>%
      purrr::map2(seq_along(.),
        ~ structure(.x, is_final = final && .y %% 2 == 0)
      )
  } else {
    splits
  }
}
# the .y %% 2 == 0 assumes that there is always an unmatched element at any
# given block. This will always be true if the user does concatenates content
# without an empty line between them. Caution: a split that uses
# `border = c(x, y)`, with y > 0, might eat such empty line.
# This can be avoided with final && grepl(pattern[[1]], .x).
