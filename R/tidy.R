## tidyverse-like functions that are for convenience

fill_downward <- function(vec) {
  out <- vector(mode = class(vec), length = length(vec))
  current <- NA
  for (i in seq_along(vec)) {
    if (!is.na(vec[i])) {
      current <- vec[i]
    }
    out[i] <- current
  }
  out
}

relocate_after <- function(x, cols, after = NULL) {
  before_after <- !as.logical(cumsum(colnames(x) == after))
  before_after_cols <- colnames(x)[before_after]
  x[c(
    before_after_cols,
    after,
    cols,
    setdiff(names(x), c(before_after_cols, after, cols))
  )]
}
