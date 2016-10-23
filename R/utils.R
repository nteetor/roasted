`%||%` <- function(a, b) if (is.null(a)) b else a

is_counting_number <- function(x) {
  is.numeric(x) && x > 0 && (x %% 1 == 0)
}

is_named <- function(x) {
  assertthat::has_attr(x, 'names')
}
