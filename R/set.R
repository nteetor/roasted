#' Set
#'
#' A set object.
#'
#' @export
set <- function() {
  Set$new()
}

Set <- R6::R6Class(
  classname = 'set',
  private = list(
    elements = NULL,
    class = NULL
  ),
  public = list(
    initialize = function() {
      self
    }

  )
)
