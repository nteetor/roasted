Iterator <- R6::R6Class(
  classname = 'iterator',
  private = list(
    collection = NULL,
    names = NULL,
    cursor = NULL
  ),
  public = list(
    initialize = function(collection) {
      private$collection <- collection
      private$names <- ls(collection$.__enclos_env__$private$elements)
      if (is.null(private$names)) stop('internal error', call. = FALSE)
      private$cursor <- private$names[1]
      self
    },
    hasNext = function() {
      length(private$names) > 0
    },
    getNext = function() {
      if (length(private$names) == 0) {
        stop('no more elements', call. = FALSE)
      }
      private$cursor <- private$names[1]
      private$names <- private$names[-1]
      private$collection$.__enclos_env__$private$elements[[private$cursor]]
    },
    remove = function() {
      if (is.null(private$previous)) {
        stop('no element to remove', call. = FALSE)
      }

      private$collection$remove(
        private$collection$.__enclos_env__$private$elements[[private$cursor]]
      )
      private$cursor <- NULL
    }
  )
)
