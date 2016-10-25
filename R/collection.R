Collection <- R6::R6Class(
  classname = 'collection',
  private = list(
    elements = NULL,
    class = NULL
  ),
  public = list(
    initialize = function(class) {
      private$elements <- new.env(parent = emptyenv())
      private$class <- class
      self
    },
    size = function() {
      length(ls(private$elements))
    },
    isEmpty = function() {
      length(ls(private$elements)) == 0
    },
    contains = function(o) {
      if (!inherits(o, private$class)) {
        stop('argument `o` does not inherit collection class ', private$class,
             call. = FALSE)
      }
      # `sapply` is used here as no result is collected, I support the total
      # and complete use of `vapply` if you are saving a result.
      sapply(
        ls(private$elements),
        function(nm) if (identical(private$elements[[nm]], o)) return(TRUE)
      )
      FALSE
    },
    iterator = function() {
      Iterator$new(self)
    },
    toList = function() {
      unname(
        mget(ls(private$elements), envir = private$elements, inherits = FALSE),
        force = TRUE
      )
    },
    add = function(e) {
      private$elements[[unique_id()]] <- e
      TRUE
    },
    remove = function(o) {
      # see above comment about the use of `sapply`
      sapply(
        ls(private$elements),
        function(nm) {
          if (identical(private$elements[[nm]], o)) {
            rm(nm, envir = private$elements, inherits = FALSE)
            return(TRUE)
          }
        }
      )
      FALSE
    },
    addAll = function(c) {
      # WORKING HERE
    }
  )
)

