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
    containsAll = function(c) {
      all(vapply(c, function(e) self$contains(e), logical(1)))
    },
    addAll = function(c) {
      any(vapply(c, function(e) self$add(e), logical(1)))
    },
    removeAll = function(c) {
      any(vapply(c, function(e) self$remove(e), logical(1)))
    },
    removeIf = function(f) {
      if (!is.function(f) && !inherits(f, 'formula')) {
        stop('argument `f` must be a function or formula', call. = FALSE)
      }

      if (is.function(f) && length(formals(f)) != 1) {
        stop('function `f` must be accept a single argument', call. = FALSE)
      }

      if (inherits(f, 'formula') && length(f) != 2) {
        stop('formula `f` must be one-sided', call. = FALSE)
      }

      modiflag <- FALSE
      itr <- self$iterator()
      while (itr$hasNext()) {
        if (is.function(f)) {
          if (f(itr$getNext())) {
            itr$remove()
            modiflag <- TRUE
          }
        } else {
          evf <- environment(f)
          evf$`.` <- itr$getNext()
          if ((eval(call('function', as.pairlist(NULL), f[[2]]), envir = evf))()) {
            itr$remove()
            modiflag <- TRUE
          }
        }
      }
      modiflag
    },
    retainAll = function(c) {
      any(vapply(c, function(e) if (!self$contains(e)) self$remove(e), logical(1)))
    },
    clear = function() {
      self$elements <- new.env(parent = emptyenv())
    },
    equals = function(o) {
      if (inherits(o, 'collection') && self$size() == o$size()) {
        all(vapply(o$toList(), function(e) self$contains(e), logical(1))) &&
          all(vapply(self$toList(), function(e) o$contains(e), logical(1)))
      } else {
        FALSE
      }
    },
    hashCode = function() {
      stop('hashCode method is not implemented', call. = FALSE)
    }
  )
)

