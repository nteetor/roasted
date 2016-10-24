#' Hash Map
#'
#' A hash map object.
#'
#' @export
hash_map <- function() {
  HashMap$new()
}

#' @param x An \R object.
#' @rdname hash_map
#' @export
is.map <- function(x) {
  inherits(x, 'map')
}

#' @rdname hash_map
#' @export
is.hash_map <- function(x) {
  inherits(x, 'hash_map')
}

HashMap <- R6::R6Class(
  classname = c('hash_map', 'map'),
  private = list(
    capacity = NULL,
    load_factor = NULL,
    mappings = NULL,
    deep_clone = function(name, value) {
      if (name == 'mappings') {
        list2env(as.list.environment(value, all.names = TRUE), parent = emptyenv())
      } else {
        value
      }
    }
  ),
  public = list(
    initialize = function() {
      private$capacity <- 16
      private$load_factor <- 0.75
      private$mappings <- new.env(size = private$capacity, parent = emptyenv())
      self
    },
    size = function() {
      length(ls(private$mappings))
    },
    isEmpty = function() {
      self$size() == 0
    },
    get = function(key) {
      if (!self$containsKey(key)) {
        NULL
      } else {
        get(key, envir = private$mappings, inherits = FALSE)
      }
    },
    containsKey = function(key) {
      exists(key, envir = private$mappings, inherits = FALSE)
    },
    put = function(key, value) {
      if (!self$containsKey(key)) {
        assign(key, value, envir = private$mappings, inherits = FALSE)
        NULL
      } else {
        old_value <- self$get(key)
        assign(key, value, envir = private$mappings, inherits = FALSE)
        old_value
      }
    },
    putAll = function(m) {
      if (is.null(m)) {
        stop('`m` is NULL', call. = FALSE)
      }
      if (is.map(m)) {
        # TODO: complete method
      } else if (is_named(m)) {
        for (nm in names(m)) {
          assign(nm, value = m[[nm]], envir = private$mappings, inherits = FALSE)
        }
      } else {
        stop('argument `m` is not named and does not inherit class map', call. = FALSE)
      }
    },
    remove = function(key, value) {
      if (missing(value)) {
        if (!self$containsKey(key)) {
          NULL
        } else {
          old_value <- self$get(key)
          rm(key, envir = private$mappings, inherits = FALSE)
          old_value
        }
      } else {
        if (self$containsKey(key) && self$get(key) == value) {
          rm(key, envir = private$mappings, inherits = FALSE)
          TRUE
        }
      }
    },
    clear = function() {
      private$mappings <- new.env(size = private$capacity, parent = emptyenv())
    },
    containsValue = function() {
      # TODO
    },
    keySet = function() {
      # TODO, requires set object
    },
    values = function() {
      # TODO, requires collection object
    },
    entrySet = function()  {

    },
    getOrDefault = function(key, defaultValue) {
      if (!self$containsKey(key)) {
        defaultValue
      } else {
        get(key, envir = private$mappings, inherits = FALSE)
      }
    },
    putIfAbsent = function(key, value) {
      if (!self$containsKey(key) || is.null(self$get(key))) {
        self$put(key, value)
      } else {
        self$get(key)
      }
    },
    replace = function(key, old, new) {
      if (self$containsKey(key) && self$get(key) == old) {
        self$put(key, new)
        TRUE
      } else {
        FALSE
      }
    },
    computeIfAbsent = function(key, f) {
      if (!is.function(f) && !inherits(f, 'formula')) {
        stop('argument `f` must be a function or one-sided formula', call. = FALSE)
      }

      if (is.function(f) && length(formals(f)) != 1) {
        stop('mapping function must only accept a single argument', call. = FALSE)
      }

      if (inherits(f, 'formula') && length(f) != 2) {
        stop('mapping formula must be one-sided', call. = FALSE)
      }

      if (!is.null(self$get(key))) {
        return(self$get(key))
      }

      if (inherits(f, 'formula')) {
        evalir <- environment(f)
        evalir$`.key` <- key
        f <- eval(call('function', as.pairlist(NULL), f[[2]]), envir = evalir)
      }

      result <- if (is.function(f)) f(key) else f()

      if (is.null(result)) {
        NULL
      } else {
        self$put(key, result)
        result
      }
    },
    computeIfPresent = function(key, f) {
      if (!is.function(f) && !inherits(f, 'formula')) {
        stop('argument `f` must be a function or one-sided formula', call. = FALSE)
      }

      if (is.function(f) && length(formals(f)) != 2) {
        stop('function must accept 2 arguments', call. = FALSE)
      }

      if (inherits(f, 'formula') && length(f) != 2) {
        stop('mapping formula must be one-sided', call. = FALSE)
      }

      if (is.null(self$get(key))) {
        return(NULL)
      }

      if (inherits(f, 'formula')) {
        evalir <- environment(f)
        evalir$`.key` <- key
        evalir$`.value` <- self$get(key)

        f <- eval(call('function', as.pairlist(NULL), f[[2]]), envir = evalir)
      }

      result <- if (is.function(f)) f(key, value) else f()

      if (is.null(result)) {
        self$remove(key)
        NULL
      } else {
        self$put(key, result)
        result
      }
    },
    compute = function(key, f) {
      if (!is.function(f) && !inherits(f, 'formula')) {
        stop('argument `f` must be a function or one-sided formula', call. = FALSE)
      }

      if (is.function(f) && length(formals(f)) != 2) {
        stop('function must accept 2 arguments', call. = FALSE)
      }

      if (inherits(f, 'formula') && length(f) != 2) {
        stop('mapping formula must be one-sided', call. = FALSE)
      }

      if (inherits(f, 'formula')) {
        evalir <- environment(f)
        evalir$`.key` <- key
        evalir$`.value` <- self$get(key)

        f <- eval(call('function', as.pairlist(NULL), f[[2]]), envir = evalir)
      }

      result <- if (is.function(f)) f(key, value) else f()

      if (is.null(result)) {
        self$remove(key)
        NULL
      } else {
        self$put(key, result)
        result
      }
    },
    merge = function(key, value, f) {
      if (!is.function(f) && !inherits(f, 'formula')) {
        stop('argument `f` must be a function or one-sided formula', call. = FALSE)
      }

      if (is.function(f) && length(formals(f)) != 2) {
        stop('function must accept 2 arguments', call. = FALSE)
      }

      if (inherits(f, 'formula') && length(f) != 2) {
        stop('mapping formula must be one-sided', call. = FALSE)
      }

      if (is.null(self$get(key))) {
        self$put(key, value)
        return(value)
      }

      if (inherits(f, 'formula')) {
        evalir <- environment(f)
        evalir$`.value` <- self$get(key)
        evalir$`..value` <- value

        f <- eval(call('function', as.pairlist(NULL), f[[2]]), envir = evalir)
      }

      result <- if (is.function(f)) f(self$get(key), value) else f()

      if (is.null(result)) {
        self$remove(key)
        NULL
      } else {
        self$put(key, result)
        result
      }
    }
  )
)
