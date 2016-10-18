#' Strings
#'
#' Initialize a newly created string object so that it represents an empty
#' character sequence.
#'
#' @param value The initial value of the string.
#' @param offset The initial offset.
#' @param count The length.
#' @param x An object to be tested.
#'
#' @export
string <- function(value) {
  if (!is.character(value) && !is.string(value)) {
    stop('`value` must be of class string or character', call. = FALSE)
  }

  String$new(value)
}

#' @export
#' @rdname string
is.string <- function(x) {
  'string' %in% class(x)
}

String <- R6::R6Class(
  classname = 'string',
  private = list(
    value = ''
  ),
  public = list(
    CASE_INSENSITIVE_ORDER = NULL,   # unused for now
    initialize = function(x) {
      if (is.string(x)) {
        private$value <- x$toString()
      } else {
        private$value <- as.character(x)
      }

      return(self)
    },
    charAt = function(index) {
      if (!is_counting_number(index)) {
        stop('`index` must be a positive integer', call. = FALSE)
      }
      if (index > nchar(private$value)) {
        stop('`index` out of bounds', call. = FALSE)
      }

      substr(private$value, start = index, stop = index)
    },
    getChars = function(begin, end) {
      if (!is_counting_number(begin) || !is_counting_number(end)) {
        stop('`begin` and `end` must be positive integers', call. = FALSE)
      }
      if (end > nchar(private$value)) {
        stop('`end` out of bounds', call. = FALSE)
      }
      if (begin > end) {
        stop('`begin` must be less than `end`', call. = FALSE)
      }

      strsplit(substr(private$value, start = begin, stop = end), split = '')[[1]]
    },
    getBytes = function() {
      charToRaw(private$value)
    },
    equals = function(object) {
      !is.null(object) &&
        is.string(object) &&
        !self$compareTo(object)
    },
    contentEquals = function(object) {
      tryCatch(
        strject <- string(object),
        error = function(e) {
          stop('Could not coerce `object` to string')
        }
      )

      self$equals(strject)
    },
    equalsIgnoreCase = function(another) {
      !is.null(another) &&
        is.string(another) &&
        self$length() == another$length() &&
        toupper(tolower(private$value)) == toupper(tolower(another$toString()))
    },
    compareTo = function(another, ignoreCase = FALSE) {
      if (!is.string(another) && !is.character(another)) {
        stop('Argument `another` must be of class string or character', call. = FALSE)
      }

      another <- string(another)
      k <- min(self$length(), another$length())

      for (i in seq_len(k)) {
        c1 <- self$charAt(i)
        c2 <- another$charAt(i)

        if (ignoreCase) {
          diff <- utf8ToInt(toupper(tolower(c1))) - utf8ToInt(toupper(tolower(c2)))
        } else {
          diff <- utf8ToInt(c1) - utf8ToInt(c2)
        }

        if (diff != 0) {
          return(diff)
        }
      }

      self$length() - another$length()
    },
    regionMatches = function(toffset, other, ooffset, len, ignoreCase = FALSE) {
      if (!is.string(other)) {
        stop('`other` must be of class string')
      }

      len <- len - 1
      if (toffset < 0 || ooffset < 0) {
        return(FALSE)
      }
      if (toffset + len > self$length() || ooffset + len > other$length()) {
        return(FALSE)
      }

      tregion <- self$substring(toffset, toffset + len)$toString()
      oregion <- other$substring(ooffset, ooffset + len)$toString()

      if (ignoreCase) {
        toupper(tolower(tregion)) == toupper(tolower(oregion))
      } else {
        tregion == oregion
      }
    },
    startsWith = function(prefix, toffset = 1) {
      if (!is.string(prefix) && !is.character(prefix)) {
        stop('`prefix` must be of class string or character', call. = FALSE)
      }

      prefix <- string(prefix)

      if (prefix$contentEquals('') || self$equals(prefix)) {
        return(TRUE)
      }

      self$regionMatches(toffset, prefix, 1, prefix$length())
    },
    endsWith = function(suffix) {
      if (!is.string(suffix) && !is.character(suffix)) {
        stop('`prefix` must be of class string or character', call. = FALSE)
      }

      suffix <- string(suffix)

      if (suffix$contentEquals('') || self$equals(suffix)) {
        return(TRUE)
      }

      self$regionMatches(self$length() - suffix$length() + 1,
                         suffix, 1, suffix$length())
    },
    hashCode = function() {
      if (private$value == '') return(0)

      sum(
        vapply(
          X = seq_len(self$length()),
          FUN = function(i) {
            utf8ToInt(self$charAt(i)) * (31 ^ (self$length() - i))
          },
          FUN.VALUE = numeric(1),
          USE.NAMES = FALSE
        )
      )
    },
    indexOf = function(str, fromIndex = NULL) {
      if (!is.string(str) && !is.character(str)) {
        stop('`str` must be of class string or character')
      }

      fromIndex <- if (is.null(fromIndex)) 1 else fromIndex

      if (fromIndex > self$length()) {
        return(-1)
      }

      target <- if (is.string(str)) str$toString() else str
      trimmed <- substr(private$value, start = fromIndex, stop = self$length())[[1]]

      regexpr(pattern = target, text = trimmed, fixed = TRUE)
    },
    lastIndexOf = function(str, fromIndex = NULL) {
      if (!is.string(str) && !is.character(str)) {
        stop('`str` must be of class string or character')
      }

      fromIndex <- if (is.null(fromIndex)) self$length() else fromIndex

      if (fromIndex <= 0) {
        return(-1)
      }

      target <- if (is.string(str)) str$toString() else str
      trimmed <- substr(private, start = 1, stop = fromIndex)

      regexpr(pattern = target, text = trimmed, fixed = TRUE)
    },
    substring = function(beginIndex, endIndex = NULL) {
      if (beginIndex < -1 ) {
        stop('`beginIndex` out of bounds')
      }

      endIndex <- if (is.null(endIndex)) self$length() else endIndex
      if (endIndex > nchar(private$value)) {
        stop('`endIndex` out of bounds')
      }

      string(substr(private$value, start = beginIndex, stop = endIndex))
    },
    concat = function(str) {
      if (!is.string(str) && !is.character(str)) {
        stop('`str` must be of class string or character')
      }

      string(paste0(private$value, str))
    },
    replace = function(oldChar, newChar) {
      if (!is.character(oldChar) || nchar(oldChar) > 1) {
        stop('`oldChar` must be a single character')
      }
      if (!is.character(newChar) || nchar(newChar) > 1) {
        stop('`newChar` must be a single character', call. = FALSE)
      }

      if (self$indexOf(oldChar) < 0) {
        self
      } else {
        string(gsub(pattern = oldChar, replacement = newChar, fixed = TRUE))
      }
    },
    matches = function(regex) {
      if (!is.character(regex)) {
        stop('`regex` must be of class character', call. = FALSE)
      }

      grepl(regex, private$value)
    },
    replaceFirst = function(regex, replacement) {
      if (!is.string(regex) && !is.character(regex)) {
        stop('Argument `regex` must be of class string or character', call. = FALSE)
      }
      if (!is.string(replacement) || !is.character(replacement)) {
        stop('Argument `replacement` must be of class string or character', call. = FALSE)
      }

      regex <- if (is.string(regex)) regex$toString() else regex
      replacement <- if (is.string(replacement)) replacement$toString() else replacement

      string(sub(pattern = regex, replacement = replacement))
    },
    replaceAll = function(regex, replacement) {
      if (!is.string(regex) && !is.character(regex)) {
        stop('Argument `regex` must be of class string or character', call. = FALSE)
      }
      if (!is.string(replacement) && !is.character(replacement)) {
        stop('Argument `replacement` must be of class string or character', call. = FALSE)
      }

      regex <- if (is.string(regex)) regex$toString() else regex
      replacement <- if (is.string(replacement)) replacement$toString() else replacement

      string(gsub(pattern = regex, replacement = replacement))
    },
    split = function(regex, limit = NULL) {
      if (!is.string(regex) && !is.character(regex)) {
        stop('Argument `regex` must be of class string or character', call. = FALSE)
      }
      if (!is.null(limit) && !is_counting_number(limit)) {
        stop('If specified, argument `limit` must be a positive integer', call. = FALSE)
      }

      if (!grepl(pattern = regex, text = private$value)) {
        return(private$value)
      }

      splitted <- strsplit(private$value, split = regex)[[1]]

      if (is.null(limit) || limit >= length(splitted)) {
        splitted
      } else {
        # The nastiness below combines the excess elements past `limit`
        # generated by splitting private$value on `regex`.
        #
        # @TODO refactor this madness
        indeces <- gregexpr(pattern = regex, text = private$value)[[1]]
        splitters <- vapply(
          X = seq_along(indeces),
          FUN = function(i) self$substring(indeces[i], attr(indeces, 'match.length')[i]),
          VALUE = character(1)
        )

        splitted_limit <- limit:(length(splitted) - 1)
        splitters_limit <- limit:length(splitted)
        pasted <- paste(splitted[splitted_limit], splitters[splitters_limit], collapse = '', sep = '')
        pasted <- paste0(pasted, splitted[length(splitted)])

        c(splitted[1:limit], pasted)
      }
    },
    toLowerCase = function() {
      string(tolower(private$value))
    },
    toUpperCase = function() {
      string(toupper(private$value))
    },
    trim = function() {
      string(trim(private$value, which = 'both'))
    },
    toString = function() {
      private$value
    },
    toCharArray = function() {
      strsplit(private$value, split = '')[[1]]
    },
    length = function() {
      nchar(private$value)
    },
    isEmpty = function() {
      self$length() == 0
    }
  )
)

#' Print a String
#'
#' Print a string object.
#'
#' @param x An object of class string.
#'
#' @export
print.string <- function(x) {
  print(x$toString())
}

#' String Length
#'
#' The length of a string object.
#'
#' @param x An object of class string.
#'
#' @export
length.string <- function(x) {
  x$length()
}


