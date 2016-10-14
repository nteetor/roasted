#' String
#'
#' Initialize a newly created String object so that it represents an empty
#' character sequence.
#'
#' @param value The initial value of the string.
#' @param offset The initial offset.
#' @param count The length.
#'
#' @export
String <- function(value = NULL, offset = 1, count = length(value)) {
  if (!is.null(value)) {
    str <- paste(value[offset:count], sep = '', collapse = '')
  }
  string$new(str)
}

string <- R6::R6Class(
  classname = 'String',
  public = list(
    .value = NULL,
    initialize = function(x) {
      self$.value <- x
      self
    },
    length = function() {
      nchar(self$.value)
    },
    isEmpty = function() {
      nchar(self$.value) == 0
    },
    charAt = function(index) {
      assert_that(is_counting_number(index))
      if (index > nchar(self$.value)) {
        stop('`index` out of bounds', call. = FALSE)
      }
      substr(self$.value, start = index, stop = index)
    },
    getChars = function(begin, end) {
      assert_that(
        is_counting_number(begin),
        is_counting_number(end)
      )
      if (begin < 1 || end > nchar(self$.value)) {
        stop('`index` out of bounds', call. = FALSE)
      }
      substr(self$.value, start = begin, stop = end)
    },
    equals = function(object) {
      !is.null(object) &&
        (class(object) == 'String') &&
        self$compareTo(object)
    },
    contentEquals = function(object) {
      self$equals(String(object))
    },
    equalsIgnoreCase = function(another) {
      class(another) == 'String' &&
        self$length() == another$length() &&
        all(
          vapply(
            X = seq_len(self$length()),
            FUN = function(i) {
              c1 <- self$getChar(i)
              c2 <- self$getChar(i)
              c1 == c2 || toupper(c1) == toupper(c2) || tolower(c1) == tolower(c2)
            },
            FUN.VALUE = logical(1),
            USE.NAMES = FALSE
          )
        )
    },
    compareTo = function(another) {
      if (class(another) != 'String') return(FALSE)

      if (another$length() > self$length()) {
        k <- self$length()
      } else if (another$length() < self$length()) {
        k <- another$length()
      } else {             # lengths equal
        k <- self$length() # could also be another$length()
      }

      for (i in seq_len(k)) {
        diff <- utf8ToInt(self$getChar(i)) - utf8ToInt(another$getChar(i))
        if (diff != 0) {
          return(diff)
        }
      }

      self$length() - another$length()
    },
    compareToIgnoreCase = function(another) {
      if (class(another) != 'String') return(FALSE)

      String(toupper(tolower(self$.value)))$compareTo(
        String(toupper(tolower(another$.value)))
      )
    },
    regionMatches = function(toffset, other, ooffset, len, ignoreCase = FALSE) {
      if (toffset < 0 || ooffset < 0) {
        return(FALSE)
      }
      if (toffset + len > self$length() || ooffset + len > other$length()) {
        return(FALSE)
      }

      for (i in seq.int(0, len, by = 1)) {
        c1 <- self$getChar(toffset + i)
        c2 <- self$getChar(ooffset + i)

        if (ignoreCase) {
          if (tolower(c1) != tolower(c2) || toupper(c1) != toupper(c2)) {
            return(FALSE)
          }
        } else {
          if (c1 != c2) {
            return(FALSE)
          }
        }
      }

      TRUE
    },
    startsWith = function(prefix, toffset = 1) {
      if (class(prefix) == 'String' || is.character(prefix)) {
        stop('`prefix` must be of class String or character', call. = FALSE)
      }

      if (prefix == '' || self$equals(prefix)) return(TRUE)

      self$regionMatches(toffset, String(prefix), 1, length(prefix))
    },
    endsWith = function(suffix) {
      if (class(prefix) == 'String' || is.character(prefix)) {
        stop('`prefix` must be of class String or character', call. = FALSE)
      }

      if (suffix == '' || self$equals(suffix)) return(TRUE)

      self$regionMatches(self$length() - length(suffix),
                         String(suffix), 1, lenth(suffix))
    },
    hashCode = function() {
      if (self$.value == '') return(0)

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
    indexOf = function(ch, fromIndex = 1) {
      if (!is.character(ch)) {
        stop('`ch` must be a single character')
      }

      index <- which(strsplit(self$.value, '')[[1]] == ch)[1]

      if (is.na(index) || index < fromIndex) -1 else index
    },
    lastIndexOf = function(ch, fromIndex = Inf) {
      if (!is.character(ch)) {
        stop('`ch` must be a single character')
      }

      indeces <- which(strsplit(self$.value, '')[[1]] == ch)
      index <- indeces[length(indeces)]

      if (is.na(index) || index > fromIndex) -1 else index
    },
    substring = function(beginIndex, endIndex) {
      if (beginIndex < -1 ) {
        stop('`beginIndex` out of bounds')
      }
      if (endIndex > nchar(self$.value)) {
        stop('`endIndex` out of bounds')
      }

      String(substr(self$.value, start = beginIndex, stop = endIndex))
    },
    concat = function(str) {
      String(paste0(self$.value, str))
    },
    replace = function(oldChar, newChar) {
      if (!is.character(oldChar) || nchar(oldChar) > 1) {
        stop('`oldChar` must be a single character')
      }
      if (!is.character(newChar) || nchar(newChar) > 1) {
        stop('`newChar` must be a single character')
      }
      splits <- strsplit(self$.value, '')[[1]]
      splits[which(splits == oldChar)] <- newChar

      String(paste(splits, sep = '', collapse = ''))
    },
    matches = function(regex) {
      if (!is.character(regex)) {
        stop('`regex` must be of class character')
      }

      grepl(regex, self$.value)
    },
    contains = function(s) {
      if (!is.character(s)) {
        stop('`s` must be of class character')
      }
      grepl(s, self$.value, fixed = TRUE)
    },
    toString = function() {
      self$.value
    }
  )
)

#' Print a String
#'
#' Print a String object.
#'
#' @param x An object of class String.
#'
#' @export
print.String <- function(x) {
  print(x$toString())
}

#' String Length
#'
#' The length of a String object.
#'
#' @param x An object of class String.
#'
#' @export
length.String <- function(x) {
  x$length()
}
