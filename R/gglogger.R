#' @import ggplot2
NULL

# Define a custom class for logs
GGLogs <- setRefClass(
  "GGLogs",
  fields = list(
    logs = "list"
  ),
  methods = list(
    add = function(log) {
      logs <<- c(logs, log)
    },
    evaluate = function(env = parent.frame()) {
      objs <- lapply(logs, function(log) log$evaluate(env))
      p <- Reduce(function(x, y) x + y, objs)
      p$logs <- NULL
      return(p)
    }
  )
)

# Define a custom class for log
GGLog <- setRefClass(
  "GGLog",
  fields = list(
    code = "character"
  ),
  methods = list(
    evaluate = function(env = parent.frame()) {
      eval(parse(text = code), envir = env)
    }
  )
)

#' Override ggplot function to log calls
#'
#' @param ... Arguments passed to ggplot2::ggplot.
#' @return A ggplot object with logged calls.
#' @export
ggplot <- function(...) {
  p <- ggplot2::ggplot(...)
  if (is.null(p$logs)) p$logs <- GGLogs$new(logs = list())
  log <- GGLog$new(code = deparse(substitute(ggplot(...))))
  p$logs$add(log)
  return(p)
}

#' Override + operator for ggplot objects to log calls
#'
#' @param e1 A ggplot object.
#' @param e2 A layer to add to the ggplot object.
#' @return A ggplot object with logged calls.
#' @export
`+.gg` <- function(e1, e2) {
  p <- ggplot2::`%+%`(e1, e2)
  if (!is.null(p$logs)) {
    log <- GGLog$new(code = deparse(substitute(e2)))
    p$logs$add(log)
  }
  return(p)
}

#' Print a GGLogs object
#'
#' @param x A GGLogs object.
#'
#' @export
print.GGLogs <- function(x, ...) {
  for (i in seq_along(x$logs)) {
    prefix <- ifelse(i == 1, "", "  ")
    suffix <- ifelse(i == length(x$logs), "", "+")
    cat(prefix, x$logs[[i]]$code, suffix, "\n")
  }
}

#' Print a GGLog object
#'
#' @param x A GGLog object.
#'
#' @export
print.GGLog <- function(x, ...) {
  cat(x$code, "\n")
}
