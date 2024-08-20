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
      p$logs <- .self
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
  if (missing(e2)) {
    cli::cli_abort(c(
            "Cannot use {.code +} with a single argument.",
      "i" = "Did you accidentally put {.code +} on a new line?"
    ))
  }

  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  e2name <- deparse(substitute(e2))

  if      (ggplot2::is.theme(e1))  p <- ggplot2::add_theme(e1, e2, e2name)
  else if (ggplot2::is.ggplot(e1)) p <- ggplot2:::add_ggplot(e1, e2, e2name)
  else if (ggplot2::is.ggproto(e1)) {
    cli::cli_abort(c(
      "Cannot add {.cls ggproto} objects together.",
      "i" = "Did you forget to add this object to a {.cls ggplot} object?"
    ))
  }
  if (!is.null(e1$logs)) {
    p$logs <- e1$logs
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
