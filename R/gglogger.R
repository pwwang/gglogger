#' @import ggplot2
NULL

#' @title GGLogs class
#' @description The `GGLogs` class is used to store logs of ggplot calls.
#'   Each log is stored as a `GGLog` object.
#'   The object can be accessed using the `$logs` field from a ggplot object.
#' @field logs A list to store the logs.
#' @export
GGLogs <- setRefClass(
  "GGLogs",

  fields = list(
    logs = "list"
  ),

  methods = list(

    add = function(log) {
        "Add a log to the list.\n
        @param log A GGLog object."
        logs <<- c(logs, log)
    },

    evaluate = function(env = parent.frame()) {
      "Evaluate all logs in the list.\n
      @param env The environment to evaluate the logs in."
      objs <- lapply(logs, function(log) log$evaluate(env))
      p <- Reduce(function(x, y) x + y, objs)
      p$logs <- .self
      return(p)
    },

    gen_code = function(setup = "library(ggplot2)") {
      "Generate code for all logs in the list.\n
      @param setup A string to setup the environment.\n
      @return A string of code."
      code <- paste(setup, collapse = "\n")
      code <- paste0(code, "\n")
      for (i in seq_along(logs)) {
        prefix <- ifelse(i == 1, "", "  ")
        suffix <- ifelse(i == length(logs), "", " +")
        code <- paste0(code, "\n", prefix, logs[[i]]$code, suffix)
      }
      code
    }
  )
)

#' @title GGLog class
#' @description The `GGLog` class is used to store a single ggplot call.
#' @field code A string to store the code of the ggplot call.
#' @export
GGLog <- setRefClass(
  "GGLog",
  fields = list(
    code = "character"
  ),
  methods = list(
    evaluate = function(env = parent.frame()) {
      "Evaluate the log.\n
      @param env The environment to evaluate the log in."
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

  if      (ggplot2::is.theme(e1))  p <- ggplot2:::add_theme(e1, e2, e2name)
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
