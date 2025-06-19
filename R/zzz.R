.onLoad <- function(libname, pkgname) {
    # override ggplot2's `+` operator for ggplot objects
    if (utils::packageVersion("ggplot2") > "3.5.2") {
        class_S3_gg <- utils::getFromNamespace("class_S3_gg", "ggplot2")
        class_ggplot <- utils::getFromNamespace("class_ggplot", "ggplot2")
        class_theme <- utils::getFromNamespace("class_theme", "ggplot2")
        if (getRversion() < "4.3.0") {
            suppressMessages(
                S7::method(`+`, list(class_S3_gg, S7::class_any)) <- .add_gg
            )
        }

        suppressMessages(
            S7::method(`+`, list(class_ggplot, S7::class_any)) <- function(e1, e2) {
                e2name <- deparse(substitute(e2, env = rlang::caller_env(2)))
                add_ggplot <- utils::getFromNamespace("add_ggplot", "ggplot2")
                p <- add_ggplot(e1, e2, e2name)
                if (!is.null(e1$logs)) {
                    p$logs <- e1$logs
                    log <- GGLog$new(code = e2name)
                    p$logs$add(log)
                }
                return(p)
            }
        )

        suppressMessages(
            S7::method(`+`, list(class_theme, S7::class_any)) <- function(e1, e2) {
                e2name <- deparse(substitute(e2, env = rlang::caller_env(2)))
                add_theme <- utils::getFromNamespace("add_theme", "ggplot2")
                p <- add_theme(e1, e2, e2name)
                if (!is.null(e1$logs)) {
                    p$logs <- e1$logs
                    log <- GGLog$new(code = e2name)
                    p$logs$add(log)
                }
                return(p)
            }
        )
    }
}

.onAttach <- function(libname, pkgname) {
    if (utils::packageVersion("ggplot2") > "3.5.2") {
        packageStartupMessage("Overwriting method +(<ggplot2::ggplot/ggplot2::theme>, <ANY>)")
    }
}
