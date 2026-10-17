.add_ggplot <- function(e1, e2) {
    e2name <- deparse(if (utils::packageVersion("S7") > "0.2.2") {
        substitute(e2)
    } else {
        substitute(e2, env = rlang::caller_env(2))
    })
    add_ggplot <- utils::getFromNamespace("add_ggplot", "ggplot2")
    p <- add_ggplot(e1, e2, e2name)
    if (!is.null(e1$logs)) {
        p$logs <- e1$logs
        log <- GGLog$new(code = e2name)
        p$logs$add(log)
    }
    p
}

.add_theme <- function(e1, e2) {
    e2name <- deparse(if (utils::packageVersion("S7") > "0.2.2") {
        substitute(e2)
    } else {
        substitute(e2, env = rlang::caller_env(2))
    })
    add_theme <- utils::getFromNamespace("add_theme", "ggplot2")
    p <- add_theme(e1, e2, e2name)
    if (!is.null(e1$logs)) {
        p$logs <- e1$logs
        log <- GGLog$new(code = e2name)
        p$logs$add(log)
    }
    p
}

# Record whether this installation could declare methods using external classes.
.S7_methods_declared <- requireNamespace("S7", quietly = TRUE) &&
    "new_external_class" %in% getNamespaceExports("S7")

if (.S7_methods_declared) {
    class_ggplot <- getExportedValue("S7", "new_external_class")("ggplot2", "class_ggplot", version = "4.0.0")
    class_theme <- getExportedValue("S7", "new_external_class")("ggplot2", "class_theme", version = "4.0.0")
    S7::method(`+`, list(class_ggplot, S7::class_any)) <- .add_ggplot
    S7::method(`+`, list(class_theme, S7::class_any)) <- .add_theme
}

.onLoad <- function(libname, pkgname) {
    # override ggplot2's `+` operator for ggplot objects
    if (utils::packageVersion("ggplot2") > "3.5.2") {
        has_S7_hooks <- "S7_on_load" %in% getNamespaceExports("S7")
        if (getRversion() < "4.3.0") {
            class_S3_gg <- utils::getFromNamespace("class_S3_gg", "ggplot2")
            suppressMessages(
                S7::method(`+`, list(class_S3_gg, S7::class_any)) <- .add_gg
            )
        }

        # Older S7 cannot resolve external classes, including in an installed
        # package built with newer S7. Register its methods directly instead.
        if (!.S7_methods_declared || !has_S7_hooks) {
            class_ggplot <- utils::getFromNamespace("class_ggplot", "ggplot2")
            class_theme <- utils::getFromNamespace("class_theme", "ggplot2")
            suppressMessages(
                S7::method(`+`, list(class_ggplot, S7::class_any)) <- .add_ggplot
            )
            suppressMessages(
                S7::method(`+`, list(class_theme, S7::class_any)) <- .add_theme
            )
        }
        if (has_S7_hooks) {
            suppressMessages(getExportedValue("S7", "S7_on_load")())
        }
    }
}

.onAttach <- function(libname, pkgname) {
    if (utils::packageVersion("ggplot2") > "3.5.2") {
        packageStartupMessage("Overwriting method +(<ggplot2::ggplot/ggplot2::theme>, <ANY>)")
    }
}

.onUnload <- function(...) {
    if (utils::packageVersion("ggplot2") > "3.5.2" &&
        "S7_on_unload" %in% getNamespaceExports("S7")) {
        getExportedValue("S7", "S7_on_unload")()
    }
}

if (.S7_methods_declared) {
    getExportedValue("S7", "S7_on_build")()
}
