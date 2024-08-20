test_that("gglogger works with ggplot only", {
    p <- ggplot()
    expect_is(p, "gg")
    expect_is(p$logs, "GGLogs")
    expect_equal(length(p$logs$logs), 1)
    expect_equal(p$logs$logs[[1]]$code, "ggplot2::ggplot()")
})

test_that("gglogger works with ggplot and layers", {
    p <- ggplot() + geom_point()
    expect_is(p, "gg")
    expect_is(p$logs, "GGLogs")
    expect_equal(length(p$logs$logs), 2)
    expect_equal(p$logs$logs[[1]]$code, "ggplot2::ggplot()")
    expect_equal(p$logs$logs[[2]]$code, "geom_point()")
})

test_that("gglogger works with ggplot and layers and themes", {
    p <- ggplot() + geom_point() + theme_minimal()
    expect_is(p, "gg")
    expect_is(p$logs, "GGLogs")
    expect_equal(length(p$logs$logs), 3)
    expect_equal(p$logs$logs[[1]]$code, "ggplot2::ggplot()")
    expect_equal(p$logs$logs[[2]]$code, "geom_point()")
    expect_equal(p$logs$logs[[3]]$code, "theme_minimal()")
})

test_that("gglogger works with ggplot and layers and themes and facets", {
    p <- ggplot() + geom_point() + theme_minimal() + facet_wrap(~cyl)
    expect_is(p, "gg")
    expect_is(p$logs, "GGLogs")
    expect_equal(length(p$logs$logs), 4)
    expect_equal(p$logs$logs[[1]]$code, "ggplot2::ggplot()")
    expect_equal(p$logs$logs[[2]]$code, "geom_point()")
    expect_equal(p$logs$logs[[3]]$code, "theme_minimal()")
    expect_equal(p$logs$logs[[4]]$code, "facet_wrap(~cyl)")
})

test_that("gglogger works with ggplot and layers and themes and facets and scales", {
    p <- ggplot() + geom_point() + theme_minimal() + facet_wrap(~cyl) + scale_color_manual(values = c("red", "blue"))
    expect_is(p, "gg")
    expect_is(p$logs, "GGLogs")
    expect_equal(length(p$logs$logs), 5)
    expect_equal(p$logs$logs[[1]]$code, "ggplot2::ggplot()")
    expect_equal(p$logs$logs[[2]]$code, "geom_point()")
    expect_equal(p$logs$logs[[3]]$code, "theme_minimal()")
    expect_equal(p$logs$logs[[4]]$code, "facet_wrap(~cyl)")
    expect_equal(p$logs$logs[[5]]$code, "scale_color_manual(values = c(\"red\", \"blue\"))")
})

test_that("gglogger works with ggplot and layers and themes and facets and scales and coordinates", {
    p <- ggplot() + geom_point() + theme_minimal() + facet_wrap(~cyl) + scale_color_manual(values = c("red", "blue")) + coord_flip()
    expect_is(p, "gg")
    expect_is(p$logs, "GGLogs")
    expect_equal(length(p$logs$logs), 6)
    expect_equal(p$logs$logs[[1]]$code, "ggplot2::ggplot()")
    expect_equal(p$logs$logs[[2]]$code, "geom_point()")
    expect_equal(p$logs$logs[[3]]$code, "theme_minimal()")
    expect_equal(p$logs$logs[[4]]$code, "facet_wrap(~cyl)")
    expect_equal(p$logs$logs[[5]]$code, "scale_color_manual(values = c(\"red\", \"blue\"))")
    expect_equal(p$logs$logs[[6]]$code, "coord_flip()")
})

test_that("gglogger works with ggplot and layers and themes and facets and scales and coordinates and stats", {
    p <- ggplot() + geom_point() + theme_minimal() + facet_wrap(~cyl) + scale_color_manual(values = c("red", "blue")) + coord_flip() + stat_smooth()
    expect_is(p, "gg")
    expect_is(p$logs, "GGLogs")
    expect_equal(length(p$logs$logs), 7)
    expect_equal(p$logs$logs[[1]]$code, "ggplot2::ggplot()")
    expect_equal(p$logs$logs[[2]]$code, "geom_point()")
    expect_equal(p$logs$logs[[3]]$code, "theme_minimal()")
    expect_equal(p$logs$logs[[4]]$code, "facet_wrap(~cyl)")
    expect_equal(p$logs$logs[[5]]$code, "scale_color_manual(values = c(\"red\", \"blue\"))")
    expect_equal(p$logs$logs[[6]]$code, "coord_flip()")
    expect_equal(p$logs$logs[[7]]$code, "stat_smooth()")
})

# test evaluate
test_that("gglogger evaluate works", {
    p <- ggplot(ggplot2::mpg) + geom_point(aes(x = displ, y = hwy))
    q <- p$logs$evaluate()
    expect_is(q, "gg")
    expect_is(q$data, "data.frame")
    expect_equal(nrow(q$data), 234)
    expect_equal(ncol(q$data), 11)
    expect_equal(q$data$displ[1], 1.8)
    expect_equal(q$data$hwy[1], 29)
    expect_is(q$layers[[1]]$geom, "GeomPoint")
    expect_equal(rlang::as_label(q$layers[[1]]$mapping$x), "displ")
    expect_equal(rlang::as_label(q$layers[[1]]$mapping$y), "hwy")
})

# test evaluate with custom environment
test_that("gglogger evaluate works with custom environment", {
    mpg <- ggplot2::mpg
    p <- ggplot(mpg) + geom_point(aes(x = displ, y = hwy))
    env <- new.env()
    env$mpg <- mpg
    env$mpg$displ[1] <- 1.9
    env$mpg$hwy[1] <- 30
    q <- p$logs$evaluate(env)
    expect_is(q, "gg")
    expect_is(q$data, "data.frame")
    expect_equal(nrow(q$data), 234)
    expect_equal(ncol(q$data), 11)
    expect_equal(q$data$displ[1], 1.9)
    expect_equal(q$data$hwy[1], 30)
    expect_is(q$layers[[1]]$geom, "GeomPoint")
    expect_equal(rlang::as_label(q$layers[[1]]$mapping$x), "displ")
    expect_equal(rlang::as_label(q$layers[[1]]$mapping$y), "hwy")
})

# test gen_code
test_that("gglogger gen_code works", {
    p <- ggplot(ggplot2::mpg) + geom_point(aes(x = displ, y = hwy))
    code <- p$logs$gen_code()
    expect_equal(code, "library(ggplot2)\n\nggplot2::ggplot(ggplot2::mpg) +\n  geom_point(aes(x = displ, y = hwy))\n")
})

# test stringify
test_that("gglogger stringify works", {
    p <- ggplot(ggplot2::mpg) + geom_point(aes(x = displ, y = hwy))
    code <- p$logs$stringify()
    expect_equal(code, "ggplot2::ggplot(ggplot2::mpg) +\n  geom_point(aes(x = displ, y = hwy))")
})

# test register
test_that("gglogger register works", {
    myggplot <- function(data) {
        ggplot2::ggplot(data) + geom_point(aes(x = displ, y = hwy))
    }
    myggplot2 <- gglogger::register(myggplot)
    p <- myggplot2(ggplot2::mpg)
    expect_is(p, "gg")
    expect_is(p$data, "data.frame")
    expect_equal(p$logs$stringify(), "myggplot(ggplot2::mpg)")
})
