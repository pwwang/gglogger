# gglogger

`gglogger` is an R package that logs the calls used to create `ggplot2` objects.

This can be useful for debugging, reproducibility, and understanding the sequence of operations used to build a plot.

Have you ever created a plot in `ggplot2` and then forgotten how you made it? Or wanted to reproduce a plot but couldn't remember the exact sequence of operations you used? `gglogger` can help!

## Installation

You can install the `gglogger` package from source using `devtools`:

```r
install.packages("devtools")
devtools::install_github("pwwang/gglogger")

# or
remotes::install_github("pwwang/gglogger")
```

## Usage

To use the `gglogger` package, simply load it along with `ggplot2` and create your plots as usual. The package will automatically log the calls used to create the plots.

```r
library(ggplot2)
# Note: gglogger must be loaded after ggplot2
library(gglogger)
# Otherwise, you can use the gglogger::ggplot function to create plots

# Create a ggplot object and add a layer
p <- ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()

# Print the logs
print(p$logs)

## ggplot(mpg, aes(x = displ, y = hwy)) +
##   geom_point()
```

Evaluate the code in logs to reproduce the plot:

```r
p$logs$evaluate()
```

You can also attach the variables in an environment for evaluation:

```r
env <- new.env()
env$mpg <- mpg
env$mpg$hwy <- mpg$hwy / 2
p$logs$evaluate(env)
```

## Limitations

`gglogger` cannot log the global settings used to create a plot, such as `theme_set()`. It can only log the calls used directly to create the plot itself.