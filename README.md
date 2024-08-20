# gglogger <a href="https://pwwang.github.io/gglogger/"><img src="man/figures/logo.png" align="right" height="134" alt="gglogger website" /></a>

`gglogger` is an R package that logs the calls used to create `ggplot2` objects.

This can be useful for debugging, reproducibility, and understanding the sequence of operations used to build a plot.

> Have you ever created a plot in `ggplot2` and then forgotten how you made it? Or wanted to reproduce a plot but couldn't remember the exact sequence of operations you used? `gglogger` can help!

## Installation

You can install the `gglogger` package from source using `devtools`:

```r
install.packages("gglogger")

# or
devtools::install_github("pwwang/gglogger")

# or
remotes::install_github("pwwang/gglogger")
```

## Usage

To use the `gglogger` package, simply load it along with `ggplot2` and create your plots as usual. The package will automatically log the calls used to create the plots.

<table>
    <tr>
        <th>Previous</th>
        <th>Now</th>
    </tr>
    <tr>
        <td>


```r
library(ggplot2)

p <- ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point()
```

</td>
<td>


```r
library(ggplot2)
# Just add gglogger after ggplot2
library(gglogger)

p <- ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point()

# Print the logs
print(p$logs)
## ggplot2::ggplot(mpg, aes(x = displ, y = hwy)) +
##   geom_point()
```

</td>
</tr>
</table>

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

## Registering a function from a ggplot2 extension

```r
library(dplyr)
library(gglogger)

mtcars_radar <- mtcars %>%
  as_tibble(rownames = "group") %>%
  mutate_at(vars(-group), scales::rescale) %>%
  tail(4) %>%
  select(1:10)

ggradar <- register(ggradar::ggradar)

p <- ggradar(mtcars_radar, legend.position = "right")
print(p$logs)

# ggradar::ggradar(mtcars_radar, legend.position = "right")
```

## Generating code to reproduce a plot

```r
# p is a ggradar plot created in the previous example

code <- p$logs$gen_code(setup = '
library(dplyr)
library(ggradar)

mtcars_radar <- mtcars %>%
  as_tibble(rownames = "group") %>%
  mutate_at(vars(-group), scales::rescale) %>%
  tail(4) %>%
  select(1:10)
')

cat(code)
## library(dplyr)
## library(ggradar)
##
## mtcars_radar <- mtcars %>%
##   as_tibble(rownames = "group") %>%
##   mutate_at(vars(-group), scales::rescale) %>%
##   tail(4) %>%
##   select(1:10)
##
##
## ggradar::ggradar(mtcars_radar, legend.position = "right")

# eval(parse(text = code)) # to reproduce the plot
```

## Limitations

`gglogger` cannot log the global settings used to create a plot, such as `theme_set()`. It can only log the calls used directly to create the plot itself. You may need to manually set these global settings when reproducing a plot, or prepare them using the `setup` argument in `gen_code()`.
