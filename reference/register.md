# Register a function that returns a ggplot object This is usually implemented by ggplot extensions.

Register a function that returns a ggplot object This is usually
implemented by ggplot extensions.

## Usage

``` r
register(f, name = NULL)
```

## Arguments

- f:

  A function that returns a ggplot object.

- name:

  A string to represent the name of the function.

## Value

A function that returns a ggplot object with logged calls.
