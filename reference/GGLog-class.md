# GGLog class

The \`GGLog\` class is used to store a single ggplot call.

## Fields

- `code`:

  A string to store the code of the ggplot call.

## Methods

- `evaluate(envir = parent.frame())`:

  Evaluate the log.

  @param envir The environment to evaluate the log in.

- `stringify()`:

  Stringify the log.

  @return A string.
