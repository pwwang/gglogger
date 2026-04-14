# GGLogs class

The \`GGLogs\` class is used to store logs of ggplot calls. Each log is
stored as a \`GGLog\` object. The object can be accessed using the
\`\$logs\` field from a ggplot object.

## Fields

- `logs`:

  A list to store the logs.

## Methods

- `add(log)`:

  Add a log to the list.

  @param log A GGLog object.

- `evaluate(envir = parent.frame())`:

  Evaluate all logs in the list.

  @param envir The environment to evaluate the logs in.

- `gen_code(setup = "library(ggplot2)")`:

  Generate code for all logs in the list.

  @param setup A string to setup the environment.

  @return A string of code.

- `stringify()`:

  Stringify all logs in the list.

  @return A string.
