# Load and translate SQL file or an explicit SQL query to desired dialect.

Load and translate SQL file or an explicit SQL query to desired dialect.

## Usage

``` r
loadRenderTranslateSql(
  sql,
  dbms = "postgresql",
  warnOnMissingParameters = TRUE,
  output = FALSE,
  outputFile,
  ...
)
```

## Arguments

- sql:

  SQL file name or SQL query

- warnOnMissingParameters:

  Should a warning be raised when parameters provided to this function
  do not appear in the parameterized SQL that is being rendered? By
  default, this is TRUE.

- output:

  Should there be a .sql file created of the result

- outputFile:

  Name of the output file
