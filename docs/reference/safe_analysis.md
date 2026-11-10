# Execute an expression with tryCatch, showing Shiny notifications on error/warning

Execute an expression with tryCatch, showing Shiny notifications on
error/warning

## Usage

``` r
safe_analysis(expr, session)
```

## Arguments

- expr:

  Expression to evaluate

- session:

  Shiny session object

## Value

Result of expr, or NULL on error
