# Read a MET data file (CSV or Excel)

Read a MET data file (CSV or Excel)

## Usage

``` r
read_met_file(path, sep = ",", header = TRUE, filename = path)
```

## Arguments

- path:

  File path

- sep:

  CSV separator (ignored for Excel)

- header:

  Whether first row is header

- filename:

  Original filename, used to recognize uploaded Excel files

## Value

Data frame. Recognized spreadsheet error cells in otherwise numeric
columns become NA and are recorded in the import_issues attribute.
