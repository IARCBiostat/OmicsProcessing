# Filter Samples Based on Case Control Criteria

This function filters the samples in the provided data frame based on
case control criteria. It excludes individuals without a matched caseset
and those matched casesets with more than two individuals. It also
provides information on excluded samples.

## Usage

``` r
filter_case_control(df, df_meta_samples, col_case_control, col_samples)
```

## Arguments

- df:

  A data frame containing feature data where rows represent samples and
  columns represent features.

- df_meta_samples:

  A data frame containing sample metadata with at least the column
  specified by `col_case_control`.

- col_case_control:

  A string specifying the column name in `df_meta_samples` that
  indicates case control groups.

- col_samples:

  A string specifying the column name in `df_meta_samples` to use for
  sample filtering (e.g., ID column).

## Value

A list containing:

- `filtered_df`: The filtered feature data frame.

- `filtered_samples`: The filtered sample data frame.

- `excluded_ids`: A vector of IDs of excluded samples.
