# Simulated Dataframe 1

A simulated dataframe with 100 rows and 101 columns. Col1 is an ID
column. The remaining columns are features with randomly assigned
values. Once values were assigned, feature missingness was introduced (5
percent) randomly for feature_2-feature_92. feature_93-feature_100
contains missing at 10, 20, 30, 40, 50, 60, 70, 80, and 90 perceent,
respectively. Sample missingness was then intriduced for ID_90-ID_100 at
10, 20, 30, 40, 50, 60, 70, 80, 90, and 100 perceent, respectively.
Missing values for ID_1 were then replaced with the mean of the feature
to give ID_1 no missingness

## Usage

``` r
data(data_features)
```

## Format

A data frame with 100 rows and 101 variables:
