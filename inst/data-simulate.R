# Generate example data for `process_data()`
rm(list=ls())
set.seed(821)

# data: feature data ====
n_rows <- 100
n_cols <- 101
# Generate random IDs
ids <- paste0("ID_", seq(1:n_rows))
# Generate random data for features
data_features <- matrix(runif(n_rows * (n_cols - 1)), nrow = n_rows)
# Create dataframe
data_features <- data.frame(ID_sample = ids, data_features)
colnames(data_features)[2:n_cols] <- paste0("feature_", 1:(n_cols - 1))
# Introduce missing values
# 5% missing for columns 3-92
for (i in 3:92) {
  num_missing <- floor(n_rows * 0.05)
  missing_indices <- sample(1:n_rows, num_missing)
  data_features[missing_indices, i] <- NA
}
# Specific missing percentages for columns 93-101
missing_percentages <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
for (i in 93:101) {
  num_missing <- floor(n_rows * missing_percentages[i - 92])
  missing_indices <- sample(1:n_rows, num_missing)
  data_features[missing_indices, i] <- NA
}

# data: feature meta-data ====
feature_names <- colnames(data_features)[2:n_cols]
missing_percentages_data_meta_features <- colMeans(is.na(data_features[, 2:n_cols]))
min_values_data_meta_features <- apply(data_features[, 2:n_cols], 2, function(x) min(x, na.rm = TRUE))
data_meta_features <- data.frame(
  ID_feature = feature_names,
  missing_pct = missing_percentages_data_meta_features,
  LOD = min_values_data_meta_features / 2
)
rownames(data_meta_features) <- NULL

# data: sample meta-data ====
batches <- paste0("batch_", sample(1:10, n_rows, replace = TRUE))
sex <- sample(1:2, n_rows, replace = TRUE)
age <- round(rnorm(n_rows, mean = 55, sd = 10)) # Generate ages with mean 55, sd 10.
data_meta_samples <- data.frame(
  ID_sample = ids,
  batch = batches,
  sex = sex,
  age = age
)

# sample missingness ====
target_rows <- 91:100
missing_percentages_rows <- seq(0.1, 1, by = 0.1)

for (i in 1:length(target_rows)) {
  row_index <- target_rows[i]
  missing_pct <- missing_percentages_rows[i]

  # Calculate number of missing values for this row
  num_missing <- floor((n_cols - 1) * missing_pct)  # Exclude the ID_sample column

  # Randomly select columns to set to NA (excluding the first column)
  missing_indices <- sample(2:n_cols, num_missing)
  data_features[row_index, missing_indices] <- NA
}

# make row 1 have 100% data using mean of column
for (j in 2:n_cols) { # Iterate over columns, starting from the second column
  if (is.na(data_features[1, j])) {
    column_mean <- mean(data_features[, j], na.rm = TRUE)  # Calculate column mean, excluding NAs
    data_features[1, j] <- column_mean  # Replace NA with column mean
  }
}

# check ====
missing_pct_row <- rowMeans(is.na(data_features[, 2:n_cols]))
missing_pct_col <- colMeans(is.na(data_features[, 2:n_cols]))

# write ====
save(data_features, file = "data/data_features.rda")
save(data_meta_features, file = "data/data_meta_features.rda")
save(data_meta_samples, file = "data/data_meta_samples.rda")
