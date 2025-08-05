test_that("normalise_SERRF_by_batch handles valid input", {
  set.seed(123)
  df <- data.frame(
    Feature1 = rnorm(12),
    Feature2 = rnorm(12),
    Feature3 = rnorm(12),
    Samplename = paste0("Sample", 1:12),
    batch = factor(rep(c("A", "B"), each = 6))
  )
  is_qc <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)

  result <- normalise_SERRF_by_batch(
    df = df,
    target_cols = c("Feature1", "Feature2", "Feature3"),
    is_qc = is_qc,
    batch_col = "batch",
    num_screen_SERRF = 2
  )

  expect_equal(dim(result), dim(df))
  expect_true(all(c("Feature1", "Feature2", "Feature3") %in% colnames(result)))
  expect_false(any(is.na(result[, c("Feature1", "Feature2", "Feature3")])))
})

test_that("normalise_SERRF_by_batch errors on non-factor batch column", {
  df <- data.frame(
    Feature1 = rnorm(4),
    batch = c("A", "A", "B", "B")
  )
  expect_error(
    normalise_SERRF_by_batch(df, target_cols = "Feature1", is_qc = c(TRUE, TRUE, FALSE, FALSE), batch_col = "batch"),
    "must be a factor"
  )
})

test_that("normalise_SERRF_by_batch errors on NA in target features", {
  df <- data.frame(
    Feature1 = c(1, NA, 3, 4),
    batch = factor(c("A", "A", "B", "B"))
  )
  expect_error(
    normalise_SERRF_by_batch(df, target_cols = "Feature1", is_qc = c(TRUE, TRUE, FALSE, FALSE), batch_col = "batch"),
    "contain NA values"
  )
})

test_that("normalise_SERRF_by_batch errors on non-numeric target features", {
  df <- data.frame(
    Feature1 = as.character(c(1, 2, 3, 4)),
    batch = factor(c("A", "A", "B", "B"))
  )
  expect_error(
    normalise_SERRF_by_batch(df, target_cols = "Feature1", is_qc = c(TRUE, TRUE, FALSE, FALSE), batch_col = "batch"),
    "must be numeric"
  )
})

test_that("normalise_SERRF_by_batch normalises synthetic data with drift", {
  set.seed(42)

  make_test_df <- function() {
    n_per_batch <- 10  # 6 samples + 4 QCs per batch
    batches <- c("Batch1", "Batch2")

    df <- do.call(rbind, lapply(batches, function(b) {
      qc <- data.frame(
        Feature1 = rnorm(4, mean = 10),
        Feature2 = rnorm(4, mean = 50),
        Feature3 = rnorm(4, mean = 100),
        Samplename = paste0("QC_", seq_len(4), "_", b),
        batch = b,
        is_qc = TRUE
      )

      drift <- seq(0, 1, length.out = 6)
      sample <- data.frame(
        Feature1 = rnorm(6, mean = 10) + drift * 3,
        Feature2 = rnorm(6, mean = 50) + drift * 5,
        Feature3 = rnorm(6, mean = 100) + drift * 10,
        Samplename = paste0("Sample_", seq_len(6), "_", b),
        batch = b,
        is_qc = FALSE
      )

      rbind(qc, sample)
    }))

    df$batch <- factor(df$batch)
    df <- df[order(df$batch, df$is_qc), ]
    rownames(df) <- NULL
    return(df)
  }

  df_test <- make_test_df()

  result <- normalise_SERRF_by_batch(
    df = df_test,
    target_cols = c("Feature1", "Feature2", "Feature3"),
    is_qc = df_test$is_qc,
    batch_col = "batch",
    num_screen_SERRF = 2
  )

  expect_equal(dim(result), dim(df_test))
  expect_true(all(c("Feature1", "Feature2", "Feature3") %in% colnames(result)))
  expect_false(any(is.na(result[, c("Feature1", "Feature2", "Feature3")])))
  expect_equal(unique(result$batch), unique(df_test$batch))
})



#' Normalization to eliminate unwanted variation using the SERRF approach: Fan 2019, Anal Chem
#' (adaption of the script available at https://raw.githubusercontent.com/slfan2013/SERRF-online/master/backup%20js/normalization.R)
#'
#' @param DataImputed: a tibble, as returned by the function_impute_knn_HT;
#' In particular, this tibble should have variables batchID, Samplename, and all imputed features (whose names are NamesFeatureAll)
#' @param NamesFeatureAll: a character vector containing the names of the features (that correspond to most columns of the DataImputed tibble)
#'
#' @return a tibble with normalized features
#' @export
#' @import tidyverse
#' @import ranger
#'
#' @examples

normalization_SERRF_batch <- function(DataImputed, NamesFeatureAll)
{
  
  num_screen_SERRF         <- 10
  NamesFeatureKept         <- colnames(DataImputed)[colnames(DataImputed)%in% NamesFeatureAll]
  NbBatch                  <- length(levels(DataImputed$batchID))
  Feature_Normalized_serrf <- DataImputed
  allQCinit                <- DataImputed %>% filter(grepl('QC', Samplename))  %>% dplyr::select(all_of(NamesFeatureKept))  %>% as.matrix
  allSamplesinit           <- DataImputed %>% filter(grepl('Sample', Samplename))  %>% dplyr::select(all_of(NamesFeatureKept))  %>% as.matrix
  
  
  # Computations of the correlation matrices (for the screening step)
  
  corrs_train = list()
  corrs_target = list()
  for(b in 1:NbBatch){
    
    current_batch = levels(DataImputed$batchID)[b]
    train         = DataImputed %>% filter(batchID == current_batch & grepl('QC', Samplename))  %>% dplyr::select(all_of(NamesFeatureKept))  %>% as.matrix
    target        = DataImputed %>% filter(batchID == current_batch & grepl('Sample', Samplename))  %>% dplyr::select(all_of(NamesFeatureKept))  %>% as.matrix
    corrs_train[[current_batch]] = cor(train, method = "spearman")
    corrs_target[[current_batch]] = cor(target, method = "spearman")
  } #whithin each Batch, correlation between features among QC, among samples 
  
  
  for (j in 1:length(NamesFeatureKept))
  {
    if (j %% 250 == 1) {cat(j, "\n")}
    for(b in 1:NbBatch){
      # cat(b,"`\n")
      current_batch   = levels(DataImputed$batchID)[b]
      e_current_batch = DataImputed %>% filter(batchID == current_batch)  %>% dplyr::select(all_of(NamesFeatureKept))  %>% as.matrix
      train           = DataImputed %>% filter(batchID == current_batch & grepl('QC', Samplename))  %>% dplyr::select(all_of(NamesFeatureKept))
      target          = DataImputed %>% filter(batchID == current_batch & grepl('Sample', Samplename))  %>% dplyr::select(all_of(NamesFeatureKept))
      
      train.index_current_batch = DataImputed %>% filter(batchID == current_batch) %>% dplyr::select(Samplename) %>% as.data.frame
      train.index_current_batch = as.character(train.index_current_batch$Samplename)
      corr_train = corrs_train[[current_batch]]
      corr_target = corrs_target[[current_batch]]
      
      corr_train_order = order(abs(corr_train[,j]),decreasing = TRUE)
      corr_target_order = order(abs(corr_target[,j]),decreasing = TRUE)
      
      sel_var = c()
      l = num_screen_SERRF
      while(length(sel_var)<(num_screen_SERRF)){
        sel_var = intersect(corr_train_order[1:l], corr_target_order[1:l])
        sel_var = sel_var[!sel_var == j]
        l = l+1
      }
      
      # forillust <- preparedata_forgraph_forIllust_SERRF(DataImputed %>% filter(Batch == current_batch), NamesFeatureKept[c(j, sel_var)])
      # ggplot(data=forillust, aes(x=RunOrder, y=Measure, size=SampleType, shape=SampleType, alpha=SampleType))+
      #   geom_point() + scale_size_manual(values=c(2,1))+
      #   scale_alpha_manual(values=c(1,0.3)) + facet_wrap(~ Feature, scales = "free", ncol=3)
      #
      
      train_data_y = train  %>% dplyr::select(all_of(NamesFeatureKept[j])) %>% scale(., scale=F)
      train_data_x = train  %>% dplyr::select(all_of(NamesFeatureKept[sel_var])) %>% scale
      test_data_x =  target %>% dplyr::select(all_of(NamesFeatureKept[sel_var])) %>% scale
      train_data = data.frame(y = train_data_y,train_data_x )
      colnames(train_data) = c("y", paste0("V",1:(ncol(train_data)-1)))
      model = ranger(y~., data = train_data)
      
      test_data = data.frame(test_data_x)
      colnames(test_data) = colnames(train_data)[-1]
      
      norm = e_current_batch[,j]
      #norm[grepl('QC', train.index_current_batch)] = e_current_batch[grepl('QC', train.index_current_batch), j]/((predict(model, data = train_data)$prediction+mean(e_current_batch[grepl('QC', train.index_current_batch), j],na.rm=TRUE))/mean(e_current_batch[grepl('QC', train.index_current_batch), j],na.rm=TRUE))
      norm[grepl('QC', train.index_current_batch)] = e_current_batch[grepl('QC', train.index_current_batch), j]/((model$predictions + mean(e_current_batch[grepl('QC', train.index_current_batch), j],na.rm=TRUE))/mean(e_current_batch[grepl('QC', train.index_current_batch), j],na.rm=TRUE))
      norm[grepl('Sample', train.index_current_batch)] =(e_current_batch[grepl('Sample', train.index_current_batch), j])/((predict(model,data = test_data)$predictions  + mean(e_current_batch[grepl('Sample', train.index_current_batch), j],na.rm=TRUE))/(median(e_current_batch[grepl('Sample', train.index_current_batch), j],na.rm = TRUE)))
      # plot(p$time[!train.index_current_batch=='qc'], (e_current_batch[j,!train.index_current_batch=='qc'])/((predict(model,data = test_data)$predictions  + mean(e_current_batch[j, !train.index_current_batch=='qc'],na.rm=TRUE))/(median(e_current_batch[j,!train.index_current_batch=='qc'],na.rm = TRUE))))
      norm[grepl('QC', train.index_current_batch)] = norm[grepl('QC', train.index_current_batch)]/(median(norm[grepl('QC', train.index_current_batch)],na.rm=TRUE)/median(allQCinit[,j],na.rm=TRUE))
      norm[grepl('Sample', train.index_current_batch)] = norm[grepl('Sample', train.index_current_batch)]/(median(norm[grepl('Sample', train.index_current_batch)],na.rm=TRUE)/median(allSamplesinit[,j],na.rm=TRUE))
      norm[!is.finite(norm)] = rnorm(length(norm[!is.finite(norm)]),sd = sd(norm[is.finite(norm)],na.rm=TRUE)*0.01)
      Feature_Normalized_serrf[Feature_Normalized_serrf$batchID == current_batch, NamesFeatureKept[j]] = norm
    }
    
    featj <- Feature_Normalized_serrf[, NamesFeatureKept[j]]
    indQC <- which(grepl("QC", DataImputed$Samplename))
    normed_target <- as.numeric(as.matrix(featj[-c(indQC),1]))
    normed_target[is.na(normed_target)] = rnorm(sum(is.na(normed_target)), mean = min(normed_target[!is.na(normed_target)], na.rm = TRUE), sd = sd(normed_target[!is.na(normed_target)])*0.1)
    normed_target[normed_target<0] = runif(1) * min(normed_target[normed_target>0], na.rm = TRUE)
    normed_train <- as.numeric(as.matrix(featj[indQC,1]))
    normed_train[is.na(normed_train)] = rnorm(sum(is.na(normed_train)), mean = min(normed_train[!is.na(normed_train)], na.rm = TRUE), sd = sd(normed_train[!is.na(normed_train)])*0.1)
    normed_train[normed_train<0] = runif(1) * min(normed_train[normed_train>0], na.rm = TRUE)
    featj[indQC,1] <- normed_train
    featj[-indQC,1] <- normed_target
    Feature_Normalized_serrf[, NamesFeatureKept[j]] <- featj
    
  }
  return(Feature_Normalized_serrf)
}
