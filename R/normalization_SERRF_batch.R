normalise <- function(
    df,
    target_cols = NULL,
    is_qc = NULL,
    batch_col) {
    num_screen_SERRF <- 10
    set.seed(0)
    # NamesFeatureKept <- colnames(df)[colnames(df) %in% NamesFeatureAll]
    NamesFeatureKept <- resolve_target_cols(df, target_cols)
    NbBatch <- length(levels(df[[batch_col]]))
    
    df$is_qc <- is_qc

    allQCinit <- df %>%
        dplyr::filter(is_qc) %>%
        dplyr::select(dplyr::all_of(NamesFeatureKept)) %>%
        as.matrix()
    Feature_Normalized_serrf <- df
    allSamplesinit <- df %>%
        dplyr::filter(!is_qc) %>%
        dplyr::select(dplyr::all_of(NamesFeatureKept)) %>%
        as.matrix()

    # Computations of the correlation matrices (for the screening step)

    corrs_train <- list()
    corrs_target <- list()
    for (b in 1:NbBatch) {
        current_batch <- levels(df[[batch_col]])[b]
        train <- df %>%
            dplyr::filter(!!rlang::sym(batch_col) == current_batch & is_qc) %>%
            dplyr::select(dplyr::all_of(NamesFeatureKept)) %>%
            as.matrix()
        target <- df %>%
            dplyr::filter(!!rlang::sym(batch_col) == current_batch & !is_qc) %>%
            dplyr::select(dplyr::all_of(NamesFeatureKept)) %>%
            as.matrix()
        corrs_train[[current_batch]] <- cor(train, method = "spearman")
        corrs_target[[current_batch]] <- cor(target, method = "spearman")
    } # whithin each Batch, correlation between features among QC, among samples


    for (j in 1:length(NamesFeatureKept))
    {
        if (j %% 250 == 1) {
            cat(j, "\n")
        }
        for (b in 1:NbBatch) {
            # cat(b,"`\n")
            current_batch <- levels(df[[batch_col]])[b]
            e_current_batch <- df %>%
                dplyr::filter(!!rlang::sym(batch_col) == current_batch) %>%
                dplyr::select(dplyr::all_of(NamesFeatureKept)) %>%
                as.matrix()
            train <- df %>%
                dplyr::filter(!!rlang::sym(batch_col) == current_batch & is_qc) %>%
                dplyr::select(dplyr::all_of(NamesFeatureKept))
            target <- df %>%
                dplyr::filter(!!rlang::sym(batch_col) == current_batch & !is_qc) %>%
                dplyr::select(dplyr::all_of(NamesFeatureKept))

            train.index_current_batch <- df %>%
                dplyr::filter(!!rlang::sym(batch_col) == current_batch) %>%
                dplyr::select(is_qc) %>%
                as.data.frame()
            
            # train.index_current_batch <- as.character(train.index_current_batch$is_qc)
            corr_train <- corrs_train[[current_batch]]
            corr_target <- corrs_target[[current_batch]]

            corr_train_order <- order(abs(corr_train[, j]), decreasing = TRUE)
            corr_target_order <- order(abs(corr_target[, j]), decreasing = TRUE)

            sel_var <- c()
            l <- num_screen_SERRF
            while (length(sel_var) < (num_screen_SERRF)) {
                sel_var <- intersect(corr_train_order[1:l], corr_target_order[1:l])
                sel_var <- sel_var[!sel_var == j]
                l <- l + 1
            }

            # forillust <- preparedata_forgraph_forIllust_SERRF(DataImputed %>% filter(Batch == current_batch), NamesFeatureKept[c(j, sel_var)])
            # ggplot(data=forillust, aes(x=RunOrder, y=Measure, size=SampleType, shape=SampleType, alpha=SampleType))+
            #   geom_point() + scale_size_manual(values=c(2,1))+
            #   scale_alpha_manual(values=c(1,0.3)) + facet_wrap(~ Feature, scales = "free", ncol=3)
            #

            train_data_y <- train %>%
                dplyr::select(dplyr::all_of(NamesFeatureKept[j])) %>%
                scale(., scale = F)
            train_data_x <- train %>%
                dplyr::select(dplyr::all_of(NamesFeatureKept[sel_var])) %>%
                scale()
            test_data_x <- target %>%
                dplyr::select(dplyr::all_of(NamesFeatureKept[sel_var])) %>%
                scale()
            train_data <- data.frame(y = train_data_y, train_data_x)
            colnames(train_data) <- c("y", paste0("V", 1:(ncol(train_data) - 1)))
            model <- ranger::ranger(y ~ ., data = train_data)

            test_data <- data.frame(test_data_x)
            colnames(test_data) <- colnames(train_data)[-1]

            norm <- e_current_batch[, j]
            # norm[grepl('QC', train.index_current_batch)] = e_current_batch[grepl('QC', train.index_current_batch), j]/((predict(model, data = train_data)$prediction+mean(e_current_batch[grepl('QC', train.index_current_batch), j],na.rm=TRUE))/mean(e_current_batch[grepl('QC', train.index_current_batch), j],na.rm=TRUE))
            norm[train.index_current_batch$is_qc] <- e_current_batch[train.index_current_batch$is_qc, j] / ((model$predictions + mean(e_current_batch[train.index_current_batch$is_qc, j], na.rm = TRUE)) / mean(e_current_batch[train.index_current_batch$is_qc, j], na.rm = TRUE))
            norm[!train.index_current_batch$is_qc] <- (e_current_batch[!train.index_current_batch$is_qc, j]) / ((predict(model, data = test_data)$predictions + mean(e_current_batch[!train.index_current_batch$is_qc, j], na.rm = TRUE)) / (median(e_current_batch[!train.index_current_batch$is_qc, j], na.rm = TRUE)))
            # plot(p$time[!train.index_current_batch=='qc'], (e_current_batch[j,!train.index_current_batch=='qc'])/((predict(model,data = test_data)$predictions  + mean(e_current_batch[j, !train.index_current_batch=='qc'],na.rm=TRUE))/(median(e_current_batch[j,!train.index_current_batch=='qc'],na.rm = TRUE))))
            norm[train.index_current_batch$is_qc] <- norm[train.index_current_batch$is_qc] / (median(norm[train.index_current_batch$is_qc], na.rm = TRUE) / median(allQCinit[, j], na.rm = TRUE))
            norm[!train.index_current_batch$is_qc] <- norm[!train.index_current_batch$is_qc] / (median(norm[!train.index_current_batch$is_qc], na.rm = TRUE) / median(allSamplesinit[, j], na.rm = TRUE))
            norm[!is.finite(norm)] <- rnorm(length(norm[!is.finite(norm)]), sd = sd(norm[is.finite(norm)], na.rm = TRUE) * 0.01)
            Feature_Normalized_serrf[Feature_Normalized_serrf[[batch_col]] == current_batch, NamesFeatureKept[j]] <- norm
        }

        featj <- Feature_Normalized_serrf[, NamesFeatureKept[j]]
        indQC <- which(df$is_qc)
        normed_target <- as.numeric(as.matrix(featj[-c(indQC), 1]))
        normed_target[is.na(normed_target)] <- rnorm(sum(is.na(normed_target)), mean = min(normed_target[!is.na(normed_target)], na.rm = TRUE), sd = sd(normed_target[!is.na(normed_target)]) * 0.1)
        normed_target[normed_target < 0] <- runif(1) * min(normed_target[normed_target > 0], na.rm = TRUE)
        normed_train <- as.numeric(as.matrix(featj[indQC, 1]))
        normed_train[is.na(normed_train)] <- rnorm(sum(is.na(normed_train)), mean = min(normed_train[!is.na(normed_train)], na.rm = TRUE), sd = sd(normed_train[!is.na(normed_train)]) * 0.1)
        normed_train[normed_train < 0] <- runif(1) * min(normed_train[normed_train > 0], na.rm = TRUE)
        featj[indQC, 1] <- normed_train
        featj[-indQC, 1] <- normed_target
        Feature_Normalized_serrf[, NamesFeatureKept[j]] <- featj
    }
    return(Feature_Normalized_serrf)
}



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
normalization_SERRF_batch <- function(DataImputed, NamesFeatureAll) {
    set.seed(0)
    num_screen_SERRF <- 10
    NamesFeatureKept <- colnames(DataImputed)[colnames(DataImputed) %in% NamesFeatureAll]
    NbBatch <- length(levels(DataImputed$batchID))
    allQCinit <- DataImputed %>%
        dplyr::filter(grepl("QC", Samplename)) %>%
        dplyr::select(dplyr::all_of(NamesFeatureKept)) %>%
        as.matrix()
    Feature_Normalized_serrf <- DataImputed
    allSamplesinit <- DataImputed %>%
        dplyr::filter(grepl("Sample", Samplename)) %>%
        dplyr::select(dplyr::all_of(NamesFeatureKept)) %>%
        as.matrix()

    # Computations of the correlation matrices (for the screening step)

    corrs_train <- list()
    corrs_target <- list()
    for (b in 1:NbBatch) {
        current_batch <- levels(DataImputed$batchID)[b]
        train <- DataImputed %>%
            dplyr::filter(batchID == current_batch & grepl("QC", Samplename)) %>%
            dplyr::select(dplyr::all_of(NamesFeatureKept)) %>%
            as.matrix()
        target <- DataImputed %>%
            dplyr::filter(batchID == current_batch & grepl("Sample", Samplename)) %>%
            dplyr::select(dplyr::all_of(NamesFeatureKept)) %>%
            as.matrix()
        corrs_train[[current_batch]] <- cor(train, method = "spearman")
        corrs_target[[current_batch]] <- cor(target, method = "spearman")
    } # whithin each Batch, correlation between features among QC, among samples


    for (j in 1:length(NamesFeatureKept))
    {
        if (j %% 250 == 1) {
            cat(j, "\n")
        }
        for (b in 1:NbBatch) {
            # cat(b,"`\n")
            current_batch <- levels(DataImputed$batchID)[b]
            e_current_batch <- DataImputed %>%
                dplyr::filter(batchID == current_batch) %>%
                dplyr::select(dplyr::all_of(NamesFeatureKept)) %>%
                as.matrix()
            train <- DataImputed %>%
                dplyr::filter(batchID == current_batch & grepl("QC", Samplename)) %>%
                dplyr::select(dplyr::all_of(NamesFeatureKept))
            target <- DataImputed %>%
                dplyr::filter(batchID == current_batch & grepl("Sample", Samplename)) %>%
                dplyr::select(dplyr::all_of(NamesFeatureKept))

            train.index_current_batch <- DataImputed %>%
                dplyr::filter(batchID == current_batch) %>%
                dplyr::select(Samplename) %>%
                as.data.frame()
            train.index_current_batch <- as.character(train.index_current_batch$Samplename)
            corr_train <- corrs_train[[current_batch]]
            corr_target <- corrs_target[[current_batch]]

            corr_train_order <- order(abs(corr_train[, j]), decreasing = TRUE)
            corr_target_order <- order(abs(corr_target[, j]), decreasing = TRUE)

            sel_var <- c()
            l <- num_screen_SERRF
            while (length(sel_var) < (num_screen_SERRF)) {
                sel_var <- intersect(corr_train_order[1:l], corr_target_order[1:l])
                sel_var <- sel_var[!sel_var == j]
                l <- l + 1
            }

            # forillust <- preparedata_forgraph_forIllust_SERRF(DataImputed %>% filter(Batch == current_batch), NamesFeatureKept[c(j, sel_var)])
            # ggplot(data=forillust, aes(x=RunOrder, y=Measure, size=SampleType, shape=SampleType, alpha=SampleType))+
            #   geom_point() + scale_size_manual(values=c(2,1))+
            #   scale_alpha_manual(values=c(1,0.3)) + facet_wrap(~ Feature, scales = "free", ncol=3)
            #

            train_data_y <- train %>%
                dplyr::select(dplyr::all_of(NamesFeatureKept[j])) %>%
                scale(., scale = F)
            train_data_x <- train %>%
                dplyr::select(dplyr::all_of(NamesFeatureKept[sel_var])) %>%
                scale()
            test_data_x <- target %>%
                dplyr::select(dplyr::all_of(NamesFeatureKept[sel_var])) %>%
                scale()
            train_data <- data.frame(y = train_data_y, train_data_x)
            colnames(train_data) <- c("y", paste0("V", 1:(ncol(train_data) - 1)))
            model <- ranger::ranger(y ~ ., data = train_data)

            test_data <- data.frame(test_data_x)
            colnames(test_data) <- colnames(train_data)[-1]

            norm <- e_current_batch[, j]
            # norm[grepl('QC', train.index_current_batch)] = e_current_batch[grepl('QC', train.index_current_batch), j]/((predict(model, data = train_data)$prediction+mean(e_current_batch[grepl('QC', train.index_current_batch), j],na.rm=TRUE))/mean(e_current_batch[grepl('QC', train.index_current_batch), j],na.rm=TRUE))
            norm[grepl("QC", train.index_current_batch)] <- e_current_batch[grepl("QC", train.index_current_batch), j] / ((model$predictions + mean(e_current_batch[grepl("QC", train.index_current_batch), j], na.rm = TRUE)) / mean(e_current_batch[grepl("QC", train.index_current_batch), j], na.rm = TRUE))
            norm[grepl("Sample", train.index_current_batch)] <- (e_current_batch[grepl("Sample", train.index_current_batch), j]) / ((predict(model, data = test_data)$predictions + mean(e_current_batch[grepl("Sample", train.index_current_batch), j], na.rm = TRUE)) / (median(e_current_batch[grepl("Sample", train.index_current_batch), j], na.rm = TRUE)))
            # plot(p$time[!train.index_current_batch=='qc'], (e_current_batch[j,!train.index_current_batch=='qc'])/((predict(model,data = test_data)$predictions  + mean(e_current_batch[j, !train.index_current_batch=='qc'],na.rm=TRUE))/(median(e_current_batch[j,!train.index_current_batch=='qc'],na.rm = TRUE))))
            norm[grepl("QC", train.index_current_batch)] <- norm[grepl("QC", train.index_current_batch)] / (median(norm[grepl("QC", train.index_current_batch)], na.rm = TRUE) / median(allQCinit[, j], na.rm = TRUE))
            norm[grepl("Sample", train.index_current_batch)] <- norm[grepl("Sample", train.index_current_batch)] / (median(norm[grepl("Sample", train.index_current_batch)], na.rm = TRUE) / median(allSamplesinit[, j], na.rm = TRUE))
            norm[!is.finite(norm)] <- rnorm(length(norm[!is.finite(norm)]), sd = sd(norm[is.finite(norm)], na.rm = TRUE) * 0.01)
            Feature_Normalized_serrf[Feature_Normalized_serrf$batchID == current_batch, NamesFeatureKept[j]] <- norm
        }

        featj <- Feature_Normalized_serrf[, NamesFeatureKept[j]]
        indQC <- which(grepl("QC", DataImputed$Samplename))
        normed_target <- as.numeric(as.matrix(featj[-c(indQC), 1]))
        normed_target[is.na(normed_target)] <- rnorm(sum(is.na(normed_target)), mean = min(normed_target[!is.na(normed_target)], na.rm = TRUE), sd = sd(normed_target[!is.na(normed_target)]) * 0.1)
        normed_target[normed_target < 0] <- runif(1) * min(normed_target[normed_target > 0], na.rm = TRUE)
        normed_train <- as.numeric(as.matrix(featj[indQC, 1]))
        normed_train[is.na(normed_train)] <- rnorm(sum(is.na(normed_train)), mean = min(normed_train[!is.na(normed_train)], na.rm = TRUE), sd = sd(normed_train[!is.na(normed_train)]) * 0.1)
        normed_train[normed_train < 0] <- runif(1) * min(normed_train[normed_train > 0], na.rm = TRUE)
        featj[indQC, 1] <- normed_train
        featj[-indQC, 1] <- normed_target
        Feature_Normalized_serrf[, NamesFeatureKept[j]] <- featj
    }
    return(Feature_Normalized_serrf)
}
