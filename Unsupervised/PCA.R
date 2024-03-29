setwd("E:/Danial/1) Milan/1) University of Milan/5. Second Trimester/Machine learning")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, doParallel, dplyr, psych, dbplyr, ff, bigmemory, biganalytics, caret, ggplot2, glmnet, DataExplorer)

cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = cores)

data <- fread("transactions.csv")
data <- as.data.frame(data)

# Preprocessing
data <- data[, !names(data) %in% c("nameOrig", "nameDest", "step")]
data$type <- as.integer(factor(data$type))
missing_values <- colSums(is.na(data))
columns_to_normalize <- c("newbalanceOrig", "amount", "newbalanceDest", "oldbalanceDest", "oldbalanceOrig")
normalized_data <- data
for (column in columns_to_normalize) {
  min_val <- min(data[[column]])
  max_val <- max(data[[column]])
  normalized_data[[column]] <- (data[[column]] - min_val) / (max_val - min_val)
}

# Make a sample of our data
fraud_sample <- normalized_data[normalized_data$isFraud == 1, ]
fraud_sample <- fraud_sample[sample(nrow(fraud_sample), 1000), ] 
notfraud <- normalized_data[normalized_data$isFraud == 0, ]
notfraud <- notfraud[sample(nrow(notfraud), 100000), ]
df_final <- rbind.data.frame(fraud_sample, notfraud)

plot_histogram(df_final)
plot_correlation(na.omit(df_final), maxcat = 5L)
plot_scatterplot(df_final, by = "amount", sampled_rows = 1000L)

# PCA with h2o
pacman::p_load(h2o)
h2o.init(max_mem_size="12G")
h2o.init(nthreads = cores)

df_final <- df_final[, !names(df_final) %in% "isFraud"]
df_final <- as.h2o(df_final)


pca <- h2o.prcomp(training_frame = df_final,
                  k = 2,
                  use_all_factor_levels = TRUE,
                  pca_method = "Randomized",
                  transform = "STANDARDIZE",
                  impute_missing = TRUE)
pred_pca=h2o.predict(pca, df_final)

z_scores <- h2o.scale(pred_pca, center = FALSE, scale = FALSE)
threshold <- 13
final_results = h2o.cbind(df_final,z_scores)
outliers <- final_results[h2o.abs(z_scores$PC1) > threshold |
                            h2o.abs(z_scores$PC2) > threshold, ]
outliers=as.data.frame(outliers)
ggplot() +
  geom_point(data = as.data.frame(final_results), aes(x = PC1, y = PC2), color = "skyblue") +
  geom_point(data = as.data.frame(outliers), aes(x = PC1, y = PC2), color = "red") +
  xlab("PC1") +
  ylab("PC2")

h2o.shutdown()
y
