setwd("E:/Danial/1) Milan/1) University of Milan/5. Second Trimester/Machine learning")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, doParallel, dplyr, psych, dbplyr, ff, bigmemory, biganalytics, caret, ggplot2, glmnet, DataExplorer)

cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = cores)

trans <- fread("transactions.csv")
colnames(trans) <- c("step", "type", "amount", "customer", "old-balance-customer", "new-balance-customer", "recipient", "old-balance-recipient", "new-balance-recipient", "isFraud")
trans <- as.data.frame(trans)


# Preprocessing
trans <- trans[, !names(trans) %in% c("customer", "recipient", "step")]
trans$type <- as.integer(factor(trans$type))
missing_values <- colSums(is.na(trans))
columns_to_normalize <- c("new-balance-customer", "amount", "new-balance-recipient", "old-balance-recipient", "old-balance-customer")
normalized_trans <- trans
for (column in columns_to_normalize) {
  min_val <- min(trans[[column]])
  max_val <- max(trans[[column]])
  normalized_trans[[column]] <- (trans[[column]] - min_val) / (max_val - min_val)
}

# Make a sample of our data
fraud <- normalized_trans[normalized_trans$isFraud == 1, ]
fraud <- fraud[sample(nrow(fraud), 1000), ] 
notfraud <- normalized_trans[normalized_trans$isFraud == 0, ]
notfraud <- notfraud[sample(nrow(notfraud), 100000), ]
trans <- rbind.data.frame(fraud, notfraud)

plot_histogram(trans)
plot_correlation(na.omit(trans), maxcat = 5L)
plot_scatterplot(trans, by = "amount", sampled_rows = 1000L)

# PCA with h2o
pacman::p_load(h2o)
h2o.init(max_mem_size="12G")
h2o.init(nthreads = cores)

trans <- trans[, !names(trans) %in% "isFraud"]
trans <- as.h2o(trans)


pca <- h2o.prcomp(training_frame = trans,
                  k = 2,
                  use_all_factor_levels = TRUE,
                  pca_method = "Randomized",
                  transform = "STANDARDIZE",
                  impute_missing = TRUE)
pred_pca=h2o.predict(pca, trans)

z_scores <- h2o.scale(pred_pca, center = FALSE, scale = FALSE)
threshold <- 15
final_results = h2o.cbind(trans,z_scores)
outliers <- final_results[h2o.abs(z_scores$PC1) > threshold |
                            h2o.abs(z_scores$PC2) > threshold, ]
outliers=as.data.frame(outliers)
ggplot() +
  geom_point(data = as.data.frame(final_results), aes(x = PC1, y = PC2), color = "blue") +
  geom_point(data = as.data.frame(outliers), aes(x = PC1, y = PC2), color = "red") +
  xlab("PC1") + ylab("PC2")

h2o.shutdown()
y
