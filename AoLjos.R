library(ggplot2)
library(tidyr)
library(reshape2)
library(randomForest)
library(GGally)

# Set working directory
getwd()
setwd("C:/Corn/Code/Sems 3/DMV/Data")

# Load data
data_whiteWine <- read.csv("winequality-white.csv", header = TRUE, sep = ";")
data_whiteWine

# Cek data
summary(data_whiteWine)
str(data_whiteWine)
head(data_whiteWine)

# Hitung korelasi
cor_matrix <- cor(data_whiteWine)
cor_matrix

# Ubah format matriks menjadi long-form untuk ggplot2
cor_melt <- melt(cor_matrix)

# Heatmap ggplot2
p1 <- ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap Korelasi (Wine Quality Dataset)",
       x = "", y = "")

p1 

# Scatterplot matrix dengan korelasi di atas diagonal
p_scatter_all <- ggpairs(data_whiteWine,
  lower = list(
    continuous = wrap("points", colour = "steelblue", alpha = 0.6)
  ),
  diag = list(
    continuous = wrap("densityDiag", fill = "lightblue", colour = "blue")
  ),
  upper = list(
    continuous = wrap("cor", size = 4, colour = "red")
  )
)

p_scatter_all

# Pilih variabel numeric + quality
vars <- c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar",
          "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide",
          "density", "pH", "sulphates", "alcohol", "quality")

# Convert to long format
long_data <- data_whiteWine %>%
  select(all_of(vars)) %>%
  pivot_longer(
    cols = -quality,
    names_to = "variable",
    values_to = "value"
  )

# Violin plot
ggplot(long_data, aes(x = factor(quality), y = value, fill = factor(quality))) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Violin Plot Setiap Variabel terhadap Quality",
    x = "Wine Quality",
    y = "Value"
  )


#test categorical for wine quality prediction

# 1. prepare data: convert quality to factor
data_class <- data_whiteWine
data_class$quality_f <- factor(data_class$quality)   # each numeric score becomes a class

# 2. split
set.seed(123)
n <- nrow(data_class)
train_idx <- sample(seq_len(n), size = floor(0.8 * n))
train_c <- data_class[train_idx, ]
test_c  <- data_class[-train_idx, ]

# 3. train randomForest classifier
set.seed(123)
rf_class <- randomForest(quality_f ~ . - quality,   # remove numeric quality column if present
                         data = train_c,
                         ntree = 500,
                         mtry = 4,
                         importance = TRUE)

print(rf_class)

# 4. predict classes and class probabilities
pred_class <- predict(rf_class, newdata = test_c)             # class labels
pred_prob  <- predict(rf_class, newdata = test_c, type = "prob")  # probabilities per class

# 5. confusion matrix and metrics
cm <- table(Predicted = pred_class, Actual = test_c$quality_f)
print(cm)

# overall accuracy
accuracy <- sum(diag(cm)) / sum(cm)
cat("Overall accuracy:", round(accuracy, 3), "\n")

# per-class precision/recall (manual)
precision <- diag(cm) / rowSums(cm)   # precision per predicted-class
recall    <- diag(cm) / colSums(cm)   # recall per actual-class
f1        <- 2 * precision * recall / (precision + recall)
metrics <- data.frame(precision = precision, recall = recall, F1 = f1)
print(metrics)

# 6. nicer report (if caret installed)
if(requireNamespace("caret", quietly = TRUE)) {
  library(caret)
  caret::confusionMatrix(cm)
}

# 7. variable importance (classification)
print(importance(rf_class))
varImpPlot(rf_class)