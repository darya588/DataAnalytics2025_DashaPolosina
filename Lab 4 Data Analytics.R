# Load libraries
library(ggplot2)
library(caret)
library(class)
library(FactoMineR)
library(factoextra)

# Load dataset
wine_data <- read.csv('C:/Users/dasha/Downloads/wine/wine.data', header = FALSE)

# Add column names based on known Wine dataset structure
colnames(wine_data) <- c("Class", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_Ash", "Magnesium",
                         "Total_Phenols", "Flavanoids", "Nonflav_Phenols", "Proanthocyanins",
                         "Color_Intensity", "Hue", "OD280_OD315", "Proline")

# Convert Class to factor
wine_data$Class <- as.factor(wine_data$Class)

# Remove Class column for PCA
wine_features <- wine_data[, -1]

# Perform PCA
pca_result <- prcomp(wine_features, center = TRUE, scale. = TRUE)

# Plot dataset using the first two PCs
wine_pca_scores <- as.data.frame(pca_result$x)
wine_pca_scores$Class <- wine_data$Class

ggplot(wine_pca_scores, aes(x = PC1, y = PC2, color = Class)) +
  geom_point(size = 2) +
  labs(title = "PCA of Wine Dataset", x = "PC1", y = "PC2") +
  theme_minimal()

# Identify variables that contribute most to PC1
pc1_contributions <- abs(pca_result$rotation[, 1])
pc1_contributions_sorted <- sort(pc1_contributions, decreasing = TRUE)
print(pc1_contributions_sorted)

# Drop least contributing variables (choose threshold, e.g., below median contribution)
threshold <- median(pc1_contributions_sorted)
selected_features <- names(pc1_contributions_sorted[pc1_contributions_sorted > threshold])

# Rerun PCA with selected features
wine_features_reduced <- wine_data[, selected_features]
pca_reduced <- prcomp(wine_features_reduced, center = TRUE, scale. = TRUE)

# Train kNN model on the original dataset
set.seed(123)
trainIndex <- createDataPartition(wine_data$Class, p = 0.8, list = FALSE)
train_original <- wine_features[trainIndex, ]
test_original <- wine_features[-trainIndex, ]
train_labels <- wine_data$Class[trainIndex]
test_labels <- wine_data$Class[-trainIndex]

# K value
k_value <- 5
knn_original_pred <- knn(train = train_original, test = test_original, cl = train_labels, k = k_value)

# Train kNN model using the first 3 PCs
train_pca <- wine_pca_scores[trainIndex, 1:3]  # First 3 PCs
test_pca <- wine_pca_scores[-trainIndex, 1:3]

knn_pca_pred <- knn(train = train_pca, test = test_pca, cl = train_labels, k = k_value)

# Compare models using contingency tables
table_original <- table(Predicted = knn_original_pred, Actual = test_labels)
table_pca <- table(Predicted = knn_pca_pred, Actual = test_labels)

print("Contingency Table for kNN on Original Data:")
print(table_original)
print("Contingency Table for kNN on PCA Data:")
print(table_pca)

# Compute precision, recall, and F1-score for both models
confusionMatrix(knn_original_pred, test_labels)
confusionMatrix(knn_pca_pred, test_labels)