###################
##### Abalone #####
###################

# read dataset
abalone <- read.csv('C:/Users/dasha/Downloads/abalone_dataset.csv')

dataset <- abalone

## add new column age.group with 3 values based on the number of rings 
dataset$age.group <- cut(dataset$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))


## alternative way of setting age.group
dataset$age.group[dataset$rings<=8] <- "young"
dataset$age.group[dataset$rings>8 & dataset$rings<=11] <- "adult"
dataset$age.group[dataset$rings>11 & dataset$rings<=35] <- "old"

# Convert age.group to factor
dataset$age.group <- as.factor(dataset$age.group)

# Select two different feature subsets
set.seed(123)  # For reproducibility

features1 <- dataset[, c("length", "diameter", "height")]
features2 <- dataset[, c("shucked_wieght", "viscera_wieght", "shell_weight")]

labels <- dataset$age.group

# Split data into training and testing sets
trainIndex <- createDataPartition(labels, p = 0.8, list = FALSE)

train1 <- features1[trainIndex,]
test1 <- features1[-trainIndex,]
train2 <- features2[trainIndex,]
test2 <- features2[-trainIndex,]

trainLabels <- labels[trainIndex]
testLabels <- labels[-trainIndex]

# Train and evaluate two kNN models
k <- 5  # Initial k value
pred1 <- knn(train = train1, test = test1, cl = trainLabels, k = k)
pred2 <- knn(train = train2, test = test2, cl = trainLabels, k = k)

# Create contingency tables
table1 <- table(Predicted = pred1, Actual = testLabels)
table2 <- table(Predicted = pred2, Actual = testLabels)

# Print contingency tables
print("Contingency Table for Model 1:")
print(table1)
print("Contingency Table for Model 2:")
print(table2)

# Compute accuracy for both models
accuracy1 <- sum(diag(table1)) / sum(table1)
accuracy2 <- sum(diag(table2)) / sum(table2)

cat("Accuracy of Model 1:", accuracy1, "\n")
cat("Accuracy of Model 2:", accuracy2, "\n")

# Find optimal k for the better model
better_features <- if (accuracy1 > accuracy2) features1 else features2
trainBetter <- better_features[trainIndex,]
testBetter <- better_features[-trainIndex,]



install.packages("factoextra")
install.packages("cluster")  

# Load libraries
library(ggplot2)
library(factoextra)
library(cluster)

# Use the best-performing feature subset
better_features <- if (accuracy1 > accuracy2) features1 else features2

# Scale the features for better K-Means performance
scaled_features <- scale(better_features)

# Determine the optimal K using the Elbow Method
set.seed(123) # Ensure reproducibility
wss <- numeric(10)  # Store within-cluster sum of squares

for (k in 1:10) {
  kmeans_model <- kmeans(scaled_features, centers = k, nstart = 25)
  wss[k] <- kmeans_model$tot.withinss
}

# Plot Elbow Method
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters K", ylab = "Total Within Sum of Squares")

# Determine optimal K using the Silhouette Method
silhouette_scores <- numeric(9)  # Store silhouette scores

for (k in 2:10) {
  km <- kmeans(scaled_features, centers = k, nstart = 25)
  silhouette_scores[k-1] <- mean(silhouette(km$cluster, dist(scaled_features))[, 3])
}

# Plot Silhouette Scores
plot(2:10, silhouette_scores, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters K", ylab = "Average Silhouette Width")

# Choose the best K based on the Elbow and Silhouette method
optimal_k <- which.max(silhouette_scores) + 1
cat("Optimal number of clusters (K):", optimal_k, "\n")

# Train final K-Means model
final_kmeans <- kmeans(scaled_features, centers = optimal_k, nstart = 25)

# Add cluster labels to the dataset
dataset$Cluster <- as.factor(final_kmeans$cluster)

# Plot clusters using two selected features
ggplot(dataset, aes(x = better_features[,1], y = better_features[,2], color = Cluster)) +
  geom_point(size = 2) +
  labs(title = paste("K-Means Clustering (K =", optimal_k, ")"),
       x = colnames(better_features)[1],
       y = colnames(better_features)[2]) +
  theme_minimal()


