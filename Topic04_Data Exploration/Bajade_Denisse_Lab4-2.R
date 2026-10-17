# Name: Denisse Gabrielle C. Bajade

# =========================================================
# BIO 118 Laboratory Exercise 4
# 4C Multivariate Analysis: PCA and Clustering
# =========================================================

install.packages("tidyverse")
install.packages("ggfortify")

library(tidyverse)
library(ggfortify)

# ============================================================
# 4.1. PRINCIPAL COMPONENT ANALYSIS USING IRIS DATASET
# ============================================================

data(iris)
head(iris)
str(iris)
summary(iris)
dim(iris)

# ============================================================
# 4.2 Principal Component Analysis (PCA)
# ============================================================

# 4.2.1 Select numerical variables
iris_data <- iris[, 1:4]
head(iris_data)

# 4.2.2 Perform PCA
pca_result <- prcomp(iris_data, center = TRUE, scale. = TRUE)

names(pca_result)
str(pca_result)
head(pca_result$x)
summary(pca_result)

# 4.2.3 PCA score plot
pca_scores <- as.data.frame(pca_result$x)
pca_scores$Species <- iris$Species
head(pca_scores)

ggplot(pca_scores, aes(x = PC1, y = PC2, color = Species)) +
  geom_point(size = 2) +
  labs(
    title = "PCA of Iris Measurements",
    x = "PC1",
    y = "PC2",
    color = "Species"
  ) +
  theme_classic()

# 4.2.4 PCA biplot using ggfortify
# Requires ggfortify to be installed.
pca_model <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)

autoplot(
  pca_model,
  data = iris,
  colour = "Species",
  loadings = TRUE,
  loadings.label = TRUE,
  loadings.colour = "black",
  loadings.label.colour = "black"
) +
  theme_minimal() +
  labs(
    title = "Principal Component Analysis (PCA) on Iris Dataset",
    subtitle = "Four measurements summarized in principal component space",
    caption = "Data source: built-in R iris dataset",
    colour = "Flower Species"
  )

# ============================================================
# 4.3 Hierarchical Clustering
# ============================================================

# 4.3.1 Standardize numerical variables and calculate distances
iris_scaled <- scale(iris_data)
dist_matrix <- dist(iris_scaled, method = "euclidean")

# 4.3.2 Perform hierarchical clustering using complete linkage
hclust_result <- hclust(dist_matrix, method = "complete")

# 4.3.3 Dendrogram
plot(
  hclust_result,
  labels = FALSE,
  main = "Hierarchical Clustering of Iris Flowers",
  xlab = "Observations",
  ylab = "Height"
)

# 4.3.4 Create three clusters
groups <- cutree(hclust_result, k = 3)

plot(
  hclust_result,
  labels = FALSE,
  main = "Hierarchical Clustering of Iris Flowers (3 Clusters)",
  xlab = "Observations",
  ylab = "Height"
)
rect.hclust(hclust_result, k = 3, border = 2:4)

iris$Cluster <- as.factor(groups)
head(iris)
table(iris$Species, iris$Cluster)

# ============================================================
# 4.4 K-means Clustering
# ============================================================

set.seed(123)
kmeans_result <- kmeans(iris_scaled, centers = 3, nstart = 20)

kmeans_result
names(kmeans_result)
head(kmeans_result$cluster)

iris$KMeans_Cluster <- as.factor(kmeans_result$cluster)
head(iris)
table(iris$Species, iris$KMeans_Cluster)

# 4.4.2 Visualize K-means clusters
ggplot(
  iris,
  aes(x = Petal.Length, y = Petal.Width, color = KMeans_Cluster)
) +
  geom_point(size = 2) +
  labs(
    title = "K-means Clustering of Iris Flowers",
    x = "Petal Length (cm)",
    y = "Petal Width (cm)",
    color = "Cluster"
  ) +
  theme_classic()

# ============================================================
# 4.5 Practice Exercise: Multivariate Analysis Using airquality
# ============================================================

data(airquality)
head(airquality)
str(airquality)
summary(airquality)

# Practice 2: Missing values
colSums(is.na(airquality))

air_data <- airquality[, c("Ozone", "Solar.R", "Wind", "Temp")]
air_complete <- na.omit(air_data)

dim(air_data)
dim(air_complete)
head(air_complete)

# 4.5.3 Perform PCA
air_pca <- prcomp(air_complete, center = TRUE, scale. = TRUE)

names(air_pca)
head(air_pca$x)
summary(air_pca)

# PC1 + PC2 cumulative variance
air_pca_summary <- summary(air_pca)
sum(air_pca_summary$importance[2, 1:2])

# 4.5.4 PCA plot
air_pca_scores <- as.data.frame(air_pca$x)

ggplot(air_pca_scores, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(
    title = "PCA of Air Quality Measurements",
    x = "PC1",
    y = "PC2"
  ) +
  theme_classic()

# 4.5.5 Add Month after PCA
air_complete_data <- airquality[
  complete.cases(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")]),
]

# Check row alignment before adding Month
stopifnot(nrow(air_complete_data) == nrow(air_pca_scores))
air_pca_scores$Month <- factor(air_complete_data$Month)

ggplot(air_pca_scores, aes(x = PC1, y = PC2, color = Month)) +
  geom_point() +
  labs(
    title = "PCA of Air Quality Measurements",
    x = "PC1",
    y = "PC2",
    color = "Month"
  ) +
  theme_classic()

# 4.5.6 Hierarchical clustering
air_scaled <- scale(air_complete)
air_dist <- dist(air_scaled, method = "euclidean")
air_hclust <- hclust(air_dist, method = "complete")

plot(
  air_hclust,
  labels = FALSE,
  main = "Hierarchical Clustering of Air Quality Observations",
  xlab = "Observations",
  ylab = "Height"
)

# 4.5.7 K-means clustering
set.seed(123)
air_kmeans <- kmeans(air_scaled, centers = 3, nstart = 20)

air_pca_scores$Cluster <- as.factor(air_kmeans$cluster)

ggplot(
  air_pca_scores,
  aes(x = PC1, y = PC2, color = Cluster)
) +
  geom_point() +
  labs(
    title = "K-means Clusters in PCA Space: Air Quality",
    x = "PC1",
    y = "PC2",
    color = "K-means Cluster"
  ) +
  theme_classic()


# 4.5.8 Relate clusters to Month
air_cluster_results <- air_complete_data
air_cluster_results$KMeans_Cluster <- as.factor(air_kmeans$cluster)

table(
  Month = air_cluster_results$Month,
  KMeans_Cluster = air_cluster_results$KMeans_Cluster
)
