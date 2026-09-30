
# =========================================================
# BIO 118 Laboratory Exercise 3
# 3D Organizing Files within an R Project
# =========================================================

# Check working directory
getwd()

# =========================================================
# Import iris_data.csv
# =========================================================

iris_data <- read.csv(
  "BIO 118/BIO 118 2/Topic03_Wrangling/Lab3D_Organizing/Data/iris_data.csv"
)

head(iris_data)

# =========================================================
# Get only the versicolor data
# =========================================================

versicolor_data <- subset(
  iris_data,
  Species == "versicolor"
)

print(versicolor_data)


# =========================================================
# Save versicolor data as a new CSV
# =========================================================

write.csv(
  versicolor_data,
  file = "BIO 118/BIO 118 2/Topic03_Wrangling/Lab3D_Organizing/Data/versicolor_data.csv",
  row.names = FALSE
)


# =========================================================
# Create the plot
# =========================================================

plot(
  versicolor_data$Sepal.Length,
  versicolor_data$Sepal.Width,
  xlab = "Sepal Length (cm)",
  ylab = "Sepal Width (cm)",
  pch = 19
)
