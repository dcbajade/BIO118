## Name: Denisse Gabrielle C. Bajade

# =========================================================
# BIO 118 Laboratory Exercise 5
# 5 Summarizing and Organizing Data
# =========================================================

# 5.1. Load and inspect the iris dataset

data("iris")
head(iris)
str(iris)
names(iris)

## 5.2. Measures of central tendency

### 5.2.1 and 5.2.2 Mean and median by species

library(dplyr)

iris %>%
  group_by(Species) %>%
  summarise(
    Mean = mean(Sepal.Length),
    Median = median(Sepal.Length)
  )

### 5.2.3 Mode function
mode_function <- function(x) {
  frequencies <- table(x)
  names(frequencies)[which.max(frequencies)]
}

### 5.2.3 Mode by species
iris %>%
  group_by(Species) %>%
  summarise(Mode = mode_function(Sepal.Length))

# 5.3 Measures of variability

## Range, Variance, SD, and IQR

iris %>%
  group_by(Species) %>%
  summarise(
    Minimum = min(Sepal.Length),
    Maximum = max(Sepal.Length),
    Range = max(Sepal.Length) - min(Sepal.Length),
    Variance = var(Sepal.Length),
    SD = sd(Sepal.Length),
    Q1 = quantile(Sepal.Length, 0.25),
    Q3 = quantile(Sepal.Length, 0.75),
    IQR = IQR(Sepal.Length)
  )

# 5.4 Frequency distribution
## 5.4.1 Freq Table
species_freq <- table(iris$Species)
species_freq

## 5.4.2 Relative frequency and percentage
prop.table(species_freq)
prop.table(species_freq) * 100

## 5.4.3 Counts and percentages in a single table
iris %>%
  count(Species, name = "Frequency") %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2))

# 5.5 Measures of distribution

install.packages("e1071")
library(e1071)

## 5.5.1 Calculate skewness and kurtosis
iris %>%
  group_by(Species) %>%
  summarise(
    Skewness = skewness(Sepal.Length),
    Kurtosis = kurtosis(Sepal.Length)
  )

# 5.6 Visualizing the data

## 5.6.1 Histogram
library(ggplot2)
ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.3) +
  facet_wrap(~ Species) +
  labs(
    title = "Distribution of Sepal Length by Iris Species",
    x = "Sepal Length (cm)",
    y = "Frequency"
  )

## 5.6.2 Boxplots
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot() +
  labs(
    title = "Sepal Length by Iris Species",
    x = "Species",
    y = "Sepal Length (cm)"
  )

# 5.7 Practice Exercise: Summarizing data using the CO2 dataset

# Load packages and dataset
library(dplyr)
library(ggplot2)
library(e1071)

# 5.7.1 Inspect the dataset
?CO2
data(CO2)
data("CO2", package = "datasets")
head(CO2)
dim(CO2)

# 5.7.2 Mean and median
CO2 %>%
  summarise(
    Mean = mean(uptake),
    Median = median(uptake)
  )

CO2 %>%
  group_by(Treatment) %>%
  summarise(
    Mean = mean(uptake),
    Median = median(uptake)
  )

# 5.7.3 Variability by treatment
CO2 %>%
  group_by(Treatment) %>%
  summarise(
    Minimum = min(uptake),
    Maximum = max(uptake),
    Range = max(uptake) - min(uptake),
    Variance = var(uptake),
    SD = sd(uptake),
    IQR = IQR(uptake)
  )

# 5.7.4 Frequency and percentage
CO2 %>%
  count(Treatment, name = "Frequency") %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2))

# 5.7.5 Skewness and kurtosis
CO2 %>%
  summarise(
    Skewness = skewness(uptake),
    Kurtosis = kurtosis(uptake)
  )

# 5.7.6 Histogram
ggplot(CO2, aes(x = uptake)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(
    title = "Distribution of Carbon Dioxide Uptake",
    x = "Carbon Dioxide Uptake (µmol m⁻² s⁻¹)",
    y = "Frequency"
  ) +
  theme_minimal()

# Boxplot by treatment
ggplot(CO2, aes(x = Treatment, y = uptake, fill = Treatment)) +
  geom_boxplot() +
  labs(
    title = "Carbon Dioxide Uptake by Treatment",
    x = "Treatment",
    y = "Carbon Dioxide Uptake (µmol m⁻² s⁻¹)"
  ) +
  theme_minimal() +
  guides(fill = "none")
