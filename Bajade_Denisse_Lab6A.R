## Name: Denisse Gabrielle C. Bajade

# =========================================================
# 6A.1 Comparing One Sample to a Reference Value
# Dataset: trees
# =========================================================

# 6A.1.1 Load and inspect the data

data("trees")

head(trees)
str(trees)
summary(trees$Height)
sum(is.na(trees$Height))


# 6A.1.2 Explore the data

# Histogram
ggplot(trees, aes(x = Height)) +
  geom_histogram(bins = 10) +
  labs(
    title = "Distribution of Black Cherry Tree Heights",
    x = "Height (feet)",
    y = "Number of Trees"
  ) +
  theme_minimal()

# Boxplot
ggplot(trees, aes(x = "", y = Height)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Black Cherry Tree Heights",
    x = NULL,
    y = "Height (feet)"
  ) +
  theme_minimal()


# 6A.1.4 Assess the assumptions and choose an appropriate test

# Q-Q plot
ggplot(trees, aes(sample = Height)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q Plot of Tree Heights",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

# Shapiro-Wilk normality test
shapiro.test(trees$Height)


# 6A.1.5 Perform the one-sample t-test

t.test(trees$Height, mu = 75)


# 6A.1.7 One-Sample Wilcoxon Signed-Rank Test

# a. Problem: Simulated tree-height dataset

tree_sample <- data.frame(
  Tree_ID = 1:20,
  Height = c(
    35, 36, 37, 38, 39,
    40, 41, 42, 43, 44,
    52, 53, 54, 55, 56,
    57, 58, 59, 60, 61
  )
)

tree_sample

# c. Explore the data

# Histogram
ggplot(tree_sample, aes(x = Height)) +
  geom_histogram(bins = 10, color = "white") +
  labs(
    title = "Distribution of Simulated Tree Heights",
    x = "Tree Height (feet)",
    y = "Number of Trees"
  ) +
  theme_minimal()

# Boxplot
ggplot(tree_sample, aes(x = "", y = Height)) +
  geom_boxplot() +
  labs(
    title = "Simulated Tree Heights",
    x = NULL,
    y = "Tree Height (feet)"
  ) +
  theme_minimal()

# d. Assess normality

# Q-Q plot
ggplot(tree_sample, aes(sample = Height)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q Plot of Simulated Tree Heights",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

# Shapiro-Wilk normality test
shapiro.test(tree_sample$Height)

# e. Perform the One-Sample Wilcoxon Signed-Rank Test

wilcox.test(
  tree_sample$Height,
  mu = 45,
  alternative = "two.sided",
  conf.int = TRUE
)


# =========================================================
# 6A.2 Comparing Paired Samples
# Dataset: sleep
# =========================================================

# 6A.2.2 Load and inspect the data

data("sleep")

head(sleep)
str(sleep)
summary(sleep)
table(sleep$group)


# 6A.2.4 Prepare the paired data

# Convert from long format to wide format
sleep_wide <- sleep %>%
  pivot_wider(
    names_from = group,
    values_from = extra,
    names_prefix = "drug"
  )

head(sleep_wide)

# Calculate the within-subject difference
sleep_wide <- sleep_wide %>%
  mutate(difference = drug1 - drug2)

# View the paired data and check for missing values
sleep_wide
colSums(is.na(sleep_wide))


# 6A.2.5 Explore the differences

# Visualize paired observations
sleep_long <- sleep_wide %>%
  pivot_longer(
    cols = c(drug1, drug2),
    names_to = "Drug",
    values_to = "Sleep_Increase"
  )

ggplot(sleep_long, aes(
  x = Drug,
  y = Sleep_Increase,
  group = ID
)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Paired Sleep Increase Under Two Drugs",
    x = "Drug",
    y = "Increase in Sleep Duration (hours)"
  ) +
  theme_minimal()

# Histogram of paired differences
ggplot(sleep_wide, aes(x = difference)) +
  geom_histogram(bins = 6, color = "white") +
  labs(
    title = "Distribution of Paired Differences in Sleep Increase",
    x = "Difference (Drug 1 - Drug 2, hours)",
    y = "Number of Subjects"
  ) +
  theme_minimal()

# Q-Q plot of paired differences
ggplot(sleep_wide, aes(sample = difference)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q Plot of Paired Differences",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

# Shapiro-Wilk normality test
shapiro.test(sleep_wide$difference)


# 6A.2.6 Perform the Paired t-test

t.test(
  sleep_wide$drug1,
  sleep_wide$drug2,
  paired = TRUE
)


# 6A.2.7 Perform the Wilcoxon Signed-Rank Test

wilcox.test(
  sleep_wide$drug1,
  sleep_wide$drug2,
  paired = TRUE,
  alternative = "two.sided"
)


# =========================================================
# 6A.3 Comparing Means of Two Independent Groups
# Dataset: iris
# =========================================================

# 6A.3.2 Load and inspect the data

data("iris")

head(iris)
str(iris)

# 6A.3.3 Prepare the data

# Select only setosa and versicolor
data_subset <- iris %>%
  filter(Species %in% c("setosa", "versicolor"))

head(data_subset)
table(data_subset$Species)

# Remove unused factor levels
data_subset <- droplevels(data_subset)

str(data_subset)
summary(data_subset)
table(data_subset$Species)


# 6A.3.5 Explore the data

# Histograms
ggplot(data_subset, aes(x = Sepal.Length)) +
  geom_histogram(bins = 10, color = "white") +
  facet_wrap(~ Species) +
  labs(
    title = "Distribution of Sepal Length by Species",
    x = "Sepal Length (cm)",
    y = "Number of Flowers"
  ) +
  theme_minimal()

# Boxplot
ggplot(data_subset, aes(
  x = Species,
  y = Sepal.Length,
  fill = Species
)) +
  geom_boxplot() +
  labs(
    title = "Comparison of Sepal Length Between Species",
    x = "Species",
    y = "Sepal Length (cm)"
  ) +
  theme_minimal() +
  guides(fill = "none")

# Descriptive statistics
data_subset %>%
  group_by(Species) %>%
  summarise(
    n = n(),
    mean_length = mean(Sepal.Length),
    sd_length = sd(Sepal.Length)
  )


# 6A.3.6 Assess the assumptions

# Q-Q plots for each group
ggplot(data_subset, aes(sample = Sepal.Length)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Species) +
  labs(
    title = "Normal Q-Q Plots of Sepal Length by Species",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

# Shapiro-Wilk normality tests
by(
  data_subset$Sepal.Length,
  data_subset$Species,
  shapiro.test
)

# Levene's test for equality of variances
leveneTest(
  Sepal.Length ~ Species,
  data = data_subset
)


# 6A.3.7 Perform Welch's Two-Sample t-test

t.test(
  Sepal.Length ~ Species,
  data = data_subset
)


# 6A.3.8 Demonstrate the Mann-Whitney U Test

wilcox.test(
  Sepal.Length ~ Species,
  data = data_subset,
  alternative = "two.sided"
)


# =========================================================
# 6A.4 Practice Activities
# =========================================================


# =========================================================
# 6A.4.1 One-Sample t-test Using airquality
# Question: Is the mean Ozone level different from 30 ppb?
# =========================================================

# 1. Inspect and prepare the data

data("airquality")

head(airquality)
str(airquality)
summary(airquality$Ozone)

# Missing and available ozone measurements
sum(is.na(airquality$Ozone))
sum(!is.na(airquality$Ozone))

# Keep only observations with recorded ozone values
ozone_data <- airquality %>%
  filter(!is.na(Ozone))

nrow(ozone_data)


# 2. Explore the distribution

# Histogram
ggplot(ozone_data, aes(x = Ozone)) +
  geom_histogram(bins = 15, color = "white") +
  labs(
    title = "Distribution of Daily Ozone Concentrations",
    x = "Ozone (ppb)",
    y = "Number of Days"
  ) +
  theme_minimal()

# Boxplot
ggplot(ozone_data, aes(x = "", y = Ozone)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Ozone Levels",
    x = NULL,
    y = "Ozone (ppb)"
  ) +
  theme_minimal()


# 4. Assess the assumptions

# Q-Q plot
ggplot(ozone_data, aes(sample = Ozone)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q Plot of Ozone",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

# Shapiro-Wilk normality test
shapiro.test(ozone_data$Ozone)


# 5. Conduct and report the analysis

# One-sample t-test (for comparison)
t.test(ozone_data$Ozone, mu = 30)

# Wilcoxon signed-rank test
wilcox.test(
  ozone_data$Ozone,
  mu = 30,
  alternative = "two.sided",
  conf.int = TRUE
)


# =========================================================
# 6A.4.2 Paired t-test Using ChickWeight
# Compare chick weights on Day 10 and Day 21.
# =========================================================

# 1. Load and inspect the data

data("ChickWeight")

head(ChickWeight)
str(ChickWeight)
summary(ChickWeight)

# 2. Prepare complete pairs

chick <- as.data.frame(ChickWeight)

# Weights at Day 10 and Day 21 in wide format
chick_wide <- chick %>%
  filter(Time %in% c(10, 21)) %>%
  select(Chick, Time, weight) %>%
  pivot_wider(
    names_from = Time,
    values_from = weight,
    names_prefix = "Day"
  )

head(chick_wide)
nrow(chick_wide)

# Identify chicks without a recorded weight at both time points
colSums(is.na(chick_wide))
chick_wide %>%
  filter(is.na(Day10) | is.na(Day21))

# Remove incomplete pairs, then calculate the difference
# Difference = Day 21 - Day 10
chick_wide <- chick_wide %>%
  filter(!is.na(Day10) & !is.na(Day21)) %>%
  mutate(difference = Day21 - Day10)

# Check the complete pairs and differences
nrow(chick_wide)
head(chick_wide)
str(chick_wide)
summary(chick_wide$difference)

# Descriptive statistics
chick_wide %>%
  summarise(
    n = n(),
    mean_day10 = mean(Day10),
    sd_day10 = sd(Day10),
    mean_day21 = mean(Day21),
    sd_day21 = sd(Day21),
    mean_difference = mean(difference),
    sd_difference = sd(difference)
  )


# 3. Explore the data

# Visualize paired chick weights
chick_long <- chick_wide %>%
  pivot_longer(
    cols = c(Day10, Day21),
    names_to = "Day",
    values_to = "Weight"
  )

ggplot(chick_long, aes(
  x = Day,
  y = Weight,
  group = Chick
)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Chick Weights on Day 10 and Day 21",
    x = "Day",
    y = "Weight (grams)"
  ) +
  theme_minimal()

# Histogram of paired differences
ggplot(chick_wide, aes(x = difference)) +
  geom_histogram(bins = 15, color = "white") +
  labs(
    title = "Distribution of Weight Differences",
    x = "Weight Difference (Day 21 - Day 10)",
    y = "Frequency"
  ) +
  theme_minimal()


# 5. Assess the assumptions

# Q-Q plot of paired differences
ggplot(chick_wide, aes(sample = difference)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q Plot of Paired Weight Differences",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

# Shapiro-Wilk normality test
shapiro.test(chick_wide$difference)


# 6. Conduct the appropriate test

# Paired t-test (if the Shapiro-Wilk p-value is 0.05 or greater)
t.test(
  chick_wide$Day21,
  chick_wide$Day10,
  paired = TRUE
)

# Wilcoxon signed-rank test (if the Shapiro-Wilk p-value is below 0.05)
wilcox.test(
  chick_wide$Day21,
  chick_wide$Day10,
  paired = TRUE,
  conf.int = TRUE
)


# =========================================================
# 6A.4.3 Independent t-test Using PlantGrowth
# Compare the control group and treatment 1.
# =========================================================

# 1. Load and inspect the data

data("PlantGrowth")

head(PlantGrowth)
str(PlantGrowth)
summary(PlantGrowth)

# 2. Select the two groups

plant_subset <- PlantGrowth %>%
  filter(group %in% c("ctrl", "trt1"))

plant_subset <- droplevels(plant_subset)

head(plant_subset)
str(plant_subset)
summary(plant_subset)
table(plant_subset$group)


# 4. Explore the data

# Descriptive statistics
plant_subset %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean_weight = mean(weight),
    sd_weight = sd(weight)
  )

# Boxplot
ggplot(plant_subset, aes(
  x = group,
  y = weight,
  fill = group
)) +
  geom_boxplot() +
  labs(
    title = "Plant Dry Weight: Control vs Treatment 1",
    x = "Group",
    y = "Dry Weight"
  ) +
  theme_minimal() +
  guides(fill = "none")


# 5. Assess the assumptions

# Q-Q plots by group
ggplot(plant_subset, aes(sample = weight)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ group) +
  labs(
    title = "Normal Q-Q Plots of Plant Weight",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

# Shapiro-Wilk normality tests
by(
  plant_subset$weight,
  plant_subset$group,
  shapiro.test
)

# Levene's test
leveneTest(
  weight ~ group,
  data = plant_subset
)


# 6. Conduct the appropriate test

# Welch's independent two-sample t-test
t.test(
  weight ~ group,
  data = plant_subset
)

# Wilcoxon rank-sum test (for comparison)
wilcox.test(
  weight ~ group,
  data = plant_subset
)