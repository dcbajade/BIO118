## Name: Denisse Gabrielle C. Bajade

# =========================================================
# BIO 118 Laboratory Exercise 6B
# Comparing Groups: Three or More Groups and Two-Factor Comparisons
# =========================================================

# Load required packages
library(tidyverse)
library(car)

# =========================================================
# 6B.1 Comparing Means of More than Two Groups
# Dataset: iris
# =========================================================

# 6B.1.1 Load and inspect the data

data("iris")

head(iris)
str(iris)
summary(iris)
table(iris$Species)


# 6B.1.2 Explore the data

# Histograms by species
ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(bins = 10, color = "white") +
  facet_wrap(~ Species) +
  labs(
    title = "Distribution of Sepal Length by Species",
    x = "Sepal Length (cm)",
    y = "Number of Flowers"
  ) +
  theme_minimal()

# Boxplot
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot() +
  labs(
    title = "Sepal Length Across Iris Species",
    x = "Species",
    y = "Sepal Length (cm)"
  ) +
  theme_minimal()

# Descriptive statistics
iris %>%
  group_by(Species) %>%
  summarise(
    n = n(),
    mean_length = mean(Sepal.Length),
    sd_length = sd(Sepal.Length)
  )


# 6B.1.3 State the hypotheses

# H0: The mean sepal length is equal among the three iris species.
# HA: At least one iris species has a different mean sepal length.
# Significance level: alpha = 0.05


# 6B.1.4 Assess the assumptions and choose an appropriate test

# A. Normality: Q-Q plots
ggplot(iris, aes(sample = Sepal.Length)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Species) +
  labs(
    title = "Normal Q-Q Plots of Sepal Length",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

# Shapiro-Wilk test for each species
by(
  iris$Sepal.Length,
  iris$Species,
  shapiro.test
)

# B. Equality of variances: Levene's test
leveneTest(
  Sepal.Length ~ Species,
  data = iris
)


# 6B.1.5 Perform one-way ANOVA

anova_result <- aov(
  Sepal.Length ~ Species,
  data = iris
)

summary(anova_result)


# 6B.1.6 Pairwise comparisons using Tukey's HSD

TukeyHSD(anova_result)


# 6B.1.7 Perform the Kruskal-Wallis test

kruskal.test(
  Sepal.Length ~ Species,
  data = iris
)


# 6B.1.8 Pairwise comparisons after Kruskal-Wallis

pairwise.wilcox.test(
  iris$Sepal.Length,
  iris$Species,
  p.adjust.method = "bonferroni"
)


# =========================================================
# 6B.2 Comparing Means Across Two Factors
# Dataset: ToothGrowth
# =========================================================

# 6B.2.1 Research question

# Does supplement type, vitamin C dose, or their interaction
# significantly affect tooth length?


# 6B.2.2 Load and inspect the data

data("ToothGrowth")

head(ToothGrowth)
str(ToothGrowth)
summary(ToothGrowth)
nrow(ToothGrowth)
table(ToothGrowth$supp, ToothGrowth$dose)

# Convert dose to a categorical variable
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
str(ToothGrowth)


# 6B.2.3 State the hypotheses

# Supplement effect:
# H0: Mean tooth length does not differ between supplement types.
# HA: Mean tooth length differs between supplement types.

# Dose effect:
# H0: Mean tooth length does not differ among dose levels.
# HA: At least one dose level has a different mean tooth length.

# Interaction effect:
# H0: There is no interaction between supplement type and dose.
# HA: There is an interaction between supplement type and dose.

# Significance level: alpha = 0.05


# 6B.2.4 Explore the data

# Summarize the observations
ToothGrowth_summary <- ToothGrowth %>%
  group_by(supp, dose) %>%
  summarise(
    n = n(),
    mean_length = mean(len),
    sd_length = sd(len),
    .groups = "drop"
  )

ToothGrowth_summary

# Boxplot
ggplot(ToothGrowth, aes(x = dose, y = len, fill = supp)) +
  geom_boxplot() +
  labs(
    title = "Tooth Length by Supplement Type and Dose",
    x = "Vitamin C Dose (mg/day)",
    y = "Tooth Length",
    fill = "Supplement Type"
  ) +
  theme_minimal()

# Interaction plot
ggplot(ToothGrowth_summary, aes(
  x = dose,
  y = mean_length,
  color = supp,
  group = supp
)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Interaction Between Supplement Type and Dose",
    x = "Vitamin C Dose (mg/day)",
    y = "Mean Tooth Length",
    color = "Supplement Type"
  ) +
  theme_minimal()


# 6B.2.5 Assess the assumptions

# A. Fit the two-way ANOVA model
tooth_aov <- aov(len ~ supp * dose, data = ToothGrowth)
summary(tooth_aov)

# B. Examine the residuals
hist(
  residuals(tooth_aov),
  main = "Histogram of ANOVA Residuals",
  xlab = "Residuals"
)

qqnorm(residuals(tooth_aov))
qqline(residuals(tooth_aov))

shapiro.test(residuals(tooth_aov))

# C. Assess equality of variances
leveneTest(len ~ supp * dose, data = ToothGrowth)


# 6B.2.6 Perform two-way ANOVA

summary(tooth_aov)


# 6B.2.7 Conduct follow-up comparisons

# Compare supplement types
TukeyHSD(tooth_aov, which = "supp")

# Compare dose levels
TukeyHSD(tooth_aov, which = "dose")


# =========================================================
# 6B.3 Practice Activities
# =========================================================


# =========================================================
# 6B.3.1 One-Way ANOVA Using airquality
# Question: Does ozone concentration differ among the five months?
# =========================================================

# 1. Inspect and prepare the data

data("airquality")

head(airquality)
str(airquality)

# Remove observations with missing ozone measurements
ozone_month <- airquality %>%
  filter(!is.na(Ozone))

# Convert Month into a factor
ozone_month <- ozone_month %>%
  mutate(
    Month = factor(
      Month,
      levels = 5:9,
      labels = c("May", "June", "July", "August", "September")
    )
  )

str(ozone_month)
nrow(ozone_month)
table(ozone_month$Month)


# 2. Explore the distribution

# Boxplot
ggplot(ozone_month, aes(x = Month, y = Ozone)) +
  geom_boxplot() +
  labs(
    title = "Ozone Concentration by Month",
    x = "Month",
    y = "Ozone (ppb)"
  ) +
  theme_minimal()

# Descriptive statistics
ozone_month %>%
  group_by(Month) %>%
  summarise(
    n = n(),
    mean_ozone = mean(Ozone),
    sd_ozone = sd(Ozone)
  )


# 3. State the hypotheses

# H0: Mean ozone concentration is equal across the five months.
# HA: At least one month has a different mean ozone concentration.
# Significance level: alpha = 0.05


# 4. Assess the assumptions

# Histograms by month
ggplot(ozone_month, aes(x = Ozone)) +
  geom_histogram(bins = 10, color = "white") +
  facet_wrap(~ Month) +
  labs(
    title = "Distribution of Ozone by Month",
    x = "Ozone (ppb)",
    y = "Number of Days"
  ) +
  theme_minimal()

# Q-Q plots by month
ggplot(ozone_month, aes(sample = Ozone)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Month) +
  labs(
    title = "Normal Q-Q Plots of Ozone by Month",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

# Shapiro-Wilk test for each month
by(
  ozone_month$Ozone,
  ozone_month$Month,
  shapiro.test
)

# Levene's test
leveneTest(
  Ozone ~ Month,
  data = ozone_month
)


# 5. Conduct and report the analysis

# One-way ANOVA with Tukey's HSD (for comparison)
ozone_aov <- aov(Ozone ~ Month, data = ozone_month)
summary(ozone_aov)
TukeyHSD(ozone_aov)

# Kruskal-Wallis test with pairwise Wilcoxon tests (Bonferroni)
kruskal.test(Ozone ~ Month, data = ozone_month)

pairwise.wilcox.test(
  ozone_month$Ozone,
  ozone_month$Month,
  p.adjust.method = "bonferroni"
)


# =========================================================
# 6B.3.2 Two-Way ANOVA Using Simulated Plant Growth Data
# Question: Do fertilizer type, irrigation level, and their interaction
# affect plant growth?
# =========================================================

# 1. Generate the simulated dataset

# Set the random seed for reproducibility
set.seed(123)

# Create simulated data
n <- 60

fertilizer <- rep(
  c("Fertilizer A", "Fertilizer B", "Fertilizer C"),
  each = n / 3
)

irrigation <- rep(
  rep(c("Low", "High"), each = n / 6),
  3
)

plant_growth <- rnorm(n, mean = 5, sd = 1) +
  ifelse(
    fertilizer == "Fertilizer B", 0.5,
    ifelse(fertilizer == "Fertilizer C", 1, 0)
  ) +
  ifelse(irrigation == "High", 1, 0)

# Create the data frame
plant_data <- data.frame(
  Fertilizer = fertilizer,
  Irrigation = irrigation,
  Plant_Growth = plant_growth
)

# Convert explanatory variables to factors
plant_data$Fertilizer <- as.factor(plant_data$Fertilizer)
plant_data$Irrigation <- as.factor(plant_data$Irrigation)


# 2. Load and inspect the data

head(plant_data)
str(plant_data)
table(plant_data$Fertilizer, plant_data$Irrigation)


# 3. Explore the data

# a. Descriptive statistics for each combination
plant_summary <- plant_data %>%
  group_by(Fertilizer, Irrigation) %>%
  summarise(
    n = n(),
    mean_growth = mean(Plant_Growth),
    sd_growth = sd(Plant_Growth),
    .groups = "drop"
  )

plant_summary

# b. Boxplot
ggplot(plant_data, aes(x = Fertilizer, y = Plant_Growth, fill = Irrigation)) +
  geom_boxplot() +
  labs(
    title = "Plant Growth by Fertilizer Type and Irrigation Level",
    x = "Fertilizer Type",
    y = "Plant Growth (cm)",
    fill = "Irrigation Level"
  ) +
  theme_minimal()

# c. Interaction plot
ggplot(plant_summary, aes(
  x = Irrigation,
  y = mean_growth,
  color = Fertilizer,
  group = Fertilizer
)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Interaction Between Fertilizer Type and Irrigation Level",
    x = "Irrigation Level",
    y = "Mean Plant Growth (cm)",
    color = "Fertilizer Type"
  ) +
  theme_minimal()


# 4. State the hypotheses

# Fertilizer effect:
# H0: Mean plant growth does not differ among fertilizer types.
# HA: At least one fertilizer type has a different mean plant growth.

# Irrigation effect:
# H0: Mean plant growth does not differ between irrigation levels.
# HA: Mean plant growth differs between irrigation levels.

# Interaction effect:
# H0: There is no interaction between fertilizer type and irrigation level.
# HA: There is an interaction between fertilizer type and irrigation level.

# Significance level: alpha = 0.05


# 5. Assess the assumptions

# a. Fit the two-way ANOVA model
plant_aov <- aov(Plant_Growth ~ Fertilizer * Irrigation, data = plant_data)

# b. Examine the residuals
hist(
  residuals(plant_aov),
  main = "Histogram of ANOVA Residuals",
  xlab = "Residuals"
)

qqnorm(residuals(plant_aov))
qqline(residuals(plant_aov))

# c. Shapiro-Wilk test on the residuals
shapiro.test(residuals(plant_aov))

# d. Levene's test across the six treatment combinations
leveneTest(Plant_Growth ~ Fertilizer * Irrigation, data = plant_data)


# 6. Perform the statistical analysis

summary(plant_aov)

# Follow-up comparisons
TukeyHSD(plant_aov, which = "Fertilizer")
TukeyHSD(plant_aov, which = "Irrigation")
