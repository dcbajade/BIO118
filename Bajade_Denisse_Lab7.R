# Name: Denisse Gabrielle C. Bajade

# =========================================================
# BIO 118 Laboratory Exercise 7
# Correlation and Simple Linear Regression
# =========================================================


# =========================================================
# 7.1 CORRELATION ANALYSIS: CRICKETS
# =========================================================

# 7.1.1 Loading and Exploring the Data

install.packages("abd")
library(abd)
library(ggplot2)
library(dplyr)

data("Crickets")

head(Crickets)
str(Crickets)


# 7.1.2 Visualizing the Relationship

ggplot(Crickets, aes(x = sperm.viability, y = lysozyme)) +
  geom_point() +
  labs(
    title = "Sperm Viability and Lysozyme Activity in Crickets",
    x = "Sperm Viability",
    y = "Lysozyme Activity"
  ) +
  theme_minimal()


# 7.1.3 Pearson and Spearman Correlation

cor_pearson <- cor.test(
  Crickets$sperm.viability,
  Crickets$lysozyme,
  method = "pearson"
)

cor_pearson


cor_spearman <- cor.test(
  Crickets$sperm.viability,
  Crickets$lysozyme,
  method = "spearman",
  exact = FALSE
)

cor_spearman


# =========================================================
# 7.2 SIMPLE LINEAR REGRESSION: RATTLESNAKES
# =========================================================

# 7.2.1 Loading and Exploring the Data

data("Rattlesnakes")

head(Rattlesnakes)
str(Rattlesnakes)

colSums(is.na(Rattlesnakes))


# 7.2.2 Visualizing the Relationship

ggplot(Rattlesnakes, aes(x = meal.size, y = temp.change)) +
  geom_point() +
  labs(
    title = "Meal Size and Temperature Change in Rattlesnakes",
    x = "Meal Size",
    y = "Temperature Change"
  ) +
  theme_minimal()


# 7.2.3 Fitting the Linear Regression Model

model <- lm(temp.change ~ meal.size, data = Rattlesnakes)

Rattlesnakes$fitted <- fitted(model)
Rattlesnakes$residuals <- residuals(model)

model


# 7.2.4 Evaluating the Regression Model

summary(model)


# 7.2.5 Visualizing the Regression Line

ggplot(Rattlesnakes, aes(x = meal.size, y = temp.change)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Linear Regression of Meal Size and Temperature Change",
    x = "Meal Size",
    y = "Temperature Change"
  ) +
  theme_minimal()


# 7.2.6 Checking Model Assumptions

head(Rattlesnakes)

ggplot(Rattlesnakes, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

plot(model, which = 2)


# 7.2.7 Predicting Temperature Change

range(Rattlesnakes$meal.size)

new_data <- data.frame(meal.size = 10)

predict(
  model,
  newdata = new_data,
  interval = "confidence"
)

predict(
  model,
  newdata = new_data,
  interval = "prediction"
)


# =========================================================
# 7.3 PRACTICE: TEMPERATURE AND OZONE CONCENTRATION
# =========================================================

# 7.3.1 Loading and Preparing the Data

data("airquality")

airquality_clean <- airquality %>%
  select(Ozone, Temp) %>%
  filter(!is.na(Ozone), !is.na(Temp))

head(airquality_clean)
str(airquality_clean)


# 7.3.2 Visualizing the Relationship

ggplot(airquality_clean, aes(x = Temp, y = Ozone)) +
  geom_point() +
  labs(
    title = "Temperature and Ozone Concentration",
    x = "Temperature (°F)",
    y = "Ozone Concentration"
  ) +
  theme_minimal()


# 7.3.3 Pearson Correlation

cor_airquality <- cor.test(
  airquality_clean$Temp,
  airquality_clean$Ozone,
  method = "pearson"
)

cor_airquality


# 7.3.4 Simple Linear Regression

ozone_model <- lm(Ozone ~ Temp, data = airquality_clean)

summary(ozone_model)


# Visualizing the Regression Line

ggplot(airquality_clean, aes(x = Temp, y = Ozone)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Linear Regression of Temperature and Ozone Concentration",
    x = "Temperature (°F)",
    y = "Ozone Concentration"
  ) +
  theme_minimal()


# 7.3.5 Checking Model Assumptions

plot(ozone_model, which = 1)

plot(ozone_model, which = 2)


# 7.3.6 Predicting Ozone Concentration at 80°F

new_temp <- data.frame(Temp = 80)

predict(
  ozone_model,
  newdata = new_temp,
  interval = "prediction"
)
