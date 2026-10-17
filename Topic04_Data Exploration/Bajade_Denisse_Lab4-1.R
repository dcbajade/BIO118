## Name: Denisse Gabrielle C. Bajade

# =========================================================
# BIO 118 Laboratory Exercise 4
# 4A Data Visualization Using ggplot2
# =========================================================

# =========================================================
# Install and Load ggplot2
# =========================================================

install.packages("ggplot2")
library(ggplot2)

# Explore built-in datasets
head(iris)
str(iris)
head(airquality)
str(airquality)

# 4.4 Creating a basic ggplot2 visualization
# Initialize plot
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))

# 4.5 Adding a geometry
# 4.5.1 Scatter plot: geom_point()
# Basic scatter plot
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

# Scatter plot colored by species
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(
    title = "Sepal Length and Sepal Width",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)",
    color = "Iris Species"
  )

# Line plot: temperature across days
ggplot(airquality, aes(x = Day, y = Temp)) +
  geom_line() +
  labs(
    title = "Daily Temperature in New York",
    x = "Day of the Month",
    y = "Temperature (°F)"
  ) +
  theme_classic()

# Line plot grouped by month
ggplot(
  airquality,
  aes(x = Day, y = Temp, group = factor(Month), color = factor(Month))
) +
  geom_line() +
  labs(
    title = "Daily Temperature by Month",
    x = "Day of the Month",
    y = "Temperature (°F)",
    color = "Month"
  ) +
  theme_classic()

# Bar plot: counts by species
ggplot(iris[25:130, ], aes(x = Species, fill = Species)) +
  geom_bar() +
  labs(
    title = "Number of Iris Observations by Species",
    x = "Species",
    y = "Count",
    fill = "Species"
  ) +
  theme_classic()

# Histogram: sepal length
ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(
    title = "Distribution of Iris Sepal Length",
    x = "Sepal Length (cm)",
    y = "Frequency"
  ) +
  theme_classic()

# Box plot: sepal length by species
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(
    title = "Sepal Length by Iris Species",
    x = "Species",
    y = "Sepal Length (cm)",
    fill = "Species"
  ) +
  theme_classic()

# Bar plot using existing values: mean petal length
mean_petal <- aggregate(Petal.Length ~ Species, data = iris, FUN = mean)
print(mean_petal)

ggplot(mean_petal, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_col() +
  labs(
    title = "Mean Petal Length by Iris Species",
    x = "Species",
    y = "Mean Petal Length (cm)",
    fill = "Species"
  ) +
  theme_classic()

# 4.5.7 Adding a trend line: geom_smooth()
# Scatter plot with a linear trend line
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Sepal Length and Sepal Width with Linear Trends",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)",
    color = "Iris Species"
  ) +
  theme_classic()

# 4.5.8 Heatmaps: geom_tile()
# Heatmap using geom_tile()
temperature_data <- data.frame(
  Site = rep(c("Site A", "Site B", "Site C"), each = 3),
  Month = rep(c("June", "July", "August"), times = 3),
  Temperature = c(27.2, 28.1, 28.5, 26.8, 27.5, 28.0, 29.0, 29.4, 30.1)
)
print(temperature_data)

ggplot(temperature_data, aes(x = Site, y = Month, fill = Temperature)) +
  geom_tile() +
  labs(
    title = "Water Temperature by Site and Month",
    x = "Sampling Site",
    y = "Month",
    fill = "Temperature (°C)"
  ) +
  theme_minimal()

# 4.6 Customizing a plot
# 4.6.1 labs(): titles and labels
# Annotation
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  annotate("text", x = 6, y = 2.5, label = "Large petal dimensions") +
  labs(
    title = "Petal Length and Petal Width",
    x = "Petal Length (cm)",
    y = "Petal Width (cm)",
    color = "Iris Species"
  ) +
  theme_classic()

# 4.6.3 Faceting: facet_wrap()
# Faceting
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  facet_wrap(~ Species) +
  labs(
    title = "Sepal Measurements by Iris Species",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)"
  ) +
  theme_minimal()

# 4.6.4 Applying simple themes
# Compare themes
theme_plot <- ggplot(
  iris,
  aes(x = Sepal.Length, y = Sepal.Width, color = Species)
) +
  geom_point() +
  labs(
    title = "Sepal Length and Sepal Width",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)",
    color = "Species"
  )
print(theme_plot + theme_minimal())
print(theme_plot + theme_classic())
print(theme_plot + theme_bw())
print(theme_plot + theme_void())

# 4.6.5 Combining multiple layers
# Combine layers
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Sepal Length and Sepal Width",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)",
    color = "Iris Species"
  ) +
  theme_classic()

# 4.7 Saving a plot is demonstrated in Practice Exercise 9 below.

# 4.8 Practice Exercises: airquality
# Practice Exercise 1: Wind vs Ozone
wind_ozone_plot <- ggplot(
  airquality,
  aes(x = Wind, y = Ozone, color = factor(Month))
) +
  geom_point(na.rm = TRUE) +
  labs(
    title = "Relationship Between Wind Speed and Ozone",
    x = "Wind Speed (mph)",
    y = "Ozone Concentration (ppb)",
    color = "Month"
  ) +
  theme_classic()
print(wind_ozone_plot)

# Practice Exercise 2: Ozone across days by month
ggplot(
  airquality,
  aes(
    x = Day,
    y = Ozone,
    group = factor(Month),
    color = factor(Month)
  )
) +
  geom_line(na.rm = TRUE) +
  labs(
    title = "Daily Ozone Levels by Month",
    x = "Day of the Month",
    y = "Ozone Concentration (ppb)",
    color = "Month"
  ) +
  theme_classic()

# Practice Exercise 3: Wind histogram
wind_histogram <- ggplot(airquality, aes(x = Wind)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black", na.rm = TRUE) +
  labs(
    title = "Distribution of Wind Speed",
    x = "Wind Speed (mph)",
    y = "Frequency"
  ) +
  theme_classic()
print(wind_histogram)

# Practice Exercise 4: Temperature by month
ggplot(
  airquality,
  aes(x = factor(Month), y = Temp, fill = factor(Month))
) +
  geom_boxplot(na.rm = TRUE) +
  labs(
    title = "Temperature Distribution by Month",
    x = "Month",
    y = "Temperature (°F)",
    fill = "Month"
  ) +
  theme_classic()

# Practice Exercise 5: Wind vs Temp with linear trend
ggplot(airquality, aes(x = Wind, y = Temp)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(
    title = "Relationship Between Wind Speed and Temperature",
    x = "Wind Speed (mph)",
    y = "Temperature (°F)"
  ) +
  theme_classic()

# Practice Exercise 6: Faceted Wind vs Ozone
ggplot(airquality, aes(x = Wind, y = Ozone)) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~ Month) +
  labs(
    title = "Wind Speed and Ozone by Month",
    x = "Wind Speed (mph)",
    y = "Ozone Concentration (ppb)"
  ) +
  theme_bw()

# Practice Exercise 7: Mean temperature by month
mean_temp <- aggregate(Temp ~ Month, data = airquality, FUN = mean, na.rm = TRUE)
mean_temp$Month <- factor(mean_temp$Month)
print(mean_temp)

ggplot(mean_temp, aes(x = Month, y = Temp, fill = Month)) +
  geom_col() +
  labs(
    title = "Mean Temperature by Month",
    x = "Month",
    y = "Mean Temperature (°F)",
    fill = "Month"
  ) +
  theme_classic()

# Practice Exercise 8: Heatmap
ggplot(airquality, aes(x = Day, y = factor(Month), fill = Temp)) +
  geom_tile() +
  labs(
    title = "Daily Temperature Across Months",
    x = "Day of the Month",
    y = "Month",
    fill = "Temperature (°F)"
  ) +
  theme_bw()

highest_temp <- airquality[which.max(airquality$Temp), c("Month", "Day", "Temp")]
print(highest_temp)

# Practice Exercise 9: save the selected plot as JPEG

ggsave(
  filename = "Bajade_Denisse_Plot.jpeg",
  plot = wind_ozone_plot,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

# ============================================================
# LABORATORY EXERCISE 4A: DATA VISUALIZATION USING GGPLOT2
# ============================================================

# Explore built-in datasets
head(iris)
str(iris)
head(airquality)
str(airquality)

# 4.4 Creating a basic ggplot2 visualization
# Initialize plot
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))

# 4.5 Adding a geometry
# 4.5.1 Scatter plot: geom_point()
# Basic scatter plot
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

# Scatter plot colored by species
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(
    title = "Sepal Length and Sepal Width",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)",
    color = "Iris Species"
  )

# Line plot: temperature across days
ggplot(airquality, aes(x = Day, y = Temp)) +
  geom_line() +
  labs(
    title = "Daily Temperature in New York",
    x = "Day of the Month",
    y = "Temperature (°F)"
  ) +
  theme_classic()

# Line plot grouped by month
ggplot(
  airquality,
  aes(x = Day, y = Temp, group = factor(Month), color = factor(Month))
) +
  geom_line() +
  labs(
    title = "Daily Temperature by Month",
    x = "Day of the Month",
    y = "Temperature (°F)",
    color = "Month"
  ) +
  theme_classic()

# Bar plot: counts by species
ggplot(iris[25:130, ], aes(x = Species, fill = Species)) +
  geom_bar() +
  labs(
    title = "Number of Iris Observations by Species",
    x = "Species",
    y = "Count",
    fill = "Species"
  ) +
  theme_classic()

# Histogram: sepal length
ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(
    title = "Distribution of Iris Sepal Length",
    x = "Sepal Length (cm)",
    y = "Frequency"
  ) +
  theme_classic()

# Box plot: sepal length by species
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(
    title = "Sepal Length by Iris Species",
    x = "Species",
    y = "Sepal Length (cm)",
    fill = "Species"
  ) +
  theme_classic()

# Bar plot using existing values: mean petal length
mean_petal <- aggregate(Petal.Length ~ Species, data = iris, FUN = mean)
print(mean_petal)

ggplot(mean_petal, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_col() +
  labs(
    title = "Mean Petal Length by Iris Species",
    x = "Species",
    y = "Mean Petal Length (cm)",
    fill = "Species"
  ) +
  theme_classic()

# 4.5.7 Adding a trend line: geom_smooth()
# Scatter plot with a linear trend line
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Sepal Length and Sepal Width with Linear Trends",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)",
    color = "Iris Species"
  ) +
  theme_classic()

# 4.5.8 Heatmaps: geom_tile()
# Heatmap using geom_tile()
temperature_data <- data.frame(
  Site = rep(c("Site A", "Site B", "Site C"), each = 3),
  Month = rep(c("June", "July", "August"), times = 3),
  Temperature = c(27.2, 28.1, 28.5, 26.8, 27.5, 28.0, 29.0, 29.4, 30.1)
)
print(temperature_data)

ggplot(temperature_data, aes(x = Site, y = Month, fill = Temperature)) +
  geom_tile() +
  labs(
    title = "Water Temperature by Site and Month",
    x = "Sampling Site",
    y = "Month",
    fill = "Temperature (°C)"
  ) +
  theme_minimal()

# 4.6 Customizing a plot
# 4.6.1 labs(): titles and labels
# Annotation
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  annotate("text", x = 6, y = 2.5, label = "Large petal dimensions") +
  labs(
    title = "Petal Length and Petal Width",
    x = "Petal Length (cm)",
    y = "Petal Width (cm)",
    color = "Iris Species"
  ) +
  theme_classic()

# 4.6.3 Faceting: facet_wrap()
# Faceting
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  facet_wrap(~ Species) +
  labs(
    title = "Sepal Measurements by Iris Species",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)"
  ) +
  theme_minimal()

# 4.6.4 Applying simple themes
# Compare themes
theme_plot <- ggplot(
  iris,
  aes(x = Sepal.Length, y = Sepal.Width, color = Species)
) +
  geom_point() +
  labs(
    title = "Sepal Length and Sepal Width",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)",
    color = "Species"
  )
print(theme_plot + theme_minimal())
print(theme_plot + theme_classic())
print(theme_plot + theme_bw())
print(theme_plot + theme_void())

# 4.6.5 Combining multiple layers
# Combine layers
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Sepal Length and Sepal Width",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)",
    color = "Iris Species"
  ) +
  theme_classic()

# 4.7 Saving a plot is demonstrated in Practice Exercise 9 below.

# 4.8 Practice Exercises: airquality
# Practice Exercise 1: Wind vs Ozone
wind_ozone_plot <- ggplot(
  airquality,
  aes(x = Wind, y = Ozone, color = factor(Month))
) +
  geom_point(na.rm = TRUE) +
  labs(
    title = "Relationship Between Wind Speed and Ozone",
    x = "Wind Speed (mph)",
    y = "Ozone Concentration (ppb)",
    color = "Month"
  ) +
  theme_classic()
print(wind_ozone_plot)

# Practice Exercise 2: Ozone across days by month
ggplot(
  airquality,
  aes(
    x = Day,
    y = Ozone,
    group = factor(Month),
    color = factor(Month)
  )
) +
  geom_line(na.rm = TRUE) +
  labs(
    title = "Daily Ozone Levels by Month",
    x = "Day of the Month",
    y = "Ozone Concentration (ppb)",
    color = "Month"
  ) +
  theme_classic()

# Practice Exercise 3: Wind histogram
wind_histogram <- ggplot(airquality, aes(x = Wind)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black", na.rm = TRUE) +
  labs(
    title = "Distribution of Wind Speed",
    x = "Wind Speed (mph)",
    y = "Frequency"
  ) +
  theme_classic()
print(wind_histogram)

# Practice Exercise 4: Temperature by month
ggplot(
  airquality,
  aes(x = factor(Month), y = Temp, fill = factor(Month))
) +
  geom_boxplot(na.rm = TRUE) +
  labs(
    title = "Temperature Distribution by Month",
    x = "Month",
    y = "Temperature (°F)",
    fill = "Month"
  ) +
  theme_classic()

# Practice Exercise 5: Wind vs Temp with linear trend
ggplot(airquality, aes(x = Wind, y = Temp)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(
    title = "Relationship Between Wind Speed and Temperature",
    x = "Wind Speed (mph)",
    y = "Temperature (°F)"
  ) +
  theme_classic()

# Practice Exercise 6: Faceted Wind vs Ozone
ggplot(airquality, aes(x = Wind, y = Ozone)) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~ Month) +
  labs(
    title = "Wind Speed and Ozone by Month",
    x = "Wind Speed (mph)",
    y = "Ozone Concentration (ppb)"
  ) +
  theme_bw()

# Practice Exercise 7: Mean temperature by month
mean_temp <- aggregate(Temp ~ Month, data = airquality, FUN = mean, na.rm = TRUE)
mean_temp$Month <- factor(mean_temp$Month)
print(mean_temp)

ggplot(mean_temp, aes(x = Month, y = Temp, fill = Month)) +
  geom_col() +
  labs(
    title = "Mean Temperature by Month",
    x = "Month",
    y = "Mean Temperature (°F)",
    fill = "Month"
  ) +
  theme_classic()

# Practice Exercise 8: Heatmap
ggplot(airquality, aes(x = Day, y = factor(Month), fill = Temp)) +
  geom_tile() +
  labs(
    title = "Daily Temperature Across Months",
    x = "Day of the Month",
    y = "Month",
    fill = "Temperature (°F)"
  ) +
  theme_bw()

highest_temp <- airquality[which.max(airquality$Temp), c("Month", "Day", "Temp")]
print(highest_temp)

# Practice Exercise 9: save the selected plot as JPEG

ggsave(
  filename = "Bajade_Denisse_Plot.jpeg",
  plot = wind_ozone_plot,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

# ============================================================
# LABORATORY EXERCISE 4B: EXPLORATORY DATA ANALYSIS
# ============================================================

# Load and inspect the WHO dataset included with tidyr
data("who", package = "tidyr")
glimpse(who)
head(who)
dim(who)

# Missing values by column
missing_by_column <- colSums(is.na(who))
print(missing_by_column)

# Convert WHO dataset from wide to long format
who_tidy <- who %>%
  pivot_longer(
    cols = -c(country, iso2, iso3, year),
    names_to = "profile",
    values_to = "cases"
  )
glimpse(who_tidy)
head(who_tidy)

# Standardize profile labels and separate category, sex, and age
who_tidy <- who_tidy %>%
  mutate(profile = gsub("newrel_", "new_rel_", profile)) %>%
  separate(
    profile,
    into = c("new", "type", "sex_age"),
    sep = "_",
    fill = "right",
    extra = "merge"
  ) %>%
  separate(
    sex_age,
    into = c("sex", "age"),
    sep = "(?<=m|f)",
    fill = "right",
    extra = "merge"
  )
glimpse(who_tidy)
head(who_tidy)

# Filter recorded new smear-positive pulmonary TB cases
tb_data <- who_tidy %>%
  filter(type == "sp", !is.na(cases))
glimpse(tb_data)

# TB cases across years
tb_by_year <- tb_data %>%
  group_by(year) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop")
print(tb_by_year)

ggplot(tb_by_year, aes(x = year, y = total_cases)) +
  geom_line() +
  labs(
    title = "Recorded TB Cases Over Time",
    x = "Year",
    y = "Total Recorded Cases"
  ) +
  theme_classic()

# TB cases by sex
tb_by_sex <- tb_data %>%
  group_by(sex) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop")
print(tb_by_sex)

ggplot(tb_by_sex, aes(x = sex, y = total_cases, fill = sex)) +
  geom_col() +
  labs(
    title = "Recorded TB Cases by Sex",
    x = "Sex",
    y = "Total Recorded Cases",
    fill = "Sex"
  ) +
  theme_classic()

# TB cases by age group
tb_by_age <- tb_data %>%
  group_by(age) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop")
print(tb_by_age)

ggplot(tb_by_age, aes(x = age, y = total_cases)) +
  geom_col() +
  labs(
    title = "Recorded TB Cases by Age Group",
    x = "Age Group",
    y = "Total Recorded Cases"
  ) +
  theme_classic()

# TB cases by year and sex
tb_year_sex <- tb_data %>%
  group_by(year, sex) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop")
glimpse(tb_year_sex)

ggplot(tb_year_sex, aes(x = year, y = total_cases, color = sex)) +
  geom_line() +
  labs(
    title = "Recorded TB Cases Over Time by Sex",
    x = "Year",
    y = "Total Recorded Cases",
    color = "Sex"
  ) +
  theme_classic()

# TB cases by year, age, and sex
tb_year_age_sex <- tb_data %>%
  group_by(year, age, sex) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop")
glimpse(tb_year_age_sex)

ggplot(tb_year_age_sex, aes(x = year, y = total_cases, color = sex)) +
  geom_point() +
  facet_wrap(~ age) +
  labs(
    title = "Recorded TB Cases Over Time by Age Group and Sex",
    x = "Year",
    y = "Total Recorded Cases",
    color = "Sex"
  ) +
  theme_classic()

# Distribution and potential outliers
ggplot(tb_data, aes(x = "", y = cases)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Recorded TB Cases",
    x = NULL,
    y = "Number of Recorded Cases"
  ) +
  theme_classic()

# Airquality missing-value check for the 4.8B practice
colSums(is.na(airquality))
