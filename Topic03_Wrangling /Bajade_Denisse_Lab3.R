## Name: Denisse Gabrielle C. Bajade

# =========================================================
# BIO 118 Laboratory Exercise 3
# 3A Tidying Data Using tidyr
# =========================================================

# =========================================================
# Load Packages
# =========================================================

library(tidyr)
library(dplyr)
library(stringr)
library(forcats)
library(lubridate)

# =========================================================
# 3.2 Messy Dataset
# =========================================================

data <- data.frame(
  site = c("Site_1", "Site_1", "Site_2", "Site_2"),
  week = c(1, 2, 1, 2),
  algae_type = c("Green algae", "Green algae", "Brown algae", "Brown algae"),
  nitrate_phosphate = c("3.5_0.9", "4.1_1.2", "5.3_1.5", "NA_NA"),
  algae_density = c(50, 55, 45, NA),
  temperature = c(20, 22, NA, 25)
)

print(data)


# =========================================================
# 3.3 Split and Combine Columns
# =========================================================

# 3.3.1 separate()

separated_data <- separate(
  data,
  nitrate_phosphate,
  into = c("nitrate", "phosphate"),
  sep = "_"
)

print(separated_data)


# 3.3.2 unite()

united_data <- unite(
  separated_data,
  site_week,
  site,
  week,
  sep = "_"
)

print(united_data)


# =========================================================
# 3.4 Handle Missing Values
# =========================================================

# 3.4.1 drop_na()

cleaned_data <- drop_na(
  data,
  algae_density,
  temperature
)

print(cleaned_data)


# =========================================================
# 3.5 Reshape Data
# =========================================================

# 3.5.1 pivot_longer()

long_data <- pivot_longer(
  separated_data,
  cols = c(nitrate, phosphate),
  names_to = "nutrient",
  values_to = "concentration"
)

print(long_data)


# 3.5.2 pivot_wider()

wide_data <- pivot_wider(
  long_data,
  names_from = nutrient,
  values_from = concentration
)

print(wide_data)


# =========================================================
# 3.6 Streamline Data Cleaning Steps
# =========================================================

tidy_data <- data %>%
  separate(
    nitrate_phosphate,
    into = c("nitrate", "phosphate"),
    sep = "_"
  ) %>%
  pivot_longer(
    cols = c(nitrate, phosphate),
    names_to = "nutrient",
    values_to = "concentration"
  ) %>%
  drop_na(concentration)

print(tidy_data)


# =========================================================
# 3.7 Practice: Simulated Gene Expression Dataset
# =========================================================

# Sample messy gene expression dataset

gene_expression <- data.frame(
  gene_id = c("Gene1", "Gene2", "Gene3", "Gene4"),
  ctrl_brain_rep1 = c(12, 18, 23, 9),
  ctrl_brain_rep2 = c(11, 19, 24, 10),
  treat_brain_rep1 = c(15, 21, 28, 11),
  treat_brain_rep2 = c(14, 22, 30, 12),
  ctrl_liver_rep1 = c(22, 30, 35, 20),
  ctrl_liver_rep2 = c(21, 31, 36, 21),
  treat_liver_rep1 = c(26, NA, 39, 25),
  treat_liver_rep2 = c(27, 34, 40, 26)
)

print(gene_expression)


# =========================================================
# Task 1: Convert wide format to long format
# =========================================================

long_gene_expression <- pivot_longer(
  gene_expression,
  cols = -gene_id,
  names_to = "measurement",
  values_to = "expression"
)

print(long_gene_expression)


# =========================================================
# Task 2: Split measurement into three columns
# =========================================================

tidy_gene_expression <- separate(
  long_gene_expression,
  measurement,
  into = c("condition", "tissue", "replicate"),
  sep = "_"
)

print(tidy_gene_expression)


# =========================================================
# Task 3: Identify the missing expression value
# =========================================================

tidy_gene_expression[is.na(tidy_gene_expression$expression), ]


# =========================================================
# Task 4: Recreate tidy_gene_expression using one pipe
# =========================================================

tidy_gene_expression <- gene_expression %>%
  pivot_longer(
    cols = -gene_id,
    names_to = "measurement",
    values_to = "expression"
  ) %>%
  separate(
    measurement,
    into = c("condition", "tissue", "replicate"),
    sep = "_"
  )

print(tidy_gene_expression)

# =========================================================
# BIO 118 Laboratory Exercise 3
# 3B Transforming Data Using dplyr
# =========================================================

# =========================================================
# 3.2 Dataset
# =========================================================
gene_data <- data.frame(
  gene_id = c("Gene1", "Gene1", "Gene2", "Gene2", "Gene3", "Gene3"),
  tissue = c("Brain", "Liver", "Brain", "Liver", "Brain", "Liver"),
  condition = c("Control", "Control", "Treatment", "Treatment", "Control", "Treatment"),
  expression = c(10.2, 11.5, 13.3, 12.8, 14.9, 15.5),
  replicate = c(1, 2, 1, 2, 1, 2)
)

print(gene_data)


# =========================================================
# 3.3 Select and Filter Data
# =========================================================

# 3.3.1 select()

selected_data <- select(
  gene_data,
  gene_id,
  tissue,
  expression
)

print(selected_data)


# 3.3.2 filter()

filtered_data <- filter(
  gene_data,
  tissue == "Brain",
  expression > 12
)

print(filtered_data)


# =========================================================
# 3.4 Create or Modify Variables
# =========================================================

# 3.4.1 mutate()

mutated_data <- mutate(
  gene_data,
  log_expression = log(expression)
)

print(mutated_data)


# =========================================================
# 3.5 Reorder Observations
# =========================================================

# 3.5.1 arrange()

arranged_data <- arrange(
  gene_data,
  expression
)

print(arranged_data)


# =========================================================
# 3.6 Summarize Data
# =========================================================

# 3.6.1 summarize()

summary_data <- gene_data %>%
  group_by(tissue) %>%
  summarize(
    mean_expression = mean(expression)
  )

print(summary_data)


# 3.6.2 count()

count_data <- count(
  gene_data,
  gene_id
)

print(count_data)


# =========================================================
# 3.7 Identify Unique Observations
# =========================================================

# 3.7.1 distinct()

distinct_data <- distinct(
  gene_data,
  gene_id,
  tissue
)

print(distinct_data)


# =========================================================
# 3.8 Change Column Names
# =========================================================

# 3.8.1 rename()

renamed_data <- rename(
  gene_data,
  gene_expression = expression
)

print(renamed_data)


# =========================================================
# 3.9 Combine Datasets
# =========================================================

# Create second dataset

gene_info <- data.frame(
  gene_id = c("Gene1", "Gene2", "Gene4"),
  description = c(
    "Apoptosis-related",
    "Cell cycle control",
    "Unknown"
  )
)

print(gene_info)


# 3.9.1 left_join()

left_join_data <- left_join(
  gene_data,
  gene_info,
  by = "gene_id"
)

print(left_join_data)


# 3.9.2 full_join()

full_join_data <- full_join(
  gene_data,
  gene_info,
  by = "gene_id"
)

print(full_join_data)


# =========================================================
# 3.10 Streamline Data Transformation
# =========================================================

processed_gene_data <- gene_data %>%
  filter(expression > 12) %>%
  group_by(tissue) %>%
  summarize(
    mean_expression = mean(expression),
    n = n()
  ) %>%
  arrange(desc(mean_expression))

print(processed_gene_data)


# =========================================================
# 3.11 Practice
# Simulated Abundance Dataset
# =========================================================

# Set seed so the simulated data are reproducible

set.seed(123)

species_list <- c(
  "Species_A",
  "Species_B",
  "Species_C",
  "Species_D"
)

region_list <- c(
  "Region_1",
  "Region_2",
  "Region_3"
)

years <- sample(
  2000:2020,
  100,
  replace = TRUE
)

abundance <- sample(
  1:500,
  100,
  replace = TRUE
)

temperature <- sample(
  15:35,
  100,
  replace = TRUE
)

abundance_data <- data.frame(
  species = sample(species_list, 100, replace = TRUE),
  region = sample(region_list, 100, replace = TRUE),
  year = years,
  abundance = abundance,
  temperature = temperature
)

print(abundance_data)


# =========================================================
# Practice Task 1
# Filter abundance > 100 and temperature > 20
# =========================================================

filtered_abundance <- filter(
  abundance_data,
  abundance > 100,
  temperature > 20
)

print(filtered_abundance)


# =========================================================
# Practice Task 2
# Select species, region, and abundance
# =========================================================

selected_abundance <- select(
  abundance_data,
  species,
  region,
  abundance
)

print(selected_abundance)


# =========================================================
# Practice Task 3
# Arrange by species alphabetically
# and abundance in descending order
# =========================================================

arranged_abundance <- arrange(
  abundance_data,
  species,
  desc(abundance)
)

print(arranged_abundance)


# =========================================================
# Practice Task 4
# Calculate overall total abundance
# and average temperature
# =========================================================

overall_summary <- summarize(
  abundance_data,
  total_abundance = sum(abundance),
  average_temperature = mean(temperature)
)

print(overall_summary)


# =========================================================
# Practice Task 5
# Group by species and calculate mean abundance
# =========================================================

species_summary <- abundance_data %>%
  group_by(species) %>%
  summarize(
    mean_abundance = mean(abundance)
  )

print(species_summary)


# =========================================================
# Practice Task 6
# Complete workflow using pipe operator
# =========================================================

regional_summary <- abundance_data %>%
  filter(year > 2010) %>%
  group_by(region) %>%
  summarize(
    mean_abundance = mean(abundance),
    mean_temperature = mean(temperature)
  ) %>%
  arrange(desc(mean_abundance))

print(regional_summary)

# =========================================================
# BIO 118 Laboratory Exercise 3
# 3C Wrangling Data Using stringr, forcats, and lubridate
# =========================================================

# =========================================================
# 3.2 Manipulate Strings Using stringr
# =========================================================

bio_data <- data.frame(
  species = c(
    "Homo sapiens ",
    "Pan troglodytes",
    " Canis lupus",
    "Homo sapiens",
    "Mus musculus"
  ),
  gene = c(
    "BRCA1-001",
    "TP53.002",
    "MYC-001",
    "BRCA2-001",
    "EGFR.003"
  ),
  condition = c(
    "Control",
    "treatment",
    "control",
    "treatment",
    "control"
  ),
  replicate = c(
    "Replicate 1",
    "Replicate 2",
    "Replicate 1",
    "Replicate 3",
    "Replicate 1"
  )
)

print(bio_data)


# =========================================================
# 3.2.1 str_detect()
# =========================================================

bio_data <- bio_data %>%
  mutate(
    is_human = str_detect(species, "Homo")
  )

print(bio_data)


# =========================================================
# 3.2.2 str_replace()
# =========================================================

bio_data <- bio_data %>%
  mutate(
    condition = str_replace(condition, "control", "Control"),
    condition = str_replace(condition, "treatment", "Treatment")
  )

print(bio_data)


# =========================================================
# 3.2.3 str_trim()
# =========================================================

bio_data <- bio_data %>%
  mutate(
    species = str_trim(species)
  )

print(bio_data)


# =========================================================
# 3.2.4 str_to_lower()
# =========================================================

bio_data <- bio_data %>%
  mutate(
    condition = str_to_lower(condition)
  )

print(bio_data)


# =========================================================
# 3.3 Working with Factors Using forcats
# =========================================================

fish_data <- data.frame(
  species = factor(
    c(
      "Salmon", "Trout", "Bass", "Salmon",
      "Bass", "Trout", "Salmon", "Catfish"
    )
  ),
  habitat = factor(
    c(
      "River", "Lake", "River", "Lake",
      "Ocean", "River", "River", "Lake"
    )
  ),
  treatment = c(
    "Control", "Treatment", "Control", "Treatment",
    "Treatment", "Control", "Control", "Treatment"
  )
)

print(fish_data)


# Convert treatment to a factor

fish_data <- fish_data %>%
  mutate(
    treatment = factor(treatment)
  )

levels(fish_data$treatment)


# =========================================================
# 3.3.1 fct_relevel()
# =========================================================

levels(fish_data$treatment)

fish_data1 <- fish_data %>%
  mutate(
    treatment = fct_relevel(
      treatment,
      "Treatment",
      "Control"
    )
  )

levels(fish_data1$treatment)


# =========================================================
# 3.3.2 fct_infreq()
# =========================================================

levels(fish_data$species)

fish_data7 <- fish_data %>%
  mutate(
    species = fct_infreq(species)
  )

levels(fish_data7$species)


# =========================================================
# 3.4 Working with Date and Time Using lubridate
# =========================================================

bloom_data <- data.frame(
  species = c(
    "Rose", "Lily", "Tulip",
    "Sunflower", "Daisy", "Rose"
  ),
  region = c(
    "North", "South", "East",
    "West", "North", "East"
  ),
  bloom_date = c(
    "2022-04-15",
    "2022-05-10",
    "2022-04-20",
    "2022-06-01",
    "2022-04-25",
    "2022-04-30"
  ),
  bloom_time = c(
    "06:30:00",
    "08:15:00",
    "07:00:00",
    "09:45:00",
    "06:45:00",
    "07:30:00"
  )
)

print(bloom_data)


# =========================================================
# 3.4.1 ymd()
# =========================================================

bloom_data <- bloom_data %>%
  mutate(
    bloom_date = ymd(bloom_date)
  )

print(bloom_data)

str(bloom_data)


# =========================================================
# 3.4.2 ymd_hms()
# =========================================================

bloom_data <- bloom_data %>%
  mutate(
    bloom_datetime = ymd_hms(
      paste(bloom_date, bloom_time)
    )
  )

print(bloom_data)


# =========================================================
# 3.4.3 year(), month(), day(), and hour()
# =========================================================

bloom_data <- bloom_data %>%
  mutate(
    year = year(bloom_datetime),
    month = month(bloom_datetime),
    day = day(bloom_datetime),
    hour = hour(bloom_datetime)
  )

print(bloom_data)


# =========================================================
# 3.4.4 difftime()
# =========================================================

bloom_data <- bloom_data %>%
  mutate(
    days_since_first_bloom = as.numeric(
      difftime(
        bloom_datetime,
        min(bloom_datetime),
        units = "days"
      )
    )
  )

print(bloom_data)


# =========================================================
# 3.5 Practice: Simulated Cell Biology Experiment Data
# =========================================================

cell_data <- data.frame(
  cell_line = c(
    "HEK293 ",
    "HeLa",
    "A549 ",
    "HepG2",
    "HeLa",
    "HEK293"
  ),
  gene_expression = c(
    "BRCA1_001",
    "TP53_002",
    "MYC_003",
    "EGFR_001",
    "BRCA2_004",
    "EGFR_005"
  ),
  treatment = c(
    "Control",
    "control",
    "Treatment",
    "TREATMENT",
    "Control",
    "control"
  ),
  observation_time = c(
    "2023-07-15 08:30:00",
    "2023-07-15 10:45:00",
    "2023-07-16 09:00:00",
    "2023-07-16 11:20:00",
    "2023-07-17 12:15:00",
    "2023-07-17 13:00:00"
  )
)

print(cell_data)


# =========================================================
# Practice: Clean and Standardize String Data
# =========================================================

cell_data <- cell_data %>%
  mutate(
    cell_line = str_trim(cell_line),
    treatment = str_to_lower(treatment)
  )

print(cell_data)


# =========================================================
# Practice: Convert and Reorder Factor Levels
# =========================================================

cell_data <- cell_data %>%
  mutate(
    treatment = factor(treatment),
    cell_line = factor(cell_line),
    treatment = fct_relevel(
      treatment,
      "treatment",
      "control"
    ),
    cell_line = fct_infreq(cell_line)
  )

print(cell_data)

levels(cell_data$treatment)
levels(cell_data$cell_line)


# =========================================================
# Practice: Work with Date and Time Data
# =========================================================

cell_data <- cell_data %>%
  mutate(
    observation_time = ymd_hms(observation_time)
  )

print(cell_data)

class(cell_data$observation_time)


# Extract year, month, day, and hour

cell_data <- cell_data %>%
  mutate(
    year = year(observation_time),
    month = month(observation_time),
    day = day(observation_time),
    hour = hour(observation_time)
  )

print(cell_data)


# Calculate time difference in hours
# between each observation and the first observation

cell_data <- cell_data %>%
  mutate(
    hours_since_first_observation = as.numeric(
      difftime(
        observation_time,
        min(observation_time),
        units = "hours"
      )
    )
  )

print(cell_data)

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
