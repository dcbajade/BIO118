# BIO 118 Laboratory Exercise 2D
# User-Defined Functions
# Denisse Gabrielle C. Bajade


# 2.1 Create and Test User-Defined Functions


# 2.1.1 Growth Rate of Population

calculate_growth_rate <- function(initial_population, final_population) {
  growth_rate <- ((final_population - initial_population) /
                    initial_population) * 100
  return(growth_rate)
}

growth_rate_result <- calculate_growth_rate(100, 150)

print(paste("Growth rate is:", growth_rate_result, "%"))


# 2.1.2 Returning a Result

check_temperature <- function(temperatures) {
  avg_temp <- mean(temperatures)
  
  if (avg_temp >= 10 && avg_temp <= 25) {
    return("Suitable")
  } else {
    return("Not suitable")
  }
}

result <- check_temperature(c(18, 20, 15, 22, 19))

print(result)


# 2.1.3 Temperature Conversion

celsius_to_fahrenheit <- function(temp_celsius) {
  temp_fahrenheit <- (temp_celsius * 9/5) + 32
  return(temp_fahrenheit)
}

temp_fahrenheit_result <- celsius_to_fahrenheit(25)

print(paste("Temperature in Fahrenheit is:", temp_fahrenheit_result))


# 2.1.4 Species Diversity Index

calculate_diversity_index <- function(species_counts) {
  N <- sum(species_counts)
  proportions <- species_counts / N
  
  diversity_index <- -sum(proportions * log(proportions))
  
  return(diversity_index)
}

species_counts <- c(10, 20, 30, 40)

diversity_index_result <- calculate_diversity_index(species_counts)

print(paste("Shannon diversity index is:", diversity_index_result))


# 2.1.5 Water Quality

assess_water_quality <- function(pollution_levels) {
  avg_pollution <- mean(pollution_levels)
  
  if (avg_pollution < 30) {
    quality <- "Good"
  } else if (avg_pollution >= 30 && avg_pollution <= 70) {
    quality <- "Moderate"
  } else {
    quality <- "Poor"
  }
  
  return(quality)
}

pollution_levels <- c(25, 35, 50, 60, 80)

water_quality_result <- assess_water_quality(pollution_levels)

print(paste("Water quality is:", water_quality_result))


# 2.2 Practice


# 2.2.1 DNA Transcription

transcribe_dna_to_rna <- function(dna_sequence) {
  rna_sequence <- gsub("T", "U", dna_sequence)
  return(rna_sequence)
}

dna_sequence <- "ATGCGTACGTAGCTAGCGT"

rna_result <- transcribe_dna_to_rna(dna_sequence)

print(paste("RNA sequence is:", rna_result))


# 2.2.2 Predicting Future Population

predict_population <- function(current_population, growth_rate, years) {
  future_population <- current_population * (1 + growth_rate)^years
  return(future_population)
}

population_result <- predict_population(1000, 0.05, 10)

print(paste(
  "Predicted population after 10 years:",
  round(population_result, 2)
))


# 2.2.3 Environmental Decay

simulate_decay <- function(initial_concentration, decay_rate) {
  concentration <- initial_concentration
  steps <- 0
  
  while (concentration >= 1) {
    concentration <- concentration * (1 - decay_rate)
    steps <- steps + 1
  }
  
  return(steps)
}

decay_steps <- simulate_decay(50, 0.10)

print(paste(
  "Number of steps until concentration drops below 1 ppm:",
  decay_steps
))