---
title: "BIO 118 Laboratory Exercise 2C"
author: "Denisse Gabrielle C. Bajade"
date: "2026-09-16"
output: html_document
---

# Student Information

**Name:** Denisse Gabrielle C. Bajade  
**Course:** BS Biology  
**Laboratory Exercise:** 2C  
**Title:** Using Control Structures

# Practice Questions and Answers

## 2.1 Conditional Statements

### 2.1.1 if Statement

```{r}
species_population <- c(150, 30, 75)

if (any(species_population < 50)) {
  print("Endangered species detected.")
}
```

### 2.1.2 if-else Statement

```{r}
temperatures <- c(18, 22, 25, 30, 15)

avg_temperature <- mean(temperatures)

if (avg_temperature > 20) {
  print("High risk of fire")
} else {
  print("Low risk of fire")
}
```

## 2.2 Loops

### 2.2.1 for Loop

```{r}
initial_population <- 100
growth_rate <- 0.10

plant_growth <- numeric(5)
plant_growth[1] <- initial_population

for (year in 2:5) {
  plant_growth[year] <- plant_growth[year - 1] * (1 + growth_rate)
}

print("Plant population growth over 5 years:")
print(plant_growth)
```

### 2.2.2 Nested for Loop

```{r}
predator_prey <- c("Wolf-Deer", "Fox-Rabbit", "Eagle-Snake")

mutualistic <- c("Bee-Flower", "Clownfish-Anemone", "Fox-Rabbit")

for (pred in predator_prey) {
  for (mut in mutualistic) {
    if (pred == mut) {
      print(paste(
        "Species interaction found in both vectors:",
        pred
      ))
    }
  }
}
```

### 2.2.3 while Loop

```{r}
concentration <- 80
decay_rate <- 0.15
month <- 0

while (concentration >= 10) {
  print(paste(
    "Month", month,
    "concentration:",
    round(concentration, 2),
    "ppm"
  ))
  
  concentration <- concentration * (1 - decay_rate)
  month <- month + 1
}
```

## 2.3 Loop Control Statements

### 2.3.1 break

```{r}
species_population <- c(
  100, 90, 80, 70, 60,
  50, 40, 30, 20, 10
)

for (pop in species_population) {
  if (pop < 50) {
    print(paste(
      "First species below critical population:",
      pop
    ))
    break
  }
}
```

### 2.3.2 repeat

```{r}
population <- 10

repeat {
  print(paste("Current population:", population))
  
  population <- population + 10
  
  if (population > 100) {
    break
  }
}
```

### 2.3.3 next

```{r}
for (i in 1:10) {
  if (i %% 2 == 0) {
    next
  }
  
  print(i)
}
```

# 2.4 Practice

## 2.4.1 Analyzing Gene Expression Data

### Creating the Gene Expression Dataset

```{r}
gene_expression <- data.frame(
  Gene = c(
    "GeneA", "GeneB", "GeneC", "GeneD", "GeneE",
    "GeneA", "GeneB", "GeneC", "GeneD", "GeneE",
    "GeneA", "GeneB", "GeneC", "GeneD", "GeneE"
  ),
  
  Expression_Level = c(
    15.3, 10.8, 12.5, 18.1, 11.7,
    20.2, 17.6, 14.3, 19.5, 16.2,
    13.8, 21.1, 19.7, 22.5, 20.3
  ),
  
  Tissue = c(
    "Tissue1", "Tissue1", "Tissue1", "Tissue1", "Tissue1",
    "Tissue2", "Tissue2", "Tissue2", "Tissue2", "Tissue2",
    "Tissue3", "Tissue3", "Tissue3", "Tissue3", "Tissue3"
  )
)

print("Gene expression data:")
print(gene_expression)
```

### Identifying Genes with Expression Level Above 20

```{r}
print("Genes with expression level above 20:")

for (i in 1:nrow(gene_expression)) {
  
  if (gene_expression$Expression_Level[i] > 20) {
    
    print(paste(
      gene_expression$Gene[i],
      "in",
      gene_expression$Tissue[i],
      "has an expression level above 20."
    ))
  }
}
```

### Using a while Loop for Tissue2


```{r}
tissue_data <- subset(
  gene_expression,
  Tissue == "Tissue2"
)

i <- 1

print("Tissue2 data until first expression level below 15:")

while (i <= nrow(tissue_data)) {
  
  print(tissue_data[i, ])
  
  if (tissue_data$Expression_Level[i] < 15) {
    break
  }
  
  i <- i + 1
}
```

## 2.4.2 Monitoring Species Populations

### Classifying Each Species

```{r}
species_populations <- c(150, 45, 75, 200, 30)

print("Species population classifications:")

for (pop in species_populations) {
  
  if (pop < 50) {
    print(paste(
      "Population:", pop,
      "- Endangered"
    ))
    
  } else {
    print(paste(
      "Population:", pop,
      "- Stable"
    ))
  }
}
```

### Population Growth Over 3 Years


```{r}
growth_rate <- 0.05

print("Population growth over 3 years:")

for (i in 1:length(species_populations)) {
  
  population <- species_populations[i]
  
  for (year in 1:3) {
    
    population <- population * (1 + growth_rate)
    
    print(paste(
      "Species", i,
      "Year", year,
      "Population:", round(population, 2)
    ))
  }
}
```

## 2.4.3 Monitoring Water Quality

### Identifying Pollution Warnings

```{r}
pollution_levels <- c(
  0, 10, 20, 30, 40,
  50, 60, 70, 80, 90, 100
)

print("Pollution warnings:")

for (level in pollution_levels) {
  
  if (level < 50) {
    next
  }
  
  if (level > 50) {
    print(paste(
      "Warning: pollution level is",
      level,
      "ppm"
    ))
  }
}
```

### Using a repeat Loop for Critical Pollution Levels

```{r}
i <- 1

print("Critical pollution monitoring:")

repeat {
  
  level <- pollution_levels[i]
  
  if (level >= 90) {
    
    print(paste(
      "CRITICAL WARNING: Pollution level reached",
      level,
      "ppm"
    ))
    
    break
  }
  
  i <- i + 1
}
```

# Conclusion

This exercise demonstrated the use of conditional statements and control structures in R. The activities showed how `if`, `if-else`, `for`, nested `for`, `while`, `repeat`, `break`, and `next` can be used to analyze and process biological data. These structures allow repetitive tasks and decision-making processes to be performed more efficiently in biological data analysis.