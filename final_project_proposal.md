Final Project Proposal
================
Stella Koo
2024-10-31

## Group members (names and UNIs)

Stella Koo (bk2959)

## Project Title

## Motivation

## Intended Final Products

## Data Sources

``` r
save_path <- "./data"

column_names <- c("age", "sex", "cp", "trestbps", "chol", "fbs", 
                  "restecg", "thalach", "exang", "oldpeak", "slope", 
                  "ca", "thal", "num")

data <- read.table("./heart+disease/processed.cleveland.data", sep = ",", header = FALSE)
colnames(data) <- column_names
write.csv(data, file.path(save_path, "cleveland.csv"), row.names = FALSE)

data <- read.table("./heart+disease/processed.hungarian.data", sep = ",", header = FALSE)
colnames(data) <- column_names
write.csv(data, file.path(save_path, "hungarian.csv"), row.names = FALSE)

data <- read.table("./heart+disease/processed.va.data", sep = ",", header = FALSE)
colnames(data) <- column_names
write.csv(data, file.path(save_path, "long_beach_va.csv"), row.names = FALSE)

data <- read.table("./heart+disease/processed.switzerland.data", sep = ",", header = FALSE)
colnames(data) <- column_names
write.csv(data, file.path(save_path, "switzerland.csv"), row.names = FALSE)
```

## Planned analyses, Visualizations, Coding Challenges

## Planned Timeline
