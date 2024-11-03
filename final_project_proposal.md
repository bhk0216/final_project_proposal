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

process_data <- function(file_path, save_name) {

    data <- read.table(file_path, sep = ",", header = FALSE)
    
    colnames(data) <- column_names
    
    write.csv(data, file.path(save_path, save_name), row.names = FALSE)
}

process_data("./heart+disease/processed.cleveland.data", "cleveland.csv")
process_data("./heart+disease/processed.hungarian.data", "hungarian.csv")
process_data("./heart+disease/processed.va.data", "long_beach_va.csv")
process_data("./heart+disease/processed.switzerland.data", "switzerland.csv")
```

## Planned analyses, Visualizations, Coding Challenges

## Planned Timeline
