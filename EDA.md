EDA
================
Stella Koo
2024-11-16

## Ian, Stella

## Data Cleaning & Preparing for Analysis

``` r
library(tidyverse)
library(pheatmap)
library(corrplot)

cleveland = read_csv("./data/cleveland.csv", na = "?")
hungary = read_csv("./data/hungarian.csv", na = "?")
long_beach = read_csv("./data/long_beach_va.csv", na = "?")
switzerland = read_csv("./data/switzerland.csv", na = "?")
```

``` r
dfs = list(cleveland, hungary, long_beach, switzerland)
locations = c("Cleveland", "Hungary", "Long Beach", "Switzerland")

combined_df = purrr::map2(dfs, locations, ~ mutate(.x, region = .y)) |>
  bind_rows()
```

``` r
# Number of missing values: should consider NOT analyzing slope, ca, thal
colSums(is.na(combined_df))
```

    ##      age      sex       cp trestbps     chol      fbs  restecg  thalach 
    ##        0        0        0       59       30       90        2       55 
    ##    exang  oldpeak    slope       ca     thal      num   region 
    ##       55       62      309      611      486        0        0

## Continuous Variables

### Descriptive Statistics

``` r
# Summary of continuous variables 
continuous_var = combined_df |>
  group_by(region) |>
  summarise(across(c(age, trestbps, chol, thalach, oldpeak), list(mean = mean, sd = sd, median = median), na.rm = TRUE)) |>
  knitr::kable(digits = 2)

continuous_var
```

| region      | age_mean | age_sd | age_median | trestbps_mean | trestbps_sd | trestbps_median | chol_mean | chol_sd | chol_median | thalach_mean | thalach_sd | thalach_median | oldpeak_mean | oldpeak_sd | oldpeak_median |
|:------------|---------:|-------:|-----------:|--------------:|------------:|----------------:|----------:|--------:|------------:|-------------:|-----------:|---------------:|-------------:|-----------:|---------------:|
| Cleveland   |    54.44 |   9.04 |         56 |        131.69 |       17.60 |             130 |    246.69 |   51.78 |         241 |       149.61 |      22.88 |            153 |         1.04 |       1.16 |            0.8 |
| Hungary     |    47.83 |   7.81 |         49 |        132.58 |       17.63 |             130 |    250.85 |   67.66 |         243 |       139.13 |      23.59 |            140 |         0.59 |       0.91 |            0.0 |
| Long Beach  |    59.35 |   7.81 |         60 |        133.76 |       21.54 |             130 |    178.75 |  114.04 |         216 |       122.80 |      21.99 |            120 |         1.32 |       1.11 |            1.5 |
| Switzerland |    55.32 |   9.03 |         56 |        130.21 |       22.56 |             125 |      0.00 |    0.00 |           0 |       121.56 |      25.98 |            121 |         0.65 |       1.06 |            0.3 |

### Correlation Heatmap

``` r
corr_matrix = cor(combined_df[, c("age", "trestbps", "chol", "thalach", "oldpeak")], use = "complete.obs")
corrplot::corrplot(corr_matrix, method = "color", addCoef.col = "black", tl.col = "black")
```

![](EDA_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Discrete Variables

### Descriptive Statistics

sex, cp, fbs, restecg, exang

- The name and/or source of your data, a description of all relevant
  variables, and the observational unit (i.e., row). Exploratory Data
  Analysis (EDA):
- Appropriate summary statistics with adequate explanation and
  interpretation.
  - examine binary or other relationships, as well as describing
    individual distributions.
  - Consider making an appropriate table
- Graphical displays with adequate explanation and interpretation.
  - histograms, density plots, and box plots to visualize distributions
    of key variables, such as cholesterol, age, thalach (maximum heart
    rate)
  - create a correlation heatmap to identify relationships between
    variables and detect multicollinearity, especially for predictors
    that may be used in regression models.
- Interactive Variables Visualization: Primary Variables:
  - Age vs. Maximum Heart Rate (thalach): Use this to explore how age
    relates to heart rate, and color points based on heart disease
    status (num) for additional insight.
  - Age vs. ST Depression (oldpeak): This can reveal if age affects
    exercise-induced ST depression differently for those with and
    without heart disease.
- Histograms or Density Plots for Distribution Insights
  - Variables:Age, Cholesterol (chol), num
