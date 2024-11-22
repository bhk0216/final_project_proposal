Modelling
================
Yixin Zheng (yz4993), Thomas Tang (tt3022), Yonghao YU (yy3564)
2024-11-16

## Yixin Zheng (yz4993), Thomas Tang (tt3022), Yonghao YU (yy3564)

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(janitor)
```

    ## 
    ## 载入程序包：'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

Interpreting num (author: Yixin Zheng): The values for `num` represent
the degree of narrowing in the coronary arteries: 0: No disease (\< 50%
diameter narrowing). 1-4: Increasing severity of disease (\> 50%
diameter narrowing, with different severities).

For convenience, this variable will binarized: 0: No heart disease
(value 0 in num). 1: Presence of heart disease (values 1-4 in num).

\*but if we want to analyze the severity of heart disease num will be
treated as a categorical variable. example code: cleaned_data \<- data
\|\> mutate(num = factor(num, levels = c(0, 1, 2, 3, 4), labels = c(“No
Disease”, “Mild”, “Moderate”, “Severe”, “Very Severe”)))

``` r
cleveland <- read_csv("./data/cleveland.csv", na = "?") |> 
  clean_names() |> 
  mutate(num = if_else(num == 0, 0, 1)) # Binarize the `num` variable: 0 = no heart disease, 1 = heart disease
```

    ## Rows: 303 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (14): age, sex, cp, trestbps, chol, fbs, restecg, thalach, exang, oldpea...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# |> drop_na() Removes rows with any missing values (optional, adjust as needed)

hungary = read_csv("./data/hungarian.csv", na = "?") |> 
  clean_names() |> 
  mutate(num = if_else(num == 0, 0, 1))
```

    ## Rows: 294 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (14): age, sex, cp, trestbps, chol, fbs, restecg, thalach, exang, oldpea...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# |> drop_na() Removes rows with any missing values (optional, adjust as needed)

long_beach = read_csv("./data/long_beach_va.csv", na = "?") |> 
  clean_names() |> 
  mutate(num = if_else(num == 0, 0, 1))
```

    ## Rows: 200 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (14): age, sex, cp, trestbps, chol, fbs, restecg, thalach, exang, oldpea...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# |> drop_na() Removes rows with any missing values (optional, adjust as needed)

switzerland = read_csv("./data/switzerland.csv", na = "?") |> 
  clean_names() |> 
  mutate(num = if_else(num == 0, 0, 1))
```

    ## Rows: 123 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (14): age, sex, cp, trestbps, chol, fbs, restecg, thalach, exang, oldpea...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# |> drop_na() Removes rows with any missing values (optional, adjust as needed) 
```

``` r
cor(cleveland$chol, cleveland$num, use = "complete.obs")
```

    ## [1] 0.08516361

# Variable Selection (author: Yonghao YU)

author: Yonghao YU

### Data Preprocessing

``` r
cleveland$region = "Cleveland"
hungary$region = "Hungarian"
long_beach$region = "Long_Beach_VA"
switzerland$region = "Switzerland"
combined_data_one = bind_rows(cleveland, hungary, long_beach, switzerland)

colnames(combined_data_one) = c("age", "sex", "cp", "trestbps", "chol", "fbs",
                             "restecg", "thalach", "exang", "oldpeak", "slope",
                             "ca", "thal", "num", "region")
combined_data_two = combined_data_one |>
  mutate(region = case_when(
    region == "Cleveland" ~ 1,
    region == "Hungarian" ~ 2,
    region == "Long_Beach_VA" ~ 3,
    region == "Switzerland" ~ 4,
  )) |>
  select(-thal,-ca) |>
  drop_na()
case_data = combined_data_two |>
  filter(num == 1)
control_data = combined_data_two |>
  filter(num == 0)
print(case_data)
```

    ## # A tibble: 324 × 13
    ##      age   sex    cp trestbps  chol   fbs restecg thalach exang oldpeak slope
    ##    <dbl> <dbl> <dbl>    <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ##  1    67     1     4      160   286     0       2     108     1     1.5     2
    ##  2    67     1     4      120   229     0       2     129     1     2.6     2
    ##  3    62     0     4      140   268     0       2     160     0     3.6     3
    ##  4    63     1     4      130   254     0       2     147     0     1.4     2
    ##  5    53     1     4      140   203     1       2     155     1     3.1     3
    ##  6    56     1     3      130   256     1       2     142     1     0.6     2
    ##  7    48     1     2      110   229     0       0     168     0     1       3
    ##  8    58     1     2      120   284     0       2     160     0     1.8     2
    ##  9    58     1     3      132   224     0       2     173     0     3.2     1
    ## 10    60     1     4      130   206     0       2     132     1     2.4     2
    ## # ℹ 314 more rows
    ## # ℹ 2 more variables: num <dbl>, region <dbl>

``` r
print(control_data)
```

    ## # A tibble: 207 × 13
    ##      age   sex    cp trestbps  chol   fbs restecg thalach exang oldpeak slope
    ##    <dbl> <dbl> <dbl>    <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ##  1    63     1     1      145   233     1       2     150     0     2.3     3
    ##  2    37     1     3      130   250     0       0     187     0     3.5     3
    ##  3    41     0     2      130   204     0       2     172     0     1.4     1
    ##  4    56     1     2      120   236     0       0     178     0     0.8     1
    ##  5    57     0     4      120   354     0       0     163     1     0.6     1
    ##  6    57     1     4      140   192     0       0     148     0     0.4     2
    ##  7    56     0     2      140   294     0       2     153     0     1.3     2
    ##  8    44     1     2      120   263     0       0     173     0     0       1
    ##  9    52     1     3      172   199     1       0     162     0     0.5     1
    ## 10    57     1     3      150   168     0       0     174     0     1.6     1
    ## # ℹ 197 more rows
    ## # ℹ 2 more variables: num <dbl>, region <dbl>

``` r
print(combined_data_two)
```

    ## # A tibble: 531 × 13
    ##      age   sex    cp trestbps  chol   fbs restecg thalach exang oldpeak slope
    ##    <dbl> <dbl> <dbl>    <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ##  1    63     1     1      145   233     1       2     150     0     2.3     3
    ##  2    67     1     4      160   286     0       2     108     1     1.5     2
    ##  3    67     1     4      120   229     0       2     129     1     2.6     2
    ##  4    37     1     3      130   250     0       0     187     0     3.5     3
    ##  5    41     0     2      130   204     0       2     172     0     1.4     1
    ##  6    56     1     2      120   236     0       0     178     0     0.8     1
    ##  7    62     0     4      140   268     0       2     160     0     3.6     3
    ##  8    57     0     4      120   354     0       0     163     1     0.6     1
    ##  9    63     1     4      130   254     0       2     147     0     1.4     2
    ## 10    53     1     4      140   203     1       2     155     1     3.1     3
    ## # ℹ 521 more rows
    ## # ℹ 2 more variables: num <dbl>, region <dbl>

author: Yonghao YU

### For Continues case

For continuous variables, we use mean and standard deviation (std) to
describe the distribution in overall samples, samples of control(num =
0), and samples of case(num = 1). Then, we use t-test to examine whether
the means of these variables are significantly different between case
group and control group (significance level = 0.05).

``` r
# 1. Mean and Std for Continuous Variables (Overall)
list_conti_all = list(
  age = combined_data_two$age,
  trestbps = combined_data_two$trestbps,
  chol = combined_data_two$chol,
  thalach = combined_data_two$thalach,
  oldpeak = combined_data_two$oldpeak
) |> 
  lapply(na.omit) 

mean_all = sapply(list_conti_all, mean) |> 
  as.data.frame()|>
  setNames("Overall Mean")

std_all = sapply(list_conti_all, sd) |> 
  as.data.frame() |>
  setNames("Overall Std")

# 2. p-value of t-test for Continuous Variables
t_test = function(variable) {
  t_test_result = t.test(combined_data_two[[variable]] ~ combined_data_two$num)
  return(data.frame(
    variable = variable,
    p_value = t_test_result$p.value
  ))
}

p_value = 
  lapply(c("age", "trestbps", "chol", "thalach", "oldpeak"), t_test) |> 
  bind_rows() |> 
  as.data.frame()

# 3. Mean and Std for Control Group
list_conti_control = list(
  age = control_data$age,
  trestbps = control_data$trestbps,
  chol = control_data$chol,
  thalach = control_data$thalach,
  oldpeak = control_data$oldpeak
) |> 
  lapply(na.omit)

mean_control = sapply(list_conti_control, mean) |> 
  as.data.frame() |>
  setNames("Control Mean")

std_control = sapply(list_conti_control, sd) |> 
  as.data.frame() |>
  setNames("Control Std")

# 4. Mean and Std for Case Group
list_conti_case = list(
  age = case_data$age,
  trestbps = case_data$trestbps,
  chol = case_data$chol,
  thalach = case_data$thalach,
  oldpeak = case_data$oldpeak
) |> 
  lapply(na.omit)

mean_case = sapply(list_conti_case, mean) |> 
  as.data.frame() |>
  setNames("Case Mean")

std_case = sapply(list_conti_case, sd) |> 
  as.data.frame() |>
  setNames("Case Std")

conti_des_df =
  as.data.frame(cbind(mean_all, std_all, mean_control, std_control, mean_case, std_case, p_value))
conti_des_df = conti_des_df[, -grep("variable", colnames(conti_des_df))] |> 
  knitr::kable(digits = 6)
conti_des_df
```

|  | Overall Mean | Overall Std | Control Mean | Control Std | Case Mean | Case Std | p_value |
|:---|---:|---:|---:|---:|---:|---:|---:|
| age | 54.843691 | 8.824069 | 52.908213 | 9.248788 | 56.080247 | 8.323177 | 0.000074 |
| trestbps | 133.406780 | 18.969496 | 129.734300 | 16.322060 | 135.753086 | 20.158831 | 0.000179 |
| chol | 216.854991 | 99.014215 | 237.043478 | 68.313903 | 203.956790 | 112.615863 | 0.000030 |
| thalach | 138.463277 | 25.833649 | 152.758454 | 22.958375 | 129.330247 | 23.329890 | 0.000000 |
| oldpeak | 1.218456 | 1.105150 | 0.726087 | 0.805741 | 1.533025 | 1.155598 | 0.000000 |

Based on the result, we can find that all five features are
significantly different between case and control.

author: Yonghao YU

### For Discrete case

For binary and categorical variables, we use count (n) and percentage
(pct) to describe the distribution in overall samples, samples of
control(num = 0), and samples of case(num = 1). Then, as the data meet
the assumption, we use chi-sq test to examine whether the distribution
of these variables are significantly different between case group and
control group (significance level = 0.05).

``` r
list_cat_all = as.data.frame(list(
  sex = combined_data_two$sex,
  cp = combined_data_two$cp,
  fbs = combined_data_two$fbs,
  restecg = combined_data_two$restecg,
  exang = combined_data_two$exang,
  slope = combined_data_two$slope,
  region = combined_data_two$region
))

# 1. Overall Counts and Chi-Square Test
cat_vars = names(list_cat_all)

count_all_function = function(variable) {
  table_value = table(list_cat_all[[variable]], combined_data_two$num) 
  chi_sq_test = chisq.test(table_value)
  
  count = table(list_cat_all[[variable]])
  total = sum(count)
  pct = count / total
  
  result_df = tibble(
    variable = rep(variable, length(count)),
    category = names(count),
    n = as.numeric(count),
    pct = round(pct, 3),
    p_value = round(chi_sq_test$p.value, 3)
  )
  
  return(result_df)
}

cat_count_chisq = lapply(cat_vars, count_all_function) |> 
  bind_rows()

# 2. Control Group Counts and Percentages
list_cat_ctrl = as.data.frame(list(
  sex = control_data$sex,
  cp = control_data$cp,
  fbs = control_data$fbs,
  restecg = control_data$restecg,
  exang = control_data$exang,
  slope = control_data$slope,
  region = control_data$region
))

cat_vars_ctrl = names(list_cat_ctrl)

count_ctrl_function = function(variable) {
  count = table(list_cat_ctrl[[variable]])
  total = sum(count)
  pct = count / total
  
  result_df = tibble(
    variable = rep(variable, length(count)),
    category = names(count),
    control_n = as.numeric(count),
    control_pct = round(pct, 3)
  )
  
  return(result_df)
}

cat_count_ctrl = lapply(cat_vars_ctrl, count_ctrl_function) |> 
  bind_rows()

# 3. Case Group Counts and Percentages
list_cat_case = as.data.frame(list(
  sex = case_data$sex,
  cp = case_data$cp,
  fbs = case_data$fbs,
  restecg = case_data$restecg,
  exang = case_data$exang,
  slope = case_data$slope,
  region = case_data$region
))

cat_vars_case = names(list_cat_case)

count_case_function = function(variable) {
  count = table(list_cat_case[[variable]])
  total = sum(count)
  pct = count / total
  
  result_df = tibble(
    variable = rep(variable, length(count)),
    category = names(count),
    case_n = as.numeric(count),
    case_pct = round(pct, 3)
  )
  return(result_df)
}

cat_count_case = lapply(cat_vars_case, count_case_function) |> 
  bind_rows()

# 4. Combine Results
final_cat_count = cat_count_chisq |>
  left_join(cat_count_ctrl, by = c("variable", "category")) |>
  left_join(cat_count_case, by = c("variable", "category"))|>
  knitr::kable(digits = 3)

print(final_cat_count)
```

    ## 
    ## 
    ## |variable |category |   n|   pct| p_value| control_n| control_pct| case_n| case_pct|
    ## |:--------|:--------|---:|-----:|-------:|---------:|-----------:|------:|--------:|
    ## |sex      |0        | 127| 0.239|   0.000|        87|       0.420|     40|    0.123|
    ## |sex      |1        | 404| 0.761|   0.000|       120|       0.580|    284|    0.877|
    ## |cp       |1        |  30| 0.056|   0.000|        18|       0.087|     12|    0.037|
    ## |cp       |2        |  70| 0.132|   0.000|        51|       0.246|     19|    0.059|
    ## |cp       |3        | 114| 0.215|   0.000|        80|       0.386|     34|    0.105|
    ## |cp       |4        | 317| 0.597|   0.000|        58|       0.280|    259|    0.799|
    ## |fbs      |0        | 446| 0.840|   0.378|       178|       0.860|    268|    0.827|
    ## |fbs      |1        |  85| 0.160|   0.378|        29|       0.140|     56|    0.173|
    ## |restecg  |0        | 297| 0.559|   0.000|       123|       0.594|    174|    0.537|
    ## |restecg  |1        |  73| 0.137|   0.000|        13|       0.063|     60|    0.185|
    ## |restecg  |2        | 161| 0.303|   0.000|        71|       0.343|     90|    0.278|
    ## |exang    |0        | 267| 0.503|   0.000|       163|       0.787|    104|    0.321|
    ## |exang    |1        | 264| 0.497|   0.000|        44|       0.213|    220|    0.679|
    ## |slope    |1        | 173| 0.326|   0.000|       119|       0.575|     54|    0.167|
    ## |slope    |2        | 310| 0.584|   0.000|        76|       0.367|    234|    0.722|
    ## |slope    |3        |  48| 0.090|   0.000|        12|       0.058|     36|    0.111|
    ## |region   |1        | 303| 0.571|   0.000|       164|       0.792|    139|    0.429|
    ## |region   |2        |  95| 0.179|   0.000|        27|       0.130|     68|    0.210|
    ## |region   |3        |  87| 0.164|   0.000|        15|       0.072|     72|    0.222|
    ## |region   |4        |  46| 0.087|   0.000|         1|       0.005|     45|    0.139|

Based on the result, we can find that except fbs, the rest of all other
binary and categorical features are significantly different between case
and control.

hypothesis (author: Yixin Zheng) \* need to run some test choose
explanatory variables?

1.  Comparing Diagnostic Factors for Heart Disease Across Regions
    explore whether certain diagnostic factors (e.g., cholesterol
    levels, exercise-induced angina) are more predictive of heart
    disease in one region compared to others.

- there are many null values in the `chol` column in switzerland
  dataset. So maybe we need to use other diagnostic factors such as
  `trestbps` if we want to compare the regions Example Hypothesis (SLR):
- **Null Hypothesis (H_0):** There is no linear relationship between
  cholesterol levels (`chol`) and the presence of heart disease (`num`).
- **Alternative Hypothesis (H_a):** There is a positive linear
  relationship between cholesterol levels (`chol`) and the presence of
  heart disease (`num`). ($\beta_1>0$) Where:
  - `num` is the target variable indicating heart disease presence.
  - `chol` is the predictor variable representing cholesterol levels.

$$ num = \beta_0 + \beta_1 \cdot \text{chol} + \epsilon $$

Example Hypothesis (MLR): - **Null Hypothesis (H_0):** Cholesterol
levels (`chol`), blood pressure (`trestbps`), and exercise-induced
angina (`exang`) are not significant predictors of heart disease
presence (`num`). - **Alternative Hypothesis (H_a):** At least one of
these variables (`chol`, `trestbps`, `exang`) is a significant predictor
of heart disease presence (`num`).

$$ num = \beta_0 + \beta_1 \cdot \text{chol} + \beta_2 \cdot \text{trestbps} + \beta_3 \cdot \text{exang} + \epsilon $$

2.  Examining Predictive Power of Clinical Indicators for Heart Disease
    in Diverse Populations

Example Hypothesis (SLR): - **Null Hypothesis (H_0):** There is no
linear relationship between maximum heart rate achieved (`thalach`) and
the presence of heart disease (`num`). - **Alternative Hypothesis
(H_a):** There is a negative linear relationship between maximum heart
rate achieved (`thalach`) and the presence of heart disease (`num`).
($\beta_1<0$)

$$ num = \beta_0 + \beta_1 \cdot \text{thalach} + \epsilon $$ Example
Hypothesis (MLR): - **Null Hypothesis (H_0):** Maximum heart rate
(`thalach`), fasting blood sugar (`fbs`), and the slope of the ST
segment (`slope`) are not significant predictors of heart disease
presence (`num`). - **Alternative Hypothesis (H_a):** At least one of
these variables (`thalach`, `fbs`, `slope`) is a significant predictor
of heart disease presence (`num`).

$$ num = \beta_0 + \beta_1 \cdot \text{thalach} + \beta_2 \cdot \text{fbs} + \beta_3 \cdot \text{slope} + \epsilon $$

3.  Influence of Age and Lifestyle Factors on Heart Disease

Example Hypothesis (SLR): - **Null Hypothesis (H_0):** There is no
linear relationship between age (`age`) and the presence of heart
disease (`num`). - **Alternative Hypothesis (H_a):** There is a positive
linear relationship between age (`age`) and the presence of heart
disease (`num`). ($\beta_1>0$)

$$ num = \beta_0 + \beta_1 \cdot \text{age} + \epsilon $$ Example
Hypothesis (MLR): - **Null Hypothesis (H_0):** Age (`age`), chest pain
type (`cp`), and exercise-induced angina (`exang`) are not significant
predictors of heart disease presence (`num`). - **Alternative Hypothesis
(H_a):** At least one of these variables (`age`, `cp`, `exang`) is a
significant predictor of heart disease presence (`num`).

$$ num = \beta_0 + \beta_1 \cdot \text{age} + \beta_2 \cdot \text{cp} + \beta_3 \cdot \text{exang} + \epsilon $$

4.  Regional Patterns in Heart Disease Diagnostic Attributes

Example Hypothesis (SLR): - **Null Hypothesis (H_0):** There is no
linear relationship between diagnostic attributes (e.g., cholesterol,
age) and heart disease (`num`) in each region. - **Alternative
Hypothesis (H_a):**The relationship between diagnostic attributes (e.g.,
cholesterol, age) and heart disease (`num`) differs significantly across
regions. - This hypothesis can be tested by performing SLR for each
region and comparing the coefficients (beta_1 values) to see if they
vary.

Example Hypothesis (MLR): - **Null Hypothesis (H_0):** Regional
differences do not significantly influence the predictive power of
diagnostic factors for heart disease. - **Alternative Hypothesis
(H_a):** Regional differences significantly influence the predictive
power of diagnostic factors for heart disease.

$$ num = \beta_0 + \beta_1 \cdot \text{chol} + \beta_2 \cdot \text{age} + \beta_3 \cdot \text{region} + \beta_4 \cdot (\text{chol} \times \text{region}) + \beta_5 \cdot (\text{age} \times \text{region}) + \epsilon $$

author: Thomas Tang

``` r
cleveland <- read.csv("./data/cleveland.csv", header = FALSE)
hungarian <- read.csv("./data/hungarian.csv", header = FALSE)
long_beach <- read.csv("./data/long_beach_va.csv", header = FALSE)
switzerland <- read.csv("./data/switzerland.csv", header = FALSE)

# Add region column and combine datasets
cleveland$region <- "Cleveland"
hungarian$region <- "Hungarian"
long_beach$region <- "Long_Beach_VA"
switzerland$region <- "Switzerland"
combined_data <- bind_rows(cleveland, hungarian, long_beach, switzerland)

colnames(combined_data) <- c("age", "sex", "cp", "trestbps", "chol", "fbs",
                             "restecg", "thalach", "exang", "oldpeak", "slope",
                             "ca", "thal", "num", "region")
combined_data <- combined_data %>%
  mutate(across(c(age, sex, cp, trestbps, chol, fbs, restecg, thalach,
                  exang, oldpeak, slope, ca, thal, num), as.numeric))
```

    ## Warning: There were 14 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `across(...)`.
    ## Caused by warning:
    ## ! 强制改变过程中产生了NA
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 13 remaining warnings.

Dropped variables with excessive missing values (ca and thal). Removed
rows with missing values in critical variables. Converted num to binary
(0 = no heart disease, 1 = heart disease) and set as a factor.

``` r
cleaned_data <- combined_data %>% select(-ca, -thal)
critical_columns <- c("num", "age", "sex", "cp", "trestbps", "chol",
                      "fbs", "restecg", "thalach", "exang", "oldpeak", "slope")
cleaned_data <- cleaned_data %>% drop_na(all_of(critical_columns))
summary(cleaned_data)
```

    ##       age             sex               cp           trestbps    
    ##  Min.   :29.00   Min.   :0.0000   Min.   :1.000   Min.   :  0.0  
    ##  1st Qu.:49.00   1st Qu.:1.0000   1st Qu.:3.000   1st Qu.:120.0  
    ##  Median :56.00   Median :1.0000   Median :4.000   Median :130.0  
    ##  Mean   :54.84   Mean   :0.7608   Mean   :3.352   Mean   :133.4  
    ##  3rd Qu.:61.00   3rd Qu.:1.0000   3rd Qu.:4.000   3rd Qu.:142.0  
    ##  Max.   :77.00   Max.   :1.0000   Max.   :4.000   Max.   :200.0  
    ##       chol            fbs            restecg          thalach     
    ##  Min.   :  0.0   Min.   :0.0000   Min.   :0.0000   Min.   : 60.0  
    ##  1st Qu.:197.0   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:120.0  
    ##  Median :233.0   Median :0.0000   Median :0.0000   Median :140.0  
    ##  Mean   :216.9   Mean   :0.1601   Mean   :0.7439   Mean   :138.5  
    ##  3rd Qu.:273.0   3rd Qu.:0.0000   3rd Qu.:2.0000   3rd Qu.:159.0  
    ##  Max.   :603.0   Max.   :1.0000   Max.   :2.0000   Max.   :202.0  
    ##      exang           oldpeak           slope            num      
    ##  Min.   :0.0000   Min.   :-1.000   Min.   :1.000   Min.   :0.00  
    ##  1st Qu.:0.0000   1st Qu.: 0.100   1st Qu.:1.000   1st Qu.:0.00  
    ##  Median :0.0000   Median : 1.000   Median :2.000   Median :1.00  
    ##  Mean   :0.4972   Mean   : 1.218   Mean   :1.765   Mean   :1.13  
    ##  3rd Qu.:1.0000   3rd Qu.: 2.000   3rd Qu.:2.000   3rd Qu.:2.00  
    ##  Max.   :1.0000   Max.   : 6.200   Max.   :3.000   Max.   :4.00  
    ##     region         
    ##  Length:531        
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ## 

``` r
cleaned_data$num <- ifelse(cleaned_data$num > 0, 1, 0)
cleaned_data$num <- as.factor(cleaned_data$num)
logistic_model <- glm(num ~ age + sex + cp + trestbps + chol + fbs +
                      restecg + thalach + exang + oldpeak,
                      data = cleaned_data, family = binomial)
summary(logistic_model)
```

    ## 
    ## Call:
    ## glm(formula = num ~ age + sex + cp + trestbps + chol + fbs + 
    ##     restecg + thalach + exang + oldpeak, family = binomial, data = cleaned_data)
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.781382   1.610308  -1.727 0.084125 .  
    ## age          0.012047   0.014888   0.809 0.418401    
    ## sex          1.409350   0.281798   5.001 5.70e-07 ***
    ## cp           0.687260   0.134930   5.093 3.52e-07 ***
    ## trestbps     0.013883   0.006743   2.059 0.039502 *  
    ## chol        -0.003090   0.001397  -2.212 0.026959 *  
    ## fbs          0.008505   0.332098   0.026 0.979569    
    ## restecg      0.156645   0.134297   1.166 0.243450    
    ## thalach     -0.021518   0.005694  -3.779 0.000158 ***
    ## exang        0.890464   0.269251   3.307 0.000942 ***
    ## oldpeak      0.586433   0.126838   4.623 3.77e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 710.13  on 530  degrees of freedom
    ## Residual deviance: 447.38  on 520  degrees of freedom
    ## AIC: 469.38
    ## 
    ## Number of Fisher Scoring iterations: 5

(author= Thomas Tang) Significant Predictors (p-value \< 0.05): sex
(1.409350): Males are significantly more likely to have heart disease
than females. Odds ratio: exp(1.409)=4.09 → Males are 4.09 times more
likely to have heart disease compared to females. cp (0.687260): Higher
chest pain levels increase the odds of heart disease. Odds ratio:
exp(0.687)=1.99. trestbps (0.013883): Resting blood pressure has a small
but significant positive effect. Odds ratio: exp(0.0139)=1.014 per unit
increase. chol (-0.003090): Cholesterol has a small negative
association. Odds ratio: exp(−0.0031)=0.997, slightly decreasing odds.
thalach (-0.021518): Higher maximum heart rate achieved decreases the
odds of heart disease. Odds ratio: exp(−0.0215)=0.978 → Protective
effect. exang (0.894064): Exercise-induced angina increases the odds of
heart disease. exp(0.894)=2.44. oldpeak (0.586433):Higher ST depression
values significantly increase the odds of heart disease. Odds ratio:
exp(0.586)=1.80. fbs (Fasting blood sugar) and age do not have
significant association with heart disease in this dataset.

AIC: 469.38 (used for model comparison).

``` r
male_data <- cleaned_data %>% filter(sex == 1)
female_data <- cleaned_data %>% filter(sex == 0)

male_model <- glm(num ~ age + cp + trestbps + chol + fbs + restecg +
                  thalach + exang + oldpeak, data = male_data, family = binomial)
female_model <- glm(num ~ age + cp + trestbps + chol + fbs + restecg +
                    thalach + exang + oldpeak, data = female_data, family = binomial)
male_results <- summary(male_model)$coefficients
female_results <- summary(female_model)$coefficients

results_table <- data.frame(
  Variable = rownames(male_results),
  Male_Estimate = male_results[, "Estimate"],
  Male_Std_Error = male_results[, "Std. Error"],
  Male_P_Value = male_results[, "Pr(>|z|)"],
  Female_Estimate = female_results[, "Estimate"],
  Female_Std_Error = female_results[, "Std. Error"],
  Female_P_Value = female_results[, "Pr(>|z|)"]
)
print(results_table)
```

    ##                Variable Male_Estimate Male_Std_Error Male_P_Value
    ## (Intercept) (Intercept)   0.489143833    1.883367077 7.950815e-01
    ## age                 age   0.011078627    0.017509693 5.269205e-01
    ## cp                   cp   0.624244440    0.150392680 3.313681e-05
    ## trestbps       trestbps   0.006669625    0.007709166 3.869536e-01
    ## chol               chol  -0.002816539    0.001546731 6.861250e-02
    ## fbs                 fbs  -0.077076106    0.364747670 8.326427e-01
    ## restecg         restecg   0.211261443    0.158412456 1.823296e-01
    ## thalach         thalach  -0.026110546    0.006843324 1.359200e-04
    ## exang             exang   0.717197611    0.319844029 2.493970e-02
    ## oldpeak         oldpeak   0.568585698    0.143981103 7.846851e-05
    ##             Female_Estimate Female_Std_Error Female_P_Value
    ## (Intercept)    -7.814925653      3.385006792    0.020960883
    ## age             0.011405862      0.030248298    0.706118236
    ## cp              0.927630109      0.341356680    0.006578104
    ## trestbps        0.033262774      0.014713351    0.023776570
    ## chol           -0.003238931      0.003523281    0.357941805
    ## fbs             0.917958044      0.873286597    0.293188211
    ## restecg         0.017694465      0.282086696    0.949983859
    ## thalach        -0.011055801      0.012020177    0.357692858
    ## exang           1.143549158      0.548980044    0.037247284
    ## oldpeak         0.677208667      0.291294270    0.020081234

Significant Predictors for Males: Chest Pain (cp): Strong positive
effect (β=0.62, p\<0.001). Resting Blood Pressure (trestbps): Positive
and weakly significant (β=0.007, p=0.039). Max Heart Rate (thalach):
Negative and significant (β=−0.026, p=0.001), indicating a protective
effect. Exercise-Induced Angina (exang): Positive and significant
(β=0.71, p=0.025). ST Depression (oldpeak): Strong positive effect
(β=0.57, p\<0.001).

Significant Predictors for Females: Chest Pain (cp): Stronger positive
effect than males (β=0.93, p=0.006). Resting Blood Pressure (trestbps):
Positive and significant (β=0.033, p=0.023). Exercise-Induced Angina
(exang): Positive and significant (β=1.14, p=0.037), stronger effect
than in males. ST Depression (oldpeak): Strong positive effect (β=0.68,
p=0.020).

Key Comparisons Between Genders Chest Pain (cp): Stronger predictor for
females (β=0.93) than males (β=0.62). Exercise-Induced Angina (exang):
Higher odds for females (β=1.14) than males (β=0.71). ST Depression
(oldpeak): Significant and strong for both genders, slightly higher in
females (β=0.68).

(author: Yonghao YU)

## Try Random Forest Classifier!

### A brief intro to Random Forest Algorithm

Random Forest is an ensemble learning algorithm used for classification
and regression tasks. It builds multiple decision trees using bootstrap
sampling (random subsets of data) and selects features randomly at each
split to increase diversity. Each tree predicts independently, and the
final output is determined by majority voting (classification) or
averaging (regression). Random Forest is robust to overfitting, handles
high-dimensional data well, and provides feature importance scores.

### First, construct the model and generate the results!

author: Yonghao YU

``` r
library(caret)
```

    ## Warning: 程序包'caret'是用R版本4.4.2 来建造的

    ## 载入需要的程序包：lattice

    ## 
    ## 载入程序包：'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(randomForest)
```

    ## Warning: 程序包'randomForest'是用R版本4.4.2 来建造的

    ## randomForest 4.7-1.2

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## 载入程序包：'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
# drop out the variable "ca" and "thal" which are have so many missing values inside
variables = c("chol", "cp", "age", "thalach", "oldpeak", "num", "restecg", "fbs", "region", "slope", "trestbps", "exang")
data = combined_data_two[, variables]
data$num = as.factor(data$num)

# check and deal with missing data
if (any(is.na(data))) {
  print("Missing value detected")
  data = na.omit(data)
  print("Missing data have been deleted")
}
# split the dataset into training and testing datasets
set.seed(42)
trainIndex = createDataPartition(data$num, p = 0.8, list = FALSE)
trainData = data[trainIndex, ]
testData = data[-trainIndex, ]

# Construct the random forest model and evaluate the model results
rf_model = randomForest(num ~ ., data = trainData, importance = TRUE)
rf_pred = predict(rf_model, testData)
rf_conf_matrix = confusionMatrix(rf_pred, testData$num)
print("The model result is")
```

    ## [1] "The model result is"

``` r
print(rf_conf_matrix)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  0  1
    ##          0 31 14
    ##          1 10 50
    ##                                           
    ##                Accuracy : 0.7714          
    ##                  95% CI : (0.6793, 0.8477)
    ##     No Information Rate : 0.6095          
    ##     P-Value [Acc > NIR] : 0.000325        
    ##                                           
    ##                   Kappa : 0.5281          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.540291        
    ##                                           
    ##             Sensitivity : 0.7561          
    ##             Specificity : 0.7812          
    ##          Pos Pred Value : 0.6889          
    ##          Neg Pred Value : 0.8333          
    ##              Prevalence : 0.3905          
    ##          Detection Rate : 0.2952          
    ##    Detection Prevalence : 0.4286          
    ##       Balanced Accuracy : 0.7687          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

From the model we can observe the following things: 1. The model
correctly classified 77.14% of the instances. 2. The true accuracy is
expected to fall 95% of the time in (0.6793, 0.8477) 3. The information
rate is 0.6095 which is less than the accuracy rate (p-value also
indicate this), indicating the model we built actually capture some
significant features. 4. The Kappa is 0.5281 which is in the range
\[40,60\], which indicate our classifier achieves moderate level of
classification 5. High sensitivity (0.7561) indicates good
identification of positives. 6. High specificity (0.7812) indicates good
identification of negatives. 7. Balanced accuracy (76.87%) suggests the
model balances its performance across both classes well.

author: Yonghao YU \# Then we show the feature importance trends(The
trend is descending according to the MeanDecreaseAccuracy)

``` r
var_imp = importance(rf_model)
var_imp_df = as.data.frame(var_imp)
var_imp_df$Variable = rownames(var_imp_df)
rownames(var_imp_df) = NULL
var_imp_df = var_imp_df[order(var_imp_df$MeanDecreaseAccuracy, decreasing = TRUE), ]
ggplot(var_imp_df, aes(x = reorder(Variable, -MeanDecreaseAccuracy))) +
  geom_line(aes(y = MeanDecreaseAccuracy, group = 1, color = "MeanDecreaseAccuracy")) +
  geom_point(aes(y = MeanDecreaseAccuracy, color = "MeanDecreaseAccuracy")) +
  geom_line(aes(y = MeanDecreaseGini, group = 1, color = "MeanDecreaseGini")) +
  geom_point(aes(y = MeanDecreaseGini, color = "MeanDecreaseGini")) +
  labs(title = "Feature Importance Trends",
       x = "Features",
       y = "Importance",
       color = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
```

![](modelling_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Then we ranked the predictors descendingly based on the
MeanDecreaseAccuracy which measures the decrease in overall model
accuracy when the variable is permuted. And we show them in a line plot!
Based on the MeanDecreaseAccuracy and MeanDecreaseGini, we can drop out
restecg, trestbps, and fbs predictors that have relatively small impact
on our prediction. And then we can focus on the first eight predictors
that have more impact on our prediction results!

### Last, simulate the prediction which predicts the num with the new data based on the model we built!

author: Yonghao YU

``` r
# construct a new dataframe which includes new data
new_data = data.frame(
  age = c(63,39),
  sex = c(1, 1),
  cp = c(1, 2),
  trestbps = c(145, 120),
  chol = c(233, 200),
  fbs = c(1, 0),
  restecg = c(2, 0),
  thalach = c(150, 160),
  exang = c(0, 1),
  oldpeak = c(2.3, 1),
  slope = c(3, 2),
  region = c(1, 2)
)
# predict the results based on the model we have trained
predicted_num = predict(rf_model, new_data)
print("The prediction result is：")
```

    ## [1] "The prediction result is："

``` r
print(data.frame(new_data, Predicted_num = predicted_num))
```

    ##   age sex cp trestbps chol fbs restecg thalach exang oldpeak slope region
    ## 1  63   1  1      145  233   1       2     150     0     2.3     3      1
    ## 2  39   1  2      120  200   0       0     160     1     1.0     2      2
    ##   Predicted_num
    ## 1             0
    ## 2             0

From the results, we can see that the model can generate some results
based on the predictor values we put in!
