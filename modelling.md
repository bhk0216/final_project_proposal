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
