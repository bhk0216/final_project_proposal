Final Project Proposal
================
Stella Koo
2024-10-31

## Group members (names and UNIs)

Stella Koo (bk2959), Yixin Zheng (yz4993)

## Project Title

The Influence of Diagnostic Attributes on Heart Disease Across Regions
(with Varying on Development Levels or Climate Groups?)

## Motivation (Ian)

## Intended Final Products (Yixin)

## Data Sources (Stella)

The Heart Disease datasets were obtained from UC Irvine’s Machine
Learning Repository that can be accessed
[here](https://archive.ics.uci.edu/dataset/45/heart+disease). This
directory includes four datasets related to heart disease diagnosis,
with all attributes represented as numeric values. The data was gathered
from the following four locations:

    1. Cleveland Clinic Foundation (cleveland.data)
    2. Hungarian Institute of Cardiology, Budapest (hungarian.data) 
    3. V.A. Medical Center, Long Beach, CA (long-beach-va.data) 
    4. University Hospital, Zurich, Switzerland (switzerland.data)

Although the original datasets contain 76 raw attributes, the source
provides processed datasets with 14 carefully selected variables that
have been widely utilized and cited in numerous research studies. This
project will focus on these processed datasets, which include the
following attributes:

    1. age: age in years
    2. sex: sex (1 = male; 0 = female)
    3. cp: chest pain type (1 = typical angina; 2 = atypical angina; 3 = non-anginal pain; 4 = asymptomatic)
    4. trestbps: resting blood pressure (in mm Hg on admission to the hospital)
    5. chol: serum cholestoral in mg/dl
    6. fbs: fasting blood sugar > 120 mg/dl (1 = true; 0 = false)
    7. restecg: resting electrocardiographic results (0 = normal; 1 = having ST-T wave abnormality; 2 = showing probable or definite left ventricular hypertrophy by Estes' criteria)
    8. thalach: maximum heart rate achieved
    9. exang: exercise induced angina (1 = yes; 0 = no)
    10. oldpeak: T depression induced by exercise relative to rest
    11. slope: slope of the peak exercise ST segment (1 = upsloping; 2 = flat, 3 = downsloping)
    12. ca: number of major vessels (0-3) colored by flourosopy
    13. thal: 3 = normal; 6 = fixed defect; 7 = reversable defect
    14. num: diagnosis of heart disease (0 = < 50% diameter narrowing; 1 = > 50% diameter narrowing)

While our goal is to explore all the variables listed above, some
datasets contain a significant amount of missing data for certain
attributes. The approach we will take to address these gaps is still
under consideration.

## Planned analyses, Visualizations, Coding Challenges (Thomas, Yonghao)

- Linear Regression Model
  - Diagnostics: Check assumptions for linear regression
  - Address hypothesis, R squared value
  - QQ Plot
- Correlation Heatmap
- Interactive Variables
- Exploratory Analysis (Graphic Displays) Try different approaches
  (maybe merge Cleveland and Long Beach, both same country) and check
  which approach works better.

## Planned Timeline (Stella)

**November 11th to 17th:**

- Meet with TA to address project-related questions and finalize the
  project title.
- Hold a group meeting to assign tasks and decide on the structure and
  sections of the website.
- Begin data cleaning and initiate the planned analysis.

**November 19th to 24th:**

- Finalize the planned analysis and start creating visualizations.
- Draft the report as the analysis progresses.
- Develop the website’s core structure in R.
- Conduct a group meeting to review progress and clarify any outstanding
  issues.

**November 25th to December 1st:**

- Complete any remaining analysis and finalize visualizations.
- Integrate all components into the website.
- Hold a group meeting to create and review the screencast (explanatory
  video).

**December 2nd to 7th:**

- Finalize the report and website, making any necessary revisions.
- Conduct peer assessment and submit final project.
