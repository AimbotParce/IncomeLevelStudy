<!-- README.md is generated from IncomeStudy.Rmd. Please edit that document. -->

Today I saw some kid on *Tik Tok* saying that there’s no point in
studying anything because doing so wouldn’t increase your income. I
thought that was a pretty bold statement, so I did what any reasonable
person would do: I went to Kaggle to find a dataset that would help me
prove him wrong.

I found a dataset called `income.csv` that contains information about
american people’s income and education.

You can find it here:
<https://www.kaggle.com/datasets/amirhosseinmirzaie/americancitizenincome?resource=download>

Here’s an explanation of the columns:

| **Column** | **Description** |
|:----------------------------------:|:----------------------------------:|
| age | Age |
| workclass | A general term indicating the employment status of an individual. |
| fnlwgt | Final weight, representing the number of individuals that this row represents (a representative sample). |
| education | Highest level of education achieved by an individual. |
| education.num | Highest level of education achieved by an individual in numerical form. |
| marital.status | Marital status of an individual. Note that Married-civ-spouse refers to a civilian spouse, and Married-AF-spouse refers to a spouse in the Armed Forces. |
| occupation | General type of occupation of an individual. |
| relationship | Relationship of this individual with others, for example, spouse (Husband). Each data point has only one relationship. |
| race | Race |
| sex | Biological sex of an individual. |
| capital.gain | Capital gains of an individual. |
| capital.loss | Capital losses of an individual. |
| hours.per.week | Number of hours the individual reported working per week. |
| native.country | Country of origin. |
| income | Income, less than or equal to $50,000 (\<=50K) or more than that (\>50K). |

For this study I’ll be using the following libraries:

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 4.4.2

``` r
library(FactoMineR)
```

    ## Warning: package 'FactoMineR' was built under R version 4.4.2

### Loading and preparing the data

``` r
df <- read_csv("income.csv")
```

    ## Rows: 2 Columns: 1
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): version https://git-lfs.github.com/spec/v1
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
summary(df)
```

    ##  version https://git-lfs.github.com/spec/v1
    ##  Length:2                                  
    ##  Class :character                          
    ##  Mode  :character
