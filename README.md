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
library(FactoMineR)
```

### Loading and preparing the data

``` r
df = read_csv("income.csv")
```

    ## Rows: 25000 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): workclass, education, marital.status, occupation, relationship, rac...
    ## dbl (6): age, fnlwgt, education.num, capital.gain, capital.loss, hours.per.week
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
summary(df)
```

    ##       age         workclass             fnlwgt         education        
    ##  Min.   :17.00   Length:25000       Min.   :  12285   Length:25000      
    ##  1st Qu.:28.00   Class :character   1st Qu.: 117983   Class :character  
    ##  Median :37.00   Mode  :character   Median : 178211   Mode  :character  
    ##  Mean   :38.61                      Mean   : 189661                     
    ##  3rd Qu.:48.00                      3rd Qu.: 237068                     
    ##  Max.   :90.00                      Max.   :1484705                     
    ##  education.num   marital.status      occupation        relationship      
    ##  Min.   : 1.00   Length:25000       Length:25000       Length:25000      
    ##  1st Qu.: 9.00   Class :character   Class :character   Class :character  
    ##  Median :10.00   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :10.08                                                           
    ##  3rd Qu.:12.00                                                           
    ##  Max.   :16.00                                                           
    ##      race               sex             capital.gain    capital.loss    
    ##  Length:25000       Length:25000       Min.   :    0   Min.   :   0.00  
    ##  Class :character   Class :character   1st Qu.:    0   1st Qu.:   0.00  
    ##  Mode  :character   Mode  :character   Median :    0   Median :   0.00  
    ##                                        Mean   : 1083   Mean   :  87.49  
    ##                                        3rd Qu.:    0   3rd Qu.:   0.00  
    ##                                        Max.   :99999   Max.   :4356.00  
    ##  hours.per.week  native.country        income         
    ##  Min.   : 1.00   Length:25000       Length:25000      
    ##  1st Qu.:40.00   Class :character   Class :character  
    ##  Median :40.00   Mode  :character   Mode  :character  
    ##  Mean   :40.44                                        
    ##  3rd Qu.:45.00                                        
    ##  Max.   :99.00

Let’s go column by column to clean the data and ensure that it’s ready
for analysis.

``` r
# workclass column should be a factor. "?" values are N/A's
df$workclass = as.factor(df$workclass)
df$workclass[df$workclass == "?"] = NA

# Education is also a factor.
df$education = as.factor(df$education)

# Also, education.num is an ordinal variable, that represents the same information 
# as education. To check that this holds through the dataset, we'll simply check
# that there's the same amount of unique values in "education" that there are in
# the combination of "education" and "education.num"
nrow(unique(df[, c("education", "education.num")])) # 16
```

    ## [1] 16

``` r
length(unique(df$education)) # 16
```

    ## [1] 16

``` r
# Marital status, occupation, relationship, race and sex are all factors
df$marital.status = as.factor(df$marital.status)
df$occupation = as.factor(df$occupation)
df$relationship = as.factor(df$relationship)
df$race = as.factor(df$race)
df$sex = as.factor(df$sex)

# Capital gain is numeric, but it looks like there may be some values that should
# be N/A's (Those that are 99999)
df$capital.gain[df$capital.gain == 99999] = NA

# Same with hours.per.week, 99 hours per week seems like a lot
df$hours.per.week[df$hours.per.week == 99] = NA

# Native country is a factor
df$native.country = as.factor(df$native.country)

# Income is a factor for some reason.
df$income = as.factor(df$income)
```

Here’s how the data looks like now:

``` r
summary(df)
```

    ##       age                   workclass         fnlwgt               education   
    ##  Min.   :17.00   Private         :17471   Min.   :  12285   HS-grad     :8025  
    ##  1st Qu.:28.00   Self-emp-not-inc: 1935   1st Qu.: 117983   Some-college:5621  
    ##  Median :37.00   Local-gov       : 1553   Median : 178211   Bachelors   :4104  
    ##  Mean   :38.61   State-gov       : 1004   Mean   : 189661   Masters     :1301  
    ##  3rd Qu.:48.00   Self-emp-inc    :  851   3rd Qu.: 237068   Assoc-voc   :1063  
    ##  Max.   :90.00   (Other)         :  757   Max.   :1484705   11th        : 905  
    ##                  NA's            : 1429                     (Other)     :3981  
    ##  education.num                 marital.status            occupation  
    ##  Min.   : 1.00   Divorced             : 3390   Prof-specialty :3198  
    ##  1st Qu.: 9.00   Married-AF-spouse    :   18   Craft-repair   :3132  
    ##  Median :10.00   Married-civ-spouse   :11518   Exec-managerial:3114  
    ##  Mean   :10.08   Married-spouse-absent:  312   Adm-clerical   :2884  
    ##  3rd Qu.:12.00   Never-married        : 8204   Sales          :2812  
    ##  Max.   :16.00   Separated            :  792   Other-service  :2524  
    ##                  Widowed              :  766   (Other)        :7336  
    ##          relationship                   race           sex       
    ##  Husband       :10146   Amer-Indian-Eskimo:  252   Female: 8282  
    ##  Not-in-family : 6345   Asian-Pac-Islander:  799   Male  :16718  
    ##  Other-relative:  744   Black             : 2412                 
    ##  Own-child     : 3897   Other             :  209                 
    ##  Unmarried     : 2661   White             :21328                 
    ##  Wife          : 1207                                            
    ##                                                                  
    ##   capital.gain    capital.loss     hours.per.week        native.country 
    ##  Min.   :    0   Min.   :   0.00   Min.   : 1.00   United-States:22415  
    ##  1st Qu.:    0   1st Qu.:   0.00   1st Qu.:40.00   Mexico       :  502  
    ##  Median :    0   Median :   0.00   Median :40.00   ?            :  437  
    ##  Mean   :  610   Mean   :  87.49   Mean   :40.28   Philippines  :  142  
    ##  3rd Qu.:    0   3rd Qu.:   0.00   3rd Qu.:45.00   Germany      :  105  
    ##  Max.   :41310   Max.   :4356.00   Max.   :98.00   Canada       :   87  
    ##  NA's   :119                       NA's   :69      (Other)      : 1312  
    ##    income     
    ##  <=50K:18955  
    ##  >50K : 6045  
    ##               
    ##               
    ##               
    ##               
    ## 

### Exploratory Data Analysis

Now that the data is clean, let’s do some exploratory data analysis to
see if we can find any interesting patterns.

Let’s start by looking at the distribution of income

``` r
table(df$income, df$education.num)
```

    ##        
    ##            1    2    3    4    5    6    7    8    9   10   11   12   13   14
    ##   <=50K   36  131  252  446  383  691  858  320 6744 4530  793  605 2391  574
    ##   >50K     0    4   13   30   20   45   47   24 1281 1091  270  204 1713  727
    ##        
    ##           15   16
    ##   <=50K  116   85
    ##   >50K   333  243

It looks like there’s a clear pattern here. Looks like the higher the
education, the more likely you are to earn more than 50K a year.

There’s an interesting tool to check whether two categorical values are
or not independent. It’s called the Chi-Square test. Let’s use it to
check if there’s a statistically significant relationship between income
and education.

``` r
chisq.test(table(df$income, df$education.num))
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  table(df$income, df$education.num)
    ## X-squared = 3454.5, df = 15, p-value < 2.2e-16

This test assumes the null hypothesis that there is no association
between the two variables. The p-value is virtually zero, so we can
reject the null hypothesis and conclude that there is a significant
association between income and education.

Having proven that, we want to check, not only if there’s an
association, but also that there is a positive correlation. We’ll check
this in two ways.

First, we’ll convert income to a numeric variable that takes values `0`
and `1` and then we’ll check the correlation between this new variable
and `education.num`.

``` r
income = as.numeric(df$income) # Easy way to create a numeric column the same size as df$income
income[df$income == "<=50K"] = 0
income[df$income == ">50K"] = 1
cor.test(df$education.num, income, method = "spearman")
```

    ## Warning in cor.test.default(df$education.num, income, method = "spearman"):
    ## Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  df$education.num and income
    ## S = 1.7371e+12, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.3329605

Simply by looking at the p-value, which is virtually zero, we can
conclude that the hypothesis of there being no correlation between these
newly created numeric variables can be rejected. This means that we,
again, can conclude that there is a correlation, which the method, also,
tells us to be positive (0.33).

Finally, we’ll use a logistic regression to check if we can predict
income based on education.

``` r
model = glm(income ~ education.num, data = df, family = "binomial")
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = income ~ education.num, family = "binomial", data = df)
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -5.046229   0.080739  -62.50   <2e-16 ***
    ## education.num  0.367352   0.007163   51.28   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 27657  on 24999  degrees of freedom
    ## Residual deviance: 24488  on 24998  degrees of freedom
    ## AIC: 24492
    ## 
    ## Number of Fisher Scoring iterations: 4

From this output, we really only care about the coefficient
*β*<sub>1</sub>, which refers to the change in the log-odds of the
income being greater than 50K for a single education level increase. The
value for this coefficient is 0.37, with a p-value of virtually zero
(meaning this is a statistically significant result). This means that
for every level of education you increase, the odds of you earning more
than 50K a year increase by 44% (since *e*<sup>0.37</sup> = 1.44, which
is the ratio of proportions).

### Conclusion

So, in conclusion, the kid on *Tik Tok* was wrong. Studying does increase
your income. Although, I must admit, I was expecting a bigger effect.
