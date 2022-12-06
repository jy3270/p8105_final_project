Regression analysis
================
Jingya Yu
2022-12-04

``` r
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
library(ggplot2)
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

Here I am interested in which features best predict the outcome - Ladder
Score. I am considering a multiple linear regression to see how
variables relate to the outcome. Once important question that I am
interested in is how much does GDP impacts the Ladder Score.

## regression data import

``` r
regression_df = 
  read_csv("data/world-happiness-report-2021.csv") %>% 
  janitor::clean_names() 
head(regression_df)
```

    ## # A tibble: 6 × 20
    ##   country_name regiona…¹ ladde…² stand…³ upper…⁴ lower…⁵ logge…⁶ socia…⁷ healt…⁸
    ##   <chr>        <chr>       <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Finland      Western …    7.84   0.032    7.90    7.78    10.8   0.954    72  
    ## 2 Denmark      Western …    7.62   0.035    7.69    7.55    10.9   0.954    72.7
    ## 3 Switzerland  Western …    7.57   0.036    7.64    7.5     11.1   0.942    74.4
    ## 4 Iceland      Western …    7.55   0.059    7.67    7.44    10.9   0.983    73  
    ## 5 Netherlands  Western …    7.46   0.027    7.52    7.41    10.9   0.942    72.4
    ## 6 Norway       Western …    7.39   0.035    7.46    7.32    11.1   0.954    73.3
    ## # … with 11 more variables: freedom_to_make_life_choices <dbl>,
    ## #   generosity <dbl>, perceptions_of_corruption <dbl>,
    ## #   ladder_score_in_dystopia <dbl>, explained_by_log_gdp_per_capita <dbl>,
    ## #   explained_by_social_support <dbl>,
    ## #   explained_by_healthy_life_expectancy <dbl>,
    ## #   explained_by_freedom_to_make_life_choices <dbl>,
    ## #   explained_by_generosity <dbl>, …

## variable description

-   Ladder Score or Happiness Score: “Please imagine a ladder, with
    steps numbered from 0 at the bottom to 10 at the top. The top of the
    ladder represents the best possible life for you and the bottom of
    the ladder represents the worst possible life for you. On which step
    of the ladder would you say you personally feel you stand at this
    time?”.

-   Log GDP per capita: It is the total monetary or market value of all
    the finished goods and services produced within a country’s borders
    in a specific time period.

-   Healthy life expectancy: Based on the data extracted from the World
    Health Organization’s (WHO)

-   Social Suport: “If you were in trouble, do you have relatives or
    friends you can count on to help you whenever you need them, or
    not?”.

-   Freedom to make life choices: “Are you satisfied or dissatisfied
    with your freedom to choose what you do with your life?”

-   Generosity: “Have you donated money to a charity in the past month?”
    on GDP per capita.

-   Corruption Perception: “Is corruption widespread throughout the
    government or not” and “Is corruption widespread within businesses
    or not?”

## change column names and check if there is na

``` r
regression_df$country_name <- as.factor(regression_df$country_name)

regression_df$regional_indicator<- as.factor(regression_df$regional_indicator)

regression_df <- regression_df %>%
  rename(Country = country_name, Region = regional_indicator, Ladder = ladder_score,
         SD.Ladder = standard_error_of_ladder_score, GDP = logged_gdp_per_capita,
         Life.exp = healthy_life_expectancy, Freedom = freedom_to_make_life_choices,
         Currpotion = perceptions_of_corruption, Ladder.Dystopia = ladder_score_in_dystopia,
         EXP.LOG.GPD = explained_by_log_gdp_per_capita, EXP.SS = explained_by_social_support,
         EXP.HLE = explained_by_healthy_life_expectancy, EXP.FREE = explained_by_freedom_to_make_life_choices,
         EXP.GEN = explained_by_generosity, EXP.CUR = explained_by_perceptions_of_corruption,
         DYS.RES = dystopia_residual)

hap <- regression_df[,-c(4,5,6,13:20)]
colnames(hap)
```

    ## [1] "Country"        "Region"         "Ladder"         "GDP"           
    ## [5] "social_support" "Life.exp"       "Freedom"        "generosity"    
    ## [9] "Currpotion"

``` r
sum(is.na(regression_df)) # there is no na
```

    ## [1] 0

## Now, I would like to check the correlations between each of the variables. In order to use use the cor() function, the variables need to be numeric. I use the select_if() function on the hap dataframe and select the data that is numeric.

## Then we print the correlation matrix and plot the correlation graph.

``` r
num.var <- select_if(hap, is.numeric)
M<-cor(num.var)
M
```

    ##                     Ladder        GDP social_support   Life.exp    Freedom
    ## Ladder          1.00000000  0.7897597      0.7568876  0.7680995  0.6077531
    ## GDP             0.78975970  1.0000000      0.7852987  0.8594606  0.4323235
    ## social_support  0.75688765  0.7852987      1.0000000  0.7232561  0.4829298
    ## Life.exp        0.76809946  0.8594606      0.7232561  1.0000000  0.4614939
    ## Freedom         0.60775307  0.4323235      0.4829298  0.4614939  1.0000000
    ## generosity     -0.01779928 -0.1992864     -0.1149459 -0.1617503  0.1694374
    ## Currpotion     -0.42114000 -0.3423374     -0.2032070 -0.3643735 -0.4013630
    ##                 generosity Currpotion
    ## Ladder         -0.01779928 -0.4211400
    ## GDP            -0.19928640 -0.3423374
    ## social_support -0.11494585 -0.2032070
    ## Life.exp       -0.16175028 -0.3643735
    ## Freedom         0.16943737 -0.4013630
    ## generosity      1.00000000 -0.1639617
    ## Currpotion     -0.16396173  1.0000000

``` r
corrplot(M)
```

![](regression_analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
