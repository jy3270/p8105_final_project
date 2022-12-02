Data
================
Jingya Yu, Landi Guo, Fengdi Zhang, Meng Fang, Yixuan Jiao
2022-11-11

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
happiness_2021 <- read_csv("data/world-happiness-report-2021.csv") %>% janitor::clean_names()
```

    ## Rows: 149 Columns: 20
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): Country name, Regional indicator
    ## dbl (18): Ladder score, Standard error of ladder score, upperwhisker, lowerw...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
happiness_pre_2021 <- read_csv("data/world-happiness-report.csv") %>% janitor::clean_names()
```

    ## Rows: 1949 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): Country name
    ## dbl (10): year, Life Ladder, Log GDP per capita, Social support, Healthy lif...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
happiness_2021 <-
  happiness_2021 %>%
  select(country_name:ladder_score,logged_gdp_per_capita:perceptions_of_corruption) %>%
  mutate(year = 2021) 

happiness_pre_2021 <-
  happiness_pre_2021 %>%
  select(-positive_affect,-negative_affect) %>%
  #mutate(regional_indicator = NA) %>%
  rename("ladder_score" = "life_ladder", 
         "logged_gdp_per_capita" = "log_gdp_per_capita",
         "healthy_life_expectancy" = "healthy_life_expectancy_at_birth")

happiness_2021 <- 
  happiness_2021 %>%
  nest(data1 = ladder_score:year)
happiness_pre_2021 <- 
  happiness_pre_2021 %>%
  nest(data2 = year:perceptions_of_corruption)
df <- 
  happiness_2021 %>%
  left_join(happiness_pre_2021) %>%
  mutate(data = map2(data1,data2,bind_rows)) %>%
  select(-data1,-data2) %>%
  unnest(data) %>%
  drop_na() %>%
  relocate(country_name,regional_indicator,year) %>%
  arrange(country_name,regional_indicator,-year)
```

    ## Joining, by = "country_name"

``` r
write_csv(df, "Data/happiness.csv")
```
