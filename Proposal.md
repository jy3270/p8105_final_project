P8105: Group Project Proposal
================
Jingya Yu, Landi Guo, Fengdi Zhang, Meng Fang, Yixuan Jiao
2022-11-11

**The group members (names and UNIs):** Jingya Yu jy3270, Meng Fang
(mf3532) , Landi Guo (lg3239), Fengdi Zhang (fz2352), Yixuan Jiao
(yj2752)

**The tentative project title:** World Happiness Analysis

**The motivation for this project**

The annual World Happiness Report for 2022 was released Friday, March
18th, and according to the ranking, the U.S. moved up three spots from
last year. Now, at a time of pandemic and war, we need such an effort
more than ever, and the lesson of the World Happiness Report over the
years is that social support, generosity to one another, and honesty in
government are crucial for well-being. How do these factors influence
our happiness?

**The intended final products**

Shiny App containing interactive interface for our analysis result;

Webpage(introduction, background, analysis, discussion board);

Report (Introduction, Data Cleaning, EDA, Analysis, Results);

**The anticipated data sources**

[World Happiness Report from
2008-2021](https://www.kaggle.com/datasets/ajaypalsinghlo/world-happiness-report-2021?resource=download&select=world-happiness-report-2021.csv)

Data Description: The happiness scores and rankings use data from the
Gallup World Poll . The report has six factors – economic production,
social support, life expectancy, freedom, absence of corruption, and
generosity – contribute to making life evaluations higher in each
country. They have no impact on the total score reported for each
country, but they do explain why some countries rank higher than others.

(Other data sources containing useful information predicting the
happiness scores may be used as well)

**The planned analyses / visualizations / coding challenges**

-   Summary statistics analysis (Mean, median, sd… of each variable)
-   Visualization
    -   Bar graph of mean Life Ladder score for each region
    -   Bar graph of countries with top 10 Life Ladder
    -   Line graph for each predictor vs. Life Ladder
    -   Geospatial analysis
-   Statistical Tests: Compare the mean happiness scores based on
    regions and continents (ANOVA).
-   Regression analysis:
    -   Using Life Ladder from year 2008-2021 to predict future Life
        Ladder score
    -   Using predictors(Log GDP per capita, Social support, Healthy
        life expectancy at birth, Freedom to make life choices,
        Generosity) to predict Life Ladder for each country
-   Coding challenges:
    -   Cleaning and merging potentially useful dataset
    -   Designing Shiny app for presenting results
    -   Using packages like leaflet to present geospatial analysis
    -   Conducting statistical test in R

**The planned timeline**

11/15-18: project review meeting

11/20: finish data cleaning

11/22: finish summary statistical analysis

11/25: finish visualization

11/28: finish statistical tests

12/2: finish regression analysis

12/7: finish webpage, finalize report

12/9: record screencast
