
# rand36sf

**Recode and scale the RAND SF-36 Questionnaire**

The `rand36sf` package recodes and scores the eight scales of the RAND
SF-36 Health Survey from raw item responses in a data frame. Items are
recoded to a 0-100 scale and averaged to create the eight scales:
Physical Functioning, Role Limitations due to Physical Health, Role
Limitations due to Emotional Problems, Energy/Fatigue, Emotional
Well-Being, Social Functioning, Pain, and General Health. This is done
as per the [scoring
manual](https://www.rand.org/health/surveys/mos/36-item-short-form/scoring.html).

## Installation

You can install the development version of rand36sf from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("maher42/rand36sf")
```

## Example

`score_rand36sf()` expects each row to be one completed questionnaire:
any leading identifier columns (here, an `id` and a `timepoint`),
followed by the 36 SF-36 items in standard order. The bundled data has
six respondents, each measured at two timepoints:

``` r
library(rand36sf)

data(sf36_example)
head(sf36_example)
```

    ##   id timepoint item1 item2 item3 item4 item5 item6 item7 item8 item9 item10
    ## 1  1         1     1     4     2     1     2     3     2     2     2      3
    ## 2  1         2     5     2     3     1     3     1     2     3     3      2
    ## 3  2         1     1     2     2     2     2     2     2     1     1      3
    ## 4  2         2     1     3     1     2     1     1     1     1     2      2
    ## 5  3         1     2     1     2     3     2     1     2     2     1      2
    ## 6  3         2     4     1     2     3     3     1     1     3     3      1
    ##   item11 item12 item13 item14 item15 item16 item17 item18 item19 item20 item21
    ## 1      1      2      1      1      2      2      2      1      1      3      5
    ## 2      2      3      2      2      1      2      2      2      2      4      1
    ## 3      2      2      2      1      2      1      1      1      2      5      4
    ## 4      3      2      2      1      2      1      1      1      1      2      1
    ## 5      2      2      2      2      1      1      1      2      2      1      6
    ## 6      3      2      2      2      2      2      2      2      1      1      1
    ##   item22 item23 item24 item25 item26 item27 item28 item29 item30 item31 item32
    ## 1      4      2      4      3      3      2      6      4      6      4      2
    ## 2      2      2      2      3      5      1      4      5      2      2      2
    ## 3      5      2      2      4      4      1      5      6      4      1      1
    ## 4      5      2      1      5      5      5      4      6      3      4      2
    ## 5      3      4      3      4      6      2      3      4      1      2      3
    ## 6      5      4      4      2      1      3      2      5      5      1      1
    ##   item33 item34 item35 item36
    ## 1      2      4      2      5
    ## 2      2      1      1      2
    ## 3      2      2      4      4
    ## 4      4      3      1      2
    ## 5      1      1      3      4
    ## 6      2      2      1      1

The first two columns are identifiers and the items begin at column 3,
which is the default for the function’s `start_col` argument.

### Scoring the survey

Pass the data to `score_rand36sf()`. It returns your original data frame
with eight new columns — one per scale — each scored on a 0 to 100
scale, where higher is better:

``` r
scored_df <- score_rand36sf(sf36_example)

head(scored_df)
```

    ##   id timepoint item1 item2 item3 item4 item5 item6 item7 item8 item9 item10
    ## 1  1         1   100    25    50     0    50   100    50    50    50    100
    ## 2  1         2     0    75   100     0   100     0    50   100   100     50
    ## 3  2         1   100    75    50    50    50    50    50     0     0    100
    ## 4  2         2   100    50     0    50     0     0     0     0    50     50
    ## 5  3         1    75   100    50   100    50     0    50    50     0     50
    ## 6  3         2    25   100    50   100   100     0     0   100   100      0
    ##   item11 item12 item13 item14 item15 item16 item17 item18 item19 item20 item21
    ## 1      0     50      0      0    100    100    100      0      0     50     20
    ## 2     50    100    100    100      0    100    100    100    100     25    100
    ## 3     50     50    100      0    100      0      0      0    100      0     40
    ## 4    100     50    100      0    100      0      0      0      0     75    100
    ## 5     50     50    100    100      0      0      0    100    100    100      0
    ## 6    100     50    100    100    100    100    100    100      0    100    100
    ##   item22 item23 item24 item25 item26 item27 item28 item29 item30 item31 item32
    ## 1     25     80     60     40     60     80    100     60      0     60     25
    ## 2     75     80     20     40     20    100     60     80     80     20     25
    ## 3      0     80     20     60     40    100     80    100     40      0      0
    ## 4      0     80      0     80     20     20     60    100     60     60     25
    ## 5     50     40     40     60      0     80     40     60    100     20     50
    ## 6      0     40     60     20    100     60     20     80     20      0      0
    ##   item33 item34 item35 item36 Physical functioning
    ## 1     25     25     25      0                   50
    ## 2     25    100      0     75                   65
    ## 3     25     75     75     25                   45
    ## 4     75     50      0     75                   30
    ## 5      0    100     50     25                   45
    ## 6     25     75      0    100                   60
    ##   Role limitations due to physical health
    ## 1                                      50
    ## 2                                      75
    ## 3                                      50
    ## 4                                      50
    ## 5                                      50
    ## 6                                     100
    ##   Role limitations due to emotional problems Energy/fatigue
    ## 1                                   33.33333             70
    ## 2                                  100.00000             70
    ## 3                                   33.33333             70
    ## 4                                    0.00000             65
    ## 5                                   66.66667             50
    ## 6                                   66.66667             45
    ##   Emotional well-being Social functioning Pain General health
    ## 1                   52               37.5 22.5             35
    ## 2                   44               25.0 87.5             40
    ## 3                   48                0.0 20.0             60
    ## 4                   44               50.0 50.0             60
    ## 5                   48               75.0 25.0             50
    ## 6                   44               50.0 50.0             45
