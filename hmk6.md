hmk6
================
Xinyi Wang
12/7/2020

``` r
library(tidyverse)
library(modelr)
library(p8105.datasets)
```

### Problem 1

``` r
homicide_df = 
  read_csv("homicide-data.csv", na = c("", "NA", "Unknown")) %>% 
  mutate(
    city_state = str_c(city, state, sep = ", "),
    victim_age = as.numeric(victim_age),
    resolution = case_when(
      disposition == "Closed without arrest" ~ 0,
      disposition == "Open/No arrest"        ~ 0,
      disposition == "Closed by arrest"      ~ 1)
  ) %>% 
  filter(
    victim_race %in% c("White", "Black"),
    city_state != "Tulsa, AL") %>% 
  select(city_state, resolution, victim_age, victim_race, victim_sex)
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_double(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_double(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

Start with one city.

``` r
baltimore_df =
  homicide_df %>% 
  filter(city_state == "Baltimore, MD")
glm(resolution ~ victim_age + victim_race + victim_sex, 
    data = baltimore_df,
    family = binomial()) %>% 
  broom::tidy() %>% 
  mutate(
    OR = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error)
  ) %>% 
  select(term, OR, starts_with("CI")) %>% 
  knitr::kable(digits = 3)
```

| term              |    OR | CI\_lower | CI\_upper |
| :---------------- | ----: | --------: | --------: |
| (Intercept)       | 1.363 |     0.975 |     1.907 |
| victim\_age       | 0.993 |     0.987 |     1.000 |
| victim\_raceWhite | 2.320 |     1.648 |     3.268 |
| victim\_sexMale   | 0.426 |     0.325 |     0.558 |

Try this across cities.

``` r
models_results_df = 
  homicide_df %>% 
  nest(data = -city_state) %>% 
  mutate(
    models = 
      map(.x = data, ~glm(resolution ~ victim_age + victim_race + victim_sex, data = .x, family = binomial())),
    results = map(models, broom::tidy)
  ) %>% 
  select(city_state, results) %>% 
  unnest(results) %>% 
  mutate(
    OR = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error)
  ) %>% 
  select(city_state, term, OR, starts_with("CI")) 
```

``` r
models_results_df %>% 
  filter(term == "victim_sexMale") %>% 
  mutate(city_state = fct_reorder(city_state, OR)) %>% 
  ggplot(aes(x = city_state, y = OR)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

<img src="hmk6_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

## Problem 2

load data

``` r
baby_df = 
  read_csv("birthweight.csv") 
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

clean the data for regression

There is no missing data

``` r
baby_df = 
 baby_df %>% 
 mutate(
  babysex = factor(case_when(
   babysex == 1 ~ "male",
   babysex == 2 ~ "female"
   )),
  frace = factor(case_when(
   frace == 1 ~ "white",
   frace == 2 ~ "black",
   frace == 3 ~ "asion",
   frace == 4 ~ "puerto",
   frace == 8 ~ "other",
   frace == 9 ~ "unknown")),
  malform = factor(case_when(
   malform == 0 ~ "absent",
   malform == 1 ~ "present"
  )),
  mrace = factor(case_when(
   mrace == 1 ~ "white",
   mrace == 2 ~ "black",
   mrace == 3 ~ "asion",
   mrace == 4 ~ "puerto",
   mrace == 8 ~ "other")),
  
  bwt = bwt *0.00220462262185
 ) 
 

sum(is.na(baby_df))
```

    ## [1] 0

take a look at variables and finally choose 7 parameters with p-value \<
0.05. They are babysex, bhead, blength, delwt, gaweeks, parity, smoken.

``` r
baby_df %>% 
 lm(bwt ~ ., data = .) %>% 
 broom::tidy() %>% 
 filter (p.value < 0.05 ) %>% 
 knitr::kable()
```

| term        |     estimate | std.error |  statistic |   p.value |
| :---------- | -----------: | --------: | ---------: | --------: |
| (Intercept) | \-13.9041902 | 1.4534283 | \-9.566478 | 0.0000000 |
| babysexmale |  \-0.0632888 | 0.0186627 | \-3.391197 | 0.0007021 |
| bhead       |    0.2883165 | 0.0076111 |  37.881183 | 0.0000000 |
| blength     |    0.1652444 | 0.0044570 |  37.075161 | 0.0000000 |
| delwt       |    0.0090406 | 0.0008704 |  10.386330 | 0.0000000 |
| gaweeks     |    0.0254620 | 0.0032306 |   7.881561 | 0.0000000 |
| parity      |    0.2106321 | 0.0892415 |   2.360248 | 0.0183069 |
| smoken      |  \-0.0107020 | 0.0012942 | \-8.269011 | 0.0000000 |

change the benchmark and look at the model with filtered parameter

``` r
model = baby_df %>% 
 mutate(babysex = fct_infreq(babysex)) %>% 
 lm(bwt ~ babysex + bhead + blength + delwt + gaweeks + parity + smoken, data = .) %>% 
 broom::tidy() %>% 
 select(term, estimate, p.value)
```
