HW5
================
Soungbin Yim
2022-11-15

### Due date

Due: November 16 at 11:59pm.

### Points

| Problem         | Points    |
|:----------------|:----------|
| Problem 0       | 20        |
| Problem 1       | –         |
| Problem 2       | 40        |
| Problem 3       | 40        |
| Optional survey | No points |

### Problem 0

This “problem” focuses on structure of your submission, especially the
use git and GitHub for reproducibility, R Projects to organize your
work, R Markdown to write reproducible reports, relative paths to load
data from local files, and reasonable naming structures for your files.
This was not prepared as a GitHub repo.

``` r
library(tidyverse)
```

## Problem 2

``` r
homicide_df =
  read_csv("./homicide-data.csv") %>%
  mutate(
    victim_age = as.numeric(victim_age),
    city_state = str_c(city, ", ", state)
  ) %>%
  group_by(city_state) %>%
  summarize(unsolved_sum = sum(disposition == "Closed without arrest", disposition == "Open/No arrest"),
    total_homicide = n()) 
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
baltimore_df = 
  homicide_df %>%
  filter(city_state %in% c("Baltimore, MD"))

Prop_baltimore_df =
  prop.test(baltimore_df$unsolved_sum,baltimore_df$total_homicide,
    alternative = c("two.sided"),
    conf.level = 0.95, correct = TRUE) %>%
  broom::tidy()

Prop_baltimore_df %>%
  select(estimate, "CI_lower" = conf.low, "CI_upper" = conf.high)
```

    ## # A tibble: 1 × 3
    ##   estimate CI_lower CI_upper
    ##      <dbl>    <dbl>    <dbl>
    ## 1    0.646    0.628    0.663

``` r
 prop.test(homicide_df$unsolved_sum,homicide_df$total_homicide,
    alternative = c("two.sided"),
    conf.level = 0.95, correct = TRUE) %>%
  broom::tidy()
```

    ## # A tibble: 1 × 56
    ##   estimate1 estimate2 estimate3 estima…¹ estim…² estim…³ estim…⁴ estim…⁵ estim…⁶
    ##       <dbl>     <dbl>     <dbl>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1     0.386     0.383     0.646    0.462   0.434   0.505   0.612   0.300   0.736
    ## # … with 47 more variables: estimate10 <dbl>, estimate11 <dbl>,
    ## #   estimate12 <dbl>, estimate13 <dbl>, estimate14 <dbl>, estimate15 <dbl>,
    ## #   estimate16 <dbl>, estimate17 <dbl>, estimate18 <dbl>, estimate19 <dbl>,
    ## #   estimate20 <dbl>, estimate21 <dbl>, estimate22 <dbl>, estimate23 <dbl>,
    ## #   estimate24 <dbl>, estimate25 <dbl>, estimate26 <dbl>, estimate27 <dbl>,
    ## #   estimate28 <dbl>, estimate29 <dbl>, estimate30 <dbl>, estimate31 <dbl>,
    ## #   estimate32 <dbl>, estimate33 <dbl>, estimate34 <dbl>, estimate35 <dbl>, …
    ## # ℹ Use `colnames()` to see all variable names

``` r
prop_homicide_df =
  homicide_df %>%
  mutate(proportion = purrr::map2(unsolved_sum,total_homicide,prop.test),
         proportion = purrr::map(proportion, broom::tidy)) %>%
  unnest(proportion) %>%
  select(1:4,8,9)

prop_homicide_df %>% 
  ggplot(aes(reorder(city_state,estimate), estimate)) +       
  geom_point(aes(color = city_state)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
    labs(
    title = "Problem 2, plot of unsolved homicide proportion estimates and CIs for each city ",
    x = "City, State",
    y = "Proportion Estimate") +
   theme(axis.text.x = element_text(angle = 90),
         legend.position = "none")
```

<img src="HW5_files/figure-gfm/unnamed-chunk-1-1.png" width="90%" /> Now
run prop.test for each of the cities in your dataset, and extract both
the proportion of unsolved homicides and the confidence interval for
each. Do this within a “tidy” pipeline, making use of purrr::map,
purrr::map2, list columns and unnest as necessary to create a tidy
dataframe with estimated proportions and CIs for each city.

This plot suggests high within-subject correlation – subjects who start
above average end up above average, and those that start below average
end up below average. Subjects in the control group generally don’t
change over time, but those in the experiment group increase their
outcome in a roughly linear way.

The Washington Post has gathered data on homicides in 50 large U.S.
cities and made the data available through a GitHub repository here. You
can read their accompanying article here.

Describe the raw data. Create a city_state variable (e.g. “Baltimore,
MD”) and then summarize within cities to obtain the total number of
homicides and the number of unsolved homicides (those for which the
disposition is “Closed without arrest” or “Open/No arrest”).

For the city of Baltimore, MD, use the prop.test function to estimate
the proportion of homicides that are unsolved; save the output of
prop.test as an R object, apply the broom::tidy to this object and pull
the estimated proportion and confidence intervals from the resulting
tidy dataframe.

Now run prop.test for each of the cities in your dataset, and extract
both the proportion of unsolved homicides and the confidence interval
for each. Do this within a “tidy” pipeline, making use of purrr::map,
purrr::map2, list columns and unnest as necessary to create a tidy
dataframe with estimated proportions and CIs for each city.

Create a plot that shows the estimates and CIs for each city – check out
geom_errorbar for a way to add error bars based on the upper and lower
limits. Organize cities according to the proportion of unsolved
homicides.
