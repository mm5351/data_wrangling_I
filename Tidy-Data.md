Tidy Data
================
Martha Mulugeta
9/24/2019

install.packages(“tidyr”)

\#\#Wide to long

``` r
pulse_data = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>% 
  janitor::clean_names() 
  
pulse_tidy_data = 
  pivot_longer(
    pulse_data, 
    bdi_score_bl:bdi_score_12m,
    names_to = "visit", 
    names_prefix = "bdi_score_",
    values_to = "bdi") %>% 

 select(id, visit, everything()) %>%
  mutate(
    visit = replace(visit, visit == "bl", "00m"),
    visit = factor(visit, levels = str_c(c("00", "01", "06", "12"), "m"))) %>%
  arrange(id, visit)

print(pulse_data, n = 12)
```

    ## # A tibble: 1,087 x 7
    ##       id   age sex   bdi_score_bl bdi_score_01m bdi_score_06m bdi_score_12m
    ##    <dbl> <dbl> <chr>        <dbl>         <dbl>         <dbl>         <dbl>
    ##  1 10003  48.0 male             7             1             2             0
    ##  2 10015  72.5 male             6            NA            NA            NA
    ##  3 10022  58.5 male            14             3             8            NA
    ##  4 10026  72.7 male            20             6            18            16
    ##  5 10035  60.4 male             4             0             1             2
    ##  6 10050  84.7 male             2            10            12             8
    ##  7 10078  31.3 male             4             0            NA            NA
    ##  8 10088  56.9 male             5            NA             0             2
    ##  9 10091  76.0 male             0             3             4             0
    ## 10 10092  74.2 fema…           10             2            11             6
    ## 11 10098  61.3 fema…           10             9            NA             7
    ## 12 10103  53.7 fema…            5             1            NA             0
    ## # … with 1,075 more rows

\#\#Separate in litters

``` r
read_csv("./data/FAS_litters.csv") %>% 
  janitor::clean_names() %>%
  separate(col = group, into = c("dose", "day_of_tx"), 3)
```

    ## Parsed with column specification:
    ## cols(
    ##   Group = col_character(),
    ##   `Litter Number` = col_character(),
    ##   `GD0 weight` = col_double(),
    ##   `GD18 weight` = col_double(),
    ##   `GD of Birth` = col_double(),
    ##   `Pups born alive` = col_double(),
    ##   `Pups dead @ birth` = col_double(),
    ##   `Pups survive` = col_double()
    ## )

    ## # A tibble: 49 x 9
    ##    dose  day_of_tx litter_number gd0_weight gd18_weight gd_of_birth
    ##    <chr> <chr>     <chr>              <dbl>       <dbl>       <dbl>
    ##  1 Con   7         #85                 19.7        34.7          20
    ##  2 Con   7         #1/2/95/2           27          42            19
    ##  3 Con   7         #5/5/3/83/3-3       26          41.4          19
    ##  4 Con   7         #5/4/2/95/2         28.5        44.1          19
    ##  5 Con   7         #4/2/95/3-3         NA          NA            20
    ##  6 Con   7         #2/2/95/3-2         NA          NA            20
    ##  7 Con   7         #1/5/3/83/3-…       NA          NA            20
    ##  8 Con   8         #3/83/3-3           NA          NA            20
    ##  9 Con   8         #2/95/3             NA          NA            20
    ## 10 Con   8         #3/5/2/2/95         28.5        NA            20
    ## # … with 39 more rows, and 3 more variables: pups_born_alive <dbl>,
    ## #   pups_dead_birth <dbl>, pups_survive <dbl>

\#\#Pivot\_wider, usually just for presentation purposes

``` r
analysis_result = tibble(
  group = c("treatment", "treatment", "placebo", "placebo"),
  time = c("pre", "post", "pre", "post"),
  mean = c(4, 8, 3.5, 4)
)

##Variable names now time, cell values now means 
pivot_wider(
  analysis_result,
  names_from = time,
  values_from = mean
)
```

    ## # A tibble: 2 x 3
    ##   group       pre  post
    ##   <chr>     <dbl> <dbl>
    ## 1 treatment   4       8
    ## 2 placebo     3.5     4
