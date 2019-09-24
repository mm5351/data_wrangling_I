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

\#\#Bind rows

``` r
fellowship_data =
   readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") %>%
  mutate(movie = "fellowship_ring")

two_towers = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") %>%
  mutate(movie = "two_towers")

return_king = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") %>%
  mutate(movie = "return_king")

##Binding rows from one table to the next to the next
lotr_tidy = 
  bind_rows(fellowship_data, two_towers, return_king) %>%
  janitor::clean_names() %>%
  pivot_longer(
    female:male,
    names_to = "sex", 
    values_to = "words") %>%
  select(movie, everything()) 
```

``` r
pup_data = 
  read_csv("./data/FAS_pups.csv", col_types = "ciiiii") %>%
  janitor::clean_names() %>%
  mutate(sex = recode(sex, `1` = "male", `2` = "female")) 

litter_data = 
  read_csv("./data/FAS_litters.csv", col_types = "ccddiiii") %>%
  janitor::clean_names() %>%
  select(-pups_survive) %>%
  mutate(
    wt_gain = gd18_weight - gd0_weight,
    group = str_to_lower(group))
```

Try to join these datasets\!\! Start with pups data (complete) and tie
in litters data. Wherever litter number is present in pups data and
litter data

``` r
fas_data = 
  left_join(pup_data, litter_data, by = "litter_number")
fas_data %>% view

full_join(pup_data, litter_data, by = "litter_number")
```

    ## # A tibble: 315 x 13
    ##    litter_number sex   pd_ears pd_eyes pd_pivot pd_walk group gd0_weight
    ##    <chr>         <chr>   <int>   <int>    <int>   <int> <chr>      <dbl>
    ##  1 #85           male        4      13        7      11 con7        19.7
    ##  2 #85           male        4      13        7      12 con7        19.7
    ##  3 #1/2/95/2     male        5      13        7       9 con7        27  
    ##  4 #1/2/95/2     male        5      13        8      10 con7        27  
    ##  5 #5/5/3/83/3-3 male        5      13        8      10 con7        26  
    ##  6 #5/5/3/83/3-3 male        5      14        6       9 con7        26  
    ##  7 #5/4/2/95/2   male       NA      14        5       9 con7        28.5
    ##  8 #4/2/95/3-3   male        4      13        6       8 con7        NA  
    ##  9 #4/2/95/3-3   male        4      13        7       9 con7        NA  
    ## 10 #2/2/95/3-2   male        4      NA        8      10 con7        NA  
    ## # … with 305 more rows, and 5 more variables: gd18_weight <dbl>,
    ## #   gd_of_birth <int>, pups_born_alive <int>, pups_dead_birth <int>,
    ## #   wt_gain <dbl>
