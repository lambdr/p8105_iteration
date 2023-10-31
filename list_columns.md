List columns
================
Derek Lamb
2023-10-31

``` r
library(tidyverse)
library(rvest)

# Set default figure options
knitr::opts_chunk$set(
  fig.width = 6,
  out.width = "90%"
)

theme_set(theme_bw() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d

# set seed for consistency
set.seed(12345)
```

### Lists

``` r
vec_numeric = 1:4
vec_char = c("my", "name", "is", "jeff")

tibble(
  num = vec_numeric,
  char = vec_char
)
```

    ## # A tibble: 4 × 2
    ##     num char 
    ##   <int> <chr>
    ## 1     1 my   
    ## 2     2 name 
    ## 3     3 is   
    ## 4     4 jeff

## Mean and sd function

``` r
mean_and_sd = function(x) {

  if (!is.numeric(x)){
    stop("Argument should be numbers")
  }
  else if (length(x) < 2){
    stop("You need at least two numbers to get standard deviations")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
}
```

Different stuff with different lengths

``` r
l = list(
  vec_numeric = 1:5,
  vec_char = LETTERS,
  matrix = matrix(1:9, nrow = 3),
  summary = summary(rnorm(100))
)
```

Accessing lists

``` r
l$vec_char
```

    ##  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
    ## [20] "T" "U" "V" "W" "X" "Y" "Z"

``` r
l[[1]]
```

    ## [1] 1 2 3 4 5

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.3804 -0.5901  0.4837  0.2452  0.9004  2.4771

### Loops

``` r
list_norm = list(
  a = rnorm(20, 1, 5),
  b = rnorm(20, 0, 7),
  c = rnorm(20, 20, 1),
  d = rnorm(20, -45, 13)
)
```

``` r
mean_and_sd(list_norm$a)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.25  4.92

``` r
mean_and_sd(list_norm$b)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.690  9.30

``` r
mean_and_sd(list_norm$c)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  19.8 0.910

``` r
mean_and_sd(list_norm$d)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -44.1  14.0

``` r
# the output vector is needed otherwise nothing will print/be stored
output = vector("list", length = 4)

# basic for loops
for (i in 1:4){
  output[[i]] = mean_and_sd(list_norm[[i]])
}
```

### Now with `map`

``` r
output = map(list_norm, mean_and_sd)
output = map(list_norm, median)
output = map(list_norm, summary)
```

# List columns

### create df

``` r
df_listcol = tibble(
  name = c("a", "b", "c", "d"),
  samp = list_norm
)
```

``` r
mean_and_sd(df_listcol$samp[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.25  4.92

``` r
map(df_listcol$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.25  4.92
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.690  9.30
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  19.8 0.910
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -44.1  14.0

``` r
df_listcol |> 
  mutate(mean_sd = map(samp, mean_and_sd),
         median = map(samp, median)) |> 
  select(name, mean_sd) |> 
  unnest(mean_sd)
```

    ## # A tibble: 4 × 3
    ##   name     mean     sd
    ##   <chr>   <dbl>  <dbl>
    ## 1 a       1.25   4.92 
    ## 2 b       0.690  9.30 
    ## 3 c      19.8    0.910
    ## 4 d     -44.1   14.0

``` r
# unnest() turns tibble into sets of lists
```

### NSDUH

``` r
nsduh_reader = function(n_table, outcome_name, url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm") {
  
  df = 
    read_html(url) |> 
    html_table() |> 
    nth(n_table) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent),
      outcome = outcome_name) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  return(df)
}
```

### import data with `for` loop

``` r
table_input = list(1, 4, 5)
name_input = list("marj", "cocaine", "heroin")

output = vector("list", length = 3)

for (i in 1:3){
  
  output[[i]] = nsduh_reader(n_table = table_input[[i]], 
                             outcome_name = name_input[[i]])
  
}

df_nsduh = bind_rows(output)
```

Try again, using maps!! Note: because we’re including the outcome name
in the input df, that process was removed from the function.

Also note that including the `read_html` step in the function slows down
the process noticeably.

``` r
nsduh_reader = function(n_table, url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm") {
  
  df = 
    read_html(url) |> 
    html_table() |> 
    nth(n_table) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent)) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  return(df)
}

df_nsduh = 
  tibble(
    name = c("marj", "cocaine", "heroin"),
    number = c(1, 4, 5)
  ) |> 
  mutate(table = map(number, nsduh_reader)) |> 
  unnest(table)
```

## one more thing

``` r
df_weather = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/Derek/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2023-09-28 10:20:09.047204 (8.524)

    ## file min/max dates: 1869-01-01 / 2023-09-30

    ## using cached file: /Users/Derek/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2023-09-28 10:20:15.065928 (3.83)

    ## file min/max dates: 1949-10-01 / 2023-09-30

    ## using cached file: /Users/Derek/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2023-09-28 10:20:17.264748 (0.994)

    ## file min/max dates: 1999-09-01 / 2023-09-30

nesting data means that you can apply the same operations on each
element df separately

``` r
df_nest = df_weather |> 
  nest(df = date:tmin)
```

can i regress `tmax` on `tmin`

``` r
df_central_park = df_nest |> 
  filter(name == "CentralPark_NY") |> 
  pull(df) |> 
  nth(1)
```

Fit lsrl for central park

``` r
lm(tmax ~ tmin, data = df_central_park)
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df_central_park)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

linear regression on whole df

``` r
weather_lm = function(df){
  lm(tmax ~ tmin, data = df)
}

weather_lm(df_central_park)
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

``` r
df_nest = df_nest |> 
  mutate(lsrl = map(df, weather_lm))
```
