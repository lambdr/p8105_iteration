Writing Functions
================
Derek Lamb
2023-10-31

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(knitr)

# Set default figure options
opts_chunk$set(
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
```

Set seed for reproducibility.

``` r
set.seed(12345)
```

## Z score function

Z scores subtract the mean and devide by the sd.

``` r
x_vec <- rnorm(20, mean = 5, sd = 0.3)
```

``` r
(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1]  0.6103734  0.7589907 -0.2228232 -0.6355576  0.6347861 -2.2717259
    ##  [7]  0.6638185 -0.4229355 -0.4324994 -1.1941438 -0.2311505  2.0874460
    ## [13]  0.3526784  0.5320552 -0.9917420  0.8878182 -1.1546150 -0.4893597
    ## [19]  1.2521303  0.2664557

Write a function to do this!

``` r
z_score = function(x) {
  
  if (!is.numeric(x)){
    stop("Argument should be numbers")
  }
  else if (length(x) < 2){
    stop("You need at least two numbers to get z scores")
  }
  
  z = (x - mean(x)) / sd(x)
  return(z)
}
```

Check that this works.

``` r
z_score(x = x_vec)
```

    ##  [1]  0.6103734  0.7589907 -0.2228232 -0.6355576  0.6347861 -2.2717259
    ##  [7]  0.6638185 -0.4229355 -0.4324994 -1.1941438 -0.2311505  2.0874460
    ## [13]  0.3526784  0.5320552 -0.9917420  0.8878182 -1.1546150 -0.4893597
    ## [19]  1.2521303  0.2664557

Keep checking

``` r
z_score(x = 3)
```

    ## Error in z_score(x = 3): You need at least two numbers to get z scores

``` r
z_score(c("My", "name", "is", "Jeff"))
```

    ## Error in z_score(c("My", "name", "is", "Jeff")): Argument should be numbers

``` r
z_score(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_score(c(TRUE, TRUE, FALSE, TRUE)): Argument should be numbers

``` r
z_score(iris)
```

    ## Error in z_score(iris): Argument should be numbers

### Multiple outputs

Write a function that returns the mean and sd from a sample.

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

Double check this works

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.02 0.250

### Start geting means and sds

``` r
x_vec = rnorm(n = 30, mean = 5, sd = 0.5)

  tibble(
    mean = mean(x_vec),
    sd = sd(x_vec)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.12 0.625

Let’s write a function that uses `n`, a true mean, and a true sd as
inputs.

``` r
sim_mean_sd = function(n_obs, mu = 5, sigma = 1){

  x_vec = rnorm(n = n_obs, mean = mu, sd = sigma)

  tibble(
    mean = mean(x_vec),
    sd = sd(x_vec)
  )
  
}
```

``` r
sim_mean_sd(n_obs = 50)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.31  1.14

``` r
sim_mean_sd(mu = 12,n_obs = 24,4)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  13.0  4.06

### Learning check (LotR)

``` r
lotr_reader = function(cells, name, path = "data/LotR_Words.xlsx") {
  
  if (!is.character(cells)) {
    stop("Cells should be a character")
  }
  
  df = readxl::read_excel(path, range = cells) |> 
    mutate(movie = name,
           Race = str_to_lower(Race)) |> 
    janitor::clean_names() |> 
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "word_count"
    ) |> 
    select(movie, everything())
  
  return(df)
}
```

Check if this works

``` r
df_lotr = bind_rows(
  lotr_reader("B3:D6", "fellowship_ring"),
  lotr_reader("F3:H6", "two_towers"),
  lotr_reader("J3:L6", "return_king")
)
```

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)


data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
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
```

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

``` r
df_marj = nsduh_reader(1, "marijuana")
```
