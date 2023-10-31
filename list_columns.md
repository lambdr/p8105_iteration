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
