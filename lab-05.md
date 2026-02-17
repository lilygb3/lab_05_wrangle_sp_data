Lab 05 - La Quinta is Spanish for next to Denny’s, Pt. 2
================
Lily Botha
02/17/2026

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
library(readr)
```

``` r
states <- read_csv("data/states.csv")
dn <- read_csv("https://github.com/DataScience4Psych/DataScience4Psych/raw/main/data/raw-data/dennys.csv")
lq <- read_csv("https://github.com/DataScience4Psych/DataScience4Psych/raw/main/data/raw-data/laquinta.csv")
```

### Exercise 1

There are 3 Denny’s locations in Alaska.

``` r
dn_ak <- dn %>%
  filter(state == "AK")
nrow(dn_ak)
```

    ## [1] 3

There are 2 La Quinta locations in Alaska.

``` r
lq_ak <- lq %>% 
  filter(state == "AK")
nrow(lq_ak)
```

    ## [1] 2

### Exercise 2

Remove this text, and add your answer for Exercise 1 here. Add code
chunks as needed. Don’t forget to label your code chunk. Do not use
spaces in code chunk labels.

### Exercise 3

…

### Exercise 4

…

### Exercise 5

…

### Exercise 6

…

Add exercise headings as needed.
