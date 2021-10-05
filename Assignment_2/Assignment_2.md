Assignment\_2
================
Ram Ayyala
10/4/2021

# Download Data

``` r
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 4.1.1

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.1

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.3     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.0     v forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.1.1

    ## Warning: package 'forcats' was built under R version 4.1.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::between()   masks data.table::between()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::first()     masks data.table::first()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::last()      masks data.table::last()
    ## x purrr::transpose() masks data.table::transpose()

``` r
fn1 <- "chs_individual.csv"
if (!file.exists(fn1))
  download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_individual.csv", destfile=fn1)

fn2 <- "chs_regional.csv"
if (!file.exists(fn2))
  download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_regional.csv", destfile=fn2)

individual <- read.csv(fn1)
individual <- as_tibble(individual)
regional <- read.csv(fn2)
regional <- as_tibble(regional)
```

# Merge Data

``` r
dat <- merge(
  # Data
  x     = individual,      
  y     = regional, 
  # List of variables to match
  by.x  = "townname",
  by.y  = "townname", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
  )
```

# Check for duplicates

``` r
nrow(dat) == nrow(individual)
```

    ## [1] TRUE

# Impute Data with missing values

``` r
library(Amelia)
```

    ## Warning: package 'Amelia' was built under R version 4.1.1

    ## Loading required package: Rcpp

    ## ## 
    ## ## Amelia II: Multiple Imputation
    ## ## (Version 1.8.0, built: 2021-05-26)
    ## ## Copyright (C) 2005-2021 James Honaker, Gary King and Matthew Blackwell
    ## ## Refer to http://gking.harvard.edu/amelia/ for more information
    ## ##

``` r
#find out which columns have missing values
idvars= c(names(which(!colSums(is.na(dat)) > 0)))
c(names(which(colSums(is.na(dat)) > 0)))
```

    ##  [1] "agepft"        "height"        "weight"        "bmi"          
    ##  [5] "asthma"        "father_asthma" "mother_asthma" "wheeze"       
    ##  [9] "hayfever"      "allergy"       "educ_parent"   "smoke"        
    ## [13] "gasstove"      "fev"           "fvc"           "mmef"         
    ## [17] "no_24hr"       "pm2_5_fr"

``` r
a.out <- amelia(dat, m = 5, idvars=idvars)
```

    ## -- Imputation 1 --
    ## 
    ##   1  2  3  4  5  6  7  8
    ## 
    ## -- Imputation 2 --
    ## 
    ##   1  2  3  4  5  6  7  8
    ## 
    ## -- Imputation 3 --
    ## 
    ##   1  2  3  4  5  6  7  8  9
    ## 
    ## -- Imputation 4 --
    ## 
    ##   1  2  3  4  5  6  7  8  9
    ## 
    ## -- Imputation 5 --
    ## 
    ##   1  2  3  4  5  6  7  8  9

``` r
dat <- a.out$imputations[[1]]
dat <- data.table(dat)
```

# Create cat variable obesity level

``` r
dat[bmi < 14, obesity_level := "underweight"]
dat[bmi >= 14 & bmi <22, obesity_level := "normal"]
dat[bmi >= 22 & bmi <=24, obesity_level := "overweight"]
dat[bmi > 24, obesity_level := "obese"]
dat[, .(
  min_bmi = min(bmi),
  max_bmi = max(bmi),
  underweight = dat %>% filter(obesity_level == "underweight") %>% count(),
  normal = dat %>% filter(obesity_level == "normal") %>% count(),
  overweight = dat %>% filter(obesity_level == "overweight") %>% count(),
  obese = dat %>% filter(obesity_level == "obese") %>% count()
  )] %>%
  knitr::kable()
```

| min\_bmi | max\_bmi | underweight.n | normal.n | overweight.n | obese.n |
|---------:|---------:|--------------:|---------:|-------------:|--------:|
|  11.2964 | 41.26613 |            43 |      951 |           91 |     115 |
