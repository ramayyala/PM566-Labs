LAB\_6
================
Ram Ayyala
10/3/2021

\#Loading Libraries

``` r
library(dplyr)
library(ggplot2)
library(tidytext)
library(tibble)
```

\#Download Data

``` r
fn <- "mtsamples.csv"
if (!file.exists(fn))
  download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/00_mtsamples/mtsamples.csv", destfile=fn)

mtsamples <- read.csv(fn)
mtsamples <- as_tibble(mtsamples)
```

## Question 1: What specialties do we have?

``` r
specialties <- mtsamples %>%
  count(medical_specialty)
specialties %>%
  arrange(desc(n)) %>%
  top_n(n,15) %>%
  knitr::kable()
```

There are `r nrow(specialties)` specialties. Let’s take a look at the
distribution

``` r
ggplot(mtsamples, aes(x=medical_specialty)) +
  geom_histogram(stat="count") +
  coord_flip()
```

The specialty categories are not uniformly distributed.
