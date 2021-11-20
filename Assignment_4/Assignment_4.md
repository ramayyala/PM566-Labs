Assignment 4
================
Ram Ayyala
11/19/2021

# HPC

## Problem 1: Make sure your code is nice

### Rewrite the following R functions to make them faster. It is OK (and recommended) to take a look at Stackoverflow and Google

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  rowSums(mat)
}

# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}

fun2alt <- function(mat) {
  t(apply(mat,1,cumsum))
}


# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), "relative","equivalent"
)
```

    ## Warning in microbenchmark::microbenchmark(fun1(dat), fun1alt(dat), "relative", :
    ## Could not measure a positive execution time for 87 evaluations.

    ## Unit: nanoseconds
    ##          expr    min     lq   mean median      uq     max neval
    ##     fun1(dat) 246000 472450 926273 694000 1105800 6464800   100
    ##  fun1alt(dat)  45600  57750 125935  81750  135150 1121400   100
    ##    "relative"      0      0     90      0      50    1100   100
    ##  "equivalent"      0      0    113      0     100    2300   100

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), "relative", "equivalent"
)
```

    ## Warning in microbenchmark::microbenchmark(fun2(dat), fun2alt(dat), "relative", :
    ## Could not measure a positive execution time for 62 evaluations.

    ## Unit: nanoseconds
    ##          expr     min      lq    mean  median      uq      max neval
    ##     fun2(dat) 2263900 2528200 3270132 2887000 3414200 10074500   100
    ##  fun2alt(dat)  575400  661250 1128584  833300 1129800  8391600   100
    ##    "relative"       0       0      84       0     100     1500   100
    ##  "equivalent"       0       0     300       0     100    16200   100

## Problem 2: Make things run faster with parallel computing

### The following function allows simulating PI

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##    3.92    0.01    4.06

``` r
system.time({
  cl <- makePSOCKcluster(4L)
  clusterSetRNGStream(cl, 1231)
  clusterExport(cl,c("sim_pi"), envir=environment())
  ans <- unlist(parLapply(cl=cl,1:4000,sim_pi, n=10000))
  print(mean(ans))
  stopCluster(cl)
})
```

    ## [1] 3.141578

    ##    user  system elapsed 
    ##    0.05    0.02    2.28

#SQL ## Setup a temporary database by running the following chunk

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

## Question 1:

### How many many movies is there avaliable in each rating catagory.

``` sql
SELECT rating, 
COUNT (*) AS "Film Number"
FROM film
GROUP BY rating
```

| rating | Film Number |
|:-------|------------:|
| G      |         180 |
| NC-17  |         210 |
| PG     |         194 |
| PG-13  |         223 |
| R      |         195 |

5 records

From the Table above, we see how many movies are available for each
category.

## Question 2:

### What is the average replacement cost and rental rate for each rating category.

``` sql
SELECT rating,
AVG(replacement_cost) AS "Average Replacement Cost",
AVG(rental_rate) AS "Average Rental Rate"
FROM film
GROUP BY rating
```

| rating | Average Replacement Cost | Average Rental Rate |
|:-------|-------------------------:|--------------------:|
| G      |                 20.12333 |            2.912222 |
| NC-17  |                 20.13762 |            2.970952 |
| PG     |                 18.95907 |            3.051856 |
| PG-13  |                 20.40256 |            3.034843 |
| R      |                 20.23103 |            2.938718 |

5 records

From the Table above, we see that the average replacement cost and
average rental rate for each rating category. It is evident that the
PG-13 movies seem to have the highest avergae replacement cost while PG
movies have the highest average rental rate. Moreover, it seems that PG
movies have the lowest average replacement cost while G movies have the
lowest average rental rate.

## Question 3:

Use table film_category together with film to find the how many films
there are with each category ID

``` sql
SELECT category_id, 
COUNT(title) as "Number of Films"
FROM film
INNER JOIN film_category
on film.film_id = film_category.film_id
GROUP BY category_id
```

| category_id | Number of Films |
|:------------|----------------:|
| 1           |              64 |
| 2           |              66 |
| 3           |              60 |
| 4           |              57 |
| 5           |              58 |
| 6           |              68 |
| 7           |              62 |
| 8           |              69 |
| 9           |              73 |
| 10          |              61 |

Displaying records 1 - 10

From the table above, we see that **category_id 15** has the most number
of films at 74 films while **category_id 12** has the least number of
films at 51 films.

## Question 4:

Incorporate table category into the answer to the previous question to
find the name of the most popular category.

``` sql
SELECT name, 
COUNT(title) as "Number of Films"
FROM film
INNER JOIN film_category
on film.film_id = film_category.film_id
INNER JOIN category
on category.category_id = film_category.category_id
GROUP BY name
ORDER BY -"Number of Films" 
```

| name        | Number of Films |
|:------------|----------------:|
| Sports      |              74 |
| Foreign     |              73 |
| Family      |              69 |
| Documentary |              68 |
| Animation   |              66 |
| Action      |              64 |
| New         |              63 |
| Drama       |              62 |
| Games       |              61 |
| Sci-Fi      |              61 |

Displaying records 1 - 10

From the above table, we see that the most popular category is the
Sports category with 74 films.

``` r
dbDisconnect(con)
```
