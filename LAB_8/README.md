LAB_9
================

## Problem 2

### Part 1

``` r
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  
  return(x)
}
fun1(50,10)
```

    ##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    ##  [1,]    4    3    4    3    4    5    5    6    4     5
    ##  [2,]    6    1    7    7    4    2    4    4    4     3
    ##  [3,]    6    1    4    4    1    6    6    3    5     3
    ##  [4,]    3    3    3    4    3    6    5    5    3     2
    ##  [5,]    2    3    5    2    2    5    3    5    3     4
    ##  [6,]    7    6    4    6    2    3    4    2    4     1
    ##  [7,]    2    3    4    3    5    5    2    3    3     7
    ##  [8,]    2    5    6    5    1    4    4    2    1     3
    ##  [9,]    3    4    6    3    5    4    3    1    4     6
    ## [10,]    3    6    5    5    3    4    1    5    4     3
    ## [11,]    1    3    3    5    4    3    4    4    4     2
    ## [12,]    5    3    5    1    2    2    4    3    5     6
    ## [13,]    4    2    3   10    2    5    3    4    4     3
    ## [14,]    4    3    0    2    2    4    4    2    4     8
    ## [15,]    3    4    3    3    3   10    6    5    3     2
    ## [16,]    3    3    4    3    4    3    3    7    1     6
    ## [17,]    3    3    9    4    3    2    4    6    2    11
    ## [18,]    2    5    5    5    2    4    4    2    7     3
    ## [19,]    4    1    2    3    6    3    2    3    4     6
    ## [20,]    4    4    4    4    3    4    1    3    5     3
    ## [21,]    3    7    2    6    5    5    5    9    2     5
    ## [22,]    4    1    3    3    6    0    2    2    7     1
    ## [23,]    4    8    2    5    5    5    2    7    6     3
    ## [24,]    2    4    5    1    9    2    5    6    6     3
    ## [25,]    4    3    6    8    4    5    4    0    5     2
    ## [26,]    4    2    3    3    2    2    6    4    3     2
    ## [27,]    4    4    4    3    7    2    1    1    2     7
    ## [28,]    3    9    5    4    2    6    3    2    4     0
    ## [29,]    3    5    7    1    1    5    4    8    1     2
    ## [30,]    5    2    5    9    6    7    7    4    9     3
    ## [31,]    5    4    4    4    1    5    1    5    9     5
    ## [32,]    1    5    4    1    2    5    2    4    6     1
    ## [33,]    3    5    4    2    4    6    6    0    4     2
    ## [34,]    5    8    3    5    2    6    3    2    5     3
    ## [35,]    3    3    5    1    3    3    0    5    1     3
    ## [36,]    4    2    2    2    1    5    2    8    6     1
    ## [37,]    6    2    2    3    3    1    5    4    5     4
    ## [38,]    5    1    4    6    5    2    0    6    4     4
    ## [39,]    3    4    2    5    2    3    1    4    5     3
    ## [40,]    3    2    4    1    8    2   11    5    2     9
    ## [41,]    6    3    2    2    2    5    2    3    5     5
    ## [42,]    4    3    4    5    6    5    4    7    4     5
    ## [43,]    4    6    4    5    4    2    5    2    6     2
    ## [44,]   11    5    6    5    4    2    4    2    1     2
    ## [45,]    6    4    2    4    5    3    3    8    2     6
    ## [46,]    4    9    2    5    2    4    3    5    1     2
    ## [47,]    4    2    2    6    2    5    9    3    4     4
    ## [48,]    2    5    1    2    4    3    8    9    5     6
    ## [49,]    8    2    3    2    4    3    6    5    6     3
    ## [50,]    6    3    8    5    4    2    1    3    1     4

``` r
fun1alt <- function(n = 100, k = 4, lambda = 4) {
  matrix(rpois(n*k,lambda),nrow=n,ncol=k)
}

# Benchmarking
microbenchmark::microbenchmark(
  fun1(n=1000),
  fun1alt(n=1000), "relative"
)
```

    ## Warning in microbenchmark::microbenchmark(fun1(n = 1000), fun1alt(n = 1000), :
    ## Could not measure a positive execution time for 34 evaluations.

    ## Unit: nanoseconds
    ##               expr     min      lq    mean  median      uq      max neval
    ##     fun1(n = 1000) 4970200 5778900 7397896 6473250 8538400 24450300   100
    ##  fun1alt(n = 1000)  162200  174550  247389  191950  245100  3322100   100
    ##         "relative"       0       0      42       0       0     2400   100

### Part 2

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}

fun2alt <- function(x) {
  # Position pf tje max value per row of x
  idx <- max.col(t(x))
  
  # Get actual max value
  x[cbind(idx, 1:ncol(x))]
}

#Do we get the same max value matrix?

all(fun2(x) == fun2alt(x))
```

    ## [1] TRUE

``` r
# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x), "relative"
)
```

    ## Warning in microbenchmark::microbenchmark(fun2(x), fun2alt(x), "relative"):
    ## Could not measure a positive execution time for 45 evaluations.

    ## Unit: nanoseconds
    ##        expr     min      lq    mean  median      uq     max neval
    ##     fun2(x) 1163400 1298100 1626946 1437300 1801300 3836400   100
    ##  fun2alt(x)  121500  129800  250540  156950  197950 7482500   100
    ##  "relative"       0       0      11       0       0     700   100

## Problem 3

``` r
library(parallel)
my_boot <- function(dat, stat, R, ncpus = 1L) {
  
  # Getting the random indices
  n <- nrow(dat)
  idx <- matrix(sample.int(n, n*R, TRUE), nrow=n, ncol=R)
 
  # Making the cluster using `ncpus`
  # STEP 1: GOES HERE
  cl <- makePSOCKcluster(ncpus)
  # STEP 2: GOES HERE
  clusterSetRNGStream(cl, 123) # Euivalent to set.seed(123)
  clusterExport(cl,c("stat","dat","idx"), envir=environment())
  
    # STEP 3: THIS FUNCTION NEEDS TO BE REPLACES WITH parLapply
  ans <- parLapply(cl=cl,seq_len(R), function(i) {
    stat(dat[idx[,i], , drop=FALSE])
  })
  
  # Coercing the list into a matrix
  ans <- do.call(rbind, ans)
  
  # STEP 4: GOES HERE
  stopCluster(cl)
  ans
  
}
```

``` r
my_stat <- function(d) coef(lm(y ~ x, data=d))

# DATA SIM
set.seed(1)
n <- 500; R <- 1e4

x <- cbind(rnorm(n)); y <- x*5 + rnorm(n)

# Checking if we get something similar as lm
ans0 <- confint(lm(y~x))
ans1 <- my_boot(dat = data.frame(x, y), my_stat, R = R, ncpus = 2L)

# You should get something like this
t(apply(ans1, 2, quantile, c(.025,.975)))
```

    ##                   2.5%      97.5%
    ## (Intercept) -0.1386903 0.04856752
    ## x            4.8685162 5.04351239

``` r
##                   2.5%      97.5%
## (Intercept) -0.1372435 0.05074397
## x            4.8680977 5.04539763
ans0
```

    ##                  2.5 %     97.5 %
    ## (Intercept) -0.1379033 0.04797344
    ## x            4.8650100 5.04883353

``` r
##                  2.5 %     97.5 %
## (Intercept) -0.1379033 0.04797344
## x            4.8650100 5.04883353
```

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 1L))
```

    ##    user  system elapsed 
    ##    0.12    0.02    4.72

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 2L))
```

    ##    user  system elapsed 
    ##    0.19    0.02    3.06
