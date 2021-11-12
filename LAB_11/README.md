Lab_11
================

``` r
library(RSQLite)
library(DBI)
```

# Setup

``` r
# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")
# Download tables
actor <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/actor.csv")
rental <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/rental.csv")
customer <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/customer.csv")
payment <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/payment_p2007_01.csv")
# Copy data.frames to database
dbWriteTable(con, "actor", actor)
dbWriteTable(con, "rental", rental)
dbWriteTable(con, "customer", customer)
dbWriteTable(con, "payment", payment)
```

``` r
dbListTables(con)
```

    ## [1] "actor"    "customer" "payment"  "rental"

# Exercise 1: Retrive the actor ID, first name and last name for all actors using the actor table. Sort by last name and then by first name.

``` r
dbGetQuery(con,
      "SELECT actor_id, last_name, first_name
      FROM actor
      ORDER by last_name, first_name
      LIMIT 5")
```

    ##   actor_id last_name first_name
    ## 1       58    AKROYD  CHRISTIAN
    ## 2      182    AKROYD     DEBBIE
    ## 3       92    AKROYD    KIRSTEN
    ## 4      118     ALLEN       CUBA
    ## 5      145     ALLEN        KIM

# Excercise 2: Retrive the actor ID, first name, and last name for actors whose last name equals ‘WILLIAMS’ or ‘DAVIS’.

``` r
dbGetQuery(con,
      "SELECT actor_id, last_name, first_name
      FROM actor
      WHERE last_name IN ('WILLIAMS', 'DAVIS') 
      LIMIT 5")
```

    ##   actor_id last_name first_name
    ## 1        4     DAVIS   JENNIFER
    ## 2       72  WILLIAMS       SEAN
    ## 3      101     DAVIS      SUSAN
    ## 4      110     DAVIS      SUSAN
    ## 5      137  WILLIAMS     MORGAN

# Exercise 3: Write a query against the rental table that returns the IDs of the customers who rented a film on July 5, 2005 (use the rental.rental_date column, and you can use the date() function to ignore the time component). Include a single row for each distinct customer ID.

``` r
dbGetQuery(con,
      "SELECT DISTINCT customer_id
      FROM rental
      WHERE date(rental_date) = '2005-07-05'
      LIMIT 5")
```

    ##   customer_id
    ## 1         565
    ## 2         242
    ## 3          37
    ## 4          60
    ## 5         594

# Exercise 4

## Exercise 4.1: Construct a query that retrives all rows from the payment table where the amount is either 1.99, 7.99, 9.99.

``` r
q <- dbSendQuery(con,"
SELECT *
FROM payment
WHERE amount IN (1.99, 7.99, 9.99)")
dbFetch(q, n=10)
```

    ##    payment_id customer_id staff_id rental_id amount               payment_date
    ## 1       16050         269        2         7   1.99 2007-01-24 21:40:19.996577
    ## 2       16056         270        1       193   1.99 2007-01-26 05:10:14.996577
    ## 3       16081         282        2        48   1.99 2007-01-25 04:49:12.996577
    ## 4       16103         294        1       595   1.99 2007-01-28 12:28:20.996577
    ## 5       16133         307        1       614   1.99 2007-01-28 14:01:54.996577
    ## 6       16158         316        1      1065   1.99 2007-01-31 07:23:22.996577
    ## 7       16160         318        1       224   9.99 2007-01-26 08:46:53.996577
    ## 8       16161         319        1        15   9.99 2007-01-24 23:07:48.996577
    ## 9       16180         330        2       967   7.99 2007-01-30 17:40:32.996577
    ## 10      16206         351        1      1137   1.99 2007-01-31 17:48:40.996577

## Exercise 4.2: Construct a query that retrives all rows from the payment table where the amount is greater then 5

``` r
q <- dbSendQuery(con,"
SELECT *
FROM payment
WHERE amount > 5")
```

    ## Warning: Closing open result set, pending rows

``` r
dbFetch(q, n=10)
```

    ##    payment_id customer_id staff_id rental_id amount               payment_date
    ## 1       16052         269        2       678   6.99 2007-01-28 21:44:14.996577
    ## 2       16058         271        1      1096   8.99 2007-01-31 11:59:15.996577
    ## 3       16060         272        1       405   6.99 2007-01-27 12:01:05.996577
    ## 4       16061         272        1      1041   6.99 2007-01-31 04:14:49.996577
    ## 5       16068         274        1       394   5.99 2007-01-27 09:54:37.996577
    ## 6       16073         276        1       860  10.99 2007-01-30 01:13:42.996577
    ## 7       16074         277        2       308   6.99 2007-01-26 20:30:05.996577
    ## 8       16082         282        2       282   6.99 2007-01-26 17:24:52.996577
    ## 9       16086         284        1      1145   6.99 2007-01-31 18:42:11.996577
    ## 10      16087         286        2        81   6.99 2007-01-25 10:43:45.996577

## Exercise 4.3: Construct a query that retrives all rows from the payment table where the amount is greater then 5 and less then 8

``` r
q <- dbSendQuery(con,"
SELECT *
FROM payment
WHERE amount > 5 and amount < 8")
```

    ## Warning: Closing open result set, pending rows

``` r
dbFetch(q, n=10)
```

    ##    payment_id customer_id staff_id rental_id amount               payment_date
    ## 1       16052         269        2       678   6.99 2007-01-28 21:44:14.996577
    ## 2       16060         272        1       405   6.99 2007-01-27 12:01:05.996577
    ## 3       16061         272        1      1041   6.99 2007-01-31 04:14:49.996577
    ## 4       16068         274        1       394   5.99 2007-01-27 09:54:37.996577
    ## 5       16074         277        2       308   6.99 2007-01-26 20:30:05.996577
    ## 6       16082         282        2       282   6.99 2007-01-26 17:24:52.996577
    ## 7       16086         284        1      1145   6.99 2007-01-31 18:42:11.996577
    ## 8       16087         286        2        81   6.99 2007-01-25 10:43:45.996577
    ## 9       16092         288        2       427   6.99 2007-01-27 14:38:30.996577
    ## 10      16094         288        2       565   5.99 2007-01-28 07:54:57.996577

## Exercise 5: Retrive all the payment IDs and their amount from the customers whose last name is ‘DAVIS’.

``` r
dbGetQuery(con,"
SELECT p.payment_id, p.amount
FROM payment AS p
  INNER JOIN customer AS c ON p.customer_id=c.customer_id
where c.last_name = 'DAVIS'"
)
```

    ## Warning: Closing open result set, pending rows

    ##   payment_id amount
    ## 1      16685   4.99
    ## 2      16686   2.99
    ## 3      16687   0.99

# Exercise 6

## Exercise 6.1: Use COUNT(\*) to count the number of rows in rental

``` r
dbGetQuery(con,"
SELECT COUNT (*)
FROM rental
")
```

    ##   COUNT (*)
    ## 1     16044

## Exercise 6.2: Use COUNT(\*) and GROUP BY to count the number of rentals for each customer_id

``` r
dbGetQuery(con,"
SELECT customer_id, COUNT (*) AS 'N Rentals'
FROM rental GROUP BY customer_id
LIMIT 5
")
```

    ##   customer_id N Rentals
    ## 1           1        32
    ## 2           2        27
    ## 3           3        26
    ## 4           4        22
    ## 5           5        38

## Exercise 6.3: Repeat the previous query and sort by the count in descending order

``` r
dbGetQuery(con,"
SELECT customer_id, COUNT (*) AS 'N Rentals'
FROM rental GROUP BY customer_id
ORDER BY COUNT(*) DESC
LIMIT 5
")
```

    ##   customer_id N Rentals
    ## 1         148        46
    ## 2         526        45
    ## 3         236        42
    ## 4         144        42
    ## 5          75        41

## Exercise 6.4: Repeat the previous query but use HAVING to only keep the groups with 40 or more.

``` r
dbGetQuery(con,"
SELECT customer_id, COUNT (*) AS 'N Rentals'
FROM rental GROUP BY customer_id
HAVING 'N Rentals' >= 40
ORDER BY COUNT(*) DESC
LIMIT 5
")
```

    ##   customer_id N Rentals
    ## 1         148        46
    ## 2         526        45
    ## 3         236        42
    ## 4         144        42
    ## 5          75        41

# Exercise 7:

``` r
dbGetQuery(con,"
SELECT MAX(amount) AS 'max', 
MIN(amount) AS 'min', 
AVG(amount) AS 'avg', 
SUM(amount) AS 'sum'
FROM payment
")
```

    ##     max  min      avg     sum
    ## 1 11.99 0.99 4.169775 4824.43

## Exercise 7.1: Modify the above query to do those calculations for each customer_id

``` r
dbGetQuery(con,"
SELECT
  customer_id,
  MAX(amount) AS 'max', 
  MIN(amount) AS 'min', 
  AVG(amount) AS 'avg', 
  SUM(amount) AS 'sum'
FROM payment GROUP BY customer_id
LIMIT 5
")
```

    ##   customer_id  max  min      avg  sum
    ## 1           1 2.99 0.99 1.990000 3.98
    ## 2           2 4.99 4.99 4.990000 4.99
    ## 3           3 2.99 1.99 2.490000 4.98
    ## 4           5 6.99 0.99 3.323333 9.97
    ## 5           6 4.99 0.99 2.990000 8.97

## Excerise 7.2: Modify the above query to only keep the customer_ids that have more then 5 payments

``` r
dbGetQuery(con,"
SELECT
  customer_id,
  MAX(amount) AS 'max', 
  MIN(amount) AS 'min', 
  AVG(amount) AS 'avg', 
  SUM(amount) AS 'sum'
FROM payment 
GROUP BY customer_id
HAVING COUNT(*) >5 
LIMIT 5
")
```

    ##   customer_id  max  min   avg   sum
    ## 1          19 9.99 0.99 4.490 26.94
    ## 2          53 9.99 0.99 4.490 26.94
    ## 3         109 7.99 0.99 3.990 27.93
    ## 4         161 5.99 0.99 2.990 17.94
    ## 5         197 3.99 0.99 2.615 20.92

# Cleanup

``` r
# clean up
dbDisconnect(con)
```
