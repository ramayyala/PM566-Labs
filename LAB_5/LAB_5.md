LAB\_5
================
Ram Ayyala
9/24/2021

# Load Libraries and data

``` r
library(data.table)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::between()   masks data.table::between()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::first()     masks data.table::first()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::last()      masks data.table::last()
    ## x purrr::transpose() masks data.table::transpose()

``` r
if (!file.exists("met_all.gz"))
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz",
    destfile = "met_all.gz",
    method   = "libcurl",
    timeout  = 60
    )
met <- data.table::fread("met_all.gz")
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

\#Stations Data Processing

``` r
# Dealing with NAs and 999999
#stations[stations$USAF==999999] <- NA_integer_
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

# Met Data Processing

``` r
met <- met[temp>=-17][order(temp)]
summary(met)
```

    ##      USAFID            WBAN            year          month        day    
    ##  Min.   :690150   Min.   :  116   Min.   :2019   Min.   :8   Min.   : 1  
    ##  1st Qu.:720927   1st Qu.: 3705   1st Qu.:2019   1st Qu.:8   1st Qu.: 8  
    ##  Median :722720   Median :13841   Median :2019   Median :8   Median :16  
    ##  Mean   :723095   Mean   :29496   Mean   :2019   Mean   :8   Mean   :16  
    ##  3rd Qu.:725090   3rd Qu.:54768   3rd Qu.:2019   3rd Qu.:8   3rd Qu.:24  
    ##  Max.   :726813   Max.   :94998   Max.   :2019   Max.   :8   Max.   :31  
    ##                                                                          
    ##       hour            min             lat             lon         
    ##  Min.   : 0.00   Min.   : 0.00   Min.   :24.55   Min.   :-124.29  
    ##  1st Qu.: 6.00   1st Qu.:20.00   1st Qu.:33.98   1st Qu.: -98.02  
    ##  Median :11.00   Median :48.00   Median :38.37   Median : -91.74  
    ##  Mean   :11.46   Mean   :39.23   Mean   :37.97   Mean   : -92.14  
    ##  3rd Qu.:17.00   3rd Qu.:55.00   3rd Qu.:41.96   3rd Qu.: -82.99  
    ##  Max.   :23.00   Max.   :59.00   Max.   :48.94   Max.   : -68.31  
    ##                                                                   
    ##       elev           wind.dir      wind.dir.qc        wind.type.code    
    ##  Min.   : -13.0   Min.   :  3      Length:2317212     Length:2317212    
    ##  1st Qu.: 101.0   1st Qu.:120      Class :character   Class :character  
    ##  Median : 252.0   Median :180      Mode  :character   Mode  :character  
    ##  Mean   : 415.1   Mean   :185                                           
    ##  3rd Qu.: 400.0   3rd Qu.:260                                           
    ##  Max.   :9999.0   Max.   :360                                           
    ##                   NA's   :732171                                        
    ##     wind.sp       wind.sp.qc          ceiling.ht    ceiling.ht.qc  
    ##  Min.   : 0.00   Length:2317212     Min.   :    0   Min.   :1.000  
    ##  1st Qu.: 0.00   Class :character   1st Qu.: 3048   1st Qu.:5.000  
    ##  Median : 2.10   Mode  :character   Median :22000   Median :5.000  
    ##  Mean   : 2.46                      Mean   :16171   Mean   :4.945  
    ##  3rd Qu.: 3.60                      3rd Qu.:22000   3rd Qu.:5.000  
    ##  Max.   :36.00                      Max.   :22000   Max.   :9.000  
    ##  NA's   :31743                      NA's   :73442                  
    ##  ceiling.ht.method    sky.cond            vis.dist      vis.dist.qc       
    ##  Length:2317212     Length:2317212     Min.   :     0   Length:2317212    
    ##  Class :character   Class :character   1st Qu.: 16093   Class :character  
    ##  Mode  :character   Mode  :character   Median : 16093   Mode  :character  
    ##                                        Mean   : 14924                     
    ##                                        3rd Qu.: 16093                     
    ##                                        Max.   :160000                     
    ##                                        NA's   :33674                      
    ##    vis.var           vis.var.qc             temp          temp.qc         
    ##  Length:2317212     Length:2317212     Min.   :-17.00   Length:2317212    
    ##  Class :character   Class :character   1st Qu.: 19.60   Class :character  
    ##  Mode  :character   Mode  :character   Median : 23.50   Mode  :character  
    ##                                        Mean   : 23.59                     
    ##                                        3rd Qu.: 27.80                     
    ##                                        Max.   : 56.00                     
    ##                                                                           
    ##    dew.point      dew.point.qc         atm.press        atm.press.qc  
    ##  Min.   :-37.20   Length:2317212     Min.   : 960.5    Min.   :1.000  
    ##  1st Qu.: 13.80   Class :character   1st Qu.:1011.8    1st Qu.:5.000  
    ##  Median : 18.10   Mode  :character   Median :1014.1    Median :9.000  
    ##  Mean   : 17.02                      Mean   :1014.2    Mean   :7.696  
    ##  3rd Qu.: 21.70                      3rd Qu.:1016.4    3rd Qu.:9.000  
    ##  Max.   : 36.00                      Max.   :1059.9    Max.   :9.000  
    ##  NA's   :6295                        NA's   :1606788                  
    ##        rh         
    ##  Min.   :  0.833  
    ##  1st Qu.: 55.790  
    ##  Median : 76.554  
    ##  Mean   : 71.641  
    ##  3rd Qu.: 90.629  
    ##  Max.   :100.000  
    ##  NA's   :6295

``` r
##Make sure there are no missing data in the key variables coded as 9999, 999, etc
met[met$elev==9999.0] <- NA
summary(met$elev)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   -13.0   101.0   252.0   414.3   400.0  4113.0     182

# Merging Data

``` r
dat <- merge(
  # Data
  x     = met,      
  y     = stations, 
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
  )
```

# Question 1: Representative station for the US

## What is the median station in terms of temperature, wind speed, and atmospheric pressure? Look for the three weather stations that best represent continental US using the quantile() function. Do these three coincide?

We generate a representative version of each station. We will use th
eaverages (median could also be a good way to represent it, but ti will
depend on case)

``` r
station_averages <- dat[,.(
  temp = mean(temp,na.rm=TRUE),
  wind.sp = mean(wind.sp, na.rm=TRUE),
  atm.press = mean(atm.press, na.rm=TRUE)
), by= USAFID]
```

# Identify Quantiles per variable

``` r
medians <- station_averages[,.(
  temp_50 = quantile(temp,probs = .5, na.rm=TRUE),
  wind.sp_50 = quantile(wind.sp,probs = .5, na.rm=TRUE),
  atm.press_50 = quantile(atm.press,probs = .5, na.rm=TRUE)
)]
medians
```

    ##     temp_50 wind.sp_50 atm.press_50
    ## 1: 23.68406   2.463685     1014.691

Now we can find stations that match these values or are the closets to
them via `which.min()`

``` r
station_averages[, temp_dist := abs(temp -medians$temp_50)]
median_temp_station <- station_averages[order(temp_dist)][1]
median_temp_station
```

    ##    USAFID     temp  wind.sp atm.press   temp_dist
    ## 1: 720458 23.68173 1.209682       NaN 0.002328907

The Median temperature station is `r median_temp station$USAFID`.

# Question 2: Representative station per state

## Just like the previous question, you are asked to identify what is the most representative, the median, station per state. This time, instead of looking at one variable at a time, look at the euclidean distance. If multiple stations show in the median, select the one located at the lowest latitude.

``` r
station_averages <- merge(
  # Data
  x     = station_averages,      
  y     = stations, 
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
  )
station_averages[, temp_50 := quantile(temp, probs=.5, na.rm=TRUE), by= STATE]
station_averages[, wind.sp_50 := quantile(wind.sp, probs=.5, na.rm=TRUE), by= STATE]
station_averages
```

    ##       USAFID     temp  wind.sp atm.press temp_dist CTRY STATE  temp_50
    ##    1:     NA      NaN      NaN       NaN       NaN <NA>  <NA>       NA
    ##    2: 690150 33.18763 3.483560  1010.379 9.5035752   US    CA 22.66268
    ##    3: 720110 31.22003 2.138348       NaN 7.5359677   US    TX 29.75188
    ##    4: 720113 23.29317 2.470298       NaN 0.3908894   US    MI 20.51970
    ##    5: 720120 27.01922 2.503079       NaN 3.3351568   US    SC 25.80545
    ##   ---                                                                 
    ## 1585: 726777 19.15492 4.673878  1014.299 4.5291393   US    MT 19.15492
    ## 1586: 726797 18.78980 2.858281  1014.902 4.8942607   US    MT 19.15492
    ## 1587: 726798 19.47014 4.449337  1014.072 4.2139153   US    MT 19.15492
    ## 1588: 726810 25.03549 3.037356  1011.730 1.3514356   US    ID 20.56798
    ## 1589: 726813 23.47809 2.435372  1012.315 0.2059716   US    ID 20.56798
    ##       wind.sp_50
    ##    1:         NA
    ##    2:   2.561738
    ##    3:   3.413810
    ##    4:   2.273423
    ##    5:   1.696119
    ##   ---           
    ## 1585:   4.151737
    ## 1586:   4.151737
    ## 1587:   4.151737
    ## 1588:   2.568944
    ## 1589:   2.568944

``` r
#euclidean distance
station_averages[, eudist := sqrt((temp-temp_50)^2 + (wind.sp - wind.sp_50)^2)]
```

# Question 3: In the middle?

## For each state, identify what is the station that is closest to the mid-point of the state. Combining these with the stations you identified in the previous question, use leaflet() to visualize all \~100 points in the same figure, applying different colors for those identified in this question.

# Question 4: Means of means

## Using the quantile() function, generate a summary table that shows the number of states included, average temperature, wind-speed, and atmospheric pressure by the variable “average temperature level,” which you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

low: temp &lt; 20 Mid: temp &gt;= 20 and temp &lt; 25 High: temp &gt;=
25 Once you are done with that, you can compute the following:

Number of entries (records), Number of NA entries, Number of stations,
Number of states included, and Mean temperature, wind-speed, and
atmospheric pressure. All by the levels described before.
