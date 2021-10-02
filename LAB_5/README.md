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

``` r
station_averages[, wind.sp_dist := abs(wind.sp -medians$wind.sp_50)]
median_wind.sp_station <- station_averages[order(wind.sp_dist)][1]
median_wind.sp_station
```

    ##    USAFID     temp  wind.sp atm.press  temp_dist wind.sp_dist
    ## 1: 724066 23.72338 2.463685  1016.077 0.03932015            0

``` r
station_averages[, atm.press_dist := abs(atm.press -medians$atm.press_50)]
median_atm.press_station <- station_averages[order(atm.press_dist)][1]
median_atm.press_station
```

    ##    USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist atm.press_dist
    ## 1: 722238 26.13978 1.470619  1014.691  2.455719    0.9930659   0.0005376377

The median temperature station is 720458. The median wind speed station
is 724066. The median atmospheric pressure station is 722238.

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

    ##       USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist atm.press_dist
    ##    1:     NA      NaN      NaN       NaN       NaN          NaN            NaN
    ##    2: 690150 33.18763 3.483560  1010.379 9.5035752  1.019875012      4.3124708
    ##    3: 720110 31.22003 2.138348       NaN 7.5359677  0.325337218            NaN
    ##    4: 720113 23.29317 2.470298       NaN 0.3908894  0.006612952            NaN
    ##    5: 720120 27.01922 2.503079       NaN 3.3351568  0.039394521            NaN
    ##   ---                                                                         
    ## 1585: 726777 19.15492 4.673878  1014.299 4.5291393  2.210193250      0.3920955
    ## 1586: 726797 18.78980 2.858281  1014.902 4.8942607  0.394595886      0.2106085
    ## 1587: 726798 19.47014 4.449337  1014.072 4.2139153  1.985651779      0.6191806
    ## 1588: 726810 25.03549 3.037356  1011.730 1.3514356  0.573671551      2.9607085
    ## 1589: 726813 23.47809 2.435372  1012.315 0.2059716  0.028312431      2.3759601
    ##       CTRY STATE  temp_50 wind.sp_50
    ##    1: <NA>  <NA>       NA         NA
    ##    2:   US    CA 22.66268   2.561738
    ##    3:   US    TX 29.75188   3.413810
    ##    4:   US    MI 20.51970   2.273423
    ##    5:   US    SC 25.80545   1.696119
    ##   ---                               
    ## 1585:   US    MT 19.15492   4.151737
    ## 1586:   US    MT 19.15492   4.151737
    ## 1587:   US    MT 19.15492   4.151737
    ## 1588:   US    ID 20.56798   2.568944
    ## 1589:   US    ID 20.56798   2.568944

``` r
#euclidean distance
station_averages[, eudist := sqrt((temp-temp_50)^2 + (wind.sp - wind.sp_50)^2)]
station_averages[ , .SD[which.min(eudist)], by = STATE]
```

    ##     STATE USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist
    ##  1:    CA 722970 22.76040 2.325982  1012.710 0.9236572   0.13770290
    ##  2:    TX 722598 29.81293 3.521417       NaN 6.1288732   1.05773203
    ##  3:    MI 725395 20.44096 2.357275  1015.245 3.2431039   0.10641018
    ##  4:    SC 723107 25.95831 1.599275       NaN 2.2742552   0.86440974
    ##  5:    IL 722076 22.34403 2.244115       NaN 1.3400341   0.21956977
    ##  6:    MO 720479 24.14775 2.508153       NaN 0.4636885   0.04446838
    ##  7:    AR 722054 26.58944 1.707136  1014.127 2.9053841   0.75654880
    ##  8:    OR 720202 17.16329 1.824696       NaN 6.5207664   0.63898841
    ##  9:    WA 720254 19.24684 1.268571       NaN 4.4372238   1.19511334
    ## 10:    GA 722197 26.70404 1.544133  1015.574 3.0199790   0.91955129
    ## 11:    MN 726553 19.67552 2.394756       NaN 4.0085430   0.06892895
    ## 12:    AL 722286 26.35793 1.675828  1014.909 2.6738730   0.78785666
    ## 13:    IN 724386 22.32575 2.243013  1014.797 1.3583049   0.22067167
    ## 14:    NC 720864 24.82394 1.612864       NaN 1.1398816   0.85082107
    ## 15:    VA 724006 24.31662 1.650539       NaN 0.6325625   0.81314569
    ## 16:    IA 725464 21.37948 2.679227       NaN 2.3045767   0.21554250
    ## 17:    PA 725204 21.87141 1.825605       NaN 1.8126473   0.63807964
    ## 18:    NE 725565 21.86100 3.098367  1015.068 1.8230630   0.63468218
    ## 19:    ID 725867 20.81272 2.702517  1012.802 2.8713440   0.23883179
    ## 20:    WI 726413 18.94233 2.028610       NaN 4.7417341   0.43507442
    ## 21:    WV 720328 21.94820 1.615064       NaN 1.7358610   0.84862125
    ## 22:    MD 722218 24.89883 1.883499       NaN 1.2147717   0.58018612
    ## 23:    AZ 722745 30.31538 3.307632  1010.144 6.6313167   0.84394710
    ## 24:    OK 720625 27.06188 3.865717       NaN 3.3778197   1.40203210
    ## 25:    WY 726654 19.85844 3.775443  1014.107 3.8256192   1.31175797
    ## 26:    LA 722041 27.84758 1.760730       NaN 4.1635250   0.70295443
    ## 27:    KY 720448 23.52994 1.605628       NaN 0.1541178   0.85805668
    ## 28:    FL 722011 27.56952 2.674074  1016.063 3.8854636   0.21038930
    ## 29:    CO 724699 21.94228 2.843091       NaN 1.7417815   0.37940656
    ## 30:    OH 724295 21.97211 2.801214  1015.742 1.7119534   0.33752936
    ## 31:    NJ 724090 23.47238 2.148058  1015.095 0.2116829   0.31562652
    ## 32:    NM 723658 24.94447 3.569281  1013.917 1.2604141   1.10559627
    ## 33:    KS 724550 24.14958 3.449278  1013.315 0.4655163   0.98559305
    ## 34:    ND 720911 18.34248 3.940128       NaN 5.3415810   1.47644349
    ## 35:    VT 726115 18.60548 1.101301  1014.985 5.0785811   1.36238366
    ## 36:    MS 722358 26.54093 1.747426  1014.722 2.8568706   0.71625830
    ## 37:    CT 725087 22.57539 2.126514  1014.534 1.1086682   0.33717120
    ## 38:    NV 724885 24.78430 2.600266  1013.825 1.1002416   0.13658083
    ## 39:    UT 725750 24.23571 3.040962  1011.521 0.5516551   0.57727752
    ## 40:    SD 726590 19.95928 3.550722  1014.284 3.7247800   1.08703725
    ## 41:    TN 720974 24.71645 1.483411       NaN 1.0323860   0.98027377
    ## 42:    NY 724988 20.44142 2.394383  1016.233 3.2426385   0.06930151
    ## 43:    RI 725079 22.27697 2.583469  1014.620 1.4070879   0.11978406
    ## 44:    MA 725088 21.20391 2.773018  1013.718 2.4801457   0.30933274
    ## 45:    DE 724180 24.56026 2.753082  1015.046 0.8761984   0.28939674
    ## 46:    NH 726116 19.23920 1.465766  1013.840 4.4448592   0.99791901
    ## 47:    ME 726077 18.49969 2.337241  1014.475 5.1843701   0.12644411
    ## 48:    MT 726798 19.47014 4.449337  1014.072 4.2139153   1.98565178
    ##     STATE USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist
    ##     atm.press_dist CTRY  temp_50 wind.sp_50     eudist
    ##  1:     1.98090662   US 22.66268   2.561738 0.25520867
    ##  2:            NaN   US 29.75188   3.413810 0.12372159
    ##  3:     0.55414742   US 20.51970   2.273423 0.11503023
    ##  4:            NaN   US 25.80545   1.696119 0.18095742
    ##  5:            NaN   US 22.43194   2.237652 0.08815472
    ##  6:            NaN   US 23.95109   2.453547 0.20409808
    ##  7:     0.56429267   US 26.24296   1.938625 0.41669854
    ##  8:            NaN   US 17.98061   2.011436 0.83837991
    ##  9:            NaN   US 19.24684   1.268571 0.00000000
    ## 10:     0.88250764   US 26.70404   1.497527 0.04660650
    ## 11:            NaN   US 19.63017   2.616482 0.22631462
    ## 12:     0.21799151   US 26.33664   1.662132 0.02531829
    ## 13:     0.10602154   US 22.25059   2.344333 0.12615513
    ## 14:            NaN   US 24.72953   1.627306 0.09550599
    ## 15:            NaN   US 24.37799   1.654183 0.06147377
    ## 16:            NaN   US 21.33461   2.680875 0.04490047
    ## 17:            NaN   US 21.69177   1.784167 0.18435880
    ## 18:     0.37686248   US 21.87354   3.192539 0.09500413
    ## 19:     1.88911023   US 20.56798   2.568944 0.27881642
    ## 20:            NaN   US 18.85524   2.053283 0.09051251
    ## 21:            NaN   US 21.94446   1.632107 0.01744915
    ## 22:            NaN   US 24.89883   1.883499 0.00000000
    ## 23:     4.54708887   US 30.32372   3.074359 0.23342190
    ## 24:            NaN   US 27.14427   3.852697 0.08341749
    ## 25:     0.58404600   US 19.80699   3.873986 0.11116551
    ## 26:            NaN   US 27.87430   1.712535 0.05510490
    ## 27:            NaN   US 23.88844   1.895486 0.46102117
    ## 28:     1.37200241   US 27.57325   2.705069 0.03121757
    ## 29:            NaN   US 21.49638   3.098777 0.51399978
    ## 30:     1.05128416   US 22.02062   2.554138 0.25179386
    ## 31:     0.40339033   US 23.47238   2.148058 0.00000000
    ## 32:     0.77461602   US 24.94447   3.776083 0.20680190
    ## 33:     1.37635327   US 24.21220   3.676997 0.23617460
    ## 34:            NaN   US 18.52849   3.956459 0.18672684
    ## 35:     0.29379796   US 18.61379   1.408247 0.30705883
    ## 36:     0.03113360   US 26.69258   1.637030 0.18758071
    ## 37:     0.15754612   US 22.36880   2.101294 0.20812123
    ## 38:     0.86580581   US 24.56293   3.035050 0.48789368
    ## 39:     3.17006719   US 24.35182   3.110795 0.13548614
    ## 40:     0.40762293   US 20.35662   3.665638 0.41362317
    ## 41:            NaN   US 24.88657   1.576035 0.19370133
    ## 42:     1.54200536   US 20.40674   2.304075 0.09673718
    ## 43:     0.07139021   US 22.53551   2.583469 0.25853660
    ## 44:     0.97304128   US 21.30662   2.710944 0.12000936
    ## 45:     0.35441624   US 24.56026   2.753082 0.00000000
    ## 46:     0.85153342   US 19.55054   1.563826 0.32641573
    ## 47:     0.21594166   US 18.79016   2.237210 0.30721662
    ## 48:     0.61918062   US 19.15492   4.151737 0.43351078
    ##     atm.press_dist CTRY  temp_50 wind.sp_50     eudist

# Question 3: In the middle?

## For each state, identify what is the station that is closest to the mid-point of the state. Combining these with the stations you identified in the previous question, use leaflet() to visualize all \~100 points in the same figure, applying different colors for those identified in this question.

``` r
#Determining the Midpoint of the States
midpoint<-dat[, .(
  lat_50=quantile(lat, probs=.5, na.rm=TRUE),
  lon_50=quantile(lon, probs=.5, na.rm=TRUE)
),by=STATE]

midpoint_data<-merge(x=dat, y=midpoint, by="STATE")

# Calculating euclidean distance
midpoint_data[, mid_eudist:=sqrt(
  (lon-lon_50)^2+(lat-lat_50)^2
)]
# Determining closest station to the midpoint
midpoint_station<-midpoint_data[ , .SD[which.min(mid_eudist)], by = STATE]

#Make leaflet
library(leaflet)
temp.pal <- colorFactor(palette ='Accent', domain=midpoint_station$STATE)
leaflet() %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addCircles(
    data = midpoint_station,
    label = midpoint_station$USAFID, color = ~ temp.pal(midpoint_station$STATE),
    opacity = 1, fillOpacity = 1, radius = 500
    )
```

    ## Assuming "lon" and "lat" are longitude and latitude, respectively

    ## Warning in RColorBrewer::brewer.pal(max(3, n), palette): n too large, allowed maximum for palette Accent is 8
    ## Returning the palette you asked for with that many colors

    ## Warning in RColorBrewer::brewer.pal(max(3, n), palette): n too large, allowed maximum for palette Accent is 8
    ## Returning the palette you asked for with that many colors

<div id="htmlwidget-948b21b24b44f8f71a86" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-948b21b24b44f8f71a86">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircles","args":[[33.177,35.259,34.257,37.285,39.467,41.384,39.133,28.228,32.564,41.691,43.567,40.477,40.711,38.068,37.578,30.558,41.876,39.173,44.316,43.322,45.097,38.704,33.761,45.699,35.584,48.39,40.893,43.205,40.617,33.463,39.6,41.702,40.28,35.417,42.6,40.217,41.597,34.283,43.767,35.38,31.15,38.427,37.4,44.535,47.104,44.778,39,43.064],[-86.783,-93.093,-111.339,-120.512,-106.15,-72.506,-75.467,-82.156,-82.985,-93.566,-116.24,-88.916,-86.375,-97.275,-84.77,-92.099,-71.021,-76.684,-69.797,-84.688,-94.507,-93.183,-90.758,-110.448,-79.101,-100.024,-97.997,-71.503,-74.25,-105.535,-116.01,-74.795,-83.115,-97.383,-123.364,-76.851,-71.412,-80.567,-99.318,-86.246,-97.717,-113.012,-77.517,-72.614,-122.287,-89.667,-80.274,-108.458],500,null,null,{"interactive":true,"className":"","stroke":true,"color":["#7FC97F","#8BC68C","#96C299","#9FBEA6","#A8BAB2","#B1B6BF","#B9B1CB","#C1AFD1","#CDB1C5","#D7B4BA","#E1B6AF","#E9B9A3","#F2BC97","#FABF8B","#FDC588","#FECF8A","#FFD88D","#FFE290","#FFEB93","#FFF496","#FFFE99","#EAEB9E","#D1D4A3","#B7BDA7","#9CA7AA","#7F91AD","#5E7DAF","#416BAF","#6C66A7","#8B60A0","#A45799","#BB4D91","#D13E8A","#E52583","#ED1679","#E72C6B","#E03A5D","#D8444F","#D14C40","#C95330","#C1591E","#B75D23","#AC5F30","#A0613C","#936347","#866451","#77655C","#666666"],"weight":5,"opacity":1,"fill":true,"fillColor":["#7FC97F","#8BC68C","#96C299","#9FBEA6","#A8BAB2","#B1B6BF","#B9B1CB","#C1AFD1","#CDB1C5","#D7B4BA","#E1B6AF","#E9B9A3","#F2BC97","#FABF8B","#FDC588","#FECF8A","#FFD88D","#FFE290","#FFEB93","#FFF496","#FFFE99","#EAEB9E","#D1D4A3","#B7BDA7","#9CA7AA","#7F91AD","#5E7DAF","#416BAF","#6C66A7","#8B60A0","#A45799","#BB4D91","#D13E8A","#E52583","#ED1679","#E72C6B","#E03A5D","#D8444F","#D14C40","#C95330","#C1591E","#B75D23","#AC5F30","#A0613C","#936347","#866451","#77655C","#666666"],"fillOpacity":1},null,null,["722300","723429","723745","724815","722061","720545","724088","721042","722217","725466","726810","724397","720961","724509","720448","720468","725068","724060","726185","725405","726583","724453","725023","726798","722201","720867","725513","726050","720581","722683","724770","725145","720928","723540","725975","725118","725074","720603","726530","721031","722570","724750","720498","726114","720388","726465","720328","726720"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]}],"limits":{"lat":[28.228,48.39],"lng":[-123.364,-69.797]}},"evals":[],"jsHooks":[]}</script>

``` r
  # And a pretty legend
 #addLegend('bottomleft', pal=temp.pal, values=midpoint_station$STATE,
          #title='Year', opacity=1)
```

# Question 4: Means of means

## Using the quantile() function, generate a summary table that shows the number of states included, average temperature, wind-speed, and atmospheric pressure by the variable “average temperature level,” which you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

low: temp &lt; 20 Mid: temp &gt;= 20 and temp &lt; 25 High: temp &gt;=
25 Once you are done with that, you can compute the following:

Number of entries (records), Number of NA entries, Number of stations,
Number of states included, and Mean temperature, wind-speed, and
atmospheric pressure. All by the levels described before.

``` r
#avg temp by state 
dat[, state_temp := mean(temp, na.rm=TRUE), by = STATE]
#Create temp cat variable
dat[, temp_cat := fifelse(
  state_temp < 20, "low-temp",
  fifelse(state_temp < 25, "mid-temp", "high-temp"))
  ]
#Check for NAs
table(met$temp_cat, useNA="always")
```

    ## 
    ## <NA> 
    ##    0

``` r
# Summarize
tab <-dat [, .(
  N_entries =.N,
  N_stations = length(unique(USAFID)),
  N_states = length(unique(STATE)),
  temp_avg= mean(temp, na.rm=TRUE),
  wind.sp_avg = mean(wind.sp, na.rm=TRUE),
  atm.press_avg = mean(atm.press, na.rm=TRUE)
), by=temp_cat]

knitr::kable(tab)
```

| temp\_cat | N\_entries | N\_stations | N\_states | temp\_avg | wind.sp\_avg | atm.press\_avg |
|:----------|-----------:|------------:|----------:|----------:|-------------:|---------------:|
| NA        |        182 |           1 |         1 |       NaN |          NaN |            NaN |
| mid-temp  |    1106048 |         777 |        25 |  22.39949 |     2.354367 |       1014.384 |
| high-temp |     787592 |         552 |        12 |  27.75375 |     2.521974 |       1013.739 |
| low-temp  |     423390 |         259 |        11 |  18.96496 |     2.635348 |       1014.366 |
