---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


### Loading and preprocessing the data  
Using **read_csv** function from readr package which automatically
format dates

```r
library(tidyverse)
Sys.setlocale("LC_ALL","ENGLISH")#because my systems works in Russian
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" 

if(!file.exists("rawzip.zip")){
  download.file(url,destfile="rawzip.zip")
}
unzip("rawzip.zip")
my_data<-read_csv("activity.csv")
```
Structure of the data  

```r
str(my_data)  
```

```
## tibble [17,568 x 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ steps   : num [1:17568] NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date[1:17568], format: "2012-10-01" "2012-10-01" ...
##  $ interval: num [1:17568] 0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   steps = col_double(),
##   ..   date = col_date(format = ""),
##   ..   interval = col_double()
##   .. )
```
### What is mean total number of steps taken per day? 
  
##### Calculate the total number of steps taken per day  
  
using **group_by** function we will group our data frame by date,
and then using **summarise** function to find total steps per day

```r
steps_per_day<-my_data%>%group_by(date)%>%
  summarise(total_steps=sum(steps))
```

```r
head(steps_per_day)  
```

```
## # A tibble: 6 x 2
##   date       total_steps
##   <date>           <dbl>
## 1 2012-10-01          NA
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```
In the code above , I used **%>%** which is calles pipe. It helps to avoid writing which data to use in each function, and allows to avoid 
nested functions  
  
##### Make a histogram of the total number of steps taken each day  
Usng ggplot2 package,and **geom_col** function we can constract bar plot using original data   

```r
ggplot(my_data)+geom_col(aes(date,steps),fill="blue")+
  ggtitle("Total number of steps taken each day")
```

```
## Warning: Removed 2304 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
  
##### Calculate and report the mean and median of the total number of steps taken per day  
Again using **group_by** and **summarise** functions  

```r
mean_median_summary<-my_data%>%group_by(date)%>%
  summarise(mean=mean(steps,na.rn=TRUE),median=median(steps,na.rm=TRUE))
```

```r
head(mean_median_summary)
```

```
## # A tibble: 6 x 3
##   date         mean median
##   <date>      <dbl>  <dbl>
## 1 2012-10-01 NA         NA
## 2 2012-10-02  0.438      0
## 3 2012-10-03 39.4        0
## 4 2012-10-04 42.1        0
## 5 2012-10-05 46.2        0
## 6 2012-10-06 53.5        0
```
  
  
### What is the average daily activity pattern?  
##### Make a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  
  
Using **group_by** and **summarise** functions,and then applying **geom_line**  

```r
time_series<-my_data%>%group_by(interval)%>%
  summarise(mean=mean(steps,na.rm=TRUE))
```

```r
ggplot(time_series)+geom_line(aes(interval,mean),color="blue")+
  ggtitle("Average steps taken averaged across all days")+ylab("average")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
  
##### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  
We actually can annotate our plot by adding vertical line(at max) using **geom_vline**  
To find which 5 minute interval has the maximum number of steps
we can apply **which.max** function to number of steps and then
use that index to find max 5 minute interval

```r
steps_per_interval<-my_data%>%group_by(interval)%>%
  summarise(average_steps=mean(steps,na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(steps_per_interval)
```

```
## # A tibble: 6 x 2
##   interval average_steps
##      <dbl>         <dbl>
## 1        0        1.72  
## 2        5        0.340 
## 3       10        0.132 
## 4       15        0.151 
## 5       20        0.0755
## 6       25        2.09
```

```r
max_index<-which.max(steps_per_interval$average_steps)
max_interval<-as.numeric(steps_per_interval[max_index,1])
```

```r
ggplot(time_series)+geom_line(aes(interval,mean),color="blue")+
  ggtitle("Average steps taken averaged across all days")+ylab("average")+
  geom_vline(xintercept = max_interval,lty="dashed")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
### Imputing missing values     
##### Calculate and report the total number of missing values in the dataset   
To find number we can use**map** function from purr package.  

```r
map(my_data, ~sum(is.na(.)))
```

```
## $steps
## [1] 2304
## 
## $date
## [1] 0
## 
## $interval
## [1] 0
```
##### Devise a strategy for filling in all of the missing values in the dataset.  
I decided to change Na_s with corresponding interval average. Fisrt of all
i will **split** steps by interval. It will create a list with name attribute coresponding to interval.

```r
splitted_steps<-split(my_data$steps,my_data$interval)
head(splitted_steps,3)
```

```
## $`0`
##  [1] NA  0  0 47  0  0  0 NA  0 34  0  0  0  0  0  0  0  0  0  0  0 10  0  0  0
## [26]  0  0  0  0  0  0 NA  0  0 NA  0  0  0  0 NA NA  0  0  0 NA  0  0  0  0  0
## [51]  0  0  0  0  0  0  0  0  0  0 NA
## 
## $`5`
##  [1] NA  0  0  0  0  0  0 NA  0 18  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [26]  0  0  0  0  0  0 NA  0  0 NA  0  0  0  0 NA NA  0  0  0 NA  0  0  0  0  0
## [51]  0  0  0  0  0  0  0  0  0  0 NA
## 
## $`10`
##  [1] NA  0  0  0  0  0  0 NA  0  7  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [26]  0  0  0  0  0  0 NA  0  0 NA  0  0  0  0 NA NA  0  0  0 NA  0  0  0  0  0
## [51]  0  0  0  0  0  0  0  0  0  0 NA
```
Then using **select** function and previously applied group_by and summarise 
i will select only steps column and then convert it vector(in order to use in iterations)  

```r
steps_per_interval_average<-my_data%>%group_by(interval)%>%
  summarise(average_steps=mean(steps,na.rm=TRUE))%>%select(average_steps)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
interval_average<-unlist(steps_per_interval_average)
head(interval_average)
```

```
## average_steps1 average_steps2 average_steps3 average_steps4 average_steps5 
##      1.7169811      0.3396226      0.1320755      0.1509434      0.0754717 
## average_steps6 
##      2.0943396
```
Using **map2** function we could create corrected list,where all NAs are replaced. **map2** function  iterate 2 objects simultaneosly. It take first argument of splitted_steps(recorded steps is 0th interval),takes fisrt argument of interval_average(average steps in 0th interval) and finally using **replace_na** replace all NAs in 0th interval with 0th interval average. And this process repeats untill all objects iterated
  

```r
corrected_list<-map2(splitted_steps,interval_average,replace_na)
head(corrected_list,3)
```

```
## $`0`
##  [1]  1.716981  0.000000  0.000000 47.000000  0.000000  0.000000  0.000000
##  [8]  1.716981  0.000000 34.000000  0.000000  0.000000  0.000000  0.000000
## [15]  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000
## [22] 10.000000  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000
## [29]  0.000000  0.000000  0.000000  1.716981  0.000000  0.000000  1.716981
## [36]  0.000000  0.000000  0.000000  0.000000  1.716981  1.716981  0.000000
## [43]  0.000000  0.000000  1.716981  0.000000  0.000000  0.000000  0.000000
## [50]  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000
## [57]  0.000000  0.000000  0.000000  0.000000  1.716981
## 
## $`5`
##  [1]  0.3396226  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
##  [7]  0.0000000  0.3396226  0.0000000 18.0000000  0.0000000  0.0000000
## [13]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
## [19]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
## [25]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
## [31]  0.0000000  0.3396226  0.0000000  0.0000000  0.3396226  0.0000000
## [37]  0.0000000  0.0000000  0.0000000  0.3396226  0.3396226  0.0000000
## [43]  0.0000000  0.0000000  0.3396226  0.0000000  0.0000000  0.0000000
## [49]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
## [55]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
## [61]  0.3396226
## 
## $`10`
##  [1] 0.1320755 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
##  [8] 0.1320755 0.0000000 7.0000000 0.0000000 0.0000000 0.0000000 0.0000000
## [15] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
## [22] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
## [29] 0.0000000 0.0000000 0.0000000 0.1320755 0.0000000 0.0000000 0.1320755
## [36] 0.0000000 0.0000000 0.0000000 0.0000000 0.1320755 0.1320755 0.0000000
## [43] 0.0000000 0.0000000 0.1320755 0.0000000 0.0000000 0.0000000 0.0000000
## [50] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
## [57] 0.0000000 0.0000000 0.0000000 0.0000000 0.1320755
```
##### Create a new dataset that is equal to the original dataset but with the missing data filled in  

Then we could transform this list into vector and replace steps column in my_data

```r
corrected_steps<-as.numeric(unlist(corrected_list))
length(corrected_steps)==nrow(my_data)
```

```
## [1] TRUE
```

```r
corrected_data<-my_data
corrected_data$steps<-corrected_steps
map(corrected_data, ~sum(is.na(.)))
```

```
## $steps
## [1] 0
## 
## $date
## [1] 0
## 
## $interval
## [1] 0
```
###### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day         
Let's compare plots with and withoud missiong values using **gridExtra** package

```r
with_na<-ggplot(my_data)+geom_col(aes(date,steps),fill="blue")+
  ggtitle("Total number of steps taken each day(with NA")
without_na<-ggplot(corrected_data)+
  geom_col(aes(date,steps),fill="red")+
  ggtitle("Total number of steps taken each day(without NA")
library(gridExtra)
with_na
```

```
## Warning: Removed 2304 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
grid.arrange(with_na,without_na,nrow=1)
```

```
## Warning: Removed 2304 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-17-2.png)<!-- -->

### Are there differences in activity patterns between weekdays and weekends?   
  
##### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.  
Using **wday** function from lubridate package we could numerically represents day of a week. Then by applying mutate function we could create new variable which takes 0 if "weekday" and 1 if "weekend

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
new_data<-mutate(corrected_data,weekday=as.numeric(wday(date)<=5))
head(new_data)
```

```
## # A tibble: 6 x 4
##   steps date       interval weekday
##   <dbl> <date>        <dbl>   <dbl>
## 1  1.72 2012-10-01        0       1
## 2  0    2012-10-01        5       1
## 3  0    2012-10-01       10       1
## 4 47    2012-10-01       15       1
## 5  0    2012-10-01       20       1
## 6  0    2012-10-01       25       1
```

```r
new_data$weekday<-factor(new_data$weekday,levels=c(0,1),
                         labels=c("weekend","weekday"))
head(new_data)
```

```
## # A tibble: 6 x 4
##   steps date       interval weekday
##   <dbl> <date>        <dbl> <fct>  
## 1  1.72 2012-10-01        0 weekday
## 2  0    2012-10-01        5 weekday
## 3  0    2012-10-01       10 weekday
## 4 47    2012-10-01       15 weekday
## 5  0    2012-10-01       20 weekday
## 6  0    2012-10-01       25 weekday
```
Contracting new plot  

```r
ggplot(new_data)+geom_col(aes(date,steps,fill=weekday))+
  facet_grid(.~weekday)
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
  
###### Make a panel plot containing a time series plot  
  
  

```r
corrected_time_series<-new_data%>%group_by(interval,weekday)%>%
  summarise(mean=mean(steps,na.rm=TRUE))
ggplot(corrected_time_series)+
  geom_line(aes(interval,mean,color=weekday))+
  ggtitle("Average steps taken averaged across all days")+ylab("average")+facet_grid(.~weekday)
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png)<!-- -->
