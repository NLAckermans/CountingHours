Untitled
================

\#Library

``` r
library(readxl)
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.0.5

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.3     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.0     v forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.0.5

    ## Warning: package 'tibble' was built under R version 4.0.5

    ## Warning: package 'tidyr' was built under R version 4.0.5

    ## Warning: package 'readr' was built under R version 4.0.5

    ## Warning: package 'dplyr' was built under R version 4.0.5

    ## Warning: package 'forcats' was built under R version 4.0.5

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggpubr)
```

    ## Warning: package 'ggpubr' was built under R version 4.0.5

\#Import & clean

``` r
data<-read_excel("CountingHours_example.xlsx")
data
```

    ## # A tibble: 12 x 15
    ##    month    `research type` `research hours` `teaching type` `teaching hours`
    ##    <chr>    <chr>                      <dbl> <chr>                      <dbl>
    ##  1 October  Project 1                     21 Class 1                        5
    ##  2 October  Project 2                      4 Class 2                       10
    ##  3 October  Project 3                     12 <NA>                          NA
    ##  4 October  Project 4                      8 <NA>                          NA
    ##  5 November Project 1                     17 Class 1                        5
    ##  6 November Project 2                      3 Class 2                       10
    ##  7 November Project 3                      8 <NA>                          NA
    ##  8 November Project 4                      2 <NA>                          NA
    ##  9 December Project 1                      3 Class 1                        5
    ## 10 December Project 2                      2 Class 2                       10
    ## 11 December Project 3                      1 <NA>                          NA
    ## 12 December Project 4                      5 <NA>                          NA
    ## # ... with 10 more variables: service type <chr>, service hours <dbl>,
    ## #   prof dev type <chr>, prof dev hours <dbl>, other type <chr>,
    ## #   other hours <dbl>, vacation <chr>, vacation hours <dbl>, conference <chr>,
    ## #   conference hours <dbl>

``` r
data_totals <- data %>%
  group_by(month) %>%
  summarise(across(ends_with("hours"), sum, na.rm=TRUE))
data_totals
```

    ## # A tibble: 3 x 8
    ##   month    `research hours` `teaching hours` `service hours` `prof dev hours`
    ##   <chr>               <dbl>            <dbl>           <dbl>            <dbl>
    ## 1 December               11               15               2                2
    ## 2 November               30               15               2                1
    ## 3 October                45               15               2                1
    ## # ... with 3 more variables: other hours <dbl>, vacation hours <dbl>,
    ## #   conference hours <dbl>

\#Plot

``` r
hours <- c("research hours", "conference hours", "service hours", 
           "prof dev hours", "other hours", "vacation hours", "conference hours")

data_totals %>%
  pivot_longer(cols = -month, names_to = 'hours')%>%
  ggplot(aes(x=month, y=value, fill=hours))+
  geom_bar(stat="identity")
```

![](Title_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

\#Percentage plot

``` r
data_perc <- data_totals %>%
  rename('research'='research hours', 
         'teaching'='teaching hours',
         'conference'='conference hours', 
         'service'='service hours',
         'prof.dev'='prof dev hours',
         'other'='other hours',
         'vacation'='vacation hours')%>%
  mutate(total=rowSums(across(where(is.numeric))))%>%
  mutate(research=research/total*100,
         teaching=teaching/total*100,
         conference=conference/total*100,
         service=service/total*100,
         prof.dev=prof.dev/total*100,
         other=other/total*100,
         vacation=vacation/total*100)%>%
  select(month, research, service, teaching, prof.dev, 
         conference, other, vacation)
data_perc
```

    ## # A tibble: 3 x 8
    ##   month    research service teaching prof.dev conference other vacation
    ##   <chr>       <dbl>   <dbl>    <dbl>    <dbl>      <dbl> <dbl>    <dbl>
    ## 1 December     19.0    3.45     25.9    3.45         0    6.90     41.4
    ## 2 November     27.8    1.85     13.9    0.926       29.6  3.70     22.2
    ## 3 October      60      2.67     20      1.33         0    5.33     10.7

``` r
percents <- c("research", "teaching", "service", "conference", "prof.dev", 
              "other", "vacation")

data_perc %>%
  pivot_longer(cols = -month, names_to = 'percents')%>%
  ggplot(aes(x=month, y=value, fill=percents))+
  geom_bar(stat="identity")+
  ylab("percentage")+
  theme(legend.title=element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  scale_fill_discrete(breaks = percents)+
  scale_color_brewer(palette = "Dark2")
```

![](Title_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
