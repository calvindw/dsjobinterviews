Mobile Video Game Company Data Scientist Test
================
Calvin de Wilde (Sidjaya)
July 2020

### Context

In July 2020, I was approached by a tech recruiter for a Data Scientist
position at a mobile video gaming company. I passed the following
technical test. You may notice some of the english sentences may be a
bit odd and gramatically incorrect. I decided to preserve any
grammatical errors to let you decide what the questions really want to
answer.

### 1\. SQL Questions

#### Table Downloads

``` r
downloads <- data.frame(
  stringsAsFactors = FALSE,
              Date = c("2019-09-16","2019-09-16",
                       "2019-09-16","2019-09-17","2019-09-17","2019-09-17",
                       "2019-09-18","2019-09-18","2019-09-18","2019-09-19",
                       "2019-09-19","2019-09-19"),
           Country = c("US","UK","DE","US","UK",
                       "DE","US","UK","DE","US","UK","DE"),
          Download = c(12000,13000,13500,12000,
                       14000,50000,14000,35000,8000,100000,5000,
                       10000)
)

downloads$Date <- as.Date(downloads$Date, "%Y-%m-%d")
downloads
```

    ##          Date Country Download
    ## 1  2019-09-16      US    12000
    ## 2  2019-09-16      UK    13000
    ## 3  2019-09-16      DE    13500
    ## 4  2019-09-17      US    12000
    ## 5  2019-09-17      UK    14000
    ## 6  2019-09-17      DE    50000
    ## 7  2019-09-18      US    14000
    ## 8  2019-09-18      UK    35000
    ## 9  2019-09-18      DE     8000
    ## 10 2019-09-19      US   100000
    ## 11 2019-09-19      UK     5000
    ## 12 2019-09-19      DE    10000

``` r
revenue <- data.frame(
  stringsAsFactors = FALSE,
       check.names = FALSE,
                         Date = c("2019-09-16","2019-09-16","2019-09-16","2019-09-17",
                                  "2019-09-17","2019-09-17","2019-09-18",
                                  "2019-09-18","2019-09-18","2019-09-19",
                                  "2019-09-19","2019-09-19"),
                      Country = c("US",
                                  "UK","DE","US","UK","DE","US","UK","DE",
                                  "US","UK","DE"),
                 `Revenue($)` = c(1000,
                                  900,1200,4000,1400,3000,500,7000,
                                  600,10000,400,1590)
           )

revenue$Date <- as.Date(revenue$Date, "%Y-%m-%d")

revenue
```

    ##          Date Country Revenue($)
    ## 1  2019-09-16      US       1000
    ## 2  2019-09-16      UK        900
    ## 3  2019-09-16      DE       1200
    ## 4  2019-09-17      US       4000
    ## 5  2019-09-17      UK       1400
    ## 6  2019-09-17      DE       3000
    ## 7  2019-09-18      US        500
    ## 8  2019-09-18      UK       7000
    ## 9  2019-09-18      DE        600
    ## 10 2019-09-19      US      10000
    ## 11 2019-09-19      UK        400
    ## 12 2019-09-19      DE       1590

From those following tables, please create a view table in sql for :

#### a. Display date and country where its have maximum download

``` r
sqldf("
     SELECT Date,Country, MAX(Download) 
     FROM downloads 
     GROUP BY Country
      ")
```

    ##         Date Country MAX(Download)
    ## 1 2019-09-17      DE         50000
    ## 2 2019-09-18      UK         35000
    ## 3 2019-09-19      US        100000

#### b. Display date and country where its have minimum revenue

``` r
sqldf("
     SELECT Date,Country, MIN(Download) 
     FROM downloads 
     GROUP BY Country
      ")
```

    ##         Date Country MIN(Download)
    ## 1 2019-09-18      DE          8000
    ## 2 2019-09-19      UK          5000
    ## 3 2019-09-16      US         12000

#### c. Display all date,country,download and revenue

Basically create a unique key column on each table based on date and
country column, the catch is, I am not sure why the resulting key column
ended up like that

``` r
sqldf("

with 

cte_downloads as (
select Date, Country,Download, Date, strftime('%Y-%m-%d',Date)||'-'||country as key
      from downloads),
      
cte_revenue as(
select Date, Country, revenue.'Revenue($)' as revenue, Date, strftime('%Y-%m-%d',Date)||'-'||country as key
from revenue)

select cte_downloads.Date as Date,
cte_downloads.Country as Country,
cte_downloads.Download as Download,
cte_revenue.revenue as Revenue
from cte_downloads       
  left join cte_revenue
        on cte_downloads.key = cte_revenue.key 
      
      ")
```

    ##          Date Country Download Revenue
    ## 1  2019-09-16      US    12000    1000
    ## 2  2019-09-16      UK    13000     900
    ## 3  2019-09-16      DE    13500    1200
    ## 4  2019-09-17      US    12000    4000
    ## 5  2019-09-17      UK    14000    1400
    ## 6  2019-09-17      DE    50000    3000
    ## 7  2019-09-18      US    14000     500
    ## 8  2019-09-18      UK    35000    7000
    ## 9  2019-09-18      DE     8000     600
    ## 10 2019-09-19      US   100000   10000
    ## 11 2019-09-19      UK     5000     400
    ## 12 2019-09-19      DE    10000    1590

#### d. Drop date and country where its have maximum revenue

The statement function MAX() at the select statement will display the
maximum value of revenue based on country, we will filter it with where
clause

``` r
sqldf("
with cte as (
        SELECT Date, 
        Country, 
        revenue.'Revenue($)' as Revenue, 
        MAX(revenue.'Revenue($)') OVER (PARTITION BY Country) AS max_value
        from revenue)

select Date, Country, Revenue
from cte
WHERE NOT(Revenue = max_value)
      ")
```

    ##         Date Country Revenue
    ## 1 2019-09-16      DE    1200
    ## 2 2019-09-18      DE     600
    ## 3 2019-09-19      DE    1590
    ## 4 2019-09-16      UK     900
    ## 5 2019-09-17      UK    1400
    ## 6 2019-09-19      UK     400
    ## 7 2019-09-16      US    1000
    ## 8 2019-09-17      US    4000
    ## 9 2019-09-18      US     500

Another method: create a ranking and filter the row with the highest
rank

``` r
sqldf("

with cte as(
            SELECT Date, 
            Country, 
            revenue.'Revenue($)' as Revenue, 
            DENSE_RANK() OVER (PARTITION BY Country ORDER BY -revenue.'Revenue($)') AS ranking
            FROM revenue)

SELECT *
FROM cte
WHERE (NOT(ranking IN (1.0)))
      ")
```

    ##         Date Country Revenue ranking
    ## 1 2019-09-19      DE    1590       2
    ## 2 2019-09-16      DE    1200       3
    ## 3 2019-09-18      DE     600       4
    ## 4 2019-09-17      UK    1400       2
    ## 5 2019-09-16      UK     900       3
    ## 6 2019-09-19      UK     400       4
    ## 7 2019-09-17      US    4000       2
    ## 8 2019-09-16      US    1000       3
    ## 9 2019-09-18      US     500       4

#### e. Drop date and country where its have minimum download

The solution is similar with the previous question, just replace MAX
with MIN

``` r
sqldf("
with cte as (
        SELECT Date, 
        Country, 
        revenue.'Revenue($)' as Revenue, 
        MIN(revenue.'Revenue($)') OVER (PARTITION BY Country) AS min_value
        from revenue)

select Date, Country, Revenue
from cte
WHERE NOT(Revenue = min_value)
      ")
```

    ##         Date Country Revenue
    ## 1 2019-09-16      DE    1200
    ## 2 2019-09-17      DE    3000
    ## 3 2019-09-19      DE    1590
    ## 4 2019-09-16      UK     900
    ## 5 2019-09-17      UK    1400
    ## 6 2019-09-18      UK    7000
    ## 7 2019-09-16      US    1000
    ## 8 2019-09-17      US    4000
    ## 9 2019-09-19      US   10000

#### f. Update date and country download value where its have maximum value to 99999

``` r
sqldf("
      select 
      
        Date, 
        Country, 
      
        case when
        (Download = max(Download) over (partition by Country)) then (99999)
        when true then (Download)
          end as Download
      
      from downloads
      
      ")
```

    ##          Date Country Download
    ## 1  2019-09-16      DE    13500
    ## 2  2019-09-17      DE    99999
    ## 3  2019-09-18      DE     8000
    ## 4  2019-09-19      DE    10000
    ## 5  2019-09-16      UK    13000
    ## 6  2019-09-17      UK    14000
    ## 7  2019-09-18      UK    99999
    ## 8  2019-09-19      UK     5000
    ## 9  2019-09-16      US    12000
    ## 10 2019-09-17      US    12000
    ## 11 2019-09-18      US    14000
    ## 12 2019-09-19      US    99999

#### g. Update date and country revenue value where its have minimum value to 0

``` r
sqldf("
      select 
      
        Date, 
        Country, 
      
        case when
        (revenue.'Revenue($)' = min(revenue.'Revenue($)') over (partition by Country)) then (0)
        when true then (revenue.'Revenue($)')
          end as Revenue
      
      from revenue
      
      ")
```

    ##          Date Country Revenue
    ## 1  2019-09-16      DE    1200
    ## 2  2019-09-17      DE    3000
    ## 3  2019-09-18      DE       0
    ## 4  2019-09-19      DE    1590
    ## 5  2019-09-16      UK     900
    ## 6  2019-09-17      UK    1400
    ## 7  2019-09-18      UK    7000
    ## 8  2019-09-19      UK       0
    ## 9  2019-09-16      US    1000
    ## 10 2019-09-17      US    4000
    ## 11 2019-09-18      US       0
    ## 12 2019-09-19      US   10000

8.  Merge table in point f and g

<!-- end list -->

``` r
sqldf("
with 

cte_download as
(
select 
      
        Date, 
        Country, 
      
        case when
        (Download = max(Download) over (partition by Country)) then (99999)
        when true then (Download)
          end as Download,
        
        strftime('%Y-%m-%d',Date)||'-'|| Country as key
      
      from downloads),

cte_revenue as
(select 
      
        Date, 
        Country, 
      
        case when
        (revenue.'Revenue($)' = min(revenue.'Revenue($)') over (partition by Country)) then (0)
        when true then (revenue.'Revenue($)')
          end as Revenue,
        
        strftime('%Y-%m-%d',Date)||'-'|| Country as key
      
      from revenue)


select cte_download.Date, cte_download.Country, cte_download.Download, cte_revenue.Revenue

from cte_download
left join cte_revenue on 
  cte_download.key = cte_revenue.key



")
```

    ##          Date Country Download Revenue
    ## 1  2019-09-16      DE    13500    1200
    ## 2  2019-09-17      DE    99999    3000
    ## 3  2019-09-18      DE     8000       0
    ## 4  2019-09-19      DE    10000    1590
    ## 5  2019-09-16      UK    13000     900
    ## 6  2019-09-17      UK    14000    1400
    ## 7  2019-09-18      UK    99999    7000
    ## 8  2019-09-19      UK     5000       0
    ## 9  2019-09-16      US    12000    1000
    ## 10 2019-09-17      US    12000    4000
    ## 11 2019-09-18      US    14000       0
    ## 12 2019-09-19      US    99999   10000

### 2\. These tables explain download results on each marketing channel

``` r
channel_a <- data.frame(
        Date = c("2019-09-16","2019-09-17","2019-09-18",
                 "2019-09-19","2019-09-20","2019-09-21","2019-09-22",
                 "2019-09-23","2019-09-24","2019-09-25","2019-09-26"),
    Download = c(12000,13500,14000,15000,15000,
                 10000,20000,5000,45000,60000,12000)
)
channel_a$Date <- as.Date(channel_a$Date, "%Y-%m-%d")

channel_a
```

    ##          Date Download
    ## 1  2019-09-16    12000
    ## 2  2019-09-17    13500
    ## 3  2019-09-18    14000
    ## 4  2019-09-19    15000
    ## 5  2019-09-20    15000
    ## 6  2019-09-21    10000
    ## 7  2019-09-22    20000
    ## 8  2019-09-23     5000
    ## 9  2019-09-24    45000
    ## 10 2019-09-25    60000
    ## 11 2019-09-26    12000

``` r
channel_b <- data.frame(
        Date = c("2019-09-16","2019-09-17","2019-09-18",
                 "2019-09-19","2019-09-20","2019-09-21","2019-09-22",
                 "2019-09-23","2019-09-24","2019-09-25","2019-09-26"),
    Download = c(13000,14500,14000,15000,17000,
                 19000,30000,1000,15000,50000,10000)
)
channel_b$Date <- as.Date(channel_b$Date, "%Y-%m-%d")

channel_b
```

    ##          Date Download
    ## 1  2019-09-16    13000
    ## 2  2019-09-17    14500
    ## 3  2019-09-18    14000
    ## 4  2019-09-19    15000
    ## 5  2019-09-20    17000
    ## 6  2019-09-21    19000
    ## 7  2019-09-22    30000
    ## 8  2019-09-23     1000
    ## 9  2019-09-24    15000
    ## 10 2019-09-25    50000
    ## 11 2019-09-26    10000

### From those following, which channel should be used for marketing channel in the future? Please explain your answer/choice

First let’s tidy the data

``` r
#tidy the data
channel_a <- channel_a %>% rename(date=Date,channel_a=Download)
channel_b <- channel_b %>% rename(date=Date,channel_b=Download)

channel_a <- channel_a %>% pivot_longer(cols=2, names_to="channel", values_to="downloads")

channel_b <- channel_b %>% pivot_longer(cols=2, names_to="channel", values_to="downloads")

df <- rbind(channel_a,channel_b)

df
```

    ## # A tibble: 22 x 3
    ##    date       channel   downloads
    ##    <date>     <chr>         <dbl>
    ##  1 2019-09-16 channel_a     12000
    ##  2 2019-09-17 channel_a     13500
    ##  3 2019-09-18 channel_a     14000
    ##  4 2019-09-19 channel_a     15000
    ##  5 2019-09-20 channel_a     15000
    ##  6 2019-09-21 channel_a     10000
    ##  7 2019-09-22 channel_a     20000
    ##  8 2019-09-23 channel_a      5000
    ##  9 2019-09-24 channel_a     45000
    ## 10 2019-09-25 channel_a     60000
    ## # ... with 12 more rows

The data is now tidy, first let’s make a bar chart to visualize the
traffic

``` r
#stacked bar chart to see trends
v <- df %>% ggplot(aes(x=date,y=downloads,fill=channel)) + 
  geom_bar(stat="identity",position = position_dodge(width=0.9)) 
v
```

![](datascientist_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Based on chart above at first glance it seems Channel A is outperforming
channel B.

Let’s look at some basic statistics:

``` r
#Compute summary statistics by groups - count, mean, sd:
df %>% group_by(channel) %>%
  summarise(
    count = n(),
    mean = mean(downloads, na.rm = TRUE),
    sd = sd(downloads, na.rm = TRUE),
    sum = sum(downloads,na.rm=TRUE))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 5
    ##   channel   count   mean     sd    sum
    ##   <chr>     <int>  <dbl>  <dbl>  <dbl>
    ## 1 channel_a    11 20136. 16751. 221500
    ## 2 channel_b    11 18045. 12626. 198500

Based on the data, channel a has a total downloads of 221,500 and
channel b has a total downloads of 198,500. The average download and
standard deviation of both channels show that channel a clearly has more
traffic. We may conclude that channel a is more reliable as it has *a
higher average traffic*.

However, to conclude whether channel a has a better performance than
channel b, I will use ANOVA test (analysis of variance) a statistical
test that can be used to check whether two groups have a significant
difference between their average or not.

``` r
#normality test as the number of data is less than 50 for each group

with(df, shapiro.test(downloads[channel == "channel_a"]))# p = 0.00069
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  downloads[channel == "channel_a"]
    ## W = 0.71355, p-value = 0.0006991

``` r
with(df, shapiro.test(downloads[channel == "channel_b"]))# p = 0.01201
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  downloads[channel == "channel_b"]
    ## W = 0.80806, p-value = 0.01201

The shapiro test gives a result of p value less than 0.05, so we can
assume the normality.

Next, we will do the ANOVA test:

``` r
# q: is the mean significant?
# Compute the analysis of variance
res.aov <- aov(downloads ~ channel, data = df)
summary(res.aov) #result is 0.744
```

    ##             Df    Sum Sq   Mean Sq F value Pr(>F)
    ## channel      1 2.405e+07  24045455   0.109  0.744
    ## Residuals   20 4.400e+09 220013636

Based on the ANOVA we got a result of p value of 0.744 which indicates
that the average downloads of Channel A and channel B does not have a
significant difference. *There is a lack of proof that either channel
outperforms or underperform each other*, therefore, either of the
channel is more than likely to have a similar performance as a marketing
channel in the future.

### 3\. If you given a task to increase revenue in mobile games, what data or variables that you would need to solve the problem ? Please explain

At very least we need to have these variables:

  - Ads Impressions. To determine how many ads were served to the user.

  - Clicks. To determine whether those impressions are good enough to
    make people click the ads.

  - Purchase. To determine whether a user buys the offered product or
    not. To make robust analysis, these demographic variables (ideally)
    should be available as well:

  - Disposable income level. To measure whether a user has enough
    disposable income to purchase a digital product offered by the video
    game.

  - Age. Older people more likely to pay for a digital service as they
    have disposable income. This could serve as a proxy variable when
    disposable income level is not available.

  - Gender. It is important to distinguish to differentiate advertised
    products between either gender.

  - Education. Some studies associated video game addiction as another
    addiction equal to gambling’s addiction. If there is a correlation
    between the level of education and addiction, we may be able to
    exploit this to increase purchase to those who are prone to video
    game addictions. One of the most successful example of mobile gaming
    revenue is Gacha system which is used by Sony’s Fate Grand Order
    where people are willing to pay to gamble their luck so they can get
    a rare character.

Using those data, we may be able to make a prediction of how much
revenue is increased if we target our ads serving to the right audience,
and reducing the ads to people who are less likely to pay for the
service, and it will optimize the average revenue per user.

### R Questions

#### Please look in the bread data csv. The data contain about bread transaction information. Suppose, I want to open bread shop, using the bread data, what suggestion that you would give? Please use R for data manipulation and data visualization.

``` r
df <- read_csv("https://raw.githubusercontent.com/calvindw/dsjobinterviews/main/mobile_game/BreadBasket_DMS.csv", 
    col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
        Time = col_time(format = "%H:%M:%S")))


df <- df %>% clean_names()
df <- df %>% mutate(item = as_factor(item),
                    transaction = as_factor(transaction))

df <- df %>% filter(item != "NONE")

df %>% head(10) 
```

    ## # A tibble: 10 x 4
    ##    date       time     transaction item         
    ##    <date>     <time>   <fct>       <fct>        
    ##  1 2016-10-30 09:58:11 1           Bread        
    ##  2 2016-10-30 10:05:34 2           Scandinavian 
    ##  3 2016-10-30 10:05:34 2           Scandinavian 
    ##  4 2016-10-30 10:07:57 3           Hot chocolate
    ##  5 2016-10-30 10:07:57 3           Jam          
    ##  6 2016-10-30 10:07:57 3           Cookies      
    ##  7 2016-10-30 10:08:41 4           Muffin       
    ##  8 2016-10-30 10:13:03 5           Coffee       
    ##  9 2016-10-30 10:13:03 5           Pastry       
    ## 10 2016-10-30 10:13:03 5           Bread

``` r
#which items are popular?
df_item <- df %>% 
  group_by(item) %>% 
  count(item) %>% 
  arrange(desc(n))  %>% ungroup()

df_item <- df_item %>%  mutate(ranking = rank(-n)) %>% 
  filter(ranking <=10) 

df_item %>% group_by(item) %>% mutate(pct = n/15008)
```

    ## # A tibble: 10 x 4
    ## # Groups:   item [10]
    ##    item              n ranking    pct
    ##    <fct>         <int>   <dbl>  <dbl>
    ##  1 Coffee         5471       1 0.365 
    ##  2 Bread          3325       2 0.222 
    ##  3 Tea            1435       3 0.0956
    ##  4 Cake           1025       4 0.0683
    ##  5 Pastry          856       5 0.0570
    ##  6 Sandwich        771       6 0.0514
    ##  7 Medialuna       616       7 0.0410
    ##  8 Hot chocolate   590       8 0.0393
    ##  9 Cookies         540       9 0.0360
    ## 10 Brownie         379      10 0.0253

``` r
df_item %>%  
  ggplot(aes(x=reorder(item,ranking),y=n)) + 
  geom_bar(stat = "identity") +
  labs(title="Top ten most purchased items")+
  ylab("Items purchased")+
  xlab("Number of purchases")+
  theme_minimal()
```

![](datascientist_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

Based on the aggregation, Coffee, Bread, Tea are the top three purchased
items.

``` r
#group by unique transaction (unique customer)
df_ag <- df %>% group_by(transaction) %>% count(transaction) %>%  ungroup() 

mean(df_ag$n) 
```

    ## [1] 2.166614

``` r
#let's order the number of transaction based on customer purchase
df_transaction <- df_ag %>% 
  select(-transaction) %>% 
  mutate(n = as_factor(n)) %>% 
  count(n, name="purchase_times") %>% 
  mutate(pct = purchase_times/sum(purchase_times))

df_transaction %>% ggplot(aes(x=n,y=purchase_times)) + 
  geom_bar(stat = "identity")+
  labs(title="Number of Purchase per Unique Customer")+
  ylab("Number of Unique Customers")+
  xlab("Number of Purchase")+
  theme_minimal()
```

![](datascientist_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

Based on the data provided, there is 9684 unique transactions occurred
from 2016-10-30 to 2017-04-29. The mean is 2. It means the average
purchase per every unique customer is 2 this can be used as a threshold
of how many items every unique customer purchased.

Now let’s see which period has the most transaction?

``` r
df_month <- df %>% group_by(month=floor_date(x=date, unit="month")) %>% count

df_month %>% ggplot(aes(x=month,y=n)) + 
  geom_bar(stat = "identity") +
  labs(title="Number of transactions per month")+
  ylab("Items purchased")+
  xlab("Month")+
  scale_x_date(date_breaks = "1 month", 
               labels=date_format("%b-%Y"))+
  theme_minimal()
```

![](datascientist_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

November 2016 seems the month with the highest purchase. Let’s see the
top items purchased in that month.

``` r
period <- interval("2016-11-01", "2016-11-30")
df_peak <- df %>% filter(date %within% period)

df_peak %>% select(-transaction) %>% 
  group_by(item) %>% count(item) %>% arrange(desc(n)) %>% ungroup()
```

    ## # A tibble: 58 x 2
    ##    item              n
    ##    <fct>         <int>
    ##  1 Coffee         1189
    ##  2 Bread           712
    ##  3 Tea             311
    ##  4 Pastry          218
    ##  5 Medialuna       208
    ##  6 Brownie         154
    ##  7 Alfajores       141
    ##  8 Muffin          127
    ##  9 Hot chocolate   118
    ## 10 Cake            116
    ## # ... with 48 more rows

Coffee, Bread, Tea are the top three purchased items in November.

**Conclusion**

As there is no price data of each item provided, we can get a following
insight:

*Which items are considered as the best selling items and which ones had
low sales*

This will reduce the risks associated with selling the said products as
there is a likely hood customer will purchase those products due to the
familiarity. As there were too many data to be displayed in a
visualization, this visualization will only display the top ten: it
appears coffee, bread, and tea are the top three most purchased items.

*How many average transactions were made by every unique customer. *

This is to manage of our expectation as we could not expect a customer
to purchase more than the typical number of purchases. Based on the
transaction data, the maximum items purchased by a customer was 11
(which only occurred 4 times). 68% of the customer would buy 2 or less.
The top three items of the shop are Coffee, Bread, Tea.

Based on the data, we have an information that the bread shop had a peak
purchase in November, declined in December and January and slowly
bouncing back in February.

**Recommendations**

The bread shop data shows what items are considered as risk averse
items: the top ten most purchased items are coffee, bread, tea, cake,
pastry, sandwich, medialuna, hot chocolate, cookies, and brownie. Based
on the data, 4 items (coffee, bread, tea, cake, and pastry) made up 74%
of the total transactions. As those items are the most purchased
product, I would recommend increasing the availability, variety, and
quality of those said products as it will increase the likelihood of
repeated purchase among the potential customers. We could also expect
our customer to buy at least two products.

Lastly, this data does not show the whole picture. While we could get an
insight which items are most likely to sell among customers, we don’t
have any information regarding the selling price of those said products,
and how much revenue stream we could get from selling those products. It
is possible that some of the least sold items may bring a considerable
revenue if they have a higher price margin even if they are sold less,
however there is a lack of data to support this statement.
