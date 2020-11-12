Fintech Lead Data Analyst SQL Test
================
Calvin de Wilde (Sidjaya)
August 2020

``` r
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(readr)
library(sqldf)
```

    ## Loading required package: gsubfn

    ## Loading required package: proto

    ## Loading required package: RSQLite

``` r
options(scipen=999)
```

## The Tables

Table name: members

Description: Member ID and information. Contains information for both
investor and borrower, which is flagged in the member\_type column

Primary\_key = member\_id

``` r
members
```

    ## # A tibble: 8 x 6
    ##   MEMBER_ID COUNTRY_CODE COMPANY_NAME   FULL_NAME  CITIZENSHIP_COUN~ MEMBER_TYPE
    ##       <dbl> <chr>        <chr>          <chr>      <chr>             <chr>      
    ## 1        52 SG           NULL           Jamie Yeo  SG                investor   
    ## 2        51 SG           NULL           Desmond C~ SG                investor   
    ## 3        23 ID           NULL           Peter Ong  US                investor   
    ## 4        24 ID           NULL           Derrick L~ MY                investor   
    ## 5        63 MY           NULL           Fany Wong  MY                investor   
    ## 6        21 SG           Good books pt~ NULL       NULL              borrower   
    ## 7        74 SG           Apple inc      NULL       NULL              borrower   
    ## 8        93 SG           Fruit pte ltd  NULL       NULL              borrower

Table name: loans

Description: Shows the loans and corresponding borrower (member\_id) who
took the loan. Each loan can only have 1 borrower

primary\_key = loan\_id

foreign\_key = member\_id \>\> members.member\_id

``` r
loans
```

    ## # A tibble: 6 x 2
    ##   LOAN_ID MEMBER_ID
    ##   <chr>   <chr>    
    ## 1 9391    21       
    ## 2 9493    93       
    ## 3 837     17       
    ## 4 5235    42       
    ## 5 123     51       
    ## 6 ...     ...

Table name: investments

Description: Shows loan details along with the investors that invested
in the loan. Summing the amount will give you the total amount raised
for each loan.

foreign\_key = member\_id \>\> members.member\_id

foreign\_key = loan\_id \>\> loans.loan\_id

``` r
investments
```

    ## # A tibble: 8 x 4
    ##   LOAN_ID MEMBER_ID `AMOUNT (USD)` INVESTMENT_DATE    
    ##   <chr>   <chr>              <dbl> <chr>              
    ## 1 9391    52                   500 2018-03-21 06:22:00
    ## 2 9391    51                 10000 2018-03-26 07:59:00
    ## 3 9391    24                   300 2017-08-02 23:59:00
    ## 4 837     23                   200 2017-06-21 06:02:00
    ## 5 837     21                    50 2017-09-18 07:27:00
    ## 6 5235    52                   400 2017-08-29 15:04:00
    ## 7 5235    63                   300 2017-10-05 09:40:00
    ## 8 5235    51                   300 2018-03-08 08:07:00

## SQL Questions

### 1\. For each available loan\_id, get the corresponding company name.

``` r
sqldf(" 
SELECT LOAN_ID, COMPANY_NAME  
FROM loans LEFT JOIN members
ON loans.MEMBER_ID = members.MEMBER_ID")
```

    ##   LOAN_ID       COMPANY_NAME
    ## 1    9391 Good books pte ltd
    ## 2    9493      Fruit pte ltd
    ## 3     837               <NA>
    ## 4    5235               <NA>
    ## 5     123               NULL
    ## 6     ...               <NA>

### 2\. Find out how many borrowers we have per country

``` r
sqldf("
SELECT COUNTRY_CODE, count(MEMBER_TYPE) AS n_count
FROM members
WHERE (MEMBER_TYPE='borrower')
GROUP BY COUNTRY_CODE"
)
```

    ##   COUNTRY_CODE n_count
    ## 1           SG       3

### 3\. Find out how many investors per CITIZENSHIP\_COUNTRY invested in loan 9391

``` r
sqldf("
select 
members.CITIZENSHIP_COUNTRY,
count(members.CITIZENSHIP_COUNTRY) as investors_count
from loans
left join members 
        on members.MEMBER_ID = loans.MEMBER_ID
where loans.LOAN_ID = '9391'
group by members.CITIZENSHIP_COUNTRY
      ")
```

    ##   CITIZENSHIP_COUNTRY investors_count
    ## 1                NULL               1

### 4\. Find out the total amount invested and number of investors who participated in at least loan in each month per year, for loans originating in SG (i.e. loans by SG borrowers)

``` r
sqldf("
with 
  members_filtered AS(
                      select MEMBER_ID,CITIZENSHIP_COUNTRY
                      from members
                      where (members.MEMBER_TYPE = 'borrower' AND members.COUNTRY_CODE = 'SG')
                      )

select 
  strftime('%Y', INVESTMENT_DATE) as year,
  strftime('%m', INVESTMENT_DATE) as month,
  count(members_filtered.MEMBER_ID) as n_investor
from members_filtered
left join  investments 
    on investments.MEMBER_ID = members_filtered.MEMBER_ID
group by year, month
having n_investor >= 1
      ")
```

    ##   year month n_investor
    ## 1 <NA>  <NA>          2
    ## 2 2017    09          1

### 5\. Find out which loan(s) raised more than USD10,000 in 2018.

``` r
sqldf("
      select 
        LOAN_ID,
        sum(investments.'AMOUNT (USD)') as sum_amount
      from investments
      where (INVESTMENT_DATE >= '2018-01-01' and 
              INVESTMENT_DATE <= '2018-12-31')
      group by LOAN_ID
      having sum_amount > 10000
      ")
```

    ##   LOAN_ID sum_amount
    ## 1    9391      10500

alternative version, using between

``` r
sqldf("
      select 
        LOAN_ID,
        sum(investments.'AMOUNT (USD)') as sum_amount
      from investments
      where INVESTMENT_DATE between '2018-01-01' and '2018-12-31'
      group by LOAN_ID
      having sum_amount > 10000
      ")
```

    ##   LOAN_ID sum_amount
    ## 1    9391      10500

### 6\. Find the top 10 largest loans in terms of amount raised in 2017 per country. Do not use UNION as we do not want to query the table multiple times.

``` r
#you can use where between to filter the date too, see solution   #5

sqldf("
with  cte as (select 
                LOAN_ID,
                COUNTRY_CODE,
                sum(investments.'AMOUNT (USD)') as sum_amount
              from investments
              left join members
                on investments.MEMBER_ID=members.MEMBER_ID
              where (INVESTMENT_DATE >= '2017-01-01' AND 
                     INVESTMENT_DATE <= '2017-12-31')
              group by LOAN_ID, COUNTRY_CODE)
              

select 
COUNTRY_CODE,
LOAN_ID,
rank () over (partition by COUNTRY_CODE order by -sum_amount) as loan_ranking_by_country,
sum_amount
from cte
              
      ")
```

    ##   COUNTRY_CODE LOAN_ID loan_ranking_by_country sum_amount
    ## 1           ID    9391                       1        300
    ## 2           ID     837                       2        200
    ## 3           MY    5235                       1        300
    ## 4           SG    5235                       1        400
    ## 5           SG     837                       2         50

## Oral questions

After passing the first round interview I was interviewed by the
company’s lead analyst related to my knowledge on the business acumen.
I thought I was doing a good job until he suddenly asked me to answer
another SQL questions. There were 3 of them, but I could only share two
as the last one was doing a live full join.

### 1.What are the different types of joins?

left, right, inner, full, cross join. I forgot cross join since I never
used it.

### 2\. What’s the difference between RANK, DENSE\_RANK, and ROW\_NUMBER?

this
[answer](https://stackoverflow.com/questions/11183572/whats-the-difference-between-rank-and-dense-rank-functions-in-oracle)
and this
[answer](https://stackoverflow.com/questions/7747327/sql-rank-versus-row-number)
from SO give a good definition:

RANK: RANK gives you the ranking within your ordered partition. Ties are
assigned the same rank, with the next ranking(s) skipped. So, if you
have 3 items at rank 2, the next rank listed would be ranked 5

DENSE\_RANK again gives you the ranking within your ordered partition,
but the ranks are consecutive. No ranks are skipped if there are ranks
with multiple items.

ROW\_NUMBER: Returns a unique number for each row starting with 1. For
rows that have duplicate values,numbers are arbitarily assigned.

### 3\. Which join I need to use when I want to show the sum of transactions between two tables? Let’s say table for transactions in January and February, both of them have unique user\_id and transaction column.

For this situation, use full join, followed by group by and select
statement
