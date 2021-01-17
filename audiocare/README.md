---
title: "SQL Test for Marketing Analyst at an Audiocare company"
author: "Calvin de Wilde (Sidjaya)"
date: "11/22/2020"
output: "github_document"
---
note: Please see the schema [here](https://docs.oracle.com/cd/E11882_01/server.112/e10831/diagrams.htm#COMSC00016). 

Yes, the files are samples from Oracle database. 

```{r setup, include=FALSE}
library(tidyverse)
library(sqldf)
library(lubridate)

employees <- read_delim("https://raw.githubusercontent.com/calvindw/dsjobinterviews/main/audiocare/employees.csv", 
    ";", escape_double = FALSE, col_types = cols(EMPLOYEE_ID = col_character(), 
        HIRE_DATE = col_date(format = "%d/%m/%Y")), 
    trim_ws = TRUE)

employees <- employees %>% mutate(HIRE_DATE = as.character(HIRE_DATE))

jobs <- read_csv("https://raw.githubusercontent.com/calvindw/dsjobinterviews/main/audiocare/jobs.csv")

customers <- read_csv("https://raw.githubusercontent.com/calvindw/dsjobinterviews/main/audiocare/customers.csv", 
    col_types = cols(CREDIT_LIMIT = col_number(), 
        ACCOUNT_MGR_ID = col_character()))

orders <- read_delim("https://raw.githubusercontent.com/calvindw/dsjobinterviews/main/audiocare/orders.csv", 
    ";", escape_double = FALSE, col_types = cols(ORDER_ID = col_character(), 
        ORDER_DATE = col_date(format = "%d/%m/%Y"), 
        CUSTOMER_ID = col_character(), ORDER_STATUS = col_character()), 
    trim_ws = TRUE)

orders <- orders %>%  mutate(ORDER_DATE = as.character(ORDER_DATE)) 

product_information <- read_csv("https://raw.githubusercontent.com/calvindw/dsjobinterviews/main/audiocare/product_information.csv", 
    col_types = cols(PRODUCT_ID = col_character(), 
        CATEGORY_ID = col_character(), WEIGHT_CLASS = col_character(), 
        SUPPLIER_ID = col_character(), LIST_PRICE = col_number(), 
        MIN_PRICE = col_number()))

departments <- read_csv("https://raw.githubusercontent.com/calvindw/dsjobinterviews/main/audiocare/departments.csv")

order_items <- read_csv("https://raw.githubusercontent.com/calvindw/dsjobinterviews/main/audiocare/order_items.csv", 
     col_types = cols(ORDER_ID = col_character(), 
         LINE_ITEM_ID = col_character(), PRODUCT_ID = col_character(), 
         UNIT_PRICE = col_number(), QUANTITY = col_number()))


```

### 1.	The company wants to select the top 3 employees for performance, in each department, in Q1 of 2019. (Assume date columns are stored as standard timestamps)

a.	Write a SQL query based on your metric for employee performance to determine the conditions above.

this code consists of two ctes:

- cte_sales will filter the date and group the data based on the employees

- cte_sales will be joined with the table from the marketing department to know the group the employees belong in the department

- the last select command will create a rank based on the department, total sales

```{r}
sqldf("

with 

cte_sales as

  (select 
    SALES_REP_ID as sales_rep_id, 
    sum(ORDER_TOTAL) as total_sales 
  from orders   
  where ORDER_DATE between '2019-01-01' and '2019-03-31'
  group by SALES_REP_ID
),

cte_aggregated_sales_per_department as
(select 

DEPARTMENT_ID as department_id,
sales_rep_id,
total_sales,
FIRST_NAME as first_name,
LAST_NAME as last_name
from cte_sales
left join employees 
  on sales_rep_id = EMPLOYEE_ID
)

select 
DEPARTMENT_NAME,
sales_rep_id,
total_sales,
first_name,
last_name,
rank () over (partition by cte_aggregated_sales_per_department.department_id order by total_sales desc) as sales_ranking

from cte_aggregated_sales_per_department
left join departments on cte_aggregated_sales_per_department.department_id = departments.DEPARTMENT_ID
      ")


```


### 2.	We are interested in whether employees with a longer tenure (time at the company) are more likely to have more sales.

a.	Write a query that computes the Year over Year percentage change in employee sales by tenure for Q1. 

This code consists of three ctes:

- cte_tenure will calculate employees tenure and adding an extra column year_tenure which will be used as categorical variable in aggregateion

- cte_sales_and_tenure will aggregate total sales by year and aggregate the sales by tenure for Q1

- cte_yoy will use LAG function to add a column of previous year sales based on employee tenure, and there will be an year_over_year_change which calculated the difference between the current year and previous year sales 

```{r}


sqldf("
with 

cte_tenure as (select
EMPLOYEE_ID,
  strftime('2019-09-30')-
  strftime(HIRE_DATE)  as year_tenure

from employees
left join orders
on EMPLOYEE_ID = SALES_REP_ID),


cte_sales_and_tenure as(

select 
sum(ORDER_TOTAL) as total_sales,
strftime('%Y',ORDER_DATE) as year,
SALES_REP_ID,
year_tenure
from orders
left join cte_tenure on
EMPLOYEE_ID = SALES_REP_ID
group by year,year_tenure),


cte_yoy as
(select year,
year_tenure,
total_sales,
lag (total_sales, 1,0) over (partition by SALES_REP_ID order by year) as previous,
(total_sales - lag (total_sales, 1,0) over (partition by SALES_REP_ID order by year))/lag (total_sales, 1,0) over (partition by SALES_REP_ID order by year) as year_over_year_change
from cte_sales_and_tenure)

select *
from cte_yoy



"
)


```


### 3.	Write a query that computes the average list price for all orders, as well as all orders that contain any of the following words (Book, Table, Candy). 


This query will produce the average list price of all order

```{r}

sqldf("
      select 
      avg(LIST_PRICE) as average_list_price
      from orders
      left join order_items
       on orders.ORDER_ID = order_items.ORDER_ID
      left join product_information
        on order_items.PRODUCT_ID = product_information.PRODUCT_ID
        
        
      ")
```

This query will produce all orders that contain book, table, candy

```{r}

sqldf("
      select orders.ORDER_ID,
      PRODUCT_NAME,
      PRODUCT_DESCRIPTION
      from orders
      left join order_items
       on orders.ORDER_ID = order_items.ORDER_ID
      left join product_information
        on order_items.PRODUCT_ID = product_information.PRODUCT_ID
      where (PRODUCT_DESCRIPTION LIKE '%Book%' or
        PRODUCT_NAME LIKE '%Table%' or 
        PRODUCT_NAME LIKE '%Candy%')
      ")

```

a.	Are there any other metrics you want to include to find frequency information about the products?

I would create a query using group by and aggregate the total sales of products within the group of year, month,customer's income bracket, order mode, and product name to determine what are the top sales items of each customer based on income level. This can be used to observe the pattern of the customer's repeat purchase and the frequency of their purchase within the observed period (year/month).

The last column "order mode" can be used to determine what is the most preferable way a customer interact with the company. Introverted people are likely to prefer online purchase and while extroverted people are more likely to prefer direct sales. By understanding the customer's psychology, the company can use the appropriate channel to approach the potential customer.


```{r}

sqldf("
      
      
      select 
      INCOME_LEVEL,
      customers.CUSTOMER_ID,
      count(orders.ORDER_ID) order_count,
      product_information.PRODUCT_NAME,
      strftime('%Y',ORDER_DATE) as year,
      strftime('%m',ORDER_DATE) as month,
      ORDER_MODE
      from orders
      left join order_items
       on orders.ORDER_ID = order_items.ORDER_ID
      left join product_information
        on order_items.PRODUCT_ID = product_information.PRODUCT_ID
      left join customers on
      orders.CUSTOMER_ID = customers.CUSTOMER_ID
      group by INCOME_LEVEL, customers.CUSTOMER_ID,product_information.PRODUCT_NAME,ORDER_MODE, year,month

      ")




```

b.	The company wants to drive sales of multiple items in the same order. How would you use this data to help the company achieve that goal?

By using that data above, we can identify the customers income bracket, their frequency of purchase and whether the frequency of their purchase is higher or lower than the average. This data can be used to determine which customers are likely to follow up the sales leads and more profitable. 

For example, customers with higher-than-average purchase would likely to be a repeat customer and it may be more reasonable to increase sales leads to customers who purchased products lower than average to increase the company's customer portfolio. Through a proper aggregation and analysis, we could determine which products the customers likely to purchase or less likely to purchase. In a marketing secene, it is quite common that a customer will repeat a purchased item of the same caetgory when the company produces a new variation. By using this data, we can understand what kind products are the customers most likely to be interested with.


### 4. The company wants to review how well account managers are interacting with their customers.  How would you extend the above schema to look at customer reviews after a call to their account manager? Please include table description and any necessary keys.

To answer this question I will need to look the total number of sales frequency made by an account manager By getting this data we could understand how frequent an account manager closing a sales per month and how it compares with the average sales number per year. We can review how well account managers are interacting with their customers based on the difference of their sales count versus the sales average per year. 

To accomplish this I will use the tables of
- Orders to get the sales data. This table will be joined with customers table using CUSTOMER_ID as the key. 

- using the table above, we could aggregate the information we want: the year, the month,  the count of total sales frequency made by the account manager per month and the average sales by year. 

- the diff_avg contains a column of the average sales substracted by the total sales. If the number is negative it means the account manager is lacking behind the average sales per year.


```{r}

sqldf("

with cte_customer_acct as
(
select  ORDER_ID,
strftime('%Y',ORDER_DATE) as year,
      strftime('%m',ORDER_DATE) as month,
ORDER_DATE,
orders.CUSTOMER_ID,
ACCOUNT_MGR_ID
from orders      
left join customers
on orders.CUSTOMER_ID = customers.CUSTOMER_ID
      ),
      
cte_agg as (select 
year,
month,
count(ORDER_ID) as total_orders,
ACCOUNT_MGR_ID
from cte_customer_acct
group by year,month,ACCOUNT_MGR_ID
)

select 
year,
month,
avg(total_orders) over (partition by year) as averge_order_count,
total_orders,
(avg(total_orders) over (partition by year) - total_orders) as diff_avg,
ACCOUNT_MGR_ID

from cte_agg
      ")


```

