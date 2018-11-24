show databases;
SET GLOBAL sql_mode = 'ONLY_FULL_GROUP_BY';
SELECT @@GLOBAL.sql_mode;
/*Task 1* Understanding the data/

/*A. Describe the data in hand in your own words. (Word Limit is 500)

This is a relational database system which allows the superstores to have details about 
the customer with segment they belong i.e. either the purchase is for a bussiness between corporate, small bussiness etc, or single consumer and demographic details, 
stores product inventory and the range of products the store maintains, 
the sales order with the date and priority of delivery which will help transport the product with an appropriate mode. 
the details of the shipped product alongwith with mode of tranportation , 
and the ledger about the sales, overall profits, discounts given, product base margin and shipping cost for each customer, each segment and each product.
The details various tables in the superstore schema are below:
Cust_Dimen Table -
  1.Customer_Name - text
  2.Province - text
  3.Region - text
  4.Customer_Segment - text
  5.Cust_id - text
Market_Fact Table -
  1.Ord_id - text 
  2.Prod_id - text 
  3.Ship_id - text 
  4.Cust_id - text 
  5.Sales - double 
  6.Discount - double 
  7.Order_Quantity - int(11) 
  8.Profit - double 
  9.Shipping_Cost - double 
 10.Product_Base_Margin - text
Orders_Dimen -
  1.Order_ID - int(11) 
  2.Order_Date - text 
  3.Order_Priority - text 
  4.Ord_id - text
Prod_Dimen -
  1.Product_Category - text 
  2.Product_Sub_Category - text 
  3.Prod_id - text
Shipping_Dimen -
  1.Order_ID int(11) 
  2.Ship_Mode text 
  3.Ship_Date text 
  4.Ship_id text
*/

/*Identify and list the Primary Keys and Foreign Keys for this dataset (Hint: 
If a table don’t have Primary Key or Foreign Key, then specifically mention it in your answer.)

Customer_Dimen = PKey is Cust_ID.
Prod_Dimen = PKey is Prod_ID.
Order_Dimen = PKey is Ord_ID.
Ship_Dimen = PKey is Ship_ID and order_ID is FKey.
Market_Fact = No Primary Key and FKey = Cust_ID,Prod_ID,Ord_ID,Ship_ID.*/

/*Task 2* Basic Analysis/

/* A. Find the total and the average sales (display total_sales and avg_sales)*/
select round(sum(sales),2) as total_sales from superstoresdb.market_fact;
select round(avg(sales),2) as avg_sales from superstoresdb.market_fact;

/* B. Display the number of customers in each region in decreasing order of no_of_customers. 
The result should contain columns Region, no_of_customers*/
select region,count(cust_id) as no_of_customers from superstoresdb.cust_dimen
group by region
order by no_of_customers desc;

/*C. Find the region having maximum customers (display the region name and max(no_of_customers)*/
select region,count(cust_id) as no_of_customers from superstoresdb.cust_dimen
group by region
order by no_of_customers desc limit 1;


/*D. Find the number and id of products sold 
in decreasing order of products sold (display product id, no_of_products sold)*/

select prod_id as Product_id, sum(Order_Quantity) as no_of_products from superstoresdb.market_fact
group by Product_id
order by no_of_products desc;

/*E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and 
the number of tables purchased (display the customer name, no_of_tables purchased)*/

/* I have used 2 way inner join*/
select  customer_name, sum(Order_Quantity) as no_of_tables
from superstoresdb.cust_dimen inner join superstoresdb.market_fact 
on superstoresdb.cust_dimen.cust_id = superstoresdb.market_fact.cust_id
inner join superstoresdb.prod_dimen 
on superstoresdb.prod_dimen.prod_id = superstoresdb.market_fact.Prod_id
where  Product_Sub_Category = 'TABLES' and region = 'ATLANTIC'
group by customer_name;


/*Task 3: Advanced Analysis*/

/*A. Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, profits)?*/

select product_category, round(sum(profit),2) as profit
from superstoresdb.prod_dimen inner join superstoresdb.market_fact 
on superstoresdb.market_fact.prod_id = superstoresdb.prod_dimen.Prod_id
group by product_category
order by profit desc;

/*B. Display the product category, product sub-category and the profit within each sub-category in three columns.*/ 

select product_category, Product_Sub_Category, round(sum(profit),2) as profit
from superstoresdb.market_fact left join  superstoresdb.prod_dimen
on superstoresdb.market_fact.prod_id = superstoresdb.prod_dimen.Prod_id
group by Product_Category,Product_Sub_Category;

/*C. Where is the least profitable product subcategory shipped the most? 
For the least profitable product sub-category, display the region-wise no_of_shipments 
and the profit made in each region in decreasing order of profits 
(i.e. region, no_of_shipments, profit_in_each_region)
o Note: You can hardcode the name of the least profitable product sub-categor*/

/* Hardcoded Tables referring the previous question and used 3 way join*/
select region, count(shipping_dimen.Ship_id) as shipcount, round(sum(profit),2) as profit 
from superstoresdb.market_fact left join  superstoresdb.prod_dimen
on superstoresdb.market_fact.prod_id = superstoresdb.prod_dimen.Prod_id 
left join superstoresdb.cust_dimen on superstoresdb.market_fact.Cust_id = superstoresdb.cust_dimen.Cust_id
left join superstoresdb.shipping_dimen on superstoresdb.market_fact.Ship_id = superstoresdb.shipping_dimen.Ship_id 
where product_sub_category = 'TABLES' 
group by region 
order by profit desc, shipcount;