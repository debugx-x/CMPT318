# import libraries
library(RSQLite)
library(dplyr)
library(lubridate)

# create database connection
db = dbConnect(SQLite(), dbname = "xcoretail2.sqlite")

# read sales table
data_sales = dbReadTable(db, "sales")

# read prices table
data_prices = dbReadTable(db, "prices")

# get stockCode and quantity from sales table
data_pop = data_sales %>% select(stockCode, quantity)

# group by stockCode and sum quantity
data_pop = data_pop %>% group_by(stockCode) %>% summarise(quantity = sum(quantity))

# get top 100 popular items
data_pop = data_pop %>% arrange(desc(quantity)) %>% head(100)

#get a list of yyyy-mm dates from the sales table
data_dates = data_sales %>% select(invoiceDate) %>% distinct() %>% mutate(date = invoiceDate) %>% select(date) %>% arrange(date)

# format dates to yyyy-mm
data_dates$date = format(data_dates$date, "%Y-%m")

# get unique dates
data_dates = data_dates %>% distinct()

# remove top 3 row from dates
data_dates = data_dates[-c(1:3),]



# get list of unique StockCodes from price table
data_stock = data_prices %>% select(stockCode) %>% distinct()

# create vector elin and esop to store difference 
elin = c()
esop = c()

# loop through each stockCode in data_pop and get predicted sales for each month
for (i in 1:nrow(data_pop)) {
  # get stockCode
  StockCode = data_pop$StockCode[i]
  
  # loop thru dates
  for (j in 1:nrow(data_dates)) {
    tMonth = data_dates[j]
    
    # get LIN prediction for that month
    pred_lin <- my.LIN(StockCode, tMonth)

    # get SOP prediction for that month
    pred_sop <- my.SOP(StockCode, tMonth)
    
    # get true quantity sold of the stockCode and month
    true <- data_sales %>% filter(StockCode == StockCode & invoiceDate >= tMonth & invoiceDate < tMonth + months(1)) %>% summarise(quantity = sum(quantity))

    # get absolute difference between LIN prediction and true
    diff_lin <- abs(pred_lin - true)

    # get difference between LIN prediction and true
    diff_sop <- abs(pred_sop - true)

    # append differences
    elin = c(elin, diff)
    esop = c(esop, diff)
  }
}

#loop thru list of dates


