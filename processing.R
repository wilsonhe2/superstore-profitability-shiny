library(tidyverse)

sales = read.csv("data/superstore_sales.csv")

sales = read.csv("data/superstore_sales.csv") |> 
  mutate("Year" = year(mdy(Order.Date)),
         "Profit_Margin" = Profit / Sales) |> 
  select(Year, State, "Subcategory" = Sub.Category, Sales, Quantity, Discount, Profit, Profit_Margin)

write_csv(sales, file = "data/sales.csv")