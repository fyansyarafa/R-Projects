# import dataset
library(openxlsx)
sales <- read.xlsx("dataset sales.xlsx", detectDates = TRUE)

# working with datatypes
sales$Region <- as.factor(sales$Region)
sales$Product <- as.factor(sales$Product)
sales$Customer.Type <- as.factor(sales$Customer.Type)
sales$Date <- format(
  as.Date(sales$Date),
  "%d-%m-%Y"
)


sales$ID <- 1:length(sales$Region)

# splitting date column
tanggal.split <- colsplit(
  summarypro2sum$Date,
  "-",
  c("Day", "Month", "Year")
)
summarypro2sumdatesplitted <- cbind(
  summarypro2sum,
  tanggal.split
)

# checking missing values
library(DataExplorer)
plot_missing(sales)

# descriptive analysis
library(ggplot2)
library(plotly)

# creating summary
colnames(sales)[4] <- "Name"
summarypro2sum <- aggregate(
  cbind(
    Sales
  )~Region+Product+Date+Customer.Type+Name,
  data = sales,
  sum
)

# sales by region
reg <- aggregate(
  cbind(
    Sales
  )~Region,
  data = sales,
  sum
)
#general
plot_ly(
  x = reg$Region,
  y = reg$Sales,
  name = "Jumlah Penjualan per Region",
  type = 'scatter',
  mode = 'lines'
)
# product in region
regpro <- aggregate(
  cbind(
    Sales
  )~Region+Product,
  data = sales,
  sum
)
library(reshape2)
regpropivot <- dcast(
  sales[,c("Region", "Product", "Sales")],
  Region~Product,
  sum
)
plot_ly(
  data = regpropivot,
  x = ~Region,
  y = ~regpropivot$`Head Light`,
  type = 'scatter',
  name = "Head Light",
  mode = 'lines'
)%>%
  add_trace(
    y = ~Helm,
    name = "Helm",
    mode = "lines"
  )%>%
  add_trace(
    y = ~Velg,
    name = "Velg",
    mode = "lines"
  ) %>%
  layout(
    yaxis = list(title="Jumlah Penjualan")
    
  )
# customer type in region
regcuspivot <- dcast(
  sales[,c("Region", "Customer.Type", "Sales")],
  Region~Customer.Type,
  sum
)
plot_ly(
  data = regcuspivot,
  x = ~Region,
  y = ~Bengkel,
  type = 'scatter',
  name = "Head Light",
  mode = 'lines'
)%>%
  add_trace(
    y = ~Enterprise,
    name = "Enterprise",
    mode = "lines"
  )%>%
  add_trace(
    y = ~Retail,
    name = "Retail",
    mode = "lines"
  ) %>%
  layout(
    yaxis = list(title="Jumlah Penjualan")
    
  )

permonth.2018 <- summarypro2sumdatesplitted[which(summarypro2sumdatesplitted$Month == 2 & summarypro2sumdatesplitted$Year == 2018) ,]

ggplotly(ggplot(
  regpropivot,
  aes(
    x = reorder(Region, Helm),
    y = Helm
  )
) +
  geom_bar(
    stat = 'identity',
    aes(fill = Helm)
  ) +
  coord_flip() +
  theme_grey() +
  scale_fill_gradient(name = "Penjualan Helm") +
  labs(
    title = "Penjualan Helm pada Setiap Region",
    y = "Jumlah Penjualan",
    x = "Region" 
  ) +
  geom_hline(yintercept = mean(regpropivot$Helm), size = 1, color = 'blue')+
  geom_hline(yintercept = (95/100)*median(regpropivot$`Head Light`), size = 1, color = 'green')+
  geom_hline(yintercept = (30/100)*median(regpropivot$`Head Light`), size = 1, color = 'red') 
  
) 
  
  
  

# boxplot sales by region
ggplotly(ggplot(
  data = regpro,
  aes(
    x = Product,
    y = Sales
    
  )
) + geom_boxplot() +
  scale_fill_brewer(palette = "Paired") +
  geom_jitter(
    shape = 16,
    position = position_jitter(0.2)
  ) +
  labs(
    title = "Apakah ?",
    y = 'Sales',
    x = 'Product'
  ) 
  
)

main <- summarypro2sumdatesplitted
main$Month <- gsub("1", "Jan", main$Month)
main$Month <- gsub("2", "Feb", main$Month)
main$Month <- gsub("3", "Mar", main$Month)
main$Month <- gsub("4", "Apr", main$Month)
main$Month <- gsub("5", "Mei", main$Month)
main$Month <- gsub("6", "Jun", main$Month)
main$Month <- gsub("7", "Jul", main$Month)
main$Month <- gsub("8", "Aug", main$Month)
main$Month <- gsub("9", "Sep", main$Month)
main$Month <- gsub("10", "Oct", main$Month)
main$Month <- gsub("11", "Nov", main$Month)
main$Month <- gsub("12", "Dec", main$Month)


main <- cbind(main, monthyear)
main <- main[,-c(10,11)]


delapan <- filter(main, as.factor(main$Year)== '2018')
sembilan <- filter(main, as.factor(main$Year)== '2019')


bymo18 <- aggregate(
  cbind(
    Sales
  )~Product+Year,
  data = main,
  mean
)

plot_ly(
  filter(bymo18, bymo18$Year==2019),
  x = ~Product,
  y = ~Sales,
  name = "2019",
  type = "scatter",
  mode = 'lines'
) %>%
  add_trace(
    data = filter(bymo18, bymo18$Year==2018),
    y = filter(bymo18, bymo18$Year==2018)$Sales,
    name = "2018",
    mode ='lines'
  )
