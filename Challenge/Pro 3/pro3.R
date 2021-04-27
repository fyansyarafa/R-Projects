library(ggplot2)
library(reshape2)
library(plotly)
library(dplyr)
library(tidyr)
library(gtable)
library(gridExtra)
library(gt)
library(numform)

df <- read.csv(file = "P1-OfficeSupplies.csv")
View(df)

TotalSales <- df$Units*df$Unit.Price
df <-  cbind(
  df,
  TotalSales
)

byreg <- aggregate(
  cbind(
    TotalSales
  )~Region+Rep,
  data = df,
  sum
)

salesdollar2 <- f_denom(byreg$TotalSales, prefix = "$",relative  = 2)
byreg <- cbind(byreg, salesdollar2)
byreg <- byreg[,-c(4,5)]
# df by regions 
regcentral <- filter(
  byreg,
  Region == 'Central'
)
regcentral <- regcentral[order(regcentral$TotalSales, decreasing = TRUE),]

regeast <- filter(
  byreg,
  Region == 'East'
)
regeast <- regeast[order(
  regeast$TotalSales, 
  decreasing = TRUE
),]

regwest <- filter(
  byreg,
  Region == 'West'
)
regwest <- regwest[order(
  regwest$TotalSales, 
  decreasing = TRUE
),]


# plotting the regions
regcentralplot <- ggplot(
  data = regcentral,
  aes(
    x = reorder(regcentral$Rep, -regcentral$TotalSales),
    y = regcentral$TotalSales
  )
) +
  geom_bar(stat = 'identity', position = 'identity', fill = 'steelblue') +
  labs(
    x = "Rep",
    y = "Total Sales",
    title = "Rep Sales in Central Region"
  ) + geom_text(aes(label=TotalSales), position=position_dodge(width=0.9), vjust = -0.4, size = 3) +
  theme_minimal()
regcentralplot 

regwestplot <- ggplot(
  data = regwest,
  aes(
    x = reorder(regwest$Rep, -regwest$TotalSales),
    y = regwest$TotalSales
  )
) +
  geom_bar(stat = 'identity', position = 'identity', fill = 'steelblue') +
  labs(
    x = "Rep",
    y = "Total Sales",
    title = "Rep Sales in West Region"
  ) + geom_text(aes(label=TotalSales), position=position_dodge(width=0.9), vjust = -0.4, size = 3) +
  theme_minimal()
regwestplot 

regeastplot <- ggplot(
  data = regeast,
  aes(
    x = reorder(regeast$Rep, -regeast$TotalSales),
    y = regeast$TotalSales
  )
) +
  geom_bar(stat = 'identity', position = 'identity', fill = 'steelblue') +
  labs(
    x = "Rep",
    y = "Total Sales",
    title = "Rep Sales in East Region"
  ) + geom_text(aes(label=TotalSales), position=position_dodge(width=0.9), vjust = -0.4, size = 3) +
  theme_minimal()
regeastplot 

regplot <- ggplot(
  data = byreg,
  aes(
    x = reorder(byreg$Rep, -byreg$TotalSales),
    y = byreg$TotalSales,
    fill = Region
  )
) +
  geom_bar(stat = 'identity') +
  facet_grid(~Region,  scales = 'free', space = 'free') +
  theme_minimal() +
  geom_text(
    aes(
      label = salesdollar2
    ),
    position = position_dodge(width = 0.9),
    vjust = -0.4,
    size = 3
  ) +
  labs(
    x = '',
    y = 'Total Sales ($)',
    title = 'Rep Sales by Regions'
  )
regplot


