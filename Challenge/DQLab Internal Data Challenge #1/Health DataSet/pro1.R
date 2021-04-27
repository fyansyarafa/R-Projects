#### WORKING WITH HDI ####
hdi <- read.csv(
  "Human Development Index (HDI).csv",
  header = TRUE,
  stringsAsFactors = FALSE
  
)
colnames(hdi) <- hdi[1,]
hdi <- hdi[-1,]

library(DataExplorer)
plot_missing(hditemp)

hdi[,3] <- as.double(hdi[,3])

#creating temp var for double values
hditemp <- hdi[,3:58]
hditemp[,2] <- as.double(hditemp[,2])


for (x in c(4:56)) {
  hditemp[,x] <- as.double(hditemp[,x])
}

#drop empty columns
hditemp <- drop_columns(hditemp, seq(2,56, by = 2))

#combining columns to one dataframe
hdi <- drop_columns(hdi, seq(3,58))
hdi <- cbind(hdi, hditemp)

hdi$`HDI Rank` <- as.numeric(hdi$`HDI Rank`)

colnames(hdi)[1] <- "HDI Rank 2017"
#### WORKING WITH "Disease case" ####
malaria <- read.csv(
  "Malaria incidence (per 1,000 people at risk).csv",
  skip = 1,
  stringsAsFactors = FALSE
)

# datatype each columns
































