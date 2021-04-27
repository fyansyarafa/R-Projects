#membaca file transaksi
library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

transaksi@itemInfo
transaksi@itemsetInfo
transaksi@data

# itemFrequency func
itemFrequency(transaksi)
data_item <- itemFrequency(transaksi, type = 'absolute')

# statistik top 3
# sorting
data_item <- sort(x = data_item, decreasing = TRUE)
#slicing
data_item <- data_item[1:3]
#put to dataframe
data_item <- data.frame('Nama.Produk'=names(data_item), 'Jumlah'=data_item,row.names = NULL)

#menulis file statistik top3
write.csv(data_item, file="top3_item_rateil.txt", eol = "\r\n")

#menampilkan grafik frekuensi penjualan
itemFrequencyPlot(transaksi)

#melihat tabel itemset per transaksi
inspect(transaksi)

#menghasilkan rules untuk transaksi
mba <- apriori(transaksi)

#melihat rule yang dihasilkan apriori
inspect(mba)

#memfilter rhs
inspect(subset(mba, rhs %in% 'Sirup'))

#memfileter lhs
inspect(subset(mba, lhs %in% 'Gula'))

#filter lhs dan rhs
inspect(subset(mba, lhs %in% "Pet Food" & rhs %in% "Sirup"))

# Menghasilkan Rules dengan Parameter Support dan Confidence
mba2 <- apriori(transaksi, parameter = list(supp = 0.1, confidence = 0.5))
inspect(mba2)        

#filter lhs rhs (2)
inspect(subset(mba2, lhs %in% "Teh Celup" | rhs %in% "Teh Celup"))

#filter lift
inspect(subset(mba2, (lhs %in% "Teh Celup" | rhs %in% "Teh Celup") & lift>1))

# Rekomendasi - Filter dengan %ain%
inspect(subset(mba2, (lhs %ain% c("Pet Food", "Gula"))))

# visualisasi
library(arulesViz)
plot(subset(mba2, lift>1.11),method = "graph")
