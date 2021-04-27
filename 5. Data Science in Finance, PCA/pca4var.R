#Panggil library openxlsx untuk membaca file data Excel
library(openxlsx)

#Baca data pada sheet "csdata" dalam file "https://academy.dqlab.id/dataset/dqlab_pcadata.xlsx"
#dan simpan data dengan nama "csdat_raw"
csdat_raw <- read.xlsx("https://academy.dqlab.id/dataset/dqlab_pcadata.xlsx", sheet = "csdata")

#Tampilkan struktur data 
str(csdat_raw)

#Tampilkan beberapa baris observasi dengan fungsi head()
head(csdat_raw)

#Tampilkan statistika deskriptif untuk semua variabel dalam data dengan fungsi summary()
csdat_raw <- subset(
  csdat_raw,
  select = -contractcode
)
summary(csdat_raw)

#Gambarkan distribusi Income berdasarkan Dependents
library(ggplot2)
ggplot(
  csdat_raw,
  aes(
    as.factor(dependents),
    income
  )
) +
  geom_boxplot() +
  xlab(
    "Dependents"
  ) +
  ggtitle(
    "Boxplot Income Berdasarkan Dependents"
  )

library(plotly)
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
plot_ly(
  csdat_raw,
  y = ~income,
  color = ~as.factor(dependents),
  type = "box"
) %>%
  layout(
    xaxis = list(
      title = "Dependents",
      titlefont = f
    ),
    yaxis = list(
      title = "Income",
      titlefont = f
    ),
    title = "Boxplot Income Berdasarkan Dependents"
  )

#Pisahkan data untuk traning set dan testing set 
#untuk tiap-tiap risk rating

#Catat indeks/ nomor baris untuk tiap-tiap risk rating
index1 <- which(csdat_raw$riskrating == 1)
index2 <- which(csdat_raw$riskrating == 2)
index3 <- which(csdat_raw$riskrating == 3)
index4 <- which(csdat_raw$riskrating == 4)
index5 <- which(csdat_raw$riskrating == 5)

#Lakukan pencatatan indeks untuk risk rating berikutnya

#80% data akan digunakan sebagai traning set.
#Ulangi langkah sampai dengan index5
ntrain1 <- round(0.8 * length(index1))
ntrain2 <- round(0.8 * length(index2))
ntrain3 <- round(0.8 * length(index3))
ntrain4 <- round(0.8 * length(index4))
ntrain5 <- round(0.8 * length(index5))

#set seed agar sampling ini bisa direproduksi
set.seed(100)


#sampling data masing-masing rating untuk training set
train1_index <- sample(index1, ntrain1)
train2_index <- sample(index2, ntrain2)
train3_index <- sample(index3, ntrain3)
train4_index <- sample(index4, ntrain4)
train5_index <- sample(index5, ntrain5)

#menyimpan data ke dalam testing set
test1_index <- setdiff(index1, train1_index)
test2_index <- setdiff(index2, train2_index)
test3_index <- setdiff(index3, train3_index)
test4_index <- setdiff(index4, train4_index)
test5_index <- setdiff(index5, train5_index)

#Menggabungkan hasil sampling masing-masing risk rating ke dalam training set
csdattrain <- do.call(
  "rbind",
  list(
    csdat_raw[train1_index,],
    csdat_raw[train2_index,],
    csdat_raw[train3_index,],
    csdat_raw[train4_index,],
    csdat_raw[train5_index,]
  )
)
cstrain <- subset(
  csdattrain, 
  select = -riskrating
)

#Menggabungkan hasil sampling masing-masing risk rating ke dalam testing set
csdattest <- do.call(
  "rbind",
  list(
    csdat_raw[test1_index,],
    csdat_raw[test2_index,],
    csdat_raw[test3_index,],
    csdat_raw[test4_index,],
    csdat_raw[test5_index,]
  )
)
cstest <- subset(csdattest, select = -riskrating)

#Menghitung korelasi antar variabel dalam data frame
dfcor <- data.frame(cor(cstrain))

library(plotly)
plot_ly(
  x = names(dfcor),
  y = names(dfcor),
  z = as.matrix(cor(cstrain)),
  type = 'heatmap',
  colors = colorRamp(c("black", "white"))
  #colors = 'Greys'
)

#Lakukan analisa PCA dengan fungsi prcomp() dan
#simpan output ke dalam obyek dengan nama pr.out
pr.out <- prcomp(
  cstrain,
  scale. = TRUE,
  center = TRUE
)

#Tampilkan output PCA dengan memanggil obyek pr.out
pr.out

#Tampilkan summary dari output PCA
summary(pr.out)

#Gambarkan Screeplot dengan menggunakan fungsi screeplot()
screeplot(
  pr.out,
  type = "line",
  ylim = c(0,2)
)

#Tambahkan garis horizontal sebagai panduan untuk menggunakan kriteria Kaiser
abline(h = 1, lty = 3, col = "red")

#Gambarkan biplot dengan menggunakan fungsi biplot()
biplot(
  pr.out, 
  scale = 0
)

#Fungsi lain yang dapat digunakan adalah autoplot dari ggfortify atau fviz_pca_biplot dari factoextra.
library(ggfortify)
autoplot(
  pr.out, 
  data = cstrain,
  loadings =TRUE,
  loadings.label = TRUE,
  scale = 0
)

library(factoextra)
fviz_pca_biplot(
  pr.out,
  label = "var",
  habillage = csdattrain$riskrating
)











