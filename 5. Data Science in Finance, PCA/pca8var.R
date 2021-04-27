#Panggil library openxlsx untuk membaca file data Excel
library(openxlsx)

#Baca data pada sheet "cslarge" dalam file "https://academy.dqlab.id/dataset/dqlab_pcadata.xlsx"
#dan simpan data dengan nama "cslarge_raw"
cslarge_raw <- read.xlsx("https://academy.dqlab.id/dataset/dqlab_pcadata.xlsx", sheet = "cslarge")

#Tampilkan struktur data 
str(cslarge_raw)

#Tampilkan beberapa baris observasi dengan fungsi head()
head(cslarge_raw)

#Tampilkan statistika deskriptif untuk semua variabel dalam data frame.
summary(cslarge_raw)

#Gambarkan distribusi income berdasarkan dependents.
library(ggplot2)
ggplot(
  cslarge_raw,
  aes(
    as.factor(dependents), income
  )
) +
  geom_boxplot() +
  xlab("Dependents") +
  ggtitle("Boxplot Income Berdasarkan Dependents")

library(plotly)
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
plot_ly(
  cslarge_raw,
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

#Gambarkan distribusi debt berdasarkan dependents seperti pada
#contoh boxplot income di atas.
library(ggplot2)
ggplot(
  cslarge_raw,
  aes(
    as.factor(dependents), debt
  )
) +
  geom_boxplot() +
  xlab("Dependents") +
  ggtitle("Boxplot Debt Berdasarkan Dependents")

library(plotly)
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
plot_ly(
  cslarge_raw,
  y = ~debt,
  color = ~as.factor(dependents),
  type = "box"
) %>%
  layout(
    xaxis = list(
      title = "Dependents",
      titlefont = f
    ),
    yaxis = list(
      title = "Debt",
      titlefont = f
    ),
    title = "Boxplot Income Berdasarkan Dependents"
  )


#Pisahkan data untuk traning set dan testing set 
#untuk tiap-tiap risk rating

#Catat indeks/ nomor baris untuk tiap-tiap risk rating
index1 <- which(cslarge_raw$riskrating == 1)
index2 <- which(cslarge_raw$riskrating == 2)

#Lakukan pencatatan indeks untuk risk rating berikutnya
index3 <- which(cslarge_raw$riskrating == 3)
index4 <- which(cslarge_raw$riskrating == 4)
index5 <- which(cslarge_raw$riskrating == 5)

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

#Ulangi langkah sampai dengan train5_index
train3_index <- sample(index3, ntrain3)
train4_index <- sample(index4, ntrain4)
train5_index <- sample(index5, ntrain5)

#menyimpan data ke dalam testing set
test1_index <- setdiff(index1, train1_index)
test2_index <- setdiff(index2, train2_index)

#Ulangi langkah sampai dengan test5_index
test3_index <- setdiff(index3, train3_index)
test4_index <- setdiff(index4, train4_index)
test5_index <- setdiff(index5, train5_index)

#Menggabungkan hasil sampling masing-masing risk rating ke dalam training set
cslarge_train <- do.call(
  "rbind",
  list(
    cslarge_raw[train1_index,],
    cslarge_raw[train2_index,],
    cslarge_raw[train3_index,],
    cslarge_raw[train4_index,],
    cslarge_raw[train5_index,]
  )
)
cstrain <- subset(
  cslarge_train, select = -c(contractcode, riskrating)
)

#Menggabungkan hasil sampling masing-masing risk rating ke dalam testing set
cslarge_test <- do.call(
  "rbind",
  list(
    cslarge_raw[test1_index,],
    cslarge_raw[test2_index,],
    cslarge_raw[test3_index,],
    cslarge_raw[test4_index,],
    cslarge_raw[test5_index,]
  )
)
cstest <- subset(
  cslarge_test,
  select = -c(contractcode, riskrating)
)

#Menghitung korelasi antar variabel dalam data frame
cor(cstrain)

#Menggambarkan matrik korelasi dengan ggcorrplot
library(ggcorrplot)
ggcorrplot(cor(cstrain))

#Lakukan analisa PCA dengan fungsi prcomp() dan
#simpan output ke dalam obyek dengan nama pr.out
pr.out <- prcomp(cstrain, scale = TRUE, center = TRUE)

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
abline(
  h = 1,
  lty = 3,
  col = "red"
)

#Gambarkan biplot dengan menggunakan fungsi biplot()
#[16]
biplot(pr.out, scale = 0) #draw first 2 principal components

#Gambarkan Principal Component dan risk rating dengan menggunakan
#fungsi autoplot() dari package ggfortify.
library(ggfortify)
autoplot(pr.out, data = csdattrain, colour = 'riskrating', 
         loadings = TRUE, loadings.label = TRUE, 
         loadings.label.size = 3, scale = 0)

#Gambarkan Principal Component dan risk rating dengan menggunakan
#fungsi fviz_pca_ind() package factoextra.

library(factoextra)
fviz_pca_ind(pr.out, label="none", habillage=csdattrain$riskrating)
