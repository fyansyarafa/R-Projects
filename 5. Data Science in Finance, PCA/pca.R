library(openxlsx)
df <- read.xlsx("https://academy.dqlab.id/dataset/dqlab_pcadata.xlsx", sheet="3varb")

#standarisasi variabel (centering dan scaling)
df <- scale(
  df,
  center = TRUE,
  scale = TRUE
)
head(df, 3)

# menghitung korelasi variabel
cormatdf <- data.frame(df)
cormat <- cor(df)
cormat
cormat2 <- as.matrix(cormat)
library(plotly)
b <- plot_ly(
  x = names(cormatdf),
  y = names(cormatdf),
  z = round(cormat2,2),
  type = 'heatmap'
  #colors = 'Greys'
)
b

# Menghitung Nilai Eigen dan Vektor Eigen
eig <- eigen(cormat)
eig

# Memilih Banyaknya Principal Component
round(
  eig$values/ncol(df),
  3
)
round(cumsum(
  eig$values/ncol(df)
),3)

pr.out <- prcomp(
  df,
  scale. = TRUE,
  center = TRUE
)
pr.out
summary(pr.out)

library(factoextra)
fviz_eig(pr.out, addlabels = TRUE)

screeplot(
  pr.out,
  type = 'line'
)
abline(
  h = 1,
  lty = 3,
  col = 'red'
)

# Visualisasi dengan Biplot
pr.out$rotation
biplot(
  pr.out, scale = 0
)

# Menghitung Skor Baru
df_new <- df %*% pr.out$rotation
df_new[1:6, 1:2]


