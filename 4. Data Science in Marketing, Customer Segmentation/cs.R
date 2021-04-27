pelanggan <- read.csv("https://academy.dqlab.id/dataset/customer_segments.txt", sep="\t")
head(pelanggan[c("Jenis.Kelamin", "Umur", "Profesi", "Tipe.Residen")])

field_yang_digunakan <- c("Jenis.Kelamin", "Umur", "Profesi")
pelanggan[field_yang_digunakan]

# konversi kategori ke numerik
pelanggan_matrix <- data.matrix(pelanggan[c("Jenis.Kelamin", "Profesi", "Tipe.Residen")])

# menggabungkan df
pelanggan <- data.frame(pelanggan, pelanggan_matrix)

#normalisasi nilai belanja
pelanggan$NilaiBelanjaSetahun <- pelanggan$NilaiBelanjaSetahun/1000000

#membuat data master
Profesi <- unique(pelanggan[c("Profesi", "Profesi.1")])
Jenis.Kelamin <- unique(pelanggan[c("Jenis.Kelamin", "Jenis.Kelamin.1")])
Tipe.Residen <- unique(pelanggan[c("Tipe.Residen", "Tipe.Residen.1")])

field_yang_digunakan = c("Jenis.Kelamin.1", 
                         "Umur", 
                         "Profesi.1", 
                         "Tipe.Residen.1",
                         "NilaiBelanjaSetahun")

# bagian kmeans
set.seed(100)

# fungsi kmean 5 clusters dengan 25 skenario random
segmentasi <- kmeans(x = pelanggan[field_yang_digunakan], centers = 5, nstart = 25)
segmentasi

# #Penggabungan hasil cluster
pelanggan$cluster <- segmentasi$cluster
str(pelanggan)

# melihat urutan masing2 cluster
cluster2 <- which(pelanggan$cluster==2)
length(cluster2)

# Melihat Data pada Cluster ke-N
c1 <- pelanggan[which(pelanggan$cluster == 1),]
c2 <- pelanggan[which(pelanggan$cluster == 2),]
c3 <- pelanggan[which(pelanggan$cluster == 3),]
c4 <- pelanggan[which(pelanggan$cluster == 4),]
c5 <- pelanggan[which(pelanggan$cluster == 5),]

library(ggplot2)
library(plotly)
p <- ggplot(c2, aes(x = Profesi, fill = Jenis.Kelamin)) + geom_bar(position = "dodge")
p

library(dplyr)
p2 <- c2[c("Profesi", "Jenis.Kelamin")] %>% count(Profesi, Jenis.Kelamin) %>%
  plot_ly(x = ~Profesi, y = ~n, color=~Jenis.Kelamin)
p2

#pie chart grouped by profesi
mtcars$manuf <- sapply(strsplit(rownames(mtcars), " "), "[[", 1)

p3 <- c2 %>%
  group_by(Profesi) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~Profesi, values = ~count) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Pie chart berdasarkan profesi",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p3

w <- table(c2$Jenis.Kelamin)
w
t <- as.data.frame(w)
t
pecentage <- t$Freq/length(c2)
pecentage

t
bp <- ggplot(t, aes(x = "", y = persentasi, fill=Jenis.Kelamin)) 
bp <- bp + geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start = 0)


p6 <- plot_ly(t, labels = ~Jenis.Kelamin, values = ~Freq, type = 'pie') %>%
  layout(title = 'Jenis Kelamin',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p6


segmentasi$centers

#Membandingkan dengan 2 cluster kmeans, masing-masing 2 dan 5 
#pake elbow method untuk menemukan cluster terbaik 
set.seed(100)
kmeans(x = pelanggan[field_yang_digunakan], centers = 2, nstart = 25)
set.seed(100)
kmeans(x = pelanggan[field_yang_digunakan], centers = 5, nstart = 25)

# Simulasi Jumlah Cluster dan SS
set.seed(100)
sse <- sapply(1:10,
              function(param_k){
                kmeans(pelanggan[field_yang_digunakan],
                       param_k,
                       nstart = 25)$tot.withinss
              })

# Grafik Elbow Effect
jumlah_cluster_max <- 10
ssdata <- data.frame(cluster=c(1:jumlah_cluster_max), sse)
ggplot(ssdata, aes(x = cluster, y = sse)) + 
  geom_line(color="red") +
  geom_point() +
  ylab("Within Cluster Sum of Errors") +
  xlab("Jumlah Cluster") +
  geom_text(aes(label=format(round(sse, 2),
                             nsmall = 2)),
            hjust = -0.2,
            vjust = 0.5) +
  scale_x_discrete(limits = c(1:jumlah_cluster_max))
# using plotly
ssdata
best <- ssdata[which(ssdata$cluster == 3),]

a <- list(
  x = best$cluster,
  y = best$sse,
  text = 'Best k',
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 20,
  ay = 40
)
plot_ly(ssdata, x = ~cluster,
        y = sse,
        type = 'scatter',
        mode = 'lines',
        marker = list(
          color = 'rgb(17, 157, 255)',
          size = 10,
          line = list(
            color = 'rgb(231, 99, 250)',
            width = 2
          )
        )
      ) %>%
  add_trace(
    mode = 'text',
    text = ~round(sse, 2), 
    textposition = 'top right',
    showlegend = FALSE
  ) %>%
  layout(annotations = a)
  
plot_ly(
  data = pelanggan, 
  x = ~Umur,
  y = ~cluster,
  color = ~Tipe.Residen,
  colors = "Set1"
)

# Menamakan Segmen
Segmen.Pelanggan <- data.frame(
  cluster = c(
    1,
    2,
    3,
    4,
    5
  ),
  Nama.Segmen = c("Diamond Senior Member",
                  "Gold Young Professional",
                  "Silver Youth Gals",
                  "Diamond Professional",
                  "Silver Mid Professional")
)


# Menggabungkan Referensi
Identitas.Cluster <- list(
  Profesi = Profesi,
  Jenis.Kelamin = Jenis.Kelamin,
  Tipe.Residen = Tipe.Residen,
  Segmentasi = segmentasi,
  Segmen.Pelanggan = Segmen.Pelanggan,
  field_yang_digunakan = field_yang_digunakan
)

# Menyimpan Objek dalam Bentuk File
saveRDS(Identitas.Cluster,"cluster2.rds")


# Mengoperasionalkan Model K-Means
#membuat data baru
databaru <- data.frame(Customer_ID="CUST-100", 
                       Nama.Pelanggan="Rudi Wilamar",
                       Umur=20,
                       Jenis.Kelamin="Wanita",
                       Profesi="Pelajar",
                       Tipe.Residen="Cluster",
                       NilaiBelanjaSetahun=3.5)
databaru

# Memuat Objek Clustering dari File
Identitas.Cluster <- readRDS(file = "cluster.rds")
Identitas.Cluster

#Merge dengan Data Referensi
databaru <- merge(
  databaru,
  Identitas.Cluster$Profesi
)
databaru <- merge(
  databaru,
  Identitas.Cluster$Jenis.Kelamin
)
databaru <- merge(
  databaru,
  Identitas.Cluster$Tipe.Residen
)



# Menentukan Cluster
databaru$Umur <- 81
which.min(
  sapply(
    1:5,
    function (x) sum(
      (databaru[Identitas.Cluster$field_yang_digunakan]-Identitas.Cluster$Segmentasi$centers[x,])^2
    )
  )
)


Identitas.Cluster$Segmen.Pelanggan[
  which.min(
    sapply(
      1:5,
      function (x) sum(
        (databaru[Identitas.Cluster$field_yang_digunakan]-Identitas.Cluster$Segmentasi$centers[x,])^2
      )
    )
  )
  ,  
  ]
