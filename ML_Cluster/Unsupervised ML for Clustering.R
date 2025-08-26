# Import Dataset
data <- data.frame(read.csv("C:/MAIN STORAGE/Downloads/data1.csv"))

# Menghapus Kolom Provinsi
x <- data[,-1]
head(x)

# STANDARISASI VARIABEL 
std_unsu <- scale(x)

# PREPROCESSING DATASET
# Cek Missing Value
colSums(is.na(data))

# Cek Duplikasi
sum(duplicated(data))

# RINGKASAN STATITSTIK
summary(x)

# K-MEANS CLUSTERING
# Menentukan k optimum
library(factoextra)
fviz_nbclust(x, kmeans, method = "wss") 
fviz_nbclust(x, kmeans, method = "silhouette") 
# Dari metode silhouette diperoleh nilai optimum cluster berada pada k=2

# AGGLOMERATIVE CLUSTERING
# PENENTUAN JUMLAH CLUSTER (k) OPTIMUM 
library(factoextra)
fviz_nbclust(x, hcut, method = "silhouette") 
fviz_nbclust(x, hcut, method = "wss")
## Hasil penentuan jumlah cluster optimum (k) dengan metode elbow silhouette didapat sebanyak 2 cluster

# HIERARCICAL CLUSTERING (SINGLE LINKAGE)
d <- dist(x = x, method = "euclidean")

# Membuat Clustering
hc_single <- hclust(d = d, method = "single")
plot(hc_single, hang = -1)

library(cluster)
library(dplyr)

cut_point_1 = cutree(hc_single, k = 2)
databind_1 <- cbind(data, Cluster = cut_point_1)

# Menghitung Silhouette Score
jarak1 = as.matrix(d)
score1 <- mean(silhouette(databind_1$Cluster,dmatrix=jarak1)[,3])
print(paste("silhouette score = ", round(score1, 10)))

# HIERARCICAL CLUSTERING (COMPLETE LINKAGE)
d <- dist(x = x, method = "euclidean")

# Membuat Clustering
hc_complete <- hclust(d = d, method = "complete")
plot(hc_complete, hang = -1)

cut_point_2 = cutree(hc_complete, k = 2)
databind_2 <- cbind(data, Cluster = cut_point_2) 

# Menghitung Silhouette Score
jarak2 = as.matrix(d)
score2 <- mean(silhouette(databind_2$Cluster,dmatrix=jarak2)[,3])
print(paste("silhouette score = ", round(score2, 10)))

# HIERARCICAL CLUSTERING (AVERAGE LINKAGE)
d <- dist(x = x, method = "euclidean")

# Membuat Clustering
hc_average <- hclust(d = d, method = "average")
plot(hc_average, hang = -1)

library(dplyr)

cut_point_3 = cutree(hc_average, k = 2)
databind_3 <- cbind(data, Cluster = cut_point_3) 

# Menghitung Silhouette Score
jarak3 = as.matrix(d)
score3 <- mean(silhouette(databind_3$Cluster,dmatrix=jarak3)[,3])
print(paste("silhouette score = ", round(score3, 10)))

# HIERARCICAL CLUSTERING (WARD METHOD)
d <- dist(x = x, method = "euclidean")

# Membuat Clustering
hc_ward <- hclust(d=d, method = "ward.D2")
plot(hc_ward, hang = -1)

library(dplyr)

cut_point_4 = cutree(hc_ward, k = 2)
databind_4 <- cbind(data, Cluster = cut_point_4) 
databind_4$Cluster

# Menghitung Silhouette Score
jarak4 = as.matrix(d)
score4 <- mean(silhouette(databind_4$Cluster,dmatrix=jarak4)[,3])
print(paste("silhouette score = ", round(score4, 10)))

# HIERARCICAL CLUSTERING (CENTROID METHOD)
d <- dist(x = x, method = "euclidean")

# Membuat Clustering
hc_centroid <- hclust(d=d, method = "centroid")
plot(hc_centroid, hang = -1)

library(dplyr)

cut_point_5 = cutree(hc_centroid, k = 2)
databind_5 <- cbind(data, Cluster = cut_point_5) 
databind_5$Cluster

# Menghitung Silhouette Score
jarak5 = as.matrix(d)
score5 <- mean(silhouette(databind_5$Cluster,dmatrix=jarak5)[,3])
print(paste("silhouette score = ", round(score5, 10)))
