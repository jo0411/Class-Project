install.packages("ISLR")
library(ISLR)
data(College)
View(College)

library(clValid)
library(plotrix)

# private column 제외하기
college_private <- College[,1]
college_x <- College[,-1]

# scale 하기
college_x_scaled <- scale(college_x, center = TRUE, scale = TRUE)

# k-means clustering - 최적 군집 개수 확인
college_clValid <- clValid(college_x_scaled, 2:10, clMethods = "kmeans", 
                        validation = c("internal", "stability"))

summary(college_clValid)

# k=3일 때 
college_kmc <- kmeans(college_x_scaled,3)
str(college_kmc)

# center 중심값, size 멤버 개수 , cluster 속한 군집
college_kmc$centers
college_kmc$size
college_kmc$cluster

# k=10일 때
college_kmc <- kmeans(college_x_scaled,10)
str(college_kmc)

# center 중심값, size 멤버 개수 , cluster 속한 군집
college_kmc$centers
college_kmc$size
college_kmc$cluster

# cluster 비교하기
cluster_kmc <- data.frame(college_x_scaled, clusterID = as.factor(college_kmc$cluster))
kmc_summary <- data.frame()

for (i in 1:(ncol(cluster_kmc)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(cluster_kmc[,i], cluster_kmc$clusterID, mean))
}

colnames(kmc_summary) <- paste("cluster", c(1:3))
rownames(kmc_summary) <- colnames(college_x)
kmc_summary

# radar chart
par(mfrow = c(1,3))
for (i in 1:3){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(kmc_summary[,i], labels = rownames(kmc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)
}
dev.off()

# t-test 검정  cluster 1, cluster 2 비교
kmc_cluster1 <- college_x[college_kmc$cluster == 2,]
kmc_cluster2 <- college_x[college_kmc$cluster == 3,]

kmc_t_result <- data.frame()

for (i in 1:17){
  
  kmc_t_result[i,1] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "two.sided")$p.value
  
  kmc_t_result[i,2] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "greater")$p.value
  
  kmc_t_result[i,3] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "less")$p.value
}

kmc_t_result

#결과물 시각화
install.packages("fpc")
library(fpc)
plotcluster(college_x_scaled, college_kmc$cluster, color=TRUE, shade=TRUE)
plot(college_x_scaled, pch= college_kmc$cluster, col=college_kmc$cluster)

# ----------- Hierarchical clustering

# clValid 이용해서 hierarchical clustering 
college_clValid <- clValid(college_x_scaled, 2:10, clMethods = "hierarchical", 
                           validation = c("internal", "stability"))
summary(college_clValid)

# spearman 상관계수

cor_Mat <- cor(t(college_x_scaled), method = "spearman")
dist_college <- as.dist(1-cor_Mat)

# hierarchical clustering 수행
hr_complete <- hclust(dist_college, method = "complete", members=NULL)
hr_wardD <- hclust(dist_college, method = "ward.D", members=NULL)
hr_single <- hclust(dist_college, method = "single", members=NULL)
hr_mcquitty <- hclust(dist_college, method = "mcquitty", members=NULL)
hr_centroid <- hclust(dist_college, method = "centroid", members=NULL)
hr_median <- hclust(dist_college, method = "median", members=NULL)
?hclust

# dendrogram 그리기
plot(hr)
plot(hr, hang = -1)
plot(as.dendrogram(hr_median), edgePar=list(col=3, lwd=1.5))

# 10개 군집 찾기
mycl <- cutree(hr_complete, k=10)
mycl

plot(as.dendrogram(hr_median), edgePar=list(col=3, lwd=2))
rect.hclust(hr_complete, k=10, border="red")



# Compare each cluster for HC
college_hc <- data.frame(college_x_scaled, 
                       clusterID = as.factor(mycl))
hc_summary <- data.frame()

for (i in 1:(ncol(college_hc)-1)){
  hc_summary = rbind(hc_summary, 
                     tapply(college_hc[,i], college_hc$clusterID, mean))
}

colnames(hc_summary) <- paste("cluster", c(1:10))
rownames(hc_summary) <- colnames(college_x)
hc_summary

# Radar chart
par(mfrow = c(2,5))
for (i in 1:10){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(hc_summary[,i], labels = rownames(hc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)
}
dev.off()

# 유사한 cluster 2 & 7
hc_cluster2 <- college_hc[college_hc$clusterID == 2, c(1:17)]
hc_cluster7 <- college_hc[college_hc$clusterID == 7, c(1:17)]

# t_test_result
hc_t_result <- data.frame()

for (i in 1:17){
  
  hc_t_result[i,1] <- t.test(hc_cluster2[,i], hc_cluster7[,i], 
                             alternative = "two.sided")$p.value
  
  hc_t_result[i,2] <- t.test(hc_cluster2[,i], hc_cluster7[,i], 
                             alternative = "greater")$p.value
  
  hc_t_result[i,3] <- t.test(hc_cluster2[,i], hc_cluster7[,i], 
                             alternative = "less")$p.value
}

hc_t_result

# 다른 cluster 6 & 8
hc_cluster6 <- college_hc[college_hc$clusterID == 6, c(1:17)]
hc_cluster8 <- college_hc[college_hc$clusterID == 8, c(1:17)]

# t_test_result
hc_t_result <- data.frame()

for (i in 1:17){
  
  hc_t_result[i,1] <- t.test(hc_cluster6[,i], hc_cluster8[,i], 
                             alternative = "two.sided")$p.value
  
  hc_t_result[i,2] <- t.test(hc_cluster6[,i], hc_cluster8[,i], 
                             alternative = "greater")$p.value
  
  hc_t_result[i,3] <- t.test(hc_cluster6[,i], hc_cluster8[,i], 
                             alternative = "less")$p.value
}

hc_t_result

#heatmap 그리기
library(gplots)
hc <- hclust(as.dist(1-cor(college_x_scaled, method="spearman")), method="complete")
mycol <- colorpanel(40, "darkblue", "yellow", "white")
heatmap.2(college_x_scaled, Rowv=as.dendrogram(hr_complete), Colv=as.dendrogram(hc), col=mycol,
          scale="row", density.info="none", trace="none", 
          RowSideColors=as.character(mycl))

# DBSCAN (문제 3번)--------------------------
library(factoextra)
library(dbscan)

ploan <- read.csv("Personal Loan.csv")
ploan_x <- ploan[,-c(1,5,10)]
ploan_x_scaled <- scale(ploan_x, center = TRUE, scale = TRUE)

# DBSCAN
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 1, minPts = 5)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 1, minPts = 8)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 1, minPts = 11)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 1, minPts = 14)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 1, minPts = 17)

DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 1.5, minPts = 5)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 1.5, minPts = 8)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 1.5, minPts = 11)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 1.5, minPts = 14)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 1.5, minPts = 17)

DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 2, minPts = 5)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 2, minPts = 8)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 2, minPts = 11)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 2, minPts = 14)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 2, minPts = 17)

DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 2.5, minPts = 5)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 2.5, minPts = 8)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 2.5, minPts = 11)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 2.5, minPts = 14)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 2.5, minPts = 17)

DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 3, minPts = 5)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 3, minPts = 8)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 3, minPts = 11)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 3, minPts = 14)
DBSCAN_ploan <- dbscan(ploan_x_scaled, eps = 3, minPts = 17)

# 시각화
fviz_cluster(DBSCAN_ploan, ploan_x_scaled, ellipse = FALSE, geom = "point",
             show.clust.cent = FALSE)


# radar chart

cluster_dbscan <- data.frame(ploan_x_scaled, clusterID = as.factor(DBSCAN_ploan$cluster))
kmc_summary <- data.frame()

for (i in 1:(ncol(cluster_dbscan)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(cluster_dbscan[,i], cluster_dbscan$clusterID, mean))
}

colnames(kmc_summary) <- paste("cluster", c(0:5))
rownames(kmc_summary) <- colnames(ploan_x)
kmc_summary

par(mfrow = c(2,3))
for (i in 1:5){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(kmc_summary[,i], labels = rownames(kmc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)
}
dev.off()


# extra question ------------------ 박종범 학우와 같이 함
# pheatmap
install.packages("pheatmap")
library(pheatmap)
library(RColorBrewer)
pheatmap(ploan_x_scaled,color=brewer.pal(9,"Blues"))

#pca
library(scatterplot3d)
pca <- prcomp(ploan_x_scaled, scale= TRUE)
summary(pca)
scatterplot3d(pca$x[,1:3],pch=20, color="blue")
#screeplot
plot(pca, type="l")

#각 개체에 대한 첫번째, 두번째 주성분 점수 및 행렬
biplot(pca)
