################################################################################
################################################################################

# PROJECT 3 Code
# Tina Giagnoni

################################################################################
################################################################################

setwd("D:/Data Mining")
# read in the breast cancer data
# breast=read.csv("wdbc.names", header = FALSE)
breast=read.csv("data.csv", header = TRUE)
breast$diagnosis=as.factor(breast$diagnosis)
is.data.frame(breast)
ncol(breast)
nrow(breast)

################################################################################
################################################################################

# PREPROCESSING

################################################################################
################################################################################


library(dplyr)
# remove id and diagnosis and X columns
b=breast %>% select(-id, -diagnosis, -X)
# scatter plot matrix tells us correlations
splom(b)
# these two have strong positive correlation
plot(b$perimeter_mean, b$radius_mean, 
     main = "Scatter plot of perimeter mean and radius mean", 
     ylab = "radius mean",
     xlab = "perimeter mean")
cor(b)[1:5,1:5]
cor(b$perimeter_mean, b$radius_mean)
m=lm(b$radius_mean~b$perimeter_mean)
abline(m, col="red")
# scale the data
scaleb=scale(b)
smallb=scaleb[,1:10]
# Begin PCA
# packages for scree plot
library(factoextra)
pr=prcomp(b, scale. = TRUE)
summary(pr)
# scree plot
fviz_screeplot(pr,addlabels=TRUE,ncp = 20)

# new data set with principal components
pr.b = data.frame(pr$x[,1:7])

################################################################################
################################################################################

# K-means clustering

################################################################################
################################################################################

# follow tutorial
# https://uc-r.github.io/kmeans_clustering

# Determine number of clusters
fviz_nbclust(pr.b, kmeans, method = "wss")

# K-Means Cluster Analysis k=2
fit <- kmeans(pr.b, 2, nstart = 100)
# results
print(fit)
p1=fviz_cluster(fit, pr.b[,1:2], geom = "point", main = "k=2")

# K-Means Cluster Analysis k=3
fit <- kmeans(pr.b, 3, nstart = 100)
# results
print(fit)
p2=fviz_cluster(fit, pr.b[,1:2], geom = "point",main = "k=3")

# K-Means Cluster Analysis k=4
fit <- kmeans(pr.b, 4, nstart = 100)
# results
print(fit)
p3=fviz_cluster(fit, pr.b[,1:2], geom = "point",main = "k=4")

# K-Means Cluster Analysis k=5
fit <- kmeans(pr.b, 5, nstart = 100)
# results
print(fit)
p4=fviz_cluster(fit, pr.b[,1:2], geom = "point",main = "k=5")

#put all plots in one plot
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

################################################################################
################################################################################

# Hierarchical Clustering

################################################################################
################################################################################

# Ward Hierarchical Clustering https://www.statmethods.net/advstats/cluster.html
# distance matrix
d <- dist(pr.b, method = "euclidean") 
fit <- hclust(d, method="ward.D")
# display dendogram
plot(fit, 
     main="Cluster Dendrogram for Breast Cancer Data",
     xlab="") 

# draw dendogram with red borders around the 2 clusters
rect.hclust(fit, k=2, border="red")
# draw dendogram with blue borders around the 3 clusters
rect.hclust(fit, k=3, border="blue")
# draw dendogram with orange borders around the 4 clusters
rect.hclust(fit, k=4, border="orange")
# draw dendogram with green borders around the 5 clusters
rect.hclust(fit, k=5, border="green")
summary(fit)
fit

#library(FactoMineR)
#res.hcpc <- HCPC(pr.b, graph = FALSE)
#fviz_dend(res.hcpc, 
#          cex = 0.7,                     # Label size
#          palette = "jco",               # Color palette see ?ggpubr::ggpar
#         rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
#          rect_border = "jco",           # Rectangle color
#          labels_track_height = 0.8      # Augment the room for labels
#)

################################################################################
################################################################################

# DBSCAN

################################################################################
################################################################################

# tutorial from http://www.sthda.com/english/wiki/wiki.php?id_contents=7940
# optimal epsilon
library(dbscan)
kNNdistplot(pr.b, 5)
# DBSCAN epsilon = 2.25
db1=dbscan(pr.b, eps = 1, minPts = 5)
summary(db1)
dbs1=fviz_cluster(db1, pr.b[,1:2], geom = "point",
                 main="eps=1")
# DBSCAN
db2=dbscan(pr.b, eps = 2, minPts = 5)
summary(db2)
db2
dbs2=fviz_cluster(db2, pr.b[,1:2], geom = "point",
                 main="eps=2")
# DBSCAN
db3=dbscan(pr.b, eps = 2.5, minPts = 5)
summary(db3)
db3
dbs3=fviz_cluster(db3, pr.b[,1:2], geom = "point",
                 main="eps=2.5")
# DBSCAN
db4=dbscan(pr.b, eps = 3, minPts = 5)
summary(db4)
db4
dbs4=fviz_cluster(db4, pr.b[,1:2], geom = "point",
                 main="eps=3")
#put all plots in one plot
library(gridExtra)
grid.arrange(dbs1, dbs2, dbs3,dbs4, nrow = 2)
################################################################################
################################################################################

# Clustering Validation

################################################################################
################################################################################
library(fpc)

cv=cluster.stats(dist(pr.b), db1$cluster)
cv$dunn
cv$avg.silwidth
cv$within.cluster.ss
cv=cluster.stats(dist(pr.b), db2$cluster)
cv$dunn
cv$avg.silwidth
cv$within.cluster.ss
cv=cluster.stats(dist(pr.b), db3$cluster)
cv$dunn
cv$avg.silwidth
cv$within.cluster.ss
cv=cluster.stats(dist(pr.b), db4$cluster)
cv$dunn
cv$avg.silwidth
cv$within.cluster.ss


library(clValid)
clv=clValid(pr.b, 2:5, clMethods = "kmeans", validation = "internal")
summary(clv)
clv=clValid(pr.b, 2:5, clMethods = "hierarchical", validation = "internal")
summary(clv)
plot(clv)
clv=clValid(pr.b, 2:5, clMethods = c("hierarchical", "kmeans"), validation = "internal")
summary(clv)
#compare kmeans and DBSCAN
grid.arrange(p1,dbs3, nrow=1)

