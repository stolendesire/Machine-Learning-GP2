imdb = read.csv("C:/Users/user/Documents/Bayes/Term 2/Machine Learning/Group Assignment 2/IMDB-Movies.csv")

# Subset the numerical columns
imdb_num <- imdb[, sapply(imdb, is.numeric)]
imdb_num <- imdb_num[, -2] # Remove the "Year" column
imdb_num <- imdb_num[, -1]

summary(imdb_num)

# Impute missing values with column means
for (i in 1:ncol(imdb_num)) {
  col_mean <- mean(imdb_num[, i], na.rm = TRUE)
  imdb_num[is.na(imdb_num[, i]), i] <- col_mean
}
imdb_num

data_normalized = scale(imdb_num)
head(data_normalized)

library(corrplot)
M<-cor(data_normalized)
corrplot(M, method="circle")

#Pre-processing the data
boxplot(imdb_num)
boxplot(data_normalized)
summary(data_normalized)
# This shows the need to normalize to scale the

#Clustering the data 
#Finding optimum number of clusters
library(NbClust)
optimum <- NbClust(data_normalized, distance='euclidean', max.nc=12, method="kmeans")
table(optimum$Best.nc[1,])
barplot(table(optimum$Best.nc[1,]), xlab="Number of Clusters", ylab="Number of criteria", 
        main="Number of clusters chosen by criteria")

######################################################################################
################# K-MEANS CLUSTERING #################################################
######################################################################################
# Perform k-means cluster analysis, set 3 clusters as suggested by above method
fit.km <- kmeans(data_normalized, 3, nstart=25, iter.max = 30)

imdb$Cluster <- fit.km$cluster

#Summary of the clusters
print("Size of clusters:")
fit.km$size

print("Centers:")
fit.km$centers

print("Total Sum of Squares (SS):")
fit.km$totss

print("Within Clusters SS")
fit.km$withinss

print("Total Within Clusters SS")
fit.km$tot.withinss

print("Between Clusters SS")
fit.km$betweenss

library(cluster)
set.seed(123)
data <- scale(iris[, 1:4]) # Example data
fit <- kmeans(data, 3) # Perform k-means clustering with 3 clusters
sil = silhouette(imdb$Cluster, dist(data_normalized)) # Calculate silhouette score
summary(sil) # View summary of silhouette score



# Visualising the plots
plot(imdb_num, col = imdb$Cluster)

# Fancy Cluster Plots
bss <- numeric()
wss <- numeric()

# Run the algorithm for different values of k 
set.seed(1234)

for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  bss[i] <- kmeans(imdb_num, centers=i)$betweenss
  wss[i] <- kmeans(imdb_num, centers=i)$tot.withinss
  
}
# Between-cluster sum of squares vs Choice of k
p3 <- qplot(1:10, bss, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# Total within-cluster sum of squares vs Choice of k
p4 <- qplot(1:10, wss, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# Subplot
install.packages("gridExtra")
library(gridExtra)
grid.arrange(p3, p4, ncol=2)

# Execution of k-means with k=3
set.seed(1234)

imdb_k3 <- kmeans(imdb_num, centers=3)

# Mean values of each cluster
aggregate(imdb_num, by=list(imdb_k3$cluster), mean)


# Fancy Clustering 
install.packages("GGally")
library(GGally)
ggpairs(cbind(imdb_num, Cluster=as.factor(imdb_k3$cluster)),
        columns=1:5, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_bw()

# Define a color vector for each cluster
colors <- c("red", "green", "blue")

# Plot the data with colors based on cluster membership
plot(data_normalized, col = colors[imdb$Cluster], pch = 19)

# Add text labels with cluster indices
legend("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3"), col = colors, pch = 19)


# Cluster Plot against 1st and 2nd principal components
library(cluster) 
clusplot(imdb_num, imdb$Cluster, color=TRUE, shade=TRUE, 
         labels=3)

#################################################################################
########### CLUSTERING BY HIERARCHICAL METHOD #####################################
######################################################################################
# Perform PCA analysis
pca = prcomp(data_normalized, center=TRUE, scale.=TRUE)

# Extract principal components
pcs=predict(pca)

# Perform hierarchical clustering
hc=hclust(dist(pcs), method="ward.D2")

# Plot dendrogram
plot(hc, hang=-1, cex=0.8, main="Dendrogram")

# Cut dendrogram to form clusters
# num_clusters value taken from the optimal numbers of clusters found above
num_clusters = 3
clusters = cutree(hc, k=num_clusters)

# Visualize clustering results
plot(pcs[,1:2], col=clusters, pch=19, main="Hierarchical Clustering Results")
legend("topright", legend=unique(clusters), col=unique(clusters), pch=19)

sil = silhouette(clusters, dist(data_normalized)) # Calculate silhouette score
summary(sil) # View summary of silhouette score



