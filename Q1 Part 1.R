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

apply(imdb_num, 2, mean)
apply(imdb_num, 2, var) 

data_normalized = scale(imdb_num)
head(data_normalized)

library(corrr)
library(ggcorrplot)
corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)
# The higher the value, the more positively correlated the two variables are. Here, Revenue..Millions. and Votes seem to be highly correlated followed by votes and rating.

pca = prcomp(data_normalized, center=FALSE, scale.=FALSE)
summary(pca)  
# The first and the second PC together explain about 73% of the variance of the dataset

install.packages("factoextra")
library(factoextra)
eig.val<-get_eigenvalue(pca)
eig.val
# The first PC(Dim1) explains 43.51% of the total variance
# The second PC(Dim2) explains 29% of the variance
# Together the Dim1 and Dim2 explian about 73% of the variance of the dataset
# 2 eigenvalues are higher than 1, hence by 'Kaiser criterion' we want to retain 2 principal components.

biplot(pca, scale = 0,cex=0.5)
pca$rotation = -pca$rotation
pca$x = -pca$x
biplot(pca, scale =0,cex=0.5)
# check the variance explained by each PC
pca$sdev #SHOWS THE STD DEVIATION ASSOCIATED WITH EACH PC
pr.var = pca$sdev ^2 #FINDS THE VARIANCE
pr.var
# proportion of variance explained
pve = pr.var/sum(pr.var)
pve #PROPROTION OF EACH VARIANCE
plot(pve, xlab = " Principal Component", ylab = "Proportion of
Variance Explained", ylim = c(0,1), type = "b")

plot(cumsum(pve), xlab = "Principal Component", ylab ="
Cumulative Proportion of Variance Explained", ylim = c(0,1),
     type = "b")

