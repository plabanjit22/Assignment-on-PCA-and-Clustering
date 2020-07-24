######################### R-Codes #########################
######################### PCA #########################
# importing the dataset
dataset = read.csv('cheese_thermophys.csv')
#Separating out important columns for applying algorithm
df = dataset[3:18]
pc = princomp(df, cor = TRUE, score = TRUE)
summary(pc)
plot(pc, type = 'l', main = 'Scree plot')

######################### Hierarchical Clustering #########################
# importing the dataset
dataset = read.csv('cheese_thermophys.csv')
#Separating out important columns for applying algorithm
df = dataset[3:18]
# Using dendrogram to find optimal number of clusters
dendrogram = hclust(d = dist(df, method = 'euclidean'), method = 'single')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Cheese Type',
     ylab = 'Euclidean distances')
# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(df, method = 'euclidean'), method = 'single')
y_1 = cutree(hc, 2)
y_2 = cutree(hc, 5)
print(y_1)
print(y_2)



######################### K-Means Clustering #########################

# importing the dataset
dataset = read.csv('cheese_thermophys.csv')
# Separating out important columns for applying algorithm
df = dataset[3:18]
# Using Elbow Method to find optimal number of clusters
set.seed(32)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(df, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'Within Cluster Sum of Squares')
# Fitting K-Means to the dataset
set.seed(11)
kmeans1 = kmeans(x = df, centers = 4, iter.max = 1000, nstart = 25)
y_1 = kmeans1$cluster
print(y_1)
print(kmeans1$size)
print(kmeans1$tot.withinss)

kmeans2 = kmeans(x = df, centers = 5, iter.max = 1000, nstart = 25)
y_2 = kmeans2$cluster
print(y_2)
print(kmeans2$size)
print(kmeans2$tot.withinss)
