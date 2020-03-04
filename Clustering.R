# K-Means Clustering
setwd("C:/Users/DELL/Desktop/R_projects/Mall-Customer-Segmentation")

library(ggplot2)
library(purrr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(cluster) 
library(gridExtra)
library(grid)
library(NbClust)
library(cowplot)
library(plotly)    # Interactive visualizations

# Importing the dataset
customer_data <- read.csv('Mall_Customers.csv')

# Checking the dataset
str(customer_data)
names(customer_data)

head(customer_data)
summary(customer_data$Age)

sd(customer_data$Age)
summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)
summary(customer_data$Age)
sd(customer_data$Spending.Score)

boxplot(customer_data$Age,
        col="yellow")

ggplot(customer_data,aes(Age)) +
  geom_histogram(fill = 'blue') +
  ggtitle("Histogram to Show Count of Age Class") +
  xlab("Age") +
  theme_bw()

ggplot(customer_data,aes(Age)) +
  geom_boxplot(aes(fill=factor(Gender),alpha=0.4)) +
  theme_bw()

ggplot(customer_data,aes(Age, fill = factor(Gender))) +
  geom_histogram() +
  ggtitle("Histogram to Show Count of Age Class") +
  xlab("Age") +
  theme_bw()

ggplot(customer_data,aes(Gender)) +
  geom_bar(aes(fill = factor(Gender))) +
  theme_bw()

boxplot(customer_data$Annual.Income..k..,
        col="yellow")

ggplot(customer_data,aes(Annual.Income..k..)) +
  geom_histogram(color = 'black',fill = 'light blue', bins = 10) +
  ggtitle("Histogram for Annual Income") +
  xlab("Age Class") +
  theme_bw()

ggplot(customer_data,aes(Annual.Income..k..)) +
  geom_density(color = 'black',fill = 'light blue', bins = 10) +
  ggtitle("Density for Annual Income") +
  xlab("Age Class") +
  theme_bw()

boxplot(customer_data$Spending.Score..1.100.,
        col="yellow")

ggplot(customer_data,aes(Spending.Score..1.100.)) +
  geom_histogram(color = 'black',fill = 'light blue', bins = 10) +
  ggtitle("Histogram for Spending Score") +
  xlab("Age Class") +
  theme_bw()

ggplot(customer_data,aes(Spending.Score..1.100.)) +
  geom_density(color = 'black',fill = 'light blue', bins = 10) +
  ggtitle("Density for Spending Score") +
  xlab("Age Class") +
  theme_bw()

# Determining Optimal Clusters

#In determining the optimal clusters, there are three popular methods:

# Elbow method
# Silhouette method
# Gap statistic

# Elbow method
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(customer_data[,3:5], k,iter.max=100,nstart=100, algorithm="Lloyd")$tot.withinss
}

# Compute and plot wss for k = 1 to k = 10
k.values <- 1:10

# extract wss for 2-10 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# "Elbow method" has been wrapped up in a single function (fviz_nbclust):
set.seed(123)
fviz_nbclust(customer_data[,3:5], kmeans, method = "wss")

# From the above graph,
# we conclude that 4 is the appropriate number of clusters 
# since it seems to be appearing at the bend in the elbow plot.

# Average Silhouette Method

# k2 <- kmeans(customer_data[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
# s2 <- plot(silhouette(k2$cluster,dist(customer_data[,3:5],"euclidean")))
# 
# k3 <- kmeans(customer_data[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
# s3 <- plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))
# 
# k4 <- kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
# s4 <- plot(silhouette(k4$cluster,dist(customer_data[,3:5],"euclidean")))
# 
# k5 <- kmeans(customer_data[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
# s5 <- plot(silhouette(k5$cluster,dist(customer_data[,3:5],"euclidean")))
# 
# k6 <- kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
# s6 <- plot(silhouette(k6$cluster,dist(customer_data[,3:5],"euclidean")))
# 
# k7 <- kmeans(customer_data[,3:5],7,iter.max=100,nstart=50,algorithm="Lloyd")
# s7 <- plot(silhouette(k7$cluster,dist(customer_data[,3:5],"euclidean")))
# 
# k8 <- kmeans(customer_data[,3:5],8,iter.max=100,nstart=50,algorithm="Lloyd")
# s8 <- plot(silhouette(k8$cluster,dist(customer_data[,3:5],"euclidean")))
# 
# k9 <- kmeans(customer_data[,3:5],9,iter.max=100,nstart=50,algorithm="Lloyd")
# s9 <- plot(silhouette(k9$cluster,dist(customer_data[,3:5],"euclidean")))
# 
# k10 <- kmeans(customer_data[,3:5],10,iter.max=100,nstart=50,algorithm="Lloyd")
# s10 <- plot(silhouette(k10$cluster,dist(customer_data[,3:5],"euclidean")))

# Use of the fviz_nbclust() function 
#to determine and visualize the optimal number of clusters
set.seed(123)
fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")

# From the above graph,
# we conclude that 6 is the appropriate number of clusters 
# since it seems to be appearing at the silhouette plot.


# Gap Statistic Method

# compute gap statistic
set.seed(123)
gap_stat <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

# Use of the fviz_nbclust() function 
#to determine and visualize the optimal number of clusters
fviz_gap_stat(gap_stat)

# Taking k = 6 as our optimal cluster

k_6 <- kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k_6


# We can visualize the results using fviz_cluster:
fviz_cluster(k_6, data = customer_data[,3:5])

p_1 <- ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k_6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

p_1

gplotly.p_1 <- ggplotly(p_1)
gplotly.p_1

# OBSERVATIONS:

# Cluster 1 (Red Color) <- Average in terms of Earning and Spending 
# Cluster 2 (Yellow Color) <- Earning less but Spending more  [Can be TARGET Customers]
# Cluster 3 (Green Color) <-  Earning less, hence Spending less
# Cluster 4 (Cyan Color) <- Average in terms of Earning and Spending
# Cluster 5 (Red Color) <- Earning high but Spending less
# Cluster 6 (Magenta Color) <- Earning high and also Spending high [TARGET Customers]


p_2 <- ggplot(customer_data, aes(x =Age, y =Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k_6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

p_2

gplotly.p_2 <- ggplotly(p_2)
gplotly.p_2

# OBSERVATIONS:

# Target Customers are usually between ages from 20 - 40 years.

# Plot
plot_grid(p_1, p_2, labels = "AUTO")

##########################
## Hierarchical Clustering

# Calculating the distance between the data-points
dist_customer <- dist(customer_data, method = 'euclidean')

# Hierarchical cluster
hc_customer <- hclust(dist_customer,
                      method = 'complete')

# Plotting the Dendogram
plot(hc_customer)

# k cluster
k = 6
# Extarcting k clusters
cluster_assignments <- cutree(hc_customer, k = k)
cluster_assignments

# data frame with clusters
customer_cluster <- mutate(customer_data,cluster = cluster_assignments)

# Visualizing k clusters
p_3 <- ggplot(customer_cluster, aes(x =Annual.Income..k.., y = Spending.Score..1.100., color = factor(cluster))) +
  geom_point()

p_3

# Visualizing k clusters
p_4 <- ggplot(customer_cluster, aes(x =Age, y = Spending.Score..1.100., color = factor(cluster))) +
  geom_point()

p_4

# Plot
plot_grid(p_3, p_4, labels = "AUTO")

###################################

# OBSERVATIONS:

# Cluster 1 (Red Color) <- Average in terms of Earning and Spending 
# Cluster 2 (Yellow Color) <- Earning less but Spending more  [Can be TARGET Customers]
# Cluster 3 (Green Color) <-  Earning less, hence Spending less
# Cluster 4 (Cyan Color) <- Average in terms of Earning and Spending
# Cluster 5 (Blue Color) <- Earning high but Spending less
# Cluster 6 (Magenta Color) <- Earning high and also Spending high [TARGET Customers]

# Target Customers are usually between ages from 20 - 40 years as they Spend more.


# data frame with clusters
customer_kmeans_cluster <- mutate(customer_data,cluster = k_6$cluster)









