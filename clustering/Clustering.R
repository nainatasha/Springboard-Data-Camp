# Load required packages
library(rattle)
library(cluster)
library(NbClust)

# Load data from rattle package
data(wine, package="rattle")

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
scale(wine[-1])

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest?
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works
# This plot suggests that 3 clusters is ideal for k-means clustering.
# This is because the wssplot() function is calculating the within
# groups sum of squares for each cluster specification between 2 and 15 (set by nc).
# Ideally, we want to minimize the within groups sum of squares when
# specifying the number of clusters to extract in the kmeans function.
# The plot indicates that the within groups sum of squares drops
# dramatically between 1 and 3 clusters, but not much thereafter.
# Therefore, 3 clusters is the ideal specification for kmeans clustering
# according to this method.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

# Exercise 3: How many clusters does this method suggest?
# This method also suggests that 3 clusters is the ideal
# specification for k means clustering.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km
fit.km <- kmeans(df, 3)

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
compare.table <- table(wine$Type, fit.km$cluster)
compare.table
# If we look at the table, we can see that for the most part, the clustering model
# differentiates among the three wine types. Type 1 wines are all assigned to cluster 3,
# type 3 are all assigned to cluster 1, and lastly, all but 6 wines of type 2 are assigned
# to cluster 2.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
clusplot(df, fit.km$cluster)
# The plot confirms that the model used minimizes the distance between each data point
# and its centroids when 3 clusters are specified.