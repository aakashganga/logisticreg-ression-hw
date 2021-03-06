# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

wine_drop <-wine[c(2:14)]
scale(wine_drop)
# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

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

wssplot(wine_drop)

# Exercise 2:
#   * How many clusters does this method suggest?
##  ANS:3
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works
#   ANS: it will calculate the sum of squares of each group and add those numbers up. 
#   When it compares the sum of different cluster number, there will be a point the number is the minimum. 
#   ex: the sum of number of clusters is 2 might be 4 but the the sum of number of clusters is 3 might be 2;
#   however when the sum of number of clusters is 4 might be 6. 
#   This can make sure the distance in each group is the smallest. 
# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine_drop, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

nc$Best.nc
#how to decide 
# Exercise 3: How many clusters does this method suggest?
## ANS:2

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(wine_drop, centers = 3)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(wine$Type,fit.km$cluster)




# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library("cluster")


clusplot(wine_drop,fit.km$cluster,diss=FALSE)
