################################# DAMI Programming Assignment 2: Clustering ##################################

## IMPORTANT! In this document, when you see [FIXME], that is where you should fill in your corresponding R code! 

## If would like to implement every part by yourself, in your submissions, you just comment out the provided codes, and write down your own codes in the corresponding place instead!

## IMPORTANT! Download the synthetic data "cluster.RData" from the course website. This data need to be used throughout the assignment!
## Save the data to your current working directory


# Load the data in your R environment
# This should give you dataframe "cluster.data" 
load("cluster.RData")

###### Part 1: Distance Calculation ######
# TASK 1. Writing a function for distance calculation:

# parameters "a" and "b" are two vectors 
# Manhattan distance is calculated between "a" and "b" when metric = "manhattan"
# otherwise Euclidean distance is calculated and returned by the function

my_dist_calculator <- function(a, b, metric ){
  
  if(metric == "manhattan"){
    # write code for calculating manhattan distance
    c <- abs(a-b)
    dist <- sum(c)
    
  }else if(metric=="euclidian"){
    # write code for calculating euclidean distance
    dist <- sqrt(sum((a-b)^2))
    
  }
  
  return(dist) 
}

#here we can test it
#print(my_dist_calculator(c(1.7, 5),c(4, 72),"euclidian"))



###### Part 2: K-Means Clustering #######
# TASK 2a. Write a function for performing K-Means clustering

# Function takes parameter "x" as dataframe to be clustered
# "k" as a number of clusters and "max.iter" as maximum allowed iteration before convergence
# Use the Euclidean distance calculator from part 1.

k_means <- function(x, k, max.iter = 20){
  
  # assign each observation in x a random cluster number (id) from 1 to k
  random_index <- sample(1:k, nrow(x), replace = TRUE)
  
  # add a new column "cluster" to dataset (x) that carries cluster id for each observation
  # we have a new dataset with random cluster id assigned to each observations 
  data_with_cluster <- cbind(x, clusterID = random_index)
  
  
  # initialize number of iteration counts to stop iterating when maximum limit is reached
  iterations = 1
  
  # plot the points
  plot(data_with_cluster[,1:2])
  
  # this block of code tries to adjust the centroids until they cannot be changed anymore
  # interate until stopping critera are met
  while(TRUE){
    
    # create a new object "centroids" to hold the mean (centroid) of each cluster
    # It is a matrix whose number of rows equals to k
    # and number of columns equals number of columns in original data (x)
    # initialize this matrix with 0 values
    centroids <- matrix(rep(0, times = k * ncol(x)), nrow = k, ncol = ncol(x))
    
    # compute the mean of each cluster and store them in "centroids"
    for(i in 1:k){
      # find observations of cluster i
      obs_of_cluster_i <- (data_with_cluster$clusterID == i)
      
      
      # find their mean and save in "centroids" matrix
      
      #É compute the mean for both x and y individually.
        centroids[i,1] <- mean(data_with_cluster$x[obs_of_cluster_i])
        centroids[i,2] <- mean(data_with_cluster$y[obs_of_cluster_i])
     
      
      
      
    }
    
    
    # ---------------------------------------------- #
    # plots the centroids discovered in each iteration
    points(centroids[,1:2], pch = 20, cex = 2)
    # waits until some charater is fed
    # done to be able to see the change in centroids
    readline(prompt = "Press Enter to continue:")
    # ---------------------------------------------- #
    
    # calculate a distance of each point in dataset x to each of k cluster centroid
    # distance measured are stored in a new matrix.
    # Dimension of this new matrix is such that rows equal to total number of observations in x
    # and columns equal to number of clusters k.
    # First column stores distance of each point to first cluster centroid, 
    # second column stores distance of each point to second cluster centroid and so on.
    # initlaize this matrix with 0 values
    dist_from_centroids <- matrix(rep(0, nrow(x) * k), nrow = nrow(x), ncol = k)
    
    # Compute the distance between each centroid and each point 
    
    for(i in 1:nrow(x)){
      for(j in 1:nrow(centroids)){
        # Use the euclidean distance calculation function written in TASK 1.
        dist_from_centroids[i,j] <- my_dist_calculator(x[i,],centroids[j,],"euclidian")
      }
    }
    
    # from the distance matrix computed, find for each observation the closest cluster centroid
    obs_new_clusterID <- apply(dist_from_centroids, 1, which.min)
    
    # If the centroid is not changing any more for each observation, stop the iteration
    if(all(obs_new_clusterID == data_with_cluster$clusterID)){ 
      km.clusters <- obs_new_clusterID
      centroid.matrix <- centroids
      break
      # If number of iterations exceed the maximum iterations allowed, stop the iteration
    }else if(iterations > max.iter){
      break
      
      # Otherwise, assign the new centroids to the dataset, and continue the iteration
    }else{ 
      data_with_cluster$clusterID <- obs_new_clusterID
      iterations <- iterations + 1
    }
  }
  
  # Plot the final cluster after iteration stops
  plot(data_with_cluster[,1:2], col = data_with_cluster$clusterID)
  points(centroid.matrix[,1:2], pch = 20, cex = 2, col = 1:k)
  
  # when iterations are done, return clusters assigned and final centroid
  return(list("clusters" = km.clusters, "centroids" = centroid.matrix))
}

# call the K-Means function we just created
km_clusters <- k_means(cluster.data, k = 3, max.iter = 15)




# look into the object returned by our function
print(km_clusters)




# TASK 2b. Elbow Heuristics to determine best guess for value of K
# For this part, use built-in kmeans() function in R
# K-means is performed on synthetic data for values of K from 1 to 7
# total within sum of square for each one of them is stored

# initialize a vector to store total within ss
within_ss <- numeric(7)

# Run k means seven times
for(k in 1:7){
  # call built-in kmeans() function in R
  km.cl <- kmeans(cluster.data,centers = k,nstart=10)
  
  # save total within ss value for different values of k
  within_ss[k] <-km.cl$tot.withinss
}

# plot number of centers Vs. total within SS
plot(x = 1:7, y = within_ss, type='b', xlab = "number of centers K", ylab = "total within SS" )

# Based on the plot, write a comment describing what value of K would you choose.

# Answer: As we make k bigger the total within ss gets smaller,which is what we wish.
#However we can't just increase k until we get the number of items,because it would be
#pointless.We try to obtain when the decrease stops being rapid and we stop there.
#In this case,after the k=3 the decrease is significantly smaller,which means I 
#would choose k to be equal to 3.



###### Part 3: K-Medoids Clustering ######
# TASK 3a. Use pam() in "cluster" library for k-medoids clustering. Try with value of k = 3
# use manhattan metric for distance calculation.

library('cluster')
# perform clustering
kmed.cl <- pam(cluster.data,k=3,metric="manhattan")

# plot points
plot(cluster.data, col = kmed.cl$clustering, pch = as.character(kmed.cl$clustering), cex = 0.7, main="k-medoids")
# Specify medoids in the plot
points(kmed.cl$medoids,pch = c("1","2","3"),cex = 2, lwd=3)

# TASK 3b. Calculate Silhouette value for clustering for K ranging from 2 to 5.
# Use manhattan distance

# dataframe to hold Silhouette value
sil_info <- data.frame("K" = 2:5, "Sil" = numeric(4))

# repeat pam() for value of k from 2 to 5
for(k in 2:5){
  kmed.cl <- pam(cluster.data,k,metric="manhattan")

 sil_info$Sil[k-1] <-kmed.cl$silinfo$avg.width
 
 
} 

print(sil_info)
# Based on the table, what value of K gives the best result

#Answer: We want as high silhouette value as possible,so I would choose k equals 3 
# that gives us the highest(0.4314134) instead of k equlas 2,4,5, which have 
# 0.4033822 , 0.3359872 , 0.3498398 silhouette value.

###### Part 4: Hierarchical Clustering ######
# TASK 4a. Perform hierarchical clustering using built-in R function
# Create smaller dataset of only 20 observations randomly picked
set.seed(101)
rand.sample <- sample(1:nrow(cluster.data), 20)
small.dataset <- cluster.data[rand.sample,]
rownames(small.dataset) <- 1:20

# Perform clustering on "small.dataset"
# Calculate distance matrix using dist() function in R
distance_matrix <- dist(small.dataset,method="euclidean")

# Call a built-in R function to perform hierarchical clustering
hierarchial.clust <- hclust(d= distance_matrix,method="complete")
plot(hierarchial.clust, hang = 0.1, main = "Hierarchical Cluster", cex = 1)


# TASK 4b. Cut the dendrogram to get three clusters
#
clusters <- cutree(hierarchial.clust,k=3)


plot(small.dataset,pch =as.character(clusters),cex=0.9,col=clusters)

########################################### END ##############################################
