###########################################
# Description
################

# The Travelling Salesman Problem (TSP) is one of the most intensively studied problems in optimizations research. 

# The problem is formulated as follows:
# Given a list of cities and the pairiwse distances between each city, what is the shortest possible 
# route that connects each city exactly once

# Despite it's easy problem formulation, the TSP is NP-hard and computationally expensive. 
# It is possible that the running time for any algorithm for the TSP increases superpolynomially with the number of cities/nodes

# Here I list several methods which attempt to 'solve' the TSP
# 1) Brutue Forcing - Consider all n! possible routes and choose the one which minimizes the total travelled distance.
# 2) Nearest Neighbour - Beginning with an initial node, iteratively pick the next subsequent node as the node which is closest to the previous node 
# 3) 2 opt - Beginning with an initial route, randomly permute two nodes and accept the new route only if the total distance is minimized 
# 4) Simmulated Anealling - An extension to local descent algorithms with the ability to escape local minima. 
#                           Local minima are escaped through the use of a decaying temperature + acceptance function in which 'worse' solutions are accepted
#                           as intermediary routes with probability roughly proportional to the change in cost function between iterations and 
#                           roughly inversely proportional to the number of iterations  

# Method 1 is compuationally prohibitive for any non-small n and therefore is not considered in this syntax. 
# Method 2 by far offers the quickest solution which reasonably approximates the global solution. However is known to produce local minima.
# Method 3 is a 'greedy' local descent algorithm which iteratively improves the route and will in the long term produce a more optimal solution than method 2
# Method 4 is the slowest, however with the addition of an acceptance function (similar to the Metropolis Hastings Algorithm), has the unique ability to escape local minima.
# Typically method 2 is used to build an iniial framework/route that feeds into more advanced methods. 
# However here I have coded simulated annealing without Nearest Neighbour boosting due to the desire of producing animations that resolve a reasonable solution
# from an initialy poor route choice
 
# Note that in this syntax I have sacrificed some computation speed in order to produce live plot animations of how the route and cost function are changing over time.
# However by updating the plot only once every 0.1 seconds if an improved solution is available, I have greatly reduced the computation burden.
# After running a few microbenchmarks, the bottleneck is in computation of the total distance
#################################################

cities <- 300 
df <- data.frame(x = sample(1000, cities), y = sample(1000, cities))
plot(df, type = 'b')

# Create a reference distance matrix to efficiently calculate the total distance of new candidate routes
dist_mtx <- as.matrix(dist(df))

# Compute the total distance given the ordering of the cities in the path
distance <- function(order){
  total_distance <- 0
  for (i in 1:(length(order)-1)) {
    total_distance <- total_distance + dist[order[i], order[i+1]]
  }
  return(total_distance)
} 
# Evaluation of the distance function takes 0.2 milliseconds for n = 300 cities. Execution takes 0.6 milliseconds without use of a distance matrix 
# This is a bottleneck. Roughly 80% of computation time during each iteration in the while() loop is spent computing total distance

# swap 3 randomly picked cities in the ordering
swap <- function(order) {
  swaps <- 3
  indices <- sample(1:length(order), swaps); temp <- rep(0,swaps)
  for (i in 1:swaps){
    temp[i] = order[indices[i]]
  }
  for (j in 1:(swaps-1)){
    order[indices[j]] = temp[j+1]
  }
  order[indices[swaps]] = temp[1]
  return(order)
} # Execution takes 0.008 milliseconds and speed seems mostly independent of n.

# Create the Simulated Annealing acceptance function, producing the probability of accepting a 'worse' intermediary solution 
accptFun <- function(delE, t) {
  return(1/(1+exp(delE/t)))
}

# Initialization
t = 1e4; order <- 1:cities; E <- distance(order = order); iter <- 1; Error <- c(E)

{
startTime <- proc.time()["elapsed"]; currentTime <- 0
wait <- 15
par(mfrow = c(1,2))
while(currentTime < wait) {
  newOrder <- swap(order = order)
  newE <- distance(order = newOrder)
  
  rand <- runif(1, min = 0, max = 1)
  acpt <- accptFun(newE - E, t)

  if(rand <= acpt) {
    delE <- newE - E
    
    cat("Iteration: ", iter, ", Time: ", currentTime["elapsed"], "\n")
    cat("Temperature: ", t, ", Prob Accept: ", acpt, "\n")
    cat("Total Distance: ", ", New Total Distance: ", newE, "\n")
    cat("Del E: ", delE, "\n")
    cat("\n\n")
    
    E <- newE
    order <- newOrder
    t <- t * 0.9
    iter <- iter + 1
    Error[iter] = E
    plot(x,y); lines(df[order,])
    plot(1:iter, Error, xlab = "Iterations", ylab = "Total Distance", type = "l")
    currentTime <- proc.time() - startTime 
    Sys.sleep(0.075)
    
  }
}
}

# plot(x,y); lines(df[order,])
