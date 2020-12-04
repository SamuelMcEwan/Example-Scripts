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
# This is a bottleneck. Roughly 90% of computation time during each iteration in the while() loop is spent computing total distance

# Create an efficient 2-opt route permutation function. Note compared to other random permutations, 2-opt is incredibly powerful
swap1 <- function(order){
  pos1 <- sample(2:(cities-2), 1)
  pos2 <- sample({1:(cities-1)}[-c(pos1-1, pos1, pos1+1)], 1)
  l <- min(pos1, pos2)
  u <- max(pos1, pos2)
  return(order[c(1:l, u:(l+1), (u+1):cities)])
} # Execution takes 0.016 milliseconds and speed seems mostly independent of n.

# Create the Simulated Annealing acceptance function which returns the the probability of accepting a 'worse' intermediary solution 
accptFun <- function(delE, t) {
  return(1/(1+exp(delE/t)))
}

# Initialization
t = 1e4; order <- 1:cities; E <- distance(order = order); iter <- 1; Error <- c(E)

{
  plot_at_time <- seq(0.1, 15, by = 0.1)
  display_plot <- rep(TRUE, length(plot_at_time))  
  t = 1e4; order <- 1:ncities; E <- distance(order = order); iter <- 1; Error <- c(E); currentTime <- 0
  startTime <- proc.time()["elapsed"]
  wait <- 15
  par(mfrow = c(1,2))
  while(currentTime < wait) {
      newOrder <- swap1(order = order)
      newE <- distance(order = newOrder)
      rand <- runif(1, min = 0, max = 1)
      acpt <- accptFun(newE - E, t)
      currentTime <- proc.time()["elapsed"] - startTime
      if(rand <= acpt | newE < E) {
        delE <- newE - E
        E <- newE
        order <- newOrder
        t <- t * 0.9
        iter <- iter + 1
        Error[iter] = E
        if(display_plot[which.min(abs(currentTime - plot_at_time))]){
          plot(df[order,], type = 'l')
          plot(1:iter, Error, xlab = "Iterations", ylab = "Total Distance", type = "l")
          cat("Iteration: ", iter, ", Time: ", currentTime, "Temperature: ", t, "\n")
          cat("Total Distance: ", E, ", New Total Distance: ", newE, "\n")
          cat("Del E: ", delE, "\n")
          cat("\n\n")
          display_plot[which.min(abs(currentTime["elapsed"] - plot_at_time))] <- FALSE
        }
      }
  }  
    
}

# plot(x,y); lines(df[order,])
