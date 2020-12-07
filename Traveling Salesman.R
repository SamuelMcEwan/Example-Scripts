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

cities <- 750 
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
# Evaluation of the distance function takes 300 microsceonds for n = 500 cities. 
# This is much too slow (even with use of a distance matrix) and likely to create a bottleneck. 
# Ie - From preliminary analysis roughly 90% of computation time during each iteration in the while() loop is spent computing total distance this way
# To avoid this expensive computation I will implement an updating formula for the total distance based on a 2-opt node permutation.

# Create an efficient 2-opt route permutation function that outputs both a permuted route and new distance (computed using an efficient updating formula)
# Note compared to other random permutations, 2-opt is incredibly powerful
swap <- function(order, dist){
  pos1 <- sample(2:(cities-2), 1)
  pos2 <- sample({1:(cities-1)}[-c(pos1-1, pos1, pos1+1)], 1)
  l <- min(pos1, pos2)
  u <- max(pos1, pos2)
  return(list(
    ord = order[c(1:l, u:(l+1), (u+1):cities)],
    dis = dist - dist_mtx[order[l], order[l+1]] - dist_mtx[order[u], order[u+1]] + dist_mtx[order[l], order[u]] + dist_mtx[order[l+1], order[u+1]]
  ))
} # Execution takes 20 microseconds and speed seems mostly independent of n.

# Proof of Dramatic Speed Increase using an Updating Formula. First I am initializing a route and computing the sum of the distances between each node
# Then I am comparing the computation speeds of the functions distance() and swap() which compute total distance. 
# Note that the swap() function has the added functionality of performing a 2-opt permutation so we would expect the swap() function to
# to be much slower. However the swap() function is 10x faster despite the burden of additional functionalities!!!
order <- 1:cities; current_distance = distance(order)
microbenchmark(distance(order),                # 290 microseconds
               swap(order, current_distance))  #  22 microseconds!!


# Create the Simulated Annealing acceptance function which returns the the probability of accepting a 'worse' intermediary solution 
accptFun <- function(delE, t) {
  return(1/(1+exp(delE/t)))
}

# The following function will attempt to iteratively find the best solution to the Travelling Salesman problem through the use of
# 2-opt route permutations and a simulated annealing acceptance function with acceptance probability proportional to the parameters temperature 
# (which is influenced by the variable decay) and 'newE - E' where 'E' is the total distance of the old route and 'newE' is the total 
# distance of a new candidate route. 
# To avoid bottlenecking the algorithm by plotting every route improvement, I have opted to plot the route only once every 0.1 seconds.
# If a plot has already been generated within a 0.1s window, no more plots will be displayed until the next 0.1s interval
Run_Travelling_Salesman <- function(starting_order, run_time, initial_temperature, decay){
  order <- starting_order; t <- initial_temperature
  
  E <- distance(order = order); iter <- 1; Error <- c(E); currentTime <- 0
  
  startTime <- proc.time()["elapsed"]
  plot_at_time <- seq(0.1, run_time, by = 0.1)
  display_plot <- rep(TRUE, length(plot_at_time))  
  par(mfrow = c(1,2))
  
  while(currentTime < wait) {
    opt2 <- swap(order = order, dist = E)
    newOrder <- opt2$ord
    newE <- opt2$dis
    rand <- runif(1, min = 0, max = 1)
    acpt <- accptFun(newE - E, t)
    
    currentTime <- proc.time()["elapsed"] - startTime
    if(rand <= acpt | newE < E) {
      delE <- newE - E
      E <- newE
      order <- newOrder
      t <- t * decay
      iter <- iter + 1
      Error[iter] = E
      if(display_plot[which.min(abs(currentTime - plot_at_time))]){
        plot(df[order,], type = 'l')
        plot(1:iter, Error, xlab = "Iterations", ylab = "Total Distance", type = "l")
        cat("Iteration: ", iter, ", Time: ", currentTime, "Temperature: ", t, ", Prob Accept: ", acpt, "\n")
        cat("Total Distance: ", E, ", New Total Distance: ", newE, "\n")
        cat("Del E: ", delE, "\n")
        cat("\n\n")
        display_plot[which.min(abs(currentTime["elapsed"] - plot_at_time))] <- FALSE
      }
    }
  }  
  
}

# Run the Travelling Salesman Problem with an abysmal randomly generated starting route, and using a 2-opt search without simulated annealling.
# Simulated annealing is 'disabled' here via setting the temperature and decay parameters to 0. Therefore the search is entirely greedy. 
# ie - new routes are only accepted if they immediately improve the cost function (total distance of the route)

Run_Travelling_Salesman(sample(cities), run_time = 50, initial_temperature = 0, decay = 0)


# Run the Travelling Salesman Problem with a 2-opt search, this time boosted by a Nearest Neighbour starting route. Again, no simulated annealing
# First of all the function Nearest_Neighbour() is created which will begin with an initial node, and iteratively pick the next subsequent node 
# as the node which is closest to the previous node. The output route using the Nearest_Neighbour method is fed as input into the Travelling Salesman Problem 
# See the improvement! 

Nearest_Neighbour <- function(){
  ord <- 1
  for(i in 1:cities){
    next_node <- dist_mtx[ord[1], -ord] %>% which.min %>% names %>% as.numeric %>% as.integer
    ord <- c(next_node, ord)
  }
  ord <- c(ord, setdiff(1:cities, ord))
  return(ord)
}

Run_Travelling_Salesman(Nearest_Neighbour(), 50, 0.01, 0.01)

