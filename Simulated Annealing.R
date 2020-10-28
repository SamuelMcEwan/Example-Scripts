# Simulated Annealing
testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

sims <- 5000; cities <- 100  #Check for testit function
x <- sample(1000,cities); y <- sample(1000, cities); df <- data.frame(x,y); plot(x,y)
plot(x,y)
fdistance <- function (x,y){  sqrt(
  (df[x,1] - df[y,1])^2 + (df[x,2] - df[y,2])^2)
}
mtx <- matrix(0, nrow=cities, ncol=cities)
for (i in 1:cities){
  for (j in 1:cities){
    mtx[i,j] <- fdistance(i,j)
  }
}
(dist <- mtx)

# compute the total distance given the ordering of the cities in the path
distance <- function(order) {
  total_distance <- 0
  
  for (i in 1:(length(order)-1)) {
    total_distance <- total_distance + dist[order[i], order[i+1]]
  }
  
  total_distance <- total_distance + dist[order[length(order)], order[1]]
  return (total_distance)
}

# swap two randomly picked cities in the ordering
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
}
# returns the probability with which a solution should be accepted
accptFun <- function(delE, t) {
  return(1/(1+exp(delE/t)))
}

# Initialization
t = 1e4; ncities <- cities; order <- sample(x = 1:ncities); E <- distance(order = order); iter <- 1; Error <- c(E)

startTime <- proc.time()
wait <- 60
par(mfrow = c(1,2))
while(proc.time() - startTime < wait) {
  newOrder <- swap(order = order)
  newE <- distance(order = newOrder)
  
  rand <- runif(1, min = 0, max = 1)
  acpt <- accptFun(newE - E, t)
  
  if(rand <= acpt) {
    delE <- newE - E
    
    # cat("Iteration: ", iter, ", Time: ", proc.time() - startTime, "Temperature: ", t, "\n")
    # cat("Order: ", order, ", New Order: ", newOrder, "\n")
    # cat("Total Distance: ", E, "Del E: ", delE, ", New Total Distance: ", newE, "\n")
    # cat("\n\n")
    
    E <- newE
    order <- newOrder
    t <- t * 0.9
    iter <- iter + 1
    Error[iter] = E
    plot(x,y); lines(df[order,])
    plot(1:iter, Error, xlab = "Iterations", ylab = "Total Distance", type = "l")
    testit(0.075)
  }
}
# plot(x,y); lines(df[order,])
