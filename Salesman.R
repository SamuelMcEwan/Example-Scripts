# Travelling Salesman
testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

x <- rnorm(150)
y <- rnorm(150)
pts <- cbind(x,y)
plot(x,y, type = 'l')

d <- function(pts,i,j){ # Don't need to take squareroot. Only used for individual level comparisons
  sum((pts[i,]-pts[j,])^2)
}
dis <- function(a){sqrt(a[1]^2 + a[2]^2)}
td <- function(pts, ord){ 
  sum(apply(pts[ord,][2:nrow(pts),] - pts[ord,][1:(nrow(pts)-1),], FUN = dis, MARGIN = 1))
  # sum((pts[ord,][2:nrow(pts),]-pts[ord,][1:(nrow(pts)-1),])^2)
}

# Distance Matrix 
mtx <- matrix(0, nrow = length(x)+1, ncol = length(x))
for(i in 1:length(x)){
  for(j in 1:length(x)){
    mtx[i,j] <- d(pts, i, j)
  }
}
mtx[nrow(mtx),] <- 1:length(x)

# Nearest Neighbour
# Find Best Starting Point
z <- total_d <- rep(0,length(x));
for(z in 1:length(x)){
ord <- rep(0,length(x))
ord[1] <- z

for (i in 2:(length(x)-1)){
  Interesting <- mtx[c(ord[i-1],nrow(mtx)), -c(ord)]
  ord[i] <- Interesting[2,which.min(Interesting[1,])]
}
ord[length(x)] <- (1:length(x))[-ord]
total_d[z] <- td(pts,ord)
}

# Route Corresponding to Best Start
start <- which.min(total_d)
ord <- rep(0,length(x))
ord[1] <- start

for (i in 2:(length(x)-1)){
  Interesting <- mtx[c(ord[i-1],nrow(mtx)), -c(ord)]
  ord[i] <- Interesting[2,which.min(Interesting[1,])]
}
ord[length(x)] <- (1:length(x))[-ord]

plot(x[ord],y[ord], type = 'l')

ord <- 1:length(x)
# n - opt
for(i in 1:10000){
  n <- 6-log(i, base = 10)+1
  ord2 <- ord
  p <- sample((n+1):(length(x)-n+1), 1)
  ord2[floor(p-n+2):round(p+n-2, 0)] <- sample(ord2[floor(p-n+2):round(p+n-2, 0)])
  if(td(pts,ord2) < td(pts,ord)){
    ord <- ord2
    print(td(pts, ord2))
    plot(x[ord], y[ord], type = 'l')
    testit(0.05)
  }
}
td(pts , ord2)

# 3 - opt for adjacent positions
for(i in 1:10000){
ord2 <- ord
positions <- sample(length(x), 3)
ord2[positions] <- sample(ord2[positions])
if(td(pts,ord2) < td(pts,ord)){
  ord <- ord2
  print(td(pts, ord2))
}
}
plot(x[ord],y[ord], type = 'l')