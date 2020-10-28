cities <- 1000*data.frame(x=runif(100), y=runif(100)) %>% round(3)
sq_distance <- function(ord){
  sum(diff(cities$x[ord])^2 + diff(cities$y[ord])^2)
}
swap <- function(ord,n){
  new_ord <- ord
  pos <- sample(size = 1, 1:(length(new_ord)-n+1))
  new_ord[pos:(pos+n-1)] <- sample(new_ord[pos:(pos+n-1)])
  new_ord
}

testit <- function(x){
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

plot(cities[ord,], type = 'l')
ord <- 1:100

its <- 10000
for(i in 1:its){
  swaps <- floor(log(its-i+16, base = 4))
  nod <- swap(ord = ord, n= swaps)
  if(sq_distance(nod) < sq_distance(ord)){
    ord <- nod
    plot(cities[ord,], type = 'l')
    cat(paste("its =", i, "dist =", round(sqrt(sq_distance(ord)),2)), "swaps =", swaps, '\n')
    testit(0.05)
  }
}


# Example Dataset
ID <- c(rep("ID_1", 3), rep("ID_2", 3), rep("ID_3",2))
Day <- c(2,2,4,1,3,4,2,2)
Temp <- c(24,24,23,28,24,21,27,27)
df <- data.frame(ID,Day,Temp)
print(df, row.names = F)

df[!duplicated(df),]
df %>% unique
df %>% distinct


df[!duplicated(df$ID),]
df %>% filter(!duplicated(ID))

