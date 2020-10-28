#####
# install.packages("rJava")
# packages <- c("rJava", "tcltk2", "rpanel", "sm", "rMouse", "KeyboardSimulator", "magrittr", "dplyr")
# lapply(packages, library, character.only = TRUE)
# testit <- function(x){p1 <- proc.time()
# Sys.sleep(x)
# proc.time() - p1}

#####
#####
# x <- rnorm(50); y <- 2.5*x + rnorm(50, mean = 0, sd = 0.1); plot(x,y)
# bords <- logical(0)
# bords <- rbind(bords, unlist(coord()))
# {bords_id <- cbind(c(0,2000,0),c(0,0, -600))
# scale_x <- (bords_id[2,1] - bords_id[1,1])/(bords[2,1] - bords[1,1])
# scale_y <- (bords_id[1,2] - bords_id[3,2])/(bords[1,2] - bords[3,2])} # Scale, etc

# post <- logical(0); t <- 0
# while(t < 20){
#   post <- rbind(post, (unlist(coord())-bords[1,])*c(scale_x,scale_y))
#   testit(0.05)
#   plot(post, type = 'l', xlim = c(0, 1950), ylim = c(-740,0))
#   t <- t + 0.05
# }

par(mfrow = c(1,1))
while(t < 60){
post <- rbind(post, unlist(coord())*c(1,-1))
testit(0.05)
plot(post, type = 'l', xlim = c(0, 1950), ylim = c(-740,0))
t <- t + 0.05
}



pos1 <- read.table("pos1")
pos2 <- read.table("pos2")
pos3 <- read.table("pos3")
d <- unique(rbind(pos1, pos2, pos3))
a <- cbind(rep(d[,1], each = 11), rep(d[,2], each = 11))
dat <- cbind(a, rep(seq(-150, 150, by = 30), nrow(d)))
plot(dat, type = 'l', xlim = c(0, 1950), ylim = c(-740,0))
rotate_up <-  function(theta, x){
m <- matrix(c(1,0,0,0,cos(theta), -sin(theta),0,sin(theta),cos(theta)), nrow = 3)  
x%*%m
}
rotate_right <-  function(theta, x){
m <- matrix(c(cos(theta), 0 , sin(theta),0,1,0,-sin(theta),0,cos(theta)), nrow = 3)  
x%*%m
}


# Rotated "Sam is distracted"
for(i in -32:0){
theta <- i*pi/64
plot(rotate_right(theta, dat), type = 'l', main = "Obviously This is Productive......", xlab = "Responsibilities", 
     ylab = "Other Tasks to Do")
testit(0.05)
}
pos()
coord()
# plot(pos1, type = 'l', xlim = c(0, 1950), ylim = c(-740,0))
# lines(pos2, type = 'l')
# lines(pos3, type = 'l')



# write.table(pos3, "pos3", sep="\t")
head(pos3)
head(pos3 + cbind(rep(100,nrow(pos3)), rep(0,nrow(pos3))))
