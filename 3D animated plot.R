# 3-d plots from scratch
x <- rnorm(10)
y <- rnorm(10)
z <- rnorm(10)

Position <- c(-5,0,0)
tx <- atan(x-Position[1]) 
ty <- atan(y-Position[2])
tz <- atan(z-Position[3])
pts <- data.frame(tx,ty,tz) # Note quite looking like observed pts (i don't think)

rx <- function(pts, t){
  as.matrix(pts)%*%matrix(c(1,0,0,0,cos(t),sin(t),0,-sin(t),cos(t)), nrow = 3, byrow = TRUE)
}
ry <- function(pts, t){
  as.matrix(pts)%*%matrix(c(cos(t),0,-sin(t),0,1,0,sin(t),0,cos(t)), nrow = 3, byrow = TRUE)
}
rz <- function(pts, t){
  as.matrix(pts)%*%matrix(c(cos(t),sin(t),0,-sin(t),cos(t),0,0,0,1), nrow = 3, byrow = TRUE)
}

speed <- pi/32
for(i in 1:32){
pts <- rx(pts, t = i*speed)
plot(pts[,2], pts[,3], xlim = c(-2,2),ylim = c(-2,2))
testit(0.5)
i <- i+1
}
