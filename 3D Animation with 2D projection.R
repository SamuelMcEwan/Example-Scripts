##########################################################
# Rotation + Translation of 3D model for 2D projection
##############

# Setup the session
library(magrittr)
rm(list = ls())

# Initialize a random '3D model' defined by a set of points
x <- rnorm(10) %>% rep(5)
y <- rnorm(10) %>% rep(5)
z <- rnorm(50)

# View 2D plot in the x and y dimensions
plot(x,y, type = 'l')

# Set the camera position
Position <- c(0,0,-1.5)
tx <- atan(x-Position[1]) 
ty <- atan(y-Position[2])
tz <- atan(z-Position[3])
pts <- data.frame(tx,ty,tz)

# Create functions which rotate the model in the x, y and z planes. 
rx <- function(pts, t){
  as.matrix(pts)%*%matrix(c(1,0,0,0,cos(t),sin(t),0,-sin(t),cos(t)), nrow = 3, byrow = TRUE)
}
ry <- function(pts, t){
  as.matrix(pts)%*%matrix(c(cos(t),0,-sin(t),0,1,0,sin(t),0,cos(t)), nrow = 3, byrow = TRUE)
}
rz <- function(pts, t){
  as.matrix(pts)%*%matrix(c(cos(t),sin(t),0,-sin(t),cos(t),0,0,0,1), nrow = 3, byrow = TRUE)
}

# Animate the model
speed <- pi/64
for(i in 1:64){
  pts <- rx(pts, t = speed) %>% ry(t = 2*speed) %>% rz(t = 0.5*speed)
  plot(pts[,1], pts[,2], xlim = c(-2,2),ylim = c(-2,2), 
       type = 'l')
  Sys.sleep(0.1)
  i <- i+1
}
