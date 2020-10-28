install.packages("beepr")
library(beepr)
# install.packages("devtools")
testit <- function(x)
{p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1}

bep <- function(x, t){
  beep(x)
  testit(t)
}





# Run this line.......
{ 
  
# Intro:
  beep(8)
  testit(5.5)
  beep(6)
  testit(1)

# Beginning of Beat - Bars 1 & 2
for(i in 1:2){
  bep(1, 0.6)
  bep(1, 0.2)
  bep(2, 0.4)
  bep(1, 0.6)
  bep(1, 0.4)
  bep(1, 0.1)
  bep(1, 0.1)
  bep(1, 0.2)
  bep(2, 0.2)
  bep(1, 0.4)
}

# Beat Part 2  - Bar 3
  bep(1, 0.6)
  bep(1, 0.2)
  bep(7, 0.4)
  bep(2, 0.6)
  bep(1, 0.4)
  bep(1, 0.1)
  bep(1, 0.1)
  bep(1, 0.2)
  bep(7, 0.2)
  bep(2, 0.4)

#              - Bar 4
  bep(1, 0.6)
  bep(1, 0.2)
  bep(7, 0.4)
  beep(2)
  bep(9, 0.6)
  bep(1, 0.4)
  bep(1, 0.1)
  bep(1, 0.1)
  bep(1, 0.2)
  bep(7, 0.2)
  beep(2)
  bep(9, 0.4)


  
# Staccatto Prior to Drop  
bep(1, 0.2)
bep(1, 0.2)
bep(1, 0.2)
bep(1, 0.1)
bep(1, 0.1)
bep(1, 0.2)
bep(1, 0.1)
bep(1, 0.1)
bep(1, 0.2)
bep(1, 0.2)
bep(1, 0.05)
bep(1, 0.05)
bep(1, 0.05)
bep(1, 0.05)
bep(1, 0.05)
bep(1, 0.05)
bep(1, 0.05)
bep(1, 0.05)
bep(1, 0.2)
bep(1, 0.2)
bep(1, 0.2)
bep(1, 0.1)
bep(1, 0.1)
bep(1, 0.2)

for (i in 1:10){bep(1, 0.1)}
for (i in 1:20){bep(1, 0.05)}
for (i in 1:40){bep(1, 0.025)}

bep(9, 1.2)

# Beat Part 2
  bep(1, 0.6)
  bep(1, 0.2)
  bep(7, 0.4)
  bep(2, 0.6)
  bep(1, 0.4)
  bep(1, 0.1)
  bep(1, 0.1)
  bep(1, 0.2)
  bep(7, 0.2)
  bep(2, 0.4)

  bep(1, 0.6)
  bep(1, 0.2)
  bep(7, 0.4)
  beep(2)
  bep(9, 0.6)
  bep(1, 0.4)
  bep(1, 0.1)
  bep(1, 0.1)
  bep(1, 0.2)
  bep(7, 0.2)
  beep(2)
  bep(9, 0.4)

# Finishing up
bep(3, 3)
beep(4)  
}








