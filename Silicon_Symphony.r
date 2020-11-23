#############################################
# Description
##################

# In 2018 when I only knew R, SPSS and SAS, I was interested in how to achieve other interesting functionalities outside of data-processing/analysis.
# This lead to some interesting hobby projects such as building webscrapers, creating novel playable games, and even creating 'music' within R. 
# This script was a fun way of creating a tutorial/guide to myself on the Sys.sleep() function which can be used for other more useful purposes such as animating plots and 
# reducing use of bandwidth when webscraping.
# Please note this script was coded whilst I had limited R experience and some computer systems may find difficulty replicating a fluid rhythm
# between subsequent sample sounds. 
# Despite it's novelty and lack of impressiveness, I have included this script in this repository to demonstrate my genuine curiosity with regards to coding
# I am continuing to improve through interest of how I can extend my skills to new areas. 
# For example this is what lead me to learn C++ to obtain faster processing speeds that would help learn facial/object detection from scratch. 

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
  bep(8, 5.5)
  beep(6, 1)

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








