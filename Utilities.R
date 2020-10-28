#####
{
packages <- c("data.table","magrittr", "tidyr", "ggplot2", "lme4", "Rmisc", "foreign","betareg", "lmtest", "emmeans", "dplyr", "tcltk2", "rpanel", "sm", "rMouse", "KeyboardSimulator")
invisible(lapply(packages, library, character.only = TRUE))

testit <- function(x){p1 <- proc.time()
Sys.sleep(x)
proc.time() - p1}
traffic <- function(){
  browseURL("https://qldtraffic.qld.gov.au/cameras.html")
  # browseURL("https://www.tomtom.com/en_gb/traffic-index/brisbane-traffic")
  }
# Click Brisbane, then scroll down to the very last option
# https://earth.google.com/web/@-27.49467388,153.03726981,9.14302988a,141.37364468d,35y,-50.41073484h,71.37488754t,0r

open <- function(x){system2("open",paste(x))}
work <- function(){
work_urls <- c(
  "https://malariajournal.biomedcentral.com/articles/10.1186/1475-2875-7-249",
  "https://genomicsclass.github.io/book/pages/dplyr_tutorial.html",
  "https://rdrr.io/cran/survminer/man/ggadjustedcurves.html",
  "https://bookdown.org/sestelo/sa_financial/adjusting-survival-curves.html"
)
invisible(lapply(work_urls, browseURL))
ff <- "C:\\Users\\SamMc\\Desktop\\Other\\Work\\"
Paths <- rep(0,2)
Extensions <- c("docx","docx","docx",'docx')
for(i in 1:length(Paths)){
  Paths[i] <- paste(ff, paste(i, ".", sep = ""), paste(Extensions[i]), sep = "")
}
lapply(Paths, open)
}

media <- function(){
  media_urls <- c(
    # "https://www.facebook.com/",
    "https://rsbuddy.com/exchange/?id=21012",
    "https://rsbuddy.com/exchange/?id=21034&",
    "https://rsbuddy.com/exchange/?id=20724&",
    "C:\\Users\\SamMc\\Desktop\\Other\\M_sweep.R"
  )
  invisible(lapply(media_urls, browseURL))
}
report <- function(x){
  if(x == 1){open("C:\\Users\\SamMc\\Desktop\\Gametocyte_Data\\Report\\Cox_Regression_White.docx")}
  if(x == 2){open("C:\\Users\\SamMc\\Desktop\\Gametocyte_Data\\Report\\Exploratory_Analyses.docx")}
  if(x == 3){open("C:\\Users\\SamMc\\Desktop\\Gametocyte_Data\\Report\\Preliminary_Report_Updated.docx")}
}
dataset <- function(x){
  if(x == 1){open("C:\\Users\\SamMc\\Desktop\\Gametocyte_Data\\Data_and_R_Scripts\\porgamind4.sav")}
  if(x == 2){open("C:\\Users\\SamMc\\Desktop\\Gametocyte_Data\\Data_and_R_Scripts\\por_gam_patterns.sav")}
  if(x != 1 & x != 2){print("Error: 1 = porgamind4; 2 = por_gam_patterns")}
}

# plot(0, xlim = c(0, 150), ylim = c(0, 80))
distracted <- function(){
pos1 <- read.table("C:/Users/SamMc/Desktop/Other/pos1")
pos2 <- read.table("C:/Users/SamMc/Desktop/Other/pos2")
pos3 <- read.table("C:/Users/SamMc/Desktop/Other/pos3")
d <- unique(rbind(pos1, pos2, pos3))
a <- cbind(rep(d[,1], each = 11), rep(d[,2], each = 11))
dat <- cbind(a, rep(seq(-150, 150, by = 30), nrow(d)))
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
}
distracted.col <- function(){
  draw.arc <- function(g, theta, u, coul){
    colours <- rep(c("blue", "red", "black", "green", "purple", "yellow", "pink", "grey", "orange"), coul)
    theta <- theta/360*2*pi
    y_c <- y0 <- 30; x <- y <- logical(0); t <- 0
    while(y_c >= 0){
      x_c <- u*cos(theta)*t
      y_c <- x_c*tan(theta) + g*x_c^2/(2*u^2*cos(theta)^2) + y0
      x <- c(x,x_c)
      y <- c(y,y_c)
      t <- t+0.1
    }
    # lines(x,y, type = 'l', lwd = 2)
    lines(x,y, type = 'l', lwd = 2*(20 + i)/10, col = colours[coul])
  }
  for(i in seq(-20, -10, length.out = 4)){
    for(j in 1:90){
      draw.arc(g = i, theta = j, u = 30, coul = -i)
    }}
}

CI_table <- function(x, n){
  paste(round(mean(x[!is.na(x)]),n), " (", round(CI(x[!is.na(x)])["lower"],n), ", ", round(CI(x[!is.na(x)])["upper"],n), ")", sep ="")
}
IQR_table <- function(x, n){
  paste(round(quantile(x, na.rm = T, 0.5), n), " [", round(quantile(x, na.rm = T, 0.25),n), ", ", round(quantile(x, na.rm = T, 0.75),n), "]", sep ="")
}
Range_table <- function(x, n){
  paste("[", round(min(x, na.rm = T), n), ", ", round(max(x, na.rm = T), n), "]", sep = "")
}
Recode <- function(var, ...){
  x <- list(...)
  if(!is.null(dim(var))){temp <- var %>% apply(MARGIN = 2, as.character)}else{temp <- var %>% as.character}
  for(i in 1:length(x)){
    label <- names(x)[i]
    levels <- paste0(x[[i]], collapse = "|")
    temp[grep(levels, var)] <- label
  }
  temp
}
Dichotomise <- function(x, break_point, leq = NULL){
    temp <- NA 
    if(!is.null(leq)){
      if(leq){
        temp[which(x <= break_point)] <- paste0("<=", break_point)
        temp[which(x > break_point)] <- paste0(">", break_point)
      }
    }else{
      temp[which(x < break_point)] <- paste0("<", break_point)
      temp[which(x >= break_point)] <- paste0(">=", break_point)
    }
    temp
}      


#####
open("")
# media()

