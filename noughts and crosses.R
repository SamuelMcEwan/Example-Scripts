##### Setup #####
# install.packages(c("rJava", "stringr", "tcltk2", "rpanel", "sm", "rMouse", "KeyboardSimulator", "magrittr", "dplyr", "ggplot2")) # rJava often needs to be re-installed each time it is used
packages <- c("rJava", "stringr", "tcltk2", "rpanel", "sm", "rMouse", "KeyboardSimulator", "magrittr", "dplyr", "ggplot2")
lapply(packages, library, character.only = TRUE)

game.plot <- base.plot <-  ggplot()+ geom_vline(xintercept = c(0,1,2,3)) +
  geom_hline(yintercept = c(0,1,2,3)) + geom_line() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank()
  )

circ <- logical(0)
for(i in 1:256){
  circ <- rbind(circ, 0.4*c(cos(2*pi*i/256), sin(2*pi*i/256))-c(0.5,0.5))
}
circle <- data.frame(x_circ = circ[,1], y_circ = circ[,2])
circle.plot <- function(x_pos,y_pos){
  geom_line(data = circle + cbind(rep(x_pos, nrow(circle)), rep(y_pos, nrow(circle))), aes(x = x_circ, y = y_circ))
}

d1 <- rbind(c(-0.4, -0.4), c(0.4, 0.4))-0.5; d1 <- data.frame(d1_x = d1[,1], d1_y = d1[,2])
d2 <- rbind(c(-0.4, 0.4), c(0.4, -0.4))-0.5; d2 <- data.frame(d2_x = d2[,1], d2_y = d2[,2])

check <- function(player){
  if(player == "Player 1"){choices <- p1.choices; name <- p1.name}
  if(player == "Player 2"){choices <- p2.choices; name <- p2.name}
  if(max(table(choices[,1])) >= 3){
    win <- as.numeric(as.character(names(which.max(table(choices[,1])))))
    Round <<- "STOP"
    game.plot <<- game.plot + geom_vline(xintercept = win-0.5, color = 'blue', size = 2)
    message(paste(name, 'wins!!'))
  }
  if(max(table(choices[,2])) >= 3){
    win <- as.numeric(as.character(names(which.max(table(choices[,2])))))
    Round <<- "STOP"
    game.plot <<- game.plot + geom_hline(yintercept = win-0.5, color = 'blue', size = 2)
    message(paste(name, 'wins!!'))
  }
  check.pt <- function(pt){
    sum(apply(choices, MARGIN = 1, function(x) identical(x, pt))) > 0
  }
  if(check.pt(c(1,1)) & check.pt(c(2,2)) & check.pt(c(3,3))){
    Round <<- "STOP"
    diag1 <- rbind(c(0, 0), c(3, 3)); diag1 <- data.frame(diag1_x = diag1[,1], diag1_y = diag1[,2])
    game.plot <<- game.plot + geom_line(data = diag1, aes(x = diag1_x, y = diag1_y), color = 'blue', size = 2)
    message(paste(name, 'wins!!'))
  }
  if(check.pt(c(1,3)) & check.pt(c(2,2)) & check.pt(c(3,1))){
    Round <<- "STOP"
    diag2 <- rbind(c(0, 3), c(3, 0)); diag2 <- data.frame(diag2_x = diag2[,1], diag2_y = diag2[,2])
    game.plot <<- game.plot + geom_line(data = diag2, aes(x = diag2_x, y = diag2_y), color = 'blue', size = 2)
    message(paste(name, 'wins!!'))
  }
  if(Round == "STOP"){
    if(player == "Player 1"){p1.wins <<- p1.wins + 1}
    if(player == "Player 2"){p2.wins <<- p2.wins + 1}
  }
}
  
  
  
##### Interactive #####
p1.choices <- p2.choices <- logical(0); p1.wins <- p2.wins <- x_pos <- y_pos <- 0; Round <- "Play"; again <- "y"; game.plot <- base.plot; drw <- "a"
{
keybd.press("ctrl+2")
Start <- readline(prompt="Type start_game to play: ") 
print(base.plot)
p1.name <- readline(prompt="Enter name of Player 1: ");   p2.name <- readline(prompt="Enter name of Player 2: ")   ; message(paste("Hi", p1.name, "and", p2.name))
while(Start == "start_game" & again == "y"){

if(sum(p1.wins, p2.wins)%% 2 == 0){
while(!(x_pos %in% 1:3 & y_pos %in% 1:3) | sum(duplicated(rbind(p1.choices, p2.choices, c(x_pos, y_pos)))) > 0 |length(x_pos) == 0){
P1.c <- readline(prompt=paste(p1.name, ", Enter location of nought: ", sep = ""))
x_pos <- as.numeric(str_extract(P1.c, "(?<=x)[0-9]"))
y_pos <- as.numeric(str_extract(P1.c, "(?<=y)[0-9]"))
}
p1.choices <- rbind(p1.choices, c(x_pos, y_pos))

game.plot <- game.plot + circle.plot(x_pos, y_pos)
print(game.plot)
check(player = "Player 1")
if(nrow(p1.choices)==5 & Start != "STOP"){
  message("DRAW, Start Again")
  drw <- "DRAW"
  check(player = "Player 1")
  check(player = "Player 2")
  print(game.plot)
  p1.choices <- p2.choices <- logical(0); x_pos <- y_pos <- 0; game.plot <- base.plot
  }

if(Round != "STOP" & drw != "DRAW"){
while(!(x_pos %in% 1:3 & y_pos %in% 1:3) | sum(duplicated(rbind(p1.choices, p2.choices, c(x_pos, y_pos)))) > 0 |length(x_pos) == 0){
  P2.c <- readline(prompt=paste(p2.name, ", Enter location of cross: ", sep = ""))
  x_pos <- as.numeric(str_extract(P2.c, "(?<=x)[0-9]"))
  y_pos <- as.numeric(str_extract(P2.c, "(?<=y)[0-9]"))
}
p2.choices <- rbind(p2.choices, c(x_pos, y_pos))

game.plot <- game.plot + geom_line(data = d1 + cbind(rep(x_pos, nrow(d1)), rep(y_pos, nrow(d1))), aes(x = d1_x, y = d1_y), color = 'red', size = 2) +
                         geom_line(data = d2 + cbind(rep(x_pos, nrow(d2)), rep(y_pos, nrow(d2))), aes(x = d2_x, y = d2_y), color = 'red', size = 2)
print(game.plot)
check(player = "Player 2")
}

if(Round == "STOP"| drw == "DRAW"){
  print(game.plot + ggtitle(paste(p1.name, "   ", p1.wins, "-", p2.wins, "   ", p2.name)))
  again <- readline(prompt="Play Again? (y/n) ")
  if(again == "y"){
    p1.choices <- p2.choices <- logical(0); x_pos <- y_pos <- 0
    game.plot <- base.plot + ggtitle(paste(p1.name, "   ", p1.wins, "-", p2.wins, "   ", p2.name))
    print(game.plot)
    Round <- "START"
    drw <- ""
  }   
}



}else{
while(!(x_pos %in% 1:3 & y_pos %in% 1:3) | sum(duplicated(rbind(p1.choices, p2.choices, c(x_pos, y_pos)))) > 0 |length(x_pos) == 0 & drw != "DRAW"){
    P2.c <- readline(prompt=paste(p2.name, ", Enter location of nought: ", sep = ""))
    x_pos <- as.numeric(str_extract(P2.c, "(?<=x)[0-9]"))
    y_pos <- as.numeric(str_extract(P2.c, "(?<=y)[0-9]"))
  }
  p2.choices <- rbind(p2.choices, c(x_pos, y_pos))
  
  game.plot <- game.plot + circle.plot(x_pos, y_pos)
  print(game.plot)
  check(player = "Player 2")
  if(nrow(p2.choices)==5 & Start != "STOP"){
      message("DRAW, Start Again")
      p1.choices <- p2.choices <- logical(0); x_pos <- y_pos <- 0; game.plot <- base.plot
      print(base.plot)}
  
  if(Round != "STOP"){
    while(!(x_pos %in% 1:3 & y_pos %in% 1:3) | sum(duplicated(rbind(p1.choices, p2.choices, c(x_pos, y_pos)))) > 0 |length(x_pos) == 0){
      P1.c <- readline(prompt=paste(p1.name, ", Enter location of cross: ", sep = ""))
      x_pos <- as.numeric(str_extract(P1.c, "(?<=x)[0-9]"))
      y_pos <- as.numeric(str_extract(P1.c, "(?<=y)[0-9]"))
    }
    p1.choices <- rbind(p1.choices, c(x_pos, y_pos))
    
    game.plot <- game.plot + geom_line(data = d1 + cbind(rep(x_pos, nrow(d1)), rep(y_pos, nrow(d1))), aes(x = d1_x, y = d1_y), color = 'red', size = 2) +
      geom_line(data = d2 + cbind(rep(x_pos, nrow(d2)), rep(y_pos, nrow(d2))), aes(x = d2_x, y = d2_y), color = 'red', size = 2)
    print(game.plot)
    check(player = "Player 1")
  }
  
  if(Round == "STOP"){
    print(game.plot + ggtitle(paste(p1.name, "   ", p1.wins, "-", p2.wins, "   ", p2.name)))
    again <- readline(prompt="Play Again? (y/n) ")
    if(again == "y"){
      p1.choices <- p2.choices <- logical(0); x_pos <- y_pos <- 0
      game.plot <- base.plot + ggtitle(paste(p1.name, "   ", p1.wins, "-", p2.wins, "   ", p2.name))
      print(game.plot)
      Round <- "START"
    }   
  }

  
  
  
}
  
  
}
}

