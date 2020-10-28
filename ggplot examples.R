library(ggplot2)
d <- data.frame(x = rnorm(100), y = rnorm(100))



ggplot() + geom_hline(yintercept = 0:3) + geom_vline(xintercept = 0:3)

ggplot(d, aes(x,y)) + geom_line()
ggplot(d, aes(x,y)) + geom_point()
ggplot(d, aes(x,y)) + geom_area()
ggplot(d, aes(x,y)) + geom_bin2d()
ggplot(d, aes(x,y)) + geom_jitter()
ggplot(d, aes(x,y)) + geom_smooth()
ggplot(d, aes(x,y)) + geom_step()
ggplot(d, aes(x,y)) + geom_violin()

