#####
# Web Scraping Example
install.packages("data.table")
library(data.table)

read.url <- function(url, ...){
  tmpFile <- tempfile()
  download.file(url, destfile = tmpFile, method = "curl")
  url.data <- fread(tmpFile, ...)
  return(url.data)
}

read.url("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/246663/pmgiftsreceivedaprjun13.csv")

#####
# GUI 1
library(tcltk2)
library(rpanel)
library(sm)
# density.draw <- function(panel) {
#   plot(density(panel$y, panel$sp), main = "Density estimate")
#   panel
# }

density.draw <- function(panel) {
  if (panel$sp.method == "normal") panel$sp <- hnorm(panel$x)
  if (panel$sp.method == "plug-in") panel$sp <- hsj(panel$x)
  with(panel, {
    if (model == TRUE) Model <- "normal" else Model <- "none"
    sm.density(x, sp, xlab = xlab, ylab = ylab, model = Model,
               rugplot = FALSE)
    title(paste("Density estimate: h =", signif(sp, 4)))
  })
  panel
}

powerplot.pars <- function(powerplot) {
  se <- powerplot$sigma * 2 / sqrt(powerplot$ngrid)
  powerplot$pow <- 1 - pnorm(2 - abs(powerplot$mu2 - powerplot$mu1) / se)
  powerplot.draw(powerplot)
}

x <- rnorm(50)
r <- diff(range(x))
density.panel <- rp.control("Density estimation", y = x, sp = r / 8)
rp.slider(density.panel, sp, r / 50, r / 4, density.draw, "Bandwidth")

#####
# GUI 2
x <- rnorm(10)
y <- rnorm(10)
z <- rnorm(10)
library(rpanel)

spin.panel <- rp.control("Spin plot", x = x, y = y, z = z, xlab = "xlab",
                         ylab = "ylab", zlab = "zlab", theta = -30, phi = 30)
rp.doublebutton(spin.panel, theta, -1,
                title = "Theta", action = rotate)
rp.doublebutton(spin.panel, phi, -1,
                title = "Phi", action = rotate)
rp.radiogroup(spin.panel, model,
              c("None", "No effects", "xlab", "zlab", paste("xlab","and","zlab")),
              title = "Model", action = model.fn)
rp.checkbox(spin.panel, residuals.showing,
            title = "Show residuals", action = residuals.fn)

#####
gulls.panel <- rp.control("STEPS: The Birds and the Bees",
                          gulls.all = gulls.all,
                          lmk.names = c("Wing tip", "Tail feathers", "Wing joint",
                                        "Bottom of bill", "Tip of beak", "Top of bill",
                                        "Top of head", "Back of head"),
                          lmks.x = c( 25, 40, 218, 417, 449, 436, 362, 330),
                          lmks.y = c(134, 167, 183, 79, 78, 52, 11, 23),
                          lmk1 = NA, lmk2 = NA)
rp.image(gulls.panel, "gulllmks.gif",
         id = "gulls.image", action = click.capture)
rp.button(gulls.panel,
          title = "Collect data", action = collect.data)
