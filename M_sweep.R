library(fun)
demo(package = "fun")
gomoku()
if (interactive()) {
  if (.Platform$OS.type == "windows")
    x11() else x11(type = "Xlib")
  lights_out()
}
if (interactive()) {
  if (.Platform$OS.type == "windows")
    x11() else x11(type = "Xlib")
  mine_sweeper()
}
if (interactive()) {
  if (.Platform$OS.type == "windows")
    x11() else x11(type = "Xlib")
  sliding_puzzle()
  sliding_puzzle(z = matrix(0:11, 3, 4))
}
tower_of_hanoi(7)