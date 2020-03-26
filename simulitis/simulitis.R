# This is a play script for simulitis

initial_conditions <- function(n, speed, xlim = c(0,100), ylim = c(0,100), cell_size = 0.5) {
  centers <- data.frame(x = runif(n,xlim[1],xlim[2]),
                        y = runif(n,ylim[1],ylim[2]))
  distances <- dist(centers)
  directions <- runif(n,0,2*pi)
  xy_directions <- data.frame(delta_x = speed * cos(directions),
                              delta_y = speed * sin(directions))
  boundary_distance <- data.frame(
    top = (ylim[2] - centers$y)^2,
    left = (xlim[1] - centers$x)^2,
    bottom = (ylim[1] - centers$y)^2,
    right = (xlim[2] - centers$x)^2
  )
  infected <- rep("Healthy",n)
  infected[sample(seq(n),size = 1)] <- "Infected"
  state <- cbind(centers,directions,xy_directions,boundary_distance,infected)
  simulitis_step <- list(state = state, speed = speed, xlim = xlim, 
                         ylim = ylim, cell_size = cell_size)
  class(simulitis_step) <- "simulitis"
  return(simulitis_step)
}

plot.simulitis <- function()

previous_step <- initial_conditions(100,0.3)

simulitis_step <- function(previous_step) {
  next_centers <- data.frame(x = state$x + state$delta_x,
                             y = state$y + state$delta_y)
}