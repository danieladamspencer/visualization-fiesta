# This is a play script for simulitis

initial_conditions <- function(n, speed, xlim = c(0,100), ylim = c(0,100), cell_size = 0.5) {
  centers <- data.frame(x = runif(n,xlim[1],xlim[2]),
                        y = runif(n,ylim[1],ylim[2]))
  # distances <- dist(centers)
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
  state <- cbind(centers,xy_directions,infected)
  simulitis_step <- list(state = state, xlim = xlim, 
                         ylim = ylim)
  class(simulitis_step) <- "simulitis"
  return(simulitis_step)
}

previous_step <- initial_conditions(100,0.3)

plot.simulitis <- function(x) {
  plot(x$state$x, x$state$y, col = c("black","red")[as.numeric(x$state$infected)], 
       pch = 19, xlim = x$xlim, ylim = x$ylim, xlab = "", ylab = "")
}

plot(previous_step)

simulitis_step <- function(previous_step) {
  delta_x <- ifelse(previous_step$state$x + previous_step$state$delta_x < min(previous_step$xlim) | 
           previous_step$state$x + previous_step$state$delta_x > max(previous_step$xlim),
         -previous_step$state$delta_x,previous_step$state$delta_x)
  delta_y <- ifelse(previous_step$state$y + previous_step$state$delta_y < min(previous_step$ylim) | 
                      previous_step$state$y + previous_step$state$delta_y > max(previous_step$ylim),
                    -previous_step$state$delta_y,previous_step$state$delta_y)
  next_centers <- data.frame(x = previous_step$state$x + delta_x,
                             y = previous_step$state$y + delta_y)
  xy_directions <- data.frame(delta_x = delta_x,
                              delta_y = delta_y)
  # distances <- as.matrix(dist(next_centers))
  simulitis_step <- list(
    state = cbind(next_centers, xy_directions, infected = previous_step$state$infected),
    xlim = previous_step$xlim,
    ylim = previous_step$ylim
  )
  class(simulitis_step) <- "simulitis"
  return(simulitis_step)
}

step_2 <- simulitis_step(previous_step)

plot(step_2)

n_steps <- 100
next_step <- initial_conditions(100,0.3)
df_out <- data.frame(
  x = next_step$state$x,
  y = next_step$state$y,
  infected = next_step$state$infected,
  t = 1
)
for(tt in seq(2,n_steps)) {
  next_step <- simulitis_step(next_step)
  df_out <- rbind(df_out,data.frame(
  x = next_step$state$x,
  y = next_step$state$y,
  infected = next_step$state$infected,
  t = tt
  ))
}

library(gganimate)
ggplot(df_out) +
  geom_point(aes(x = x, y = y, color = infected)) +
  transition_states(t, wrap = T) +
  scale_color_discrete("") +
  theme_bw()
