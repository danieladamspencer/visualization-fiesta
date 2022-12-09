library(tidyverse)
# face ----
ggplot() +
  geom_point(aes(x = c(1,2), y = c(3,3)), pch = 19, size = 20) +
  geom_rect(aes(xmin = 0.5, xmax = 2.5, ymin = 0, ymax = 1), fill = "black") +
  lims(x = c(0,3), y = c(-1,4)) +
  theme_void()

# header ----
circle_grid <-
  expand.grid(x = seq(-1,1,by = 0.001), y = seq(0,1,by = 0.001)) %>%
  mutate(in_circ = ifelse(x^2 + y^2 <= 1, TRUE, FALSE)) %>%
  filter(in_circ)
sunset <- ggplot(circle_grid) +
  geom_raster(aes(x = x, y = y, fill = y), show.legend = F) +
  scale_fill_gradient(low = 'red', high = 'purple') +
  theme_void()

ggsave("~/Desktop/sunset.jpg", plot = sunset, width = 1500, height = 500, units = "px")

# Ukraine ----
ggplot() +
  geom_rect(aes(xmin = c(0,0), xmax = c(3,3), ymin = c(0,1.5), ymax = c(1.5,3), fill = c("high","low")), show.legend = F) +
  scale_fill_manual(values = c("yellow","blue")) +
  theme_void()

# peace ----
circle_grid <-
  expand.grid(x = seq(-1,1,by = 0.001), y = seq(-1,1,by = 0.001)) %>%
  mutate(in_circ = x^2 + y^2 <= 1) %>%
  filter(in_circ)
cutout <- circle_grid %>%
  mutate(smaller_circ = x^2 + y^2 <= 0.7,
         not_near_yaxis = abs(x) > 0.15,
         below_leg_top = y < -(2/3)*abs(x),
         above_leg_bottom = y > -0.3 - (2/3)*abs(x),
         in_leg = below_leg_top & above_leg_bottom) %>%
  filter(smaller_circ,not_near_yaxis,!in_leg)
ggplot(circle_grid) +
  geom_raster(aes(x = x, y = y, fill = y), show.legend = F) +
  scale_fill_gradient2(low = 'red', mid = "green",high = 'purple') +
  geom_raster(aes(x = x, y = y), fill = "white", data = cutout, show.legend = F) +
  theme_void()

# states ----
num_states <- 10
size_of_world <- 1e3
set.seed(88675309)
space <-
  matrix(
    sample(seq(num_states), replace = TRUE, size = size_of_world ^ 2) %% num_states,
    size_of_world,
    size_of_world
  )
library(Rcpp)
evolutionCpp_code <-
"NumericMatrix evolutionCpp(NumericMatrix x, int num_states, int num_steps) {
  int num_rows = x.nrow(), num_cols = x.ncol(), val;
  int up, left, right, down, num_up_one, val_plus_one;
  NumericVector neighbor_vals(8);
  for (int s = 0; s < num_steps; s++) {
    for (int i = 0; i < num_rows; i++) {
      for (int j = 0; j < num_cols; j++) {
        val = x(i,j);
        val_plus_one = val + 1;
        val_plus_one = val_plus_one % num_states;
        num_up_one = 0;
        up = (i - 1) % num_rows;
        left = (j - 1) % num_cols;
        down = (i + 1) % num_rows;
        right = (j + 1) % num_cols;
        neighbor_vals(0) = x(up, j); // up
        neighbor_vals(1) = x(up, right); // up-right
        neighbor_vals(2) = x(i, right); // right
        neighbor_vals(3) = x(down, right); // down-right
        neighbor_vals(4) = x(down, j); // down
        neighbor_vals(5) = x(down, left); // down-left
        neighbor_vals(6) = x(i, left); // left
        neighbor_vals(7) = x(up, left); // up-left
        for(int k = 0; k < 8; k++) {
          if(neighbor_vals(k) == val_plus_one) num_up_one += 1;
        }
        if(num_up_one >= 2) {
          x(i,j) = val_plus_one;
        }
      }
    }
  }
  return x;
}"
cppFunction(evolutionCpp_code)
new_space <- evolutionCpp(space, num_states, num_steps = 3500)
reshape2::melt(new_space) %>%
# reshape2::melt(space) %>%
  ggplot() +
  geom_raster(aes(x = Var1, y = Var2, fill = value), show.legend = F) +
  scale_fill_gradient2(mid = "black", high = "green") +
  theme_void()
