# Peter's final grade
# This is a function to calculate your final grade
grade_calc <- function(final, midterm_1, midterm_2, paper, homework) {
  return(final*0.3 + midterm_1 * 0.2 + midterm_2 * 0.2 + paper * 0.2 + homework * 0.1)
}

# This is a package that contains a lot of pre-made functions
require(tidyverse)

# This is the code for the first plot (When you don't know the paper grade or the final exam grade)
expand.grid(final = seq(100), paper = seq(100)) %>% # Creates a grid of possible grade values
  mutate(midterm_1 = 51, # Plugging in the midterm and homework grades
         midterm_2 = 71,
         homework = 92.97,
         final_grade = grade_calc(final,midterm_1,midterm_2,paper,homework)) %>% # Calculates the final grade
  ggplot() + # Starts the plotting
  geom_raster(aes(x = paper, y = final, fill = final_grade)) + # Makes the plotting area
  scale_fill_gradient2("Final Grade", # This makes the colors that fill the plotting area
                       low = "red", mid = "yellow", 
                       high = "green",midpoint = 60) +
  labs(x = "Paper Grade", y = "Final Exam") + # This makes the axis labels
  theme_bw() + # This makes the theme for the plot
  theme(panel.grid = element_blank()) # This gets rid of a grid, which I didn't like the look of

# This is the code for the second plot (When you won't take the final)
data.frame(paper = seq(100), final = 0, midterm_1 = 71, midterm_2 = 51, homework = 92.97) %>% # Creates the data where you don't know the grade you got on the paper
  mutate(final_grade = grade_calc(final,midterm_1,midterm_2,paper,homework)/.7) %>% # Calculate the final grade
  ggplot() + # Start plotting
  geom_line(aes(x = paper, y = final_grade)) + # Makes the line in the plot
  labs(x = "Paper Grade", y = "Final Grade") + # This makes the axis labels
  theme_bw() # This makes the theme for the plot