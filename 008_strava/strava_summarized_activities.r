# This is an R script to read an analye my Strava data
library(data.table)
library(ggplot2)
library(lubridate)

# Data directory
data_dir <- "C:/Users/danie/Downloads/strava_20251129/"

# Read the summarized activities data
activities <- fread(file.path(data_dir, "activities.csv"))

# Get rid of duplicate column names
setnames(activities, make.unique(names(activities)))

# Parse the Activity Date column to date format
activities[, Date := parse_date_time(`Activity Date`,
  orders = "b d, Y, I:M:S p", tz = "UTC"
)]

# Convert Distance from kilometers to miles
activities[, Distance_miles := Distance * 0.621371]

ggplot(activities) +
  geom_point(aes(x = Date, y = Distance_miles)) +
  labs(
    title = "Strava Activities Over Time",
    x = "Date",
    y = "Distance (miles)"
  ) +
  theme_minimal()
