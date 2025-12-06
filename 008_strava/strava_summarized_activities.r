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

# Plot all activity distances over time
ggplot(activities) +
  geom_point(aes(x = Date, y = Distance_miles)) +
  geom_smooth(aes(x = Date, y = Distance_miles), method = "loess", se = FALSE, color = "blue") +
  labs(
    title = "Strava Activities Over Time",
    x = "Date",
    y = "Distance (miles)"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 18))

# Aggregate distance to weekly totals ----
activities[, Week := week(Date)]
activities[, Year := year(Date)]

# Make a helper function to determine if a year is a leap year
is_leap_year <- function(year) {
  (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
}

# if week is 53, and year is not a leap year, apply 1/7 of the distance to week 52 of that year, and the rest to week 1 of the next year. If it is a leap year, apply 2/7 to week 52 and 5/7 to week 1 of next year.
week_53 <- activities[,
  {
    week53 <- .SD[Week == 53]
    if (nrow(week53) == 0) week53 <- data.table(Year = .BY$Year, Distance_miles = 0)
    if (is_leap_year(.BY$Year)) {
      Dmiles <- sum(week53$Distance_miles) * c(2 / 7, 5 / 7)
    } else {
      Dmiles <- sum(week53$Distance_miles) * c(1 / 7, 6 / 7)
    }
    .(
      Week = c(52, 1),
      Year2 = c(.BY$Year, .BY$Year + 1),
      Distance_miles = Dmiles
    )
  },
  by = Year
]
week_53[, Year := Year2][, Year2 := NULL]

# Merge in the distances from week 53 and sum with distance for week 52 and week 1 of next year
weekly_activities <- rbind(
  activities[Week != 53, .(Year, Week, Distance_miles)],
  week_53
)

# Now aggregate to weekly totals
weekly_totals <- weekly_activities[, .(Total_Distance = sum(Distance_miles)), by = .(Year, Week)]

# Create a Date column for the start of each week
weekly_totals[, Week_Start := as.Date(paste(Year, Week, 1, sep = "-"), format = "%Y-%U-%u")]
setorder(weekly_totals, Week_Start)
# Remove any dates after December 1, 2025
weekly_totals <- weekly_totals[Week_Start <= as.Date("2025-12-01")]

# Plot weekly totals over time
ggplot(weekly_totals) +
  geom_col(aes(x = Week_Start, y = Total_Distance), fill = "lightgreen") +
  geom_smooth(aes(x = Week_Start, y = Total_Distance), method = "loess", se = FALSE, color = "darkgreen") +
  labs(
    title = "Weekly Total Distances of Strava Activities",
    y = "Total Distance (miles)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    axis.title.x = element_blank()
  )

# Calculate total time spent on activities per week ----
weekly_time <- activities[, .(Total_Time_Hours = sum(`Moving Time` / 3600)), by = .(Year, Week)]

# Split time for week 53 as done for distance
week_53_time <- activities[,
  {
    week53 <- .SD[Week == 53]
    if (nrow(week53) == 0) week53 <- data.table(Year = .BY$Year, Moving_Time = 0)
    if (is_leap_year(.BY$Year)) {
      time_hours <- sum(week53$`Moving Time`) / 3600 * c(2 / 7, 5 / 7)
    } else {
      time_hours <- sum(week53$`Moving Time`) / 3600 * c(1 / 7, 6 / 7)
    }
    .(
      Week = c(52, 1),
      Year2 = c(.BY$Year, .BY$Year + 1),
      Total_Time_Hours = time_hours
    )
  },
  by = Year
]
week_53_time[, Year := Year2][, Year2 := NULL]

# Merge in the times from week 53 and sum with time for week 52 and week 1 of next year
weekly_time_totals <- rbind(
  weekly_time[Week != 53, .(Year, Week, Total_Time_Hours)],
  week_53_time
)

# Now aggregate to weekly totals
weekly_time_totals <- weekly_time_totals[, .(Total_Time_Hours = sum(Total_Time_Hours)), by = .(Year, Week)]

# Create a Date column for the start of each week
weekly_time_totals[, Week_Start := as.Date(paste(Year, Week, 1, sep = "-"), format = "%Y-%U-%u")]
setorder(weekly_time_totals, Week_Start)

# Remove any dates after December 1, 2025
weekly_time_totals <- weekly_time_totals[Week_Start <= as.Date("2025-12-01")]

# Merge weekly distance and time data and calculate average speed
weekly_summary <- merge(
  weekly_totals,
  weekly_time_totals,
  by = c("Year", "Week", "Week_Start")
)

# Calculate average speed in minutes per mile
weekly_summary[, Avg_Speed_Min_per_Mile := (Total_Time_Hours * 60) / Total_Distance]

# Plot average speed over time
ggplot(weekly_summary) +
  geom_point(aes(x = Week_Start, y = Avg_Speed_Min_per_Mile), fill = "lightcoral") +
  geom_smooth(aes(x = Week_Start, y = Avg_Speed_Min_per_Mile), method = "loess", se = FALSE, color = "darkred") +
  labs(
    title = "Weekly Average Speed of Strava Activities",
    y = "Average Speed (minutes per mile)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    axis.title.x = element_blank()
  )
