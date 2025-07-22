library(future.apply)
library(skimr)
library(scales)
library(janitor)
library(hms)
library(plotly)
library(htmlwidgets)
library(htmltools)
library(tidyverse)
library(DT)

# Enable parallel processing
plan(multisession)

# combine the data
data_files <- list.files("data/2024-divvy-tripdata", full.names = TRUE)
combined_data <- future_lapply(data_files, function(x) {
  read_csv(x) |> 
    mutate(source_file = basename(x))  
}) |> 
  bind_rows()

# Disable parallel workers
plan(sequential)

# adding the columns "ride_length" and "day_of_week"
df <- combined_data |>
  mutate(ride_length = as_hms(as.numeric(ended_at - started_at, units = "secs")),
         day_of_week = wday(started_at, label = TRUE, abbr = FALSE))

df <-  df |> filter(!ride_length <= 0)

# cleaning the column names
df <- clean_names(df)

# overview of the data
skim(df)
glimpse(df)

# changing the default theme to minimal
theme_set(
  theme_minimal())

# defining custom settings
custom_settings <- list(
    scale_x_discrete(labels = c("Casual", "Member")),
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())),
    scale_fill_brewer(palette = "Dark2"),
    scale_color_brewer(palette = "Dark2"),
    labs(x = NULL, y = NULL)
    )

# number of rides for each group
plot_1 <- df |> ggplot(aes(x = member_casual, fill = member_casual)) +
  geom_bar(show.legend = F) +
  geom_text(aes(label = after_stat((prop * 100) |>
                                     round(1) |>
                                     paste0("%")),
                group = 1,
                y = after_stat(count)/2),
            size = 8,
            stat = "count",
            show.legend = F) +
  labs(title = "Number of rides per group") +
  custom_settings

# making the plot interactive for the report
interactive_plot_1 <- ggplotly(plot_1,  tooltip = c("x", "count")) |>
  hide_legend()

# create a directory for any imported files used in quarto
dir.create("Quarto_imported_files")

# Save as HTML file
saveWidget(interactive_plot_1, "Quarto_imported_files/interactive_plot_1.html",
           selfcontained = FALSE,
           libdir = "shared_libs")


# calculating the summarizations and deviations for each group
ride_length_by_group_summary <- df |> 
  group_by(member_casual) |>
  summarise(ride_length_avg = as_hms(mean(ride_length)),
            ride_length_sd = as_hms(sd(ride_length)),
            ride_length_median = as_hms(median(ride_length)),
            ride_length_Q1 = as_hms(quantile(ride_length, 0.25)),
            ride_length_Q3 = as_hms(quantile(ride_length, 0.75)),
            n = n())

# saving the object to be loaded in quarto
saveRDS(
  ride_length_by_group_summary, 
  file = "Quarto_imported_files/ride_length_summary.rds"
)

# displaying the summarizations
ride_length_by_group_summary |>
  mutate(
    across(c(ride_length_avg, ride_length_sd, ride_length_median, ride_length_Q1, ride_length_Q3), 
                ~ hms::round_hms(., digits = 0))) |>
  knitr::kable()

# median of ride length plot
median_ride_duration_difference_plot <- ride_length_by_group_summary |>
  ggplot(aes(x = member_casual,
             colour = member_casual,
             text = paste(
               "Group:", member_casual,
               "<br>Median:", format(as_hms(round(ride_length_median, 0)), "%H:%M:%S"),
               "<br>Q1:", format(as_hms(round(ride_length_Q1, 0)), "%H:%M:%S"),
               "<br>Q3:", format(as_hms(round(ride_length_Q3, 0)), "%H:%M:%S"),
               "<br>Sample size:", format(n, big.mark = ",")
             ))) +
  geom_pointrange(
    aes(
    y = ride_length_median,
    ymin = ride_length_Q1,
    ymax = ride_length_Q3,
    size = n
    ),
    linewidth = 1.5,
    show.legend = FALSE) +
  # geom_text(   ## this part is for pdf format
  #   aes(
  #     y = ride_length_Q1 * 0.9,
  #     label = paste0("n = ", n)
  #   ),
  #   color = "black",
  #   size = 3.5
  #   ) +
  scale_size_continuous(
    range = c(2,4) 
  ) +
  labs(
       title = "Ride Duration: Casual vs. Member",
       subtitle = "Median, Interquartile range and sample size of ride_length") +
  theme(plot.title = element_text(face = "bold")) +
  custom_settings +
  scale_y_continuous(
    labels = function(x) format(hms::hms(round(x)), "%H:%M:%S")
  )

# make interactive with ggplotly
median_ride_duration_interactive_plot <- median_ride_duration_difference_plot |> 
  ggplotly(
    tooltip = "text") |> 
  hide_legend()

# save to directory
saveWidget(median_ride_duration_interactive_plot,
           "Quarto_imported_files/median_ride_duration_interactive_plot.html",
           selfcontained = FALSE,
           libdir = "shared_libs")

# number of rides for type by group
rides_by_type <- df |> ggplot(aes(x = rideable_type)) +
  geom_bar(aes(fill = member_casual),
           position = 'dodge') +
  custom_settings +
  scale_fill_brewer(palette = "Dark2", labels = c("Casual", "Member")) +
  scale_x_discrete(labels = c("Classic Bike", "Electric Bike", "Electric Scooter")) +
  labs(
    title = "Number of Rides by Type and Group"
  ) +
  theme(
    legend.title = element_blank()
  )

# make interactive
rides_by_type_interactive_plot <- ggplotly(rides_by_type) |>
  style(
    traces = 1, 
    name = "Casual"  
  ) |> 
  style(
    traces = 2, 
    name = "Member" 
  ) |> 
  layout(
    legend = list(title = list(text = ""))  # Remove title
  )

saveWidget(rides_by_type_interactive_plot,
           "Quarto_imported_files/rides_by_type_interactive_plot.html",
           selfcontained = FALSE,
           libdir = "shared_libs")

# ride length median by type and group 
ride_length_by_type <- df |> 
  group_by(rideable_type, member_casual) |>
  summarise(duration_median = median(ride_length), .groups = "drop") |>
  ggplot(aes(x = rideable_type, y = duration_median, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_x_discrete(labels = c("Classic Bike", "Electric Bike", "Electric Scooter")) +
  scale_fill_brewer(palette = "Dark2", labels = c("Casual", "Member")) +
  labs(
    x = NULL,
    y = NULL,
    title = "Median of Ride Duration for each Ride Type and Group"
  ) +
  theme(
    legend.title = element_blank()
  )

# interactive ride length median by type and group 
interactive_ride_length_by_type <- ggplotly(ride_length_by_type) |>
  style(
    traces = 1, 
    name = "Casual"  
  ) |> 
  style(
    traces = 2, 
    name = "Member" 
  ) |> 
  layout(
    legend = list(title = list(text = ""))  # Remove title
  )

# save plot
saveWidget(interactive_ride_length_by_type, "Quarto_imported_files/interactive_ride_length_by_type.html",
           selfcontained = FALSE,
           libdir = "shared_libs")



# number of rides for weekdays by group
weekday_rides <- df |> ggplot(aes(x = day_of_week)) +
  geom_bar(aes(fill = member_casual),
           position = 'dodge') + 
  labs(x = NULL, y = NULL, title = "Number of Rides by Day of the Week and Group") +
  scale_fill_brewer(palette = "Dark2", labels = c("Casual", "Member")) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme(
    legend.title = element_blank()
  )
# rides by weekdays interactive
interactive_weekday_rides <- ggplotly(weekday_rides) |>
  style(
    traces = 1, 
    name = "Casual"  
  ) |> 
  style(
    traces = 2, 
    name = "Member" 
  ) |> 
  layout(
    legend = list(title = list(text = ""))
  )

saveWidget(interactive_weekday_rides,
           "Quarto_imported_files/interactive_weekday_rides.html",
           selfcontained = FALSE,
           libdir = "shared_libs")

# percentage of rides of each group for weekdays
weekday_prides <- df |> 
  ggplot(aes(x = day_of_week)) +
  geom_bar(
    aes(
      fill = member_casual,
      y = after_stat(prop),
      group = member_casual,
      text = after_stat(
        paste0("Day: ", levels(df$day_of_week), "<br>",
               "Group: ", ifelse(group == 1, "Casual", "Member"), "<br>",
               "Percentage: ", scales::percent(prop)))
      ),
      position = 'dodge'
    ) + 
      labs(x = NULL, y = NULL, title = "Percentage of Rides by Day of the Week within each Group") +
      scale_fill_brewer(palette = "Dark2", labels = c("Casual", "Member")) +
      scale_y_continuous(labels = label_percent()) +
      theme(
        legend.title = element_blank()
      )
    
# percentage of rides by weekdays interactive
interactive_weekday_prides <- ggplotly(weekday_prides, tooltip = "text") |>
  style(
    traces = 1, 
    name = "Casual"  
  ) |> 
  style(
    traces = 2, 
    name = "Member" 
  ) |> 
  layout(
    legend = list(title = list(text = ""))
  )

saveWidget(interactive_weekday_prides,
           "Quarto_imported_files/interactive_weekday_prides.html",
           selfcontained = F,
           libdir = "shared_libs")

# ride length median by weekday and group
weekday_ridelength <- df |>
  group_by(day_of_week, member_casual) |>
  summarize(duration_median = median(ride_length), .groups = "drop") |>
  ggplot(aes(x = day_of_week, y = duration_median, colour = member_casual)) +
  geom_line(aes(group = member_casual), linewidth = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2", labels = c("Casual", "Member")) +
  scale_y_time(limits = c(0, NA)) +
  labs(x = NULL, y = NULL, title = "Median of Ride Duration for each Weekday by Group") +
  theme(legend.title = element_blank())

# ride length median by weekday interactive
interactive_weekday_ridelength <- ggplotly(weekday_ridelength) |>
  style(
    traces = 1, 
    name = "Casual"  
  ) |> 
  style(
    traces = 2, 
    name = "Member" 
  ) |>
  layout(
    legend = list(title = list(text = ""))
  )

saveWidget(interactive_weekday_ridelength, "Quarto_imported_files/interactive_weekday_ridelength.html",
           selfcontained = F,
           libdir = "shared_libs")

### look at deepseek to adjust background

# number of rides for each month by group
month_rides <- df |> mutate(month_of_year = month(started_at, label = TRUE)) |>
  ggplot(aes(x = month_of_year)) +
  geom_bar(aes(fill = member_casual), position = "dodge") +
  scale_fill_brewer(palette = "Dark2", labels = c("Casual", "Member")) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL,
    y = NULL,
    title = "Number of Rides by Month and Group"
  ) +
  theme(legend.title = element_blank())

# rides for each month interactive
interactive_month_rides <- ggplotly(month_rides) |>
  style(
    traces = 1, 
    name = "Casual"  
  ) |> 
  style(
    traces = 2, 
    name = "Member" 
  ) |>
  layout(
    legend = list(title = list(text = ""))
  )

saveWidget(interactive_month_rides, "Quarto_imported_files/interactive_month_rides.html",
           selfcontained = F,
           libdir = "shared_libs")

# percentage of rides of each group for each month
month_prides <- df |> mutate(month_of_year = month(started_at, label = TRUE)) |>
  ggplot(aes(x = month_of_year, fill = member_casual)) +
  geom_bar(aes(group = member_casual,
               y = after_stat(prop),
               text = paste0(
                 "Month: ", month.abb, "<br>",
                 "Group: ", ifelse(group == 1, "Casual", "Member"), "<br>",
                 "Percentage: ", scales::percent(after_stat(prop))
                            )
              )
           , position = "dodge"
           ) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_brewer(palette = "Dark2", labels = c("Casual", "Member")) +
  labs(x = NULL, y = NULL, title = "Percentage of Rides by Month within each Group") +
  theme(legend.title = element_blank())

# percentage of rides for months interactive
interactive_month_prides <- ggplotly(month_prides, tooltip = "text") |>
  style(
    traces = 1,
    name = "Casual"
  ) |>
  style(
    traces = 2,
    name = "Member"
  ) |>
  layout(
    legend = list(title = list(text = ""))
  )

saveWidget(interactive_month_prides, "Quarto_imported_files/interactive_month_prides.html",
           selfcontained = F,
           libdir = "shared_libs")

# ride length median for each month by group
month_ride_length <- df |> mutate(month_of_year = month(started_at, label = TRUE)) |>
  group_by(month_of_year, member_casual) |>
  summarise(ride_length_median = median(ride_length), .groups = "drop") |>
  ggplot(aes(x = month_of_year,
             y = ride_length_median,
             colour = member_casual)) +
  geom_line(aes(group = member_casual), linewidth = 1) +
  scale_y_time(limits = c(0, NA)) +
  scale_color_brewer(palette = "Dark2", labels = c("Casual", "Member")) +
  labs(x = NULL, y = NULL, title = "Median of Ride Duration for each Month by Group") +
  theme(
    legend.title = element_blank()
  )

# ride length median by month interactive
interactive_month_ride_length <- month_ride_length |>
  ggplotly(tooltip = c("x", "y", "colour")) |>
  style(
    traces = 1,
    name = "Casual"
  ) |>
  style(
    traces = 2,
    name = "Member"
  ) |>
  layout(
    legend = list(title = list(text = ""))
  )

saveWidget(interactive_month_ride_length, "Quarto_imported_files/interactive_month_ridelength.html",
           selfcontained = F,
           libdir = "shared_libs")

# number of rides for hour of day by group
hour_rides <- df |>
  mutate(hour_of_day = hour(started_at)) |>
  ggplot(aes(x = hour_of_day)) +
  geom_bar(aes(fill = member_casual)) +
  scale_x_continuous(
    breaks = seq(0, 23, by = 2),  # Major breaks every 2 hours
    labels = paste0(seq(0, 23, by = 2), "h"),  # Add "h" suffix
    expand = c(0, 0)
  ) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_fill_brewer(palette = "Dark2", labels = c("Casual", "Member")) +
  labs(x = NULL, y = NULL, title = "Number of Rides in each Hour of Day") +
  theme(
    legend.title = element_blank()
  )

# number of rides per hour interactive
interactive_hour_rides <- ggplotly(hour_rides) |>
  style(
    traces = 1,
    name = "Casual"
  ) |>
  style(
    traces = 2,
    name = "Member"
  ) |>
  layout(
    legend = list(title = list(text = ""))
  )
  
# saving the rides per hour int. plot
saveWidget(interactive_hour_rides, "Quarto_imported_files/interactive_hour_rides.html",
           selfcontained = F,
           libdir = "shared_libs")


# ride duration median for hour of day by group
hour_ride_length <- df |>
  mutate(hour_of_day = hour(started_at)) |>
  group_by(hour_of_day, member_casual) |>
  summarize(ride_length_median = median(ride_length), .groups = "drop") |>
  ggplot(aes(x = hour_of_day, y = ride_length_median)) +
  geom_line(aes(colour = member_casual), linewidth = 1) +
  scale_x_continuous(
    breaks = seq(0, 23, by = 2),
    labels = paste0(seq(0, 23, by = 2), "h"),
    expand = c(0, 0)
  ) +
  scale_colour_brewer(palette = "Dark2", labels = c("Casual", "Member")) +
  scale_y_time(limits = c(0, NA)) +
  labs(x = NULL, y = NULL, title = "Median of Ride Duration by Hour of Day in each Group") +
  theme(
    legend.title = element_blank()
  )

# ride duration for hour interactive
interactive_hour_ride_length <- ggplotly(hour_ride_length) |>
  style(
  traces = 1,
  name = "Casual"
  ) |>
  style(
    traces = 2,
    name = "Member"
  ) |>
  layout(
    legend = list(title = list(text = ""))
  )

# saving the ride_length per hour interactive plot
saveWidget(interactive_hour_ride_length, "Quarto_imported_files/interactive_hour_ride_length.html",
           selfcontained = F,
           libdir = "shared_libs")

# number of rides for starting stations by group
start_station_count <- df |> 
  count(start_station_name, member_casual) |>
  rename(n_rides_start = n,
         station_name = start_station_name) |>
  arrange(desc(n_rides_start)) |>
  filter(!is.na(station_name))

# saving the start_station_count object to be loaded in quarto file
saveRDS(
  start_station_count, 
  file = "Quarto_imported_files/start_station_count.rds"
)

# number of rides for starting stations by group interactive
datatable(
  start_station_count,
  options = list(
    pageLength = 10,
    lengthMenu = list(c(10, 20, 30, 40, 50), c('10', '20', '30', '40', '50'))
  ),
  extensions = c('Buttons'),
  class = 'stripe hover'
)

# number of rides for destination stations by group
end_station_count <- df |> 
  count(end_station_name, member_casual) |>
  rename(n_rides_end = n,
         station_name = end_station_name) |>
  arrange(desc(n_rides_end)) |>
  filter(!is.na(station_name))

# saving the end_station_count object to be loaded in quarto file
saveRDS(
  end_station_count, 
  file = "Quarto_imported_files/end_station_count.rds"
)

# number of rides for destination stations by group interactive
datatable(
  end_station_count,
  options = list(
    pageLength = 10,
    lengthMenu = list(c(10, 20, 30, 40, 50), c('10', '20', '30', '40', '50'))
  ),
  extensions = c('Buttons'),
  class = 'stripe hover'
)


# combining the number of rides for start and end stations
total_station_count <- start_station_count |>
  full_join(end_station_count) |>
  mutate(total_station_n = n_rides_start + n_rides_end) |>
  arrange(desc(total_station_n))

# plotting the number of total rides of stations by group
total_station_rides <- total_station_count |>
  group_by(member_casual) |>
  slice_max(total_station_n, n = 10) |>
  arrange(desc(total_station_n)) |>
  ungroup() |>
  ggplot(aes(x = fct_reorder(station_name, total_station_n) |> fct_rev(),
             y = total_station_n)) +
  geom_col(aes(fill = member_casual), show.legend = F) +
  facet_wrap(~ member_casual, scales = "free_x") +
  labs(x = "Top 10 used stations", y = NULL, title = "Number of Rides for the 10 Most Used Stations per Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 20))
        ) +
  scale_fill_brewer(palette = "Dark2")

# total station rides plot interactive
interactive_total_station <- ggplotly(total_station_rides, tooltip = c("y")) |>
  hide_legend()
  

# saving total station rides plot interactive
saveWidget(interactive_total_station, "Quarto_imported_files/interactive_total_station.html",
           selfcontained = F,
           libdir = "shared_libs")

