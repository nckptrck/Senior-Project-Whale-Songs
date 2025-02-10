source("Packages.R")
source("AWS.R")
source("Data Capstone Reading Wav Files.R")

 

# Change this to the location you have it in
wav <- "6805.230201090825.wav"


df3 <- merging_data(wav, end_freq = 800, step_size = 25, annotated = TRUE)



df_long <- df3 %>%
  pivot_longer(
    cols = -c(time_start, time_end), # Adjust to your frequency range column naming
    names_to = "frequency_range", # New column for frequency ranges
    values_to = "amplitude" # New column for amplitude values
  ) %>% # Exclude the 0-24Hz, 25-49Hz frequency range
  mutate(
    # Extract the numeric start of the range for ordering
    numeric_start = as.numeric(gsub("-.*", "", frequency_range)),
    frequency_range = factor(frequency_range, levels = unique(frequency_range[order(numeric_start)]))
  ) %>%
  select(-numeric_start) # Remove the helper column after ordering

# Plot a line graph for all amplitudes over time (including 0-24Hz)
ggplot(df_long, aes(x = time_start, y = amplitude, color = frequency_range, group = frequency_range)) +
  geom_line() +
  labs(
    title = "Amplitude Over Time for Different Frequency Ranges",
    x = "Time Start (Seconds)",
    y = "Amplitude",
    color = "Frequency Range"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend placement
    plot.title = element_text(hjust = 0.5) # Center the title
  )


# df_long <- df3 %>%
#   pivot_longer(
#     cols = -c(time_start, time_end), # Pivot all columns except `time_start` and `time_end`
#     names_to = "frequency_range",
#     values_to = "amplitude"
#   ) %>%
#   filter(!frequency_range %in% c("0-24Hz", "25-49Hz")) %>% # Exclude the 0-24Hz, 25-49Hz frequency range
#   mutate(
#     # Extract the numeric start of the range for ordering
#     numeric_start = as.numeric(gsub("-.*", "", frequency_range)),
#     frequency_range = factor(frequency_range, levels = unique(frequency_range[order(numeric_start)]))
#   ) %>%
#   select(-numeric_start) # Remove the helper column after ordering
# 
# 
# # Plot a line graph for amplitudes over time (excluding the 0-24Hz, 24-49Hz)
# ggplot(df_long, aes(x = time_start, y = amplitude, color = frequency_range, group = frequency_range)) +
#   geom_line() +
#   labs(
#     title = "Amplitude Over Time for Frequency Ranges (Excluding 0-24Hz, 25-49Hz)",
#     x = "Time Start (Seconds)",
#     y = "Amplitude",
#     color = "Frequency Range"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "right", # Adjust legend placement
#     plot.title = element_text(hjust = 0.5) # Center the title
#   )














