library(tuneR)  # For reading WAV files
library(signal) # For signal processing
library(dplyr)
library(tibble)

<<<<<<< HEAD
 
wav <- "6805.230201090825.wav"
df <- merging_data(wav, end_freq = 800, step_size = 25, annotated = TRUE)

library(dplyr)

# df_long <- df %>%
#   pivot_longer(
#     cols = -c(time_start, time_end), # Adjust to your frequency range column naming
#     names_to = "frequency_range", # New column for frequency ranges
#     values_to = "amplitude" # New column for amplitude values
#   ) %>% # Exclude the 0-24Hz, 25-49Hz frequency range
#   mutate(
#     # Extract the numeric start of the range for ordering
#     numeric_start = as.numeric(gsub("-.*", "", frequency_range)),
#     frequency_range = factor(frequency_range, levels = unique(frequency_range[order(numeric_start)]))
#   ) %>%
#   select(-numeric_start) # Remove the helper column after ordering
# 
# # Plot a line graph for all amplitudes over time (including 0-24Hz)
# ggplot(df_long, aes(x = time_start, y = amplitude, color = frequency_range, group = frequency_range)) +
#   geom_line() +
#   labs(
#     title = "Amplitude Over Time for Different Frequency Ranges",
#     x = "Time Start (Seconds)",
#     y = "Amplitude",
#     color = "Frequency Range"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "right", # Adjust legend placement
#     plot.title = element_text(hjust = 0.5) # Center the title
#   )


# df_long <- df %>%
=======
library(tidyr)
library(ggplot2)


wav_to_frequency_csv <- function(wav_path,
                                 time_period, # This is in seconds 
                                 freq_ranges,
                                 start_time = NULL, end_time = NULL, # This is so we can put out the parts that only have the annotations
                                 output_path = NULL) {
  # Read WAV file
  wav_data <- readWave(wav_path)
  
  # Convert to mono if stereo
  if (wav_data@stereo) {
    audio_data <- rowMeans(wav_data@left, wav_data@right)
  } else {
    audio_data <- wav_data@left
  }
  # Get sample rate
  sample_rate <- wav_data@samp.rate
  
  # Calculate samples per period
  samples_per_period <- round(time_period * sample_rate)
  num_periods <- floor(length(audio_data) / samples_per_period)

  results <- list()
  results$time_start <- numeric()
  results$time_end <- numeric()
  
  column_names <- paste0(sapply(freq_ranges, function(x) paste0(x[1], "-", x[2], "Hz")))
  for (name in column_names) {
    results[[name]] <- numeric()
  }
  
  # Process each time period
  for (i in 0:(num_periods-1)) {
    start_idx <- i * samples_per_period + 1
    end_idx <- (i + 1) * samples_per_period
    
    # Get time segment
    segment <- audio_data[start_idx:end_idx]
    
    # Perform FFT
    fft_result <- abs(fft(segment))
    fft_freq <- seq(0, sample_rate/2, length.out = floor(length(segment)/2))
    
    # Only keep first half of FFT (due to symmetry)
    fft_magnitude <- fft_result[1:length(fft_freq)]
    
    # Add time information
    results$time_start <- c(results$time_start, i * time_period)
    results$time_end <- c(results$time_end, (i + 1) * time_period)
    
    # Calculate average magnitude for each frequency range
    for (j in seq_along(freq_ranges)) {
      range <- freq_ranges[[j]]
      min_freq <- range[1]
      max_freq <- range[2]
      column_name <- column_names[j]
      
      # Find frequencies within range
      mask <- fft_freq >= min_freq & fft_freq <= max_freq
      avg_magnitude <- mean(fft_magnitude[mask])
      
      results[[column_name]] <- c(results[[column_name]], avg_magnitude)
    }
  }
  df <- as_tibble(results)
  
  # Save to CSV if output path is provided
  if (!is.null(output_path)) {
    write.csv(df, output_path, row.names = FALSE)
  }
  
  # This is limiting the dataframe to get a snapshot
  if (!is.null(start_time)) {
    df <- df %>% filter(time_start >= start_time)
  }
  if (!is.null(end_time)) {
    df <- df %>% filter(time_start <= end_time)
  }
  return(df)
}

# Change this to the location you have it in
wav <- "/Users/raymondyang/Desktop/Data Capstone Project/Whale Project/wav files/6805.230201090825.wav"

# create the freq ranges    
generate_freq_ranges <- function(end_freq, # where do you want the freq to stop
                                 step_size) {
  num_ranges <- ceiling(end_freq / step_size)
  freq_ranges <- list() 
  for (i in 1:num_ranges) {
    start <- (i - 1) * step_size
    end <- min(start + step_size - 1, end_freq)
    freq_ranges[[i]] <- c(start, end)
  }
  return(freq_ranges)
}
freq_ranges <- generate_freq_ranges(300, 25) 




df3 <- wav_to_frequency_csv(wav, .1, freq_ranges, start_time = 100, end_time = 150)




# df_long <- df3 %>%
>>>>>>> 27bbd1f17d84d208a7a3248bd6e1022c2ee3999b
#   pivot_longer(
#     cols = -c(time_start, time_end), # Adjust to your frequency range column naming
#     names_to = "frequency_range", # New column for frequency ranges
#     values_to = "amplitude" # New column for amplitude values
#   )
# 
# # Plot a line graph for all amplitudes over time (including 0-24Hz)
# ggplot(df_long, aes(x = time_start, y = amplitude, color = frequency_range, group = frequency_range)) +
#   geom_line() +
#   labs(
#     title = "Amplitude Over Time for Different Frequency Ranges",
#     x = "Time Start (Seconds)",
#     y = "Amplitude",
#     color = "Frequency Range"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "right", # Adjust legend placement
#     plot.title = element_text(hjust = 0.5) # Center the title
#   )


<<<<<<< HEAD
library(tidymodels)
library(kernlab)

# Modeling

df2 <- merging_data(wav, 
                    end_freq = 600, 
                    step_size = 25, 
                    time_period = 0.1, 
                    annotated = TRUE)

#split 75% - 25% train/test split
train_data <- df2[0:13000,]
test_data <- df2[13001:18000,]

# cross val
set.seed(1738)
train_csv <- vfold_cv(train_split, v = 5)


#SVM test
svm_recipe <- recipe(song ~ ., data = train_data) |> 
  step_rm(time_start, time_end, annotation_num, time_interval) |>  # Remove unnecessary columns
  step_normalize(all_numeric_predictors())  # Normalize predictors

svm_tune <- svm_rbf(cost = tune(), rbf_sigma = tune()) |> 
  set_engine("kernlab") |> 
  set_mode("classification")

# Create a grid of hyperparameters for tuning
svm_grid <- grid_regular(cost(c(-5, 2)), rbf_sigma(range = c(0.01, 1)), levels = 5)

# Create a workflow
svm_tune_wf <- workflow() |> 
  add_recipe(svm_recipe) |> 
  add_model(svm_tune)

# Perform hyperparameter tuning
svm_grid_search <- tune_grid(
  svm_tune_wf,
  resamples = train_csv,
  grid = svm_grid,
  metrics = metric_set(f_meas, precision, recall)
)

# Fit the final SVM model using the best parameters
svm_final <- svm_rbf(cost = 1,  # Adjust based on tuning results
                     rbf_sigma = 0.1) |> 
  set_engine("kernlab") |> 
  set_mode("classification")

svm_final_wf <- workflow() |> 
  add_recipe(svm_recipe) |> 
  add_model(svm_final)

svm_final_fit <- svm_final_wf |> 
  fit(train_data)

# Make predictions on the test set
test_data$svm_pred <- predict(svm_final_fit, new_data = test_data)$.pred_class

# Compute performance metrics
f_meas(test_data, truth = song, estimate = svm_pred)
precision(test_data, truth = song, estimate = svm_pred)
recall(test_data, truth = song, estimate = svm_pred)

# Confusion matrix
conf_mat(test_data, truth = song, estimate = svm_pred)
=======
df_long <- df3 %>%
  pivot_longer(
    cols = -c(time_start, time_end), # Pivot all columns except `time_start` and `time_end`
    names_to = "frequency_range",
    values_to = "amplitude"
  ) %>%
  filter(!frequency_range %in% c("0-24Hz", "25-49Hz")) %>% # Exclude the 0-24Hz, 25-49Hz frequency range
  mutate(
    # Extract the numeric start of the range for ordering
    numeric_start = as.numeric(gsub("-.*", "", frequency_range)),
    frequency_range = factor(frequency_range, levels = unique(frequency_range[order(numeric_start)]))
  ) %>%
  select(-numeric_start) # Remove the helper column after ordering


# Plot a line graph for amplitudes over time (excluding the 0-24Hz, 24-49Hz)
ggplot(df_long, aes(x = time_start, y = amplitude, color = frequency_range, group = frequency_range)) +
  geom_line() +
  labs(
    title = "Amplitude Over Time for Frequency Ranges (Excluding 0-24Hz, 25-49Hz)",
    x = "Time Start (Seconds)",
    y = "Amplitude",
    color = "Frequency Range"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend placement
    plot.title = element_text(hjust = 0.5) # Center the title
  )
>>>>>>> 27bbd1f17d84d208a7a3248bd6e1022c2ee3999b
















