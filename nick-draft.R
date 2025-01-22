library(tuneR)  # For reading WAV files
library(signal) # For signal processing
library(dplyr)
library(tibble)
library(tidyverse)
library(here)
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
wav <- here('data/6805.230201090825.wav')

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
freq_ranges <- generate_freq_ranges(1600, 100) 

df <- wav_to_frequency_csv(wav, .1, freq_ranges)



# merge csv file and txt file
target <- read_table(here('data/6805.230201090825-SS.txt'))

# clean annotations
target <- target[,1:8]

colnames(target) <- c('selection', 'view', 'x', 'channel', 'begin_time', 'end_time', 'low_freq', 'high_freq')

target <- target |> 
  mutate(view = paste(view, x)) |> 
  select(-x)

# create target column
df$song <- 0

for(i in 1:nrow(target)){
  begin <- target$begin_time[i]
  end <- target$end_time[i]
  
  df$song <- ifelse(df$time_start >= begin & df$time_start <= end | df$time_end >= begin & df$time_end <= end,
                    1,
                    df$song)
  
}

df$song <- factor(df$song, levels = c(1,0))

# Plot 1: full plot of average amplitude by frequency and song presence
df |> 
  group_by(song) |> 
  summarise(across(everything(), mean)) |> 
  select(-c(time_start,time_end)) |> 
  pivot_longer(cols = c("0-99Hz", "100-199Hz","200-299Hz","300-399Hz","400-499Hz","500-599Hz", "600-699Hz","700-799Hz",
                        "800-899Hz","900-999Hz","1000-1099Hz","1100-1199Hz","1200-1299Hz","1300-1399Hz","1400-1499Hz",
                        "1500-1599Hz"), names_to = 'freq') |> 
  mutate(freq = factor(freq, levels = c("0-99Hz", "100-199Hz","200-299Hz","300-399Hz","400-499Hz","500-599Hz", "600-699Hz","700-799Hz",
                                        "800-899Hz","900-999Hz","1000-1099Hz","1100-1199Hz","1200-1299Hz","1300-1399Hz","1400-1499Hz",
                                        "1500-1599Hz"))) |> 
  ggplot(aes(x = freq, y = value, group = song, color = song)) +
  geom_line() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x = "Frequency", 
       y = "Average Amplitude",
       title = "Average Amplitude By Frequency and Song Presence: 0Hz-1599Hz",
       color = "Whale Song") +
  scale_color_manual(values = c('red2', 'blue3'))


# Pot 2: zoom in on 0-500 Hz
df |> 
  group_by(song) |> 
  summarise(across(everything(), mean)) |> 
  select(-c(time_start,time_end)) |> 
  pivot_longer(cols = c("0-99Hz", "100-199Hz","200-299Hz","300-399Hz","400-499Hz","500-599Hz"), names_to = 'freq') |> 
  mutate(freq = factor(freq, levels = c("0-99Hz", "100-199Hz","200-299Hz","300-399Hz","400-499Hz","500-599Hz"))) |> 
  ggplot(aes(x = freq, y = value, group = song, color = song)) +
  geom_line() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x = "Frequency", 
       y = "Average Amplitude",
       title = "Average Amplitude By Frequency and Song Presence: 0Hz-599Hz",
       color = "Whale Song") +
  scale_color_manual(values = c('red2', 'blue3'))


df |> 
  group_by(song) |> 
  summarise(across(everything(), mean)) |> 
  select(-c(time_start,time_end)) |> 
  pivot_longer(cols = c("600-699Hz","700-799Hz",
                        "800-899Hz","900-999Hz","1000-1099Hz","1100-1199Hz","1200-1299Hz","1300-1399Hz","1400-1499Hz",
                        "1500-1599Hz"), names_to = 'freq') |> 
  mutate(freq = factor(freq, levels = c("600-699Hz","700-799Hz",
                                        "800-899Hz","900-999Hz","1000-1099Hz","1100-1199Hz","1200-1299Hz","1300-1399Hz","1400-1499Hz",
                                        "1500-1599Hz"))) |> 
  ggplot(aes(x = freq, y = value, group = song, color = song)) +
  geom_line() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x = "Frequency", 
       y = "Average Amplitude",
       title = "Average Amplitude By Frequency and Song Presence: 600Hz-1599Hz",
       color = "Whale Song") +
  scale_color_manual(values = c('red2', 'blue3'))



df |> group_by(song) |> 
  summarize(mean.amp.100 = mean(`100-199Hz`),
            sd.amp.100 = sd(`100-199Hz`))

# Create binary vector--is the amplitude at 100Hz higher than the mean of the 
# non whale songs + 3 SDs

2342 + 3*947


df$binary <- ifelse(df$`100-199Hz` >= 5183, 1, 0)


df |> 
  filter(time_start %% 0.5 == 0) |> 
  ggplot(aes(x = time_start, y = binary, color = song)) +
  geom_line() +
  scale_color_manual(values = c('red2', 'blue3'))

