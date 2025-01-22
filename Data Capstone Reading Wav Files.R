library(tuneR)  # For reading WAV files
library(signal) # For signal processing
library(dplyr)
library(tibble)
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
wav <- "C:/Users/alexj/Downloads/6805.230201090825.wav"

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

df3 <- wav_to_frequency_csv(wav, .1, freq_ranges, start_time = 6.4, end_time = 7.7)
