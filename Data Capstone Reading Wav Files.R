# ------------------------- Fast Fourier Transform -----------------------------
# used to fast fourier transform the wav files and make them tabular
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

# ------------------------ create the freq ranges ------------------------------
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

# -------------------- can read in data from AWS -------------------------------
merging_data <- function(name, end_freq, step_size, 
                         time_period = .1, # you can change this
                         start_time = NULL, end_time = NULL, # to shrink around one part if necessary
                         annotated = FALSE # This is for when the data is not annotated we can still read in the files into csv form
){
  freq_ranges <- generate_freq_ranges(end_freq = end_freq, step_size = step_size)
  
  # grabbing the wav file
  grab_wav_files(name)
  
  # creating the data frame
  wav_file <- wav_to_frequency_csv(name, time_period, freq_ranges, start_time, end_time)
  
  # if there are annotations reading in the annotations txt
  if (annotated){
    txt_name <- sub("\\.wav$", "-SS", name)
    target <- grab_txt_files(txt_name)
    
    # adding the indicator column & annotation (selection) number
    wav_file$song <- 0
    wav_file$annotation_num <- 0
    # adding a file name key for combining files
    wav_file$filename <- name
    for(i in 1:nrow(target)){
      begin <- target$begin_time[i]
      end <- target$end_time[i]
      
      # target column
      wav_file$song <- ifelse(wav_file$time_start >= begin & wav_file$time_start <= end | wav_file$time_end >= begin & wav_file$time_end <= end,
                              1,
                              wav_file$song)
      # annotation (selection) number
      wav_file$annotation_num <- ifelse(wav_file$time_start >= begin & wav_file$time_start <= end | wav_file$time_end >= begin & wav_file$time_end <= end,
                                        i,
                                        wav_file$annotation_num)
    }
    wav_file$song <- factor(wav_file$song, levels = c(1,0))
  }
  
  wav_file$time_interval <- rleid(wav_file$annotation_num)
  # deleting the file to save space
  if (file.exists(name)) {
    file.remove(name)
  }
  return (wav_file)
}

# ------------------------------ Documentation ---------------------------------
# for not annotated data can also use wav_to_frequency_csv but this would require you to have downloaded the data prior


# how to use merging_data
# file <- "6805.230201090825.wav"   # need to change this to the files that you want to pull

# df <- merging_data(file, 800, 50)

# ---------------------------- FFT to CSV From AWS -----------------------------
fft_to_csv <- function(name, end_freq, step_size, 
                         time_period = .1, # you can change this
                         start_time = NULL, end_time = NULL, # to shrink around one part if necessary
                         annotated = FALSE # This is for when the data is not annotated we can still read in the files into csv form
                       ){
  freq_ranges <- generate_freq_ranges(end_freq = end_freq, step_size = step_size)
  
  # grabbing the wav file
  grab_wav_files(name)
  
  # creating the data frame
  wav_file <- wav_to_frequency_csv(name, time_period, freq_ranges, start_time, end_time)
  
  # if there are annotations reading in the annotations txt
  if (annotated){
    txt_name <- sub("\\.wav$", "-SS", name)
    target <- grab_txt_files(txt_name)
    
    # adding the indicator column & annotation (selection) number
    wav_file$song <- 0
    wav_file$annotation_num <- 0
    for(i in 1:nrow(target)){
      begin <- target$begin_time[i]
      end <- target$end_time[i]
      
      # target column
      wav_file$song <- ifelse(wav_file$time_start >= begin & wav_file$time_start <= end | wav_file$time_end >= begin & wav_file$time_end <= end,
                              1,
                              wav_file$song)
      # annotation (selection) number
      wav_file$annotation_num <- ifelse(wav_file$time_start >= begin & wav_file$time_start <= end | wav_file$time_end >= begin & wav_file$time_end <= end,
                                        i,
                                        wav_file$annotation_num)
    }
    wav_file$song <- factor(wav_file$song, levels = c(1,0))
  }
  
  wav_file$time_interval <- rleid(wav_file$annotation_num)
  # deleting the file to save space
  if (file.exists(name)) {
    file.remove(name)
  }
  file_name <- sub("\\.wav$", "_fft.csv", name)
  write.csv(wav_file, file_name)
  return ('done')
}

# ----------------------- Creation of fft_csv ----------------------------------
# wavs_names <- read.table("Wav file names", header = TRUE, sep = " ", colClasses = c("character", "numeric"))
# wavs <- as.list(wavs_names$Filename)
# 
# for (i in wavs_names) {
#   name <- paste0(i, ".wav")
#   merging_data_melfcc_csv(name,numcep = 13, wintime = .2, hoptime = .1, maxfreq = 800, annotated = TRUE)
#   rm(wav_file)
#   gc()
#   
# }

# combining to one master csv 
# can edit the training one

# test <- c(31,27,29)
# validation <- c(17, 22,32,28,23)
# all_indices <- 1:38
# remaining <- setdiff(all_indices, c(test, validation))
# 
# train_fft <- data.frame()
# for(i in remaining){
#   wav_name <- wavs[[i]]
#   name <- paste0(wav_name, "_fft.csv")
#   file <- read.csv(name)
#   train_fft <- rbind(train_fft,file )
# }
# write.csv(train_fft, "train_fft.csv")

# need to change the number of songs in validation and test
# assign new annotation numbers so there won't be 2 annotation number 2
