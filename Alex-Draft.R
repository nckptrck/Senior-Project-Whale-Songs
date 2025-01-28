source("Packages.R")
source("AWS.R")
source("Data Capstone Reading Wav Files.R")


merging_data <- function(name, end_freq, step_size, time_period = .1, start_time = NULL, end_time = NULL, annotated = FALSE){
  freq_ranges <- generate_freq_ranges(end_freq = end_freq, step_size = step_size)
  
  # grabbing the wav file
  grab_wav_files(name)
  
  # creating the data frame
  wav_file <- wav_to_frequency_csv(name, time_period, freq_ranges,start_time, end_time)
  
  # if there are annotations reading in the annotations txt
  if (annotated){
    txt_name <- sub("\\.wav$", "-SS", name)
    target <- grab_txt_files(txt_name)
    
    # adding the indicator column
    wav_file$song <- 0
    for(i in 1:nrow(target)){
      begin <- target$begin_time[i]
      end <- target$end_time[i]
      
      wav_file$song <- ifelse(wav_file$time_start >= begin & wav_file$time_start <= end | wav_file$time_end >= begin & wav_file$time_end <= end,
                              1,
                              wav_file$song)
    }
    wav_file$song <- factor(wav_file$song, levels = c(1,0))
  }
  
  # deleting the file to save space
  if (file.exists(name)) {
    file.remove(name)
  }
  return (wav_file)
}

# this is how to use 

file <- "6805.230201090825.wav"   # need to change this to the files that you want to pull

df <- merging_data(file, 800, 50)

# model testing









