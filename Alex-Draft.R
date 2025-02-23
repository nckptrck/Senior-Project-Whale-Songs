# ------------------------------- Setup ----------------------------------------
{
source("Packages.R")
source("AWS.R")
source("Data Capstone Reading Wav Files.R")
set.seed(1738)

# library(keras)
# library(tensorflow)

custom_metrics <- metric_set(accuracy, precision, recall, f_meas)
file <- "6805.230201090825.wav"}
file <- "6805.230201180825.wav"
file <- "6805.230201150825.wav"

test <- "6805.230204003826.wav"

# -------------------------- Getting Wav Names ---------------------------------

wavs_names <- read.table("Wav file names", header = TRUE, sep = " ", colClasses = c("character", "numeric"))
wavs <- as.list(wavs_names$Filename)


# ----------------------------- FTT Read-in ------------------------------------
# need to change this to the files that you want to pull

df <- merging_data(file, 300, 20, annotated = TRUE)

# model testing

# -------------------------- STFT Read in Function -----------------------------
merging_data_stft <- function(name, n = 16384, overlap = 6784, annotated = FALSE, window = NULL){
  # grabbing the wav file
  grab_wav_files(name)
  
  # creating the data frame
  audio <- readWave(name)
  audio_data <- as.numeric(audio@left)
  audio4 <- specgram(audio_data, n = n, Fs = audio@samp.rate, overlap = overlap)
  
  # this is taking the magnitude in order to remove the imaginary numbers
  a <- Mod(audio4$S[1:137,])
  a <- t(a)
  wav_file <- as.data.frame(a) %>%
    mutate(
      time_start =(seq(0, by = 0.1, length.out = nrow(a))),
      time_end = time_start + .1)
  
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
                              1, wav_file$song)
      # annotation (selection) number
      wav_file$annotation_num <- ifelse(wav_file$time_start >= begin & wav_file$time_start <= end | wav_file$time_end >= begin & wav_file$time_end <= end,
                                        i, wav_file$annotation_num)
    }
    wav_file$song <- factor(wav_file$song, levels = c(1,0))
  }
  
  wav_file$time_interval <- rleid(wav_file$annotation_num)
  # deleting the file to save space
  if (file.exists(name)) {
    file.remove(name)
  }
  file_name <- sub("\\.wav$", "_stft.csv", name)
  write.csv(wav_file, file_name)
  return ("done")
}

df <- merging_data_stft(test, annotated = TRUE)

write.csv(df, "230204003826_Test_STFT.csv")

# ------------------------------- melfcc ---------------------------------------
merging_data_melfcc <- function(name, numcep, maxfreq, wintime, hoptime, annotated = FALSE){
  grab_wav_files(name)
  audio <- readWave(name)
  
  audio2 <- melfcc(audio, numcep = numcep, maxfreq = maxfreq, wintime = wintime, hoptime = hoptime)
  
  wav_file <- as.data.frame(audio2) %>%
    mutate(
      time_start =(seq(0, by = hoptime, length.out = nrow(audio2))),
      time_end = time_start + hoptime)
  
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
                              1, wav_file$song)
      # annotation (selection) number
      wav_file$annotation_num <- ifelse(wav_file$time_start >= begin & wav_file$time_start <= end | wav_file$time_end >= begin & wav_file$time_end <= end,
                                        i, wav_file$annotation_num)
    }
    wav_file$song <- factor(wav_file$song, levels = c(1,0))
  }
  
  wav_file$time_interval <- rleid(wav_file$annotation_num)
  # deleting the file to save space
  if (file.exists(name)) {
    file.remove(name)
  }
  file_name <- sub("\\.wav$", "_melfcc.csv", name)
  write.csv(wav_file, file_name)
  return ("done")
}

df1 <- merging_data_melfcc(file, numcep = 13, wintime = .2, hoptime = .1, maxfreq = 800, annotated = TRUE)
df <- df1


# ----------------------------- Creating CSVs -----------------------------------
start <- wavs[[31]]
wavs_subset <- wavs[31:length(wavs)]
for (i in wavs_subset) {
  name <- paste0(i, ".wav")
  merging_data_melfcc(name,numcep = 13, wintime = .2, hoptime = .1, maxfreq = 800, annotated = TRUE)
  rm(wav_file)
  gc()

}

test <- c(31,27,29)
validation <- c(17, 22,32,28,23)
all_indices <- 1:38
remaining <- setdiff(all_indices, c(test, validation))


train_melfcc <- data.frame()
for(i in remaining){
  wav_name <- wavs[[i]]
  name <- paste0(wav_name, "_melfcc.csv")
  file <- read.csv(name)
  train_melfcc <- rbind(train_melfcc,file )
}
write.csv(train_melfcc, "train_melfcc.csv")

validation_stft <- data.frame()
for(i in remaining){
  wav_name <- wavs[[i]]
  name <- paste0(wav_name, "_stft.csv")
  file <- read.csv(name)
  validation_stft <- rbind(validation_stft,file)
  rm(file)
  gc()
}
write.csv(validation_stft, "training_stft.csv")

## ----------------------- Melfcc Documentation --------------------------------
audio <- readWave(file)
audio2 <- (melfcc(audio, numcep = 13, maxfreq = 800, wintime = .1, hoptime= .05))
# each row is .1 long but because of the hop they start at every .05 seconds
# because there  is some overlap with the amount of overlap

audio3 <- as.data.frame(audio2)

audio3 <- melfcc(audio, numcep = 20, maxfreq = 800, wintime = .05, hoptime = .025)
# I think that each row is .5 seconds of time where each row start is increasing by .025 seconds

audio4 <- melfcc(audio, nbands = 80, numcep = 13, maxfreq = 800, wintime = .1, hoptime = .05)
# most fo the values appear to be larger not sure what changing the number of bands would do or why 

audio4_diff_2 <- audio4 - audio2

mfccs <- melfcc(
  audio,                # Input audio signal
  numcep = 20,          # Number of MFCCs
  wintime = 0.05,      # Window length (25 ms)
  hoptime = 0.025,     # Frame shift (12.5 ms, 50% overlap)
  nbands = 40,          # Number of Mel filters
  preemph = 0.97,       # Pre-emphasis coefficient
  fbtype = "htkmel"     # Filter bank type
)


# -------------------------- Temporal Context ----------------------------------

create_temporal_data <- function(df, window_size) {
  windowed_data <- list()
  num_windows <- nrow(df) - window_size + 1
  
  # If num_windows is less than 1, pad the dataframe to ensure at least one window
  if (num_windows < 1) {
    padding <- df[rep(nrow(df), window_size - nrow(df)), ]
    df <- rbind(df, padding)
    num_windows <- 1
  }
  
  for (i in 1:num_windows) {
    window <- df[i:(i + window_size - 1), ]
    
    # Calculate the mean of each frequency range within the window
    window_means <- colMeans(window[, grep("Hz", colnames(df))])
    
    # Add the time_start and time_end of the window
    window_means <- c(time_start = window$time_start[1], 
                      time_end = window$time_end[window_size], 
                      window_means)
    windowed_data[[i]] <- window_means
  }
  windowed_df <- do.call(rbind, windowed_data) %>% as_tibble()
  # Ensure the rows align by matching time_start
  merged_df <- df %>%
    left_join(windowed_df, by = c("time_start" = "time_start"))
  
  return(merged_df)
}

df2 <- create_temporal_data(df1, 5)

df <- df2 |> 
  filter(!(if_any(everything(), is.na) & song == 0))

# right now to make sure not to use temporal_data it is not helping
df <- df1

df_cvs <- vfold_cv(df, v = 5)


# --------------------------- Train/Test Split ---------------------------------
df1 <- read.csv(here("230201090825_STFT.csv"))
df2 <- read.csv(here("230201180825_STFT.csv"))
df3 <- read.csv(here("230201150825_STFT.csv"))
df_test <- read.csv(here("230204003826_Test_STFT.csv"))
df <- rbind(df1, df2, df3)

{
  df_train <- df |> 
    mutate(song = as.factor(song)) |> 
    dplyr::select(-X)
  df_test <- df_test |> 
    mutate(song = as.factor(song))
#df_test <- df[13001:nrow(df), ]
df_cvs <- vfold_cv(df_train, v = 5)

whale_recipe <- recipe(song ~ ., data = df_train) |> 
  step_rm(time_start, time_end, annotation_num, time_interval)
# step_rm(time_start, time_end.x, time_end.y,annotation_num) 

whale_recipe_knn <- recipe(song~., data = df_train) |> 
  step_rm(time_start, time_end, annotation_num, time_interval) |> 
  step_normalize(all_numeric_predictors())
}

# ------------------------- Model 1: Random Forest -----------------------------

{
  whale_rf <- rand_forest(mtry = tune(), 
                        min_n = tune(), 
                        trees = tune()) |> 
  set_engine("ranger") |> 
  set_mode("classification")


whale_wrkf <- workflow() |>
  add_recipe(whale_recipe) |> 
  add_model(whale_rf) 

rf_grid <- grid_regular(mtry(c(1,10)),
                        min_n(c(1,10)),
                        trees(c(1,30)),
                        levels = 20)
}

rf_grid_search <-
  tune_grid(
    whale_wrkf,
    resamples = df_cvs,
    grid = rf_grid,
    metrics = custom_metrics
  )

tuning_metrics <- rf_grid_search %>% collect_metrics()|> filter(.metric == "precision") %>%  slice_max(mean)

# mtry = 30 , min_n = 20

# ------------------------------ KNN Model -------------------------------------

{
  knn_tune <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

neighbor_grid <- grid_regular(neighbors(c(1,60)),
                              levels = 3)

knn_tune_wf <- workflow() |> 
  add_recipe(whale_recipe_knn) |> 
  add_model(knn_tune)}

knn_grid_search <- tune_grid(knn_tune_wf,
                             resamples = df_cvs,
                             grid = neighbor_grid,
                             metrics = metric_set(roc_auc, precision, recall))

knn_grid_search |> collect_metrics() |> filter(.metric == 'precision')


# ------------------------- Training & Testing ---------------------------------

whale_rf_best <- rand_forest(mtry = 30,
                             min_n = 20,
                             trees = 1000) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

whale_wrkf_best <- workflow() |>
  add_recipe(whale_recipe) |> 
  add_model(whale_rf_best)

final_model <- fit(whale_wrkf_best, data = df_train)
df_test$rf.pred <- predict(final_model, new_data = df_test)$.pred_class


# fitting best model (Precision)
knn <- nearest_neighbor(neighbors = 30) %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_wf <- workflow() |> 
  add_recipe(whale_recipe_knn) |> 
  add_model(knn)
knn_fit <- knn_wf |> fit(df_train)

# predict on test set
df_test$knn.pred <- predict(knn_fit, new_data = df_test)$.pred_class


# ------------------------------ Results ---------------------------------------

importance_scores <- final_model$fit$fit$fit$variable.importance
importance_scores <- sort(importance_scores, decreasing = TRUE)
print(importance_scores)

# Compute accuracy, precision, recall, and F1-score
precision(df_test, truth = song, estimate = knn.pred)
precision(df_test, truth = song, estimate = rf.pred)

# confusion matrix
conf_mat(df_test, truth = song, estimate = knn.pred)
conf_mat(df_test, truth = song, estimate = rf.pred)

# -------------------------- Time Interval Testing -----------------------------

# proportion of each whale song being predicted as 1
df_test |> 
  mutate(knn.pred = ifelse(as.numeric(knn.pred) == 2,0,1),
         rf.pred = ifelse(as.numeric(rf.pred) == 2,0,1))  |> 
  group_by(annotation_num) |> 
  summarise(n.obs = n(),
            n.knn = sum(knn.pred),
            prop.knn = sum(knn.pred)/n(),
            n.rf.roc = sum(rf.pred),
            prop.rf.roc = sum(rf.pred)/n()
  )

# table with time intervals
res <- df_test |> 
  mutate(knn.pred = ifelse(as.numeric(knn.pred) == 2,0,1),
         rf.pred = ifelse(as.numeric(rf.pred) == 2,0,1)) |> 
  group_by(time_interval) |> 
  summarise(n.obs = n(),
            n.knn = sum(knn.pred),
            prop.knn = sum(knn.pred)/n(),
            n.rf.roc = sum(rf.pred),
            prop.rf.roc = sum(rf.pred)/n())

res

# ------------------------- Writing the Results --------------------------------
# this is specifically for the stft sthere is something off about the leveling
df_test$knn.pred <- factor(df_test$knn.pred, levels = levels(df_test$song))

conf_mat(df_test, truth = song, estimate = knn.pred)
res <- df_test |> 
  mutate(knn.pred = ifelse(as.numeric(knn.pred) == 2,0,1)) |> 
  group_by(time_interval) |> 
  summarise(n.obs = n(),
            n.knn = sum(knn.pred),
            prop.knn = sum(knn.pred)/n())

res |>  
  filter(time_interval%%2 == 0) |> 
  mutate(time_interval = time_interval/2,
         true_postives = n.obs - n.knn) |> 
  filter(true_postives !=0) |> 
  dplyr::select(time_interval, n.obs, true_postives)
res |>  
  filter(time_interval%%2 == 0) |> 
  mutate(time_interval = time_interval/2,
         true_postives = n.obs - n.knn) |> 
  filter(true_postives !=0) |> 
  dplyr::select(time_interval, n.obs, true_postives) |>
  summarise(count = n())

res <- res |> 
  mutate(true_positives = n.obs - n.knn)

write.csv(res, "230204003826 using STFT knn = 30.csv")

# ---------------------- Testing on Another Annotated File ---------------------

file1 <- '6805.230201150825.wav'
new_file <- merging_data(file1, 600, 25, annotated = TRUE)

test_prdictions2  <- predict(final_model, new_data = new_file, type = "class")

test_results2 <- new_file %>%
  bind_cols(test_prdictions2)

test_results2 %>%
  conf_mat(truth = song, estimate = .pred_class)
test_results2 %>%
  custom_metrics(truth = song, estimate = .pred_class)

file2 <- '6805.230201180825.wav'
new_file1 <- merging_data(file2, 600, 25, annotated = TRUE)

test_predictions2  <- predict(final_model, new_data = new_file1, type = "class")

test_results3 <- new_file1 %>%
  bind_cols(test_predictions2)

test_results3 %>%
  conf_mat(truth = song, estimate = .pred_class)
test_results3 %>%
  custom_metrics(truth = song, estimate = .pred_class)










# -------------------- Depreciated STFT Reading Function -----------------------
merging_data_stft_do_not_use <- function(name, end_freq, step_size,
                                         time_period = .1, # you can change this
                                         start_time = NULL, end_time = NULL, # to shrink around one part if necessary
                                         annotated = FALSE, # This is for when the data is not annotated we can still read in the files into csv form
                                         window_length = 1024,
                                         overlap = 0.5, window_type = "hanning"){
  
  freq_ranges <- generate_freq_ranges(end_freq = end_freq, step_size = step_size)
  
  # grabbing the wav file
  grab_wav_files(name)
  
  # creating the data frame
  wav_file <- wav_to_df(name, time_period, freq_ranges,window_length = 1024, overlap = 0.5, window_type = "hanning")
  
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
  
  # deleting the file to save space
  if (file.exists(name)) {
    file.remove(name)
  }
  return (wav_file)
}

