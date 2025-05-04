# ------------------------------- Setup ----------------------------------------

# This allows us to use the functions that are found in these files.
# These fils must be in the same folder as Alex-Draft in order to use source

{
source("Packages.R")
# source("Creating Predictions.R")
source("AWS.R")
source("Data Capstone Reading Wav Files.R")
set.seed(1738)
library(here)
}


# ----------------------------- FTT Read-in ------------------------------------
# need to change this to the files that you want to pull

file <- "The file name hre"

df <- merging_data(file, 300, 20, annotated = TRUE)   # this code is from the Data Capstone Reading Wav Files

# model testing

# -------------------------- STFT Read in Function -----------------------------

# The stft takes the longest because the frequency bands are so small. 
# This transform does not perform as well so no need run

# n: Window length: this is the time unit larger means finer frequency detail. Ours is .1 sec to keep consistent
# overlap: Overlap Window: This is the length of time for the overlap to be .0707 secs
# freq: Max frequency: This limits the rows that are in the dataframe but must be calculated on the side. 
    # audio@samp.rate / n = bin size. Then do max frequency (that you want) / bin size to get your n

merging_data_stft_csv <- function(name, n = 16384, overlap = 6784, freq = 51,annotated = FALSE, window = NULL){
  # grabbing the wav file
  grab_wav_files(name)
  
  # creating the data frame
  audio <- readWave(name)
  audio_data <- as.numeric(audio@left)
  audio4 <- specgram(audio_data, n = n, Fs = audio@samp.rate, overlap = overlap)  # this is the transformation
  
  # this is taking the magnitude in order to remove the imaginary numbers
  a <- Mod(audio4$S[1:freq,]) # limiting how big the dataframe will be by capping the max frequency
  
  a <- t(a)
  
  # This adds the time for each row
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
  
  # THis will write the file out to a csv 
  # file_name <- sub("\\.wav$", "_stft.csv", name)
  # write.csv(wav_file, file_name)
  return ("done")
}

df <- merging_data_stft(file, annotated = TRUE)

# ------------------------------- melfcc ---------------------------------------

# This transform performed the best

# numcep: Number of cepstral coefficients. We used 13 but could go up to 20.
# wintime: Window Time. Used .1 seconds to be consistent with the other transforms
# hoptime: Hop Time. Used .05 to keep consistent with the short time fourier transform.

merging_data_melfcc_csv <- function(name, numcep, maxfreq, wintime, hoptime, annotated = FALSE){
  grab_wav_files(name)
  audio <- readWave(name)
  
  # This is a built in melfcc function that does the transform
  audio2 <- melfcc(audio, numcep = numcep, maxfreq = maxfreq, wintime = wintime, hoptime = hoptime)
  
  # Adding the time to the dataframe
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
  
  # can make the dataframe in to a csv
  # file_name <- sub("\\.wav$", "_melfcc.csv", name)
  # write.csv(wav_file, file_name)
  return ("done")
}

df1 <- merging_data_melfcc(file, numcep = 13, wintime = .2, hoptime = .1, maxfreq = 800, annotated = TRUE)
df <- df1

# Extra melfcc documentation I used to better understand the transformation

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


# -------------------------- Getting Wav Names ---------------------------------
{
  wavs_names <- read.table("Wav file names", header = TRUE, sep = " ", colClasses = c("character", "numeric"))
  wavs <- as.list(wavs_names$Filename)
}

# ----------------------------- Creating CSVs -----------------------------------

# This is the validation set that we choose. 
# wavs is the original list made
wavs_subset <- wavs[c(17, 22, 32, 28, 23, 14, 13, 8, 36, 9)]

# This works to convert all of the files into a csv after the transformation
for (i in wavs_subset) {
  name <- paste0(i, ".wav")
  merging_data_stft_csv(name, annotated = TRUE)
}

# This is just ot split up the names of files into different lists. 
test <- c(31,27,29)
validation <- c(17, 22, 32, 28, 23, 14, 13, 8, 36, 9)
all_indices <- 1:38
remaining <- setdiff(all_indices, c(test, validation))


# examples of how we combined the validation and test to make one csv that holds multiple files

test_melfcc <- data.frame()
for(i in validation){
  wav_name <- wavs[[i]]
  name <- paste0(wav_name, "_melfcc.csv")
  file <- read.csv(name)
  file <- file |> 
    mutate(file_name = wav_name)
  test_melfcc <- rbind(test_melfcc,file )
}
write.csv(test_melfcc, "validation_melfcc_10.csv")

validation_stft <- data.frame()
for(i in validation){
  wav_name <- wavs[[i]]
  name <- paste0(wav_name, "_stft.csv")
  file <- read.csv(name)
  file <- file |> 
    mutate(file_name = wav_name)
  validation_stft <- rbind(validation_stft,file)
  rm(file)
  gc()
}
write.csv(validation_stft, "validation_10_stft.csv")



# --------------------------- Train/Test Split ---------------------------------

# When the files were initially combinded the intervals did not match up and added file number column

fix_annotation_num <- function(file){
  file <- file |> 
    mutate(filenumber = cumsum(time_start == 0.0),
           time_interval = 1 + cumsum(song != lag(song, default = first(song))),) |> 
    dplyr::select(-X,-X.1)
  file$song <- factor(file$song, levels = c(1,0))
  write.csv(file, "test_melfcc.csv")
}

test_melfcc <- read.csv(here("test_melfcc.csv"))
fix_annotation_num(test_melfcc)


df_stft_train <- read.csv(here("training_stft.csv"))
df_stft_validation <- read.csv(here("validation_stft.csv"))
df_stft_test <- read.csv(here("test_stft.csv"))


df_melfcc_train <- read.csv(here("train_melfcc.csv"))
df_melfcc_validation <- read.csv(here("validation_melfcc.csv"))
df_melfcc_test <- read.csv(here("test_melfcc.csv"))


# Slicing the train set taking 20 random training files
{
  df_train <- df_stft_train |> 
    mutate(filenumber = cumsum(time_start == 0.0),
           song = as.factor(song)) |> 
    dplyr::select(V1:V51, everything()) |>
    dplyr::select(-X,-X.1)
  unique_file_numbers <- unique(df_train$filenumber)
  random_file_numbers <- sample(unique_file_numbers, 20, replace = FALSE)
  first_10_from_20 <- random_file_numbers_20[1:10]
  df_train <- df_train|> 
    filter(filenumber %in% first_10_from_20)
  write.csv(df_train, "df_stft_train_10_files.csv")
df_cvs <- vfold_cv(df_train, v = 5)  
  rm(df_stft_train)
  gc()
}

df_test <- df_test |> 
  mutate(song = as.factor(song))
#df_test <- df[13001:nrow(df), ]
df_validation <- df_melfcc_validation|> 
  mutate(song = as.factor(song))

# ------------------------------- Recipes --------------------------------------

{
whale_recipe <- recipe(song ~ ., data = df_train) |> 
  step_rm(time_start, time_end, annotation_num, time_interval,filenumber)
# step_rm(time_start, time_end.x, time_end.y,annotation_num) 

whale_recipe_knn <- recipe(song~., data = df_train) |> 
  step_rm(time_start, time_end, annotation_num, time_interval,filenumber) |> 
  step_normalize(all_numeric_predictors())
}

# ------------------------- Model 1: Random Forest -----------------------------

{
  whale_rf <- rand_forest(mtry = tune(), 
                        min_n = tune(),
                        trees = 20) |> 
  set_engine("ranger") |> 
  set_mode("classification")


whale_wrkf <- workflow() |>
  add_recipe(whale_recipe) |> 
  add_model(whale_rf) 

rf_grid <- grid_regular(mtry(c(1,30)),
                        min_n(c(1,30)),
                        levels = 3)
} # will probably take an hour

rf_grid_search <-
  tune_grid(
    whale_wrkf,
    resamples = df_cvs,
    grid = rf_grid,
    metrics = custom_metrics
  )

rf_grid_search %>% collect_metrics()|> filter(.metric == "precision")

# mtry = 10 , min_n = 10, 0.983     5 0.000145
# min_n = 33, 0.983     5 0.000152
# 1         1 precision binary     0.983     5 0.000199

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

best_knn <- select_best(knn_grid_search, metric = "precision")

knn_final <- finalize_model(knn_tune, best_knn)

knn_final <- nearest_neighbor(neighbors = 30) |> 
  set_engine('kknn') |> 
  set_mode("classification")
knn_final_wf <- workflow() |> 
  add_recipe(whale_recipe_knn) |> 
  add_model(knn_final)
knn_final_fit <- knn_final_wf |> fit(df_train)



# ------------------------- Training & Testing ---------------------------------

whale_rf_best <- rand_forest(mtry = 10,
                             min_n = 33,
                             trees = 1000) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

whale_wrkf_best <- workflow() |>
  add_recipe(whale_recipe) |> 
  add_model(whale_rf_best)

final_model <- fit(whale_wrkf_best, data = df_train)
df_test$rf.pred <- predict(final_model, new_data = df_test)$.pred_class


# random forest

df_validation$rf.pred <- predict(final_model, new_data = df_validation)$.pred_class
validation_metrics <- df_validation %>%
  metrics(truth = song, estimate = rf.pred)
print(validation_metrics)

conf_mat(df_validation, truth = song, estimate = rf.pred)

# This makes a dataframe. Each row is an interval and returns number of predictions, true postives, and total  
melf_val_res <- df_validation |> 
  mutate(rf.pred = ifelse(as.numeric(rf.pred) == 2,0,1),
         knn.pred = ifelse(as.numeric(knn.pred) == 2,0,1),
         y.song = ifelse(as.numeric(time_interval)%% 2 ==0, "yes", "no")) |> 
  group_by(time_interval,filenumber, y.song) |> 
  summarise(n.obs = n(),
    n.rf.roc = sum(rf.pred),
    n.knn = sum(knn.pred),
    t.f.postives.rf = n.obs - n.rf.roc,
    t.f.postivies.knn = n.obs - n.knn,
    .groups = "drop") 

write.csv(melf_val_res,"melfcc_val_res_knn30_rf10_33.csv")

df_test$rf.pred <- predict(rf_final_fit, new_data = df_test)$.pred_class
test_metrics <- df_test %>%
  metrics(truth = song, estimate = rf.pred)
print(test_metrics)


# Knn

df_validation$knn.pred <- predict(knn_final_fit, new_data = df_validation)$.pred_class
validation_metrics <- df_validation %>%
  metrics(truth = song, estimate = knn.pred)
print(validation_metrics)

# Final evaluation on the test set
df_test$knn.pred <- predict(knn_final_fit, new_data = df_test)$.pred_class
test_metrics <- df_test %>%
  metrics(truth = song, estimate = knn.pred)
print(test_metrics)


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

# -------------------------- Merging Results -----------------------------------

# This combines both stft and melfcc in order to compare results

{
  melfcc_valid <- read.csv("melfcc_val_res_full.csv")
  stft_valid <- read.csv("stft_val_res_full.csv")
  melfcc_valid <- melfcc_valid |> 
    dplyr::select(-X, -time_interval) |> 
    rename(knn.pred.mel = knn.pred,
           rf.pred.mel = rf.pred)
  stft_valid <- stft_valid |> 
    dplyr::select(-X, -time_interval)
  df_valid <- merge(stft_valid, melfcc_valid, by = c("time_start","time_end","filenumber", "song","annotation_num"), all.x = TRUE)
  
  df_valid <- arrange(df_valid, filenumber)
  df_valid[is.na(df_valid)] <- 0
}

df_valid <- read.csv("val_res_full.csv")



valid_results <-  df_valid |> 
  group_by(filenumber, annotation_num) |> 
  summarise(n.obs = n(),
            n.knn.stft = sum(knn.pred.stft),
            n.knn.mel = sum(knn.pred.mel),
            n.rf.stft = sum(rf.pred.stft),
            n.rf.mel = sum(rf.pred.mel))
# for seeing where there there is difference in predicitong the annotation number
valid_results |> 
  filter(n.knn.stft == 0| n.knn.mel ==0 | n.rf.stft == 0 | n.rf.mel == 0)


# for where no model predicts the model and how many different songs
valid_results |> 
  filter(n.knn.stft == 0& n.knn.mel ==0 & n.rf.stft == 0 & n.rf.mel == 0) |> 
  group_by(filenumber) |> 
  summarise(total = sum(n()))

df_valid %>%
  group_by(filenumber, anno) %>%  # Regroup by filenumber
  filter(song == 1) |> 
  summarise(
    total_n.obs = sum( n()),
    total_n.knn.stft = sum(sum(knn.pred.stft)),
    total_n.knn.mel = sum(sum(knn.pred.mel)),
    total_n.rf.stft = sum(sum(rf.pred.stft)),
    total_n.rf.mel = sum(sum(rf.pred.mel))
  )


# ------------------------ Stores Results in Separate csv ----------------------

# This helps to individually look at each wav file

{
  knn_final <- nearest_neighbor(neighbors = 30) |> 
    set_engine('kknn') |> 
    set_mode("classification")
  knn_final_wf <- workflow() |> 
    add_recipe(whale_recipe_knn) |> 
    add_model(knn_final)
  knn_final_fit <- knn_final_wf |> fit(df_train)
  df_validation$knn.pred <- predict(knn_final_fit, new_data = df_validation)$.pred_class
  
  whale_rf_best <- rand_forest(mtry = 10,
                               min_n = 33,
                               trees = 1000) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("classification")
  
  whale_wrkf_best <- workflow() |>
    add_recipe(whale_recipe) |> 
    add_model(whale_rf_best)
  
  final_model <- fit(whale_wrkf_best, data = df_train)
  df_validation$rf.pred <- predict(final_model, new_data = df_validation)$.pred_class
}

df_valid_res <- df_validation |> 
  dplyr::select(time_start,time_end,song,annotation_num,time_interval,filenumber,knn.pred,rf.pred)

write.csv(df_valid_res, "melfcc_val_res_full.csv")

## adding file names to validation test.csv
test <- c(31,27,29)
validation <- c(17, 22,32,28,23)

#results <- read.csv("")


split_results <- split(df_valid, df_valid$filenumber)

for (file_num in names(split_results)) {
  name <- paste0(wavs[validation[as.numeric(file_num)]], "_s_m_res")
  file_name <- paste0(name, ".csv")  # Create a unique file name
  write.csv(split_results[[file_num]], file_name, row.names = FALSE)
  cat("Written:", file_name, "\n")
}


# ---------------------- 230206100827 to selection table -----------------------

# Example of how to use the results to create a selection table

to_selection_table <- function(name # ex. 6805.230205030826
                               ){
  pred <- read.csv(paste0(name,"_s_m_res.csv"))
  
  pred$total.pred <- 0
  possible_preds <- c("knn.pred.mel", "knn.pred.stft", "rf.pred.stft", "rf.pred.mel") # Looks for each of the predictions
  for(pred in possible_preds) {
    if(pred %in% colnames(df)) {
      df$total.pred <- df$total.pred + as.numeric(levels(df[[pred]]))[df[[pred]]]
    }
  }
  
  predictions.v1 <- pred |> 
    mutate(rolling.avg = frollmean(total.pred, n = 30, fill = 0),
           final_pred = ifelse(rolling.avg>0, 1, 0),
           pred_number = rleid(final_pred),
           `Begin Time (s)` = time_start - 2,
           `End Time (s)` = time_end - 2)
  
  # visualize across time
  predictions.v1 |> 
    filter(final_pred != 0) |> 
    slice(1:100) |>
    ggplot(aes(x = `Begin Time (s)`)) +
    geom_point(aes(y = 1, color = "Prediction"), size = 1) +
    # Assuming you have a column representing song presence
    geom_point(aes(y = 1, color = "Song"), size = .5, alpha = .3) +
    scale_color_manual(values = c("Prediction" = "blue", "Song" = "red")) +
    labs(y = "Presence", color = "Type") +
    theme_minimal()
  
  selection.table <- predictions.v1 |> 
    filter(final_pred == 1) |> 
    group_by(pred_number) |> 
    summarise( `Begin Time (s)`= min(`Begin Time (s)`),
               `End Time (s)` = max(`End Time (s)`)) |>
    mutate(Selection = row_number(),
           View = "Spectrogram 1",
           Channel = 1) |> 
    dplyr::select(Selection,View, Channel, `Begin Time (s)`, `End Time (s)`)
  
  # write model predictions to .txt
  # this can be put straight into raveen
  write_tsv(selection.table, paste0("pred.stft.melfcc.",name ,".txt"))
}






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

# ----------------------- Comparing results in Raven ---------------------------

# This was manual error analysis and does not need to be run

file <- read.csv("230204030826_melfcc_res.csv") 
file <- read.csv("230205000826_melfcc_res.csv") 
file <- read.csv("230206100827_melfcc_res.csv") 

file <- read.csv("230205210826_melfcc_res.csv")
file <- read.csv("230205030826_melfcc_res.csv")

# file <-file |> 
#   filter(knn.pred == 1 | rf.pred == 1 | song == 1 )
write.csv(manual_errors, "230206100827_melfcc_false_postives.csv")


{
  manual_errors <- file |> 
    filter(song != 1) |> 
    dplyr::select(time_start, time_end, knn.pred, rf.pred)
  # they are all good not sure how to add that as a column   230204030826
  
  # I think these should also all be good but havent listen to it yet 230205000826
  # 15,16 is before call 84 and I think is in the same song just not annotated
  # there are 1 song missing where neither predicts the other 3 have one prediction from either knn or rf
  
  # 230206100827 
  # missed 45, 91,97, 105, 112 without a knn or rf
  # rf has a guess at 7 songs that knn missed completely
  
  
  
  res <- file |> 
    group_by(annotation_num) |> 
    summarise(n.obs = n(),
              n.knn = sum(knn.pred),
              n.rf = sum(rf.pred)
    ) 
  
  res |> 
    count( n.knn > 0 | n.rf > 0)
}




# -------------------------- Checking prediction window melfcc --------------------
# {
#   for (i in wavs_subset) {
#     name <- paste0(i, ".wav")
#     merging_data_melfcc_csv(name,numcep = 13, wintime = .2, hoptime = .1, maxfreq = 800, annotated = TRUE)
#     gc()
#   }
#   test_melfcc <- data.frame()
#   for(i in validation){
#     wav_name <- wavs[[i]]
#     name <- paste0(wav_name, "_melfcc.csv")
#     file <- read.csv(name)
#     file <- file |> 
#       mutate(file_name = wav_name)
#     test_melfcc <- dplyr::bind_rows(test_melfcc,file )
#   }
#   write.csv(test_melfcc, "validation_melfcc_10.csv")
#   df <- test_melfcc |> 
#     mutate(filenumber = cumsum(time_start == 0.0),
#            song = as.factor(song)) |>
#     dplyr::select(-X)
#   rf_melfcc <- readRDS("rf_final_melfcc.rds")
#   df$rf.pred.mel <- predict(rf_melfcc, new_data = df)$.pred_class
#   rm(rf_melfcc)
#   # This is for K Nearest Neighbors
#   knn_melfcc <- readRDS("knn_final_melfcc.rds")
#   df$knn.pred.mel <- predict(knn_melfcc, new_data = df)$.pred_class
#   rm(knn_melfcc)
#   df <- df |> 
#     dplyr::select("filenumber","time_start","time_end","knn.pred.mel","rf.pred.mel")
#   print("MELFCC is complete")
#   
#   write.csv(df, "full_10_val_melfcc.csv")
#   results <- validation_metrics(df, wavs, validation, wide_grid, p_grid)
# }

grab_txt_files2 <- function(name, Bucket = "s3://whale-recordings/", Place = "Avila", number = "2"){
  Object <- paste0("CPhydrophone/", Place, "/Deployment ", number, "/selection-tables/", name, ".txt")
  
  if (object_exists(object = Object, bucket = Bucket,
                    key = Sys.getenv("AWS_ACCESS_KEY_ID"), secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                    region = Sys.getenv("AWS_DEFAULT_REGION")) == TRUE ){
    
    table <- get_object(object = Object, bucket = Bucket,
                        key = Sys.getenv("AWS_ACCESS_KEY_ID"), secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                        region = Sys.getenv("AWS_DEFAULT_REGION"))
    
    file <- rawToChar(table)
    txt <- read_tsv(file)
    
    return(txt)
  }
  return ("The name is not correct")
}

validation_metrics_mel <- function(data,
                                   wavs,
                                   validation_idx,
                                   wide_grid, p_grid
){
  # create dataframe for results
  results <- data.frame(filenumber = numeric(),
                        prediction = character(),
                        Total.Calls = numeric(),
                        Precision = numeric(),
                        Recall = numeric(),
                        Detected.Calls = numeric(),
                        False.Positives = numeric(),
                        Missed.Calls = numeric(),
                        len.avg = numeric())
  # loop through validation files
  for(f_name in wavs[validation_idx]){
    # read in selection table (stored in validation folder in R project)
    txt_name <- paste0(f_name, "-SS")
    truth <- grab_txt_files2(txt_name)
    # remove duplicate time annotations
    truth <- truth |> 
      dplyr::filter(View == "Spectrogram 1")
    
    # filter data frame to specific file
    predictions <- data |>
      filter(filename == f_name) |> 
      dplyr::select(filenumber, time_start, time_end, knn.pred.mel, rf.pred.mel)
    for(t in wide_grid){
      # make predictions
      wide_preds <- get_model_predictions_mel(predictions,t = t)
      
      # get metrics
      wide_metrics <- data.frame(get_metrics(wide_preds, truth))
      
      # format for output
      wide_metrics <- wide_metrics |> mutate(filename = f_name,
                                             prediction = "wide",
                                             len.avg = t)
      # add to data frame
      results <- rbind(results,wide_metrics)
    }
    for(t in p_grid){
      # make predictions
      precise_preds <- get_model_predictions_mel(predictions,t = t)
      
      # get metrics
      precise_metrics <- data.frame(get_metrics(precise_preds, truth))
      # format for output
      precise_metrics <- precise_metrics |> mutate(filename = f_name,
                                                   prediction = "precise",
                                                   len.avg = t)
      # add to data frame
      results <- rbind(results,precise_metrics)
    }
    
  }
  # format results
  results |> 
    dplyr::select(filename, prediction, len.avg,
                  Total.Calls, Detected.Calls, False.Positives, Missed.Calls,
                  Precision, Recall)
}

# if straight from the predictions use as.numeric(levels(knn.pred.mel))[knn.pred.mel] + 
# as.numeric(levels(rf.pred.mel))[rf.pred.mel]

# if from csv use as.numeric(knn.pred.mel) + 
# as.numeric(rf.pred.mel)

get_model_predictions_mel <- function(preds,t = 30, write = F){
  # extract filename for output
  name <- preds$filename[1]
  output <- preds |> 
    # add other models into total.pred
    mutate(total.pred = as.numeric(knn.pred.mel) + 
             as.numeric(rf.pred.mel),
           rolling.avg = frollmean(total.pred, n = t, fill = 0),
           final_pred = ifelse(rolling.avg>0, 1, 0),
           pred_number= rleid(final_pred),
           `Begin Time (s)` = time_start, - (t/10),
           `End Time (s)` = time_end - (t/10) ) |> 
    filter(final_pred == 1) |> 
    group_by(pred_number) |> 
    summarise( `Begin Time (s)`= min(`Begin Time (s)`),
               `End Time (s)` = max(`End Time (s)`)) |>
    mutate(Selection = row_number(),
           View = "Spectrogram 1",
           Channel = 1) |> 
    dplyr::select(Selection,View, Channel, `Begin Time (s)`, `End Time (s)`)
  
  path <- paste0("model.pred.", name, ".txt", sep = "")
  if(write){
    write_tsv(output, path)
  }
  return(output)
}

get_metrics <- function(preds,truth){
  # loop through model predictions
  # define true and false positives
  tp <- 0
  fp <- 0
  total <- nrow(truth)
  for(i in 1:nrow(preds)) {
    row <- preds[i,]
    p.begin <- row$`Begin Time (s)`
    p.end <- row$`End Time (s)`
    # loop through selection table
    flag <- FALSE
    for(j in 1:nrow(truth)){
      trow <- truth[j,]
      t.begin <- trow$`Begin Time (s)`
      t.end <- trow$`End Time (s)`
      if(max(t.begin,p.begin) - min(t.end,p.end) <= 1){ # check if they overlap (within 1s)
        tp <- tp + 1
        flag <- T
      }
    }
    if(!flag){ # check if there was no overlap (false positive)
      fp <- fp + 1
    }
  }
  # number of missed annotations
  fn <- total - tp
  if(fn < 0){
    fn <- 0
  }
  
  # precision and recall
  p <- tp / (tp + fp)
  r <- tp/total
  return(list('Total Calls' = total,
              'Precision' = p,
              'Recall' = r,
              'Detected Calls' = tp,
              'False Positives' = fp,
              'Missed Calls' = fn))
}

{
  melfcc_results <- read.csv("full_10_val_melfcc.csv")
  wavs_names <- read.table("Wav file names", header = TRUE, sep = " ", colClasses = c("character", "numeric"))
  wavs <- as.list(wavs_names$Filename)
  validation <- c(17, 22, 32, 28, 23, 14, 13, 8, 36, 9)
  wide_grid <- seq(100,600, by = 50)
  p_grid <- seq(5,50, by = 5)
}

selected_filenames <- wavs[validation] 
selected_filenames <- unlist(selected_filenames)
melfcc_results <- melfcc_results |> 
  mutate(filename = selected_filenames[filenumber])

results <- validation_metrics_mel(melfcc_results, wavs, validation, wide_grid, p_grid)

results <- results |> 
  arrange(filename, len.avg)

write.csv(results, "validation_results_melfcc_10_song_calls.csv")

# ----------------- Making Predictions and metrics for STFT --------------------

{
  valid_stft <- read.csv("validation_10_stft.csv")
  df <- valid_stft |> 
        mutate(filenumber = cumsum(time_start == 0.0),
               song = as.factor(song)) |>
        dplyr::select(-X)
  rf_stft <- readRDS("rf_final_stft.rds")
  df$rf.pred.stft <- predict(rf_stft, new_data = df)$.pred_class
    rm(rf_stft)
    # This is for K Nearest Neighbors
    knn_stft <- readRDS("knn_final_stft.rds")
    df$knn.pred.stft <- predict(knn_stft, new_data = df)$.pred_class
    rm(knn_stft)
    df <- df |>
      dplyr::select("file_name","time_start","time_end","knn.pred.stft","rf.pred.stft")

    write.csv(df, "val_10_stft_pred.csv")
}
valid_stft <- read.csv("val_10_stft_pred.csv")
valid_stft <- valid_stft |> 
  mutate(filenumber = cumsum(time_start == 0.0))
write.csv(valid_stft, "val_10_stft_pred.csv")

wavs_subset_stft <- wavs[c(17, 22, 32, 28, 23)]
get_model_predictions_stft <- function(preds,t = 30, write = F){
  # extract filename for output
  name <- preds$filename
  output <- preds |> 
    # add other models into total.pred
    mutate(total.pred = as.numeric(knn.pred.stft) + 
             as.numeric(rf.pred.stft),
           rolling.avg = frollmean(total.pred, n = t, fill = 0),
           final_pred = ifelse(rolling.avg>0, 1, 0),
           pred_number= rleid(final_pred),
           `Begin Time (s)` = time_start, - (t/10),
           `End Time (s)` = time_end - (t/10) ) |> 
    filter(final_pred == 1) |> 
    group_by(pred_number) |> 
    summarise( `Begin Time (s)`= min(`Begin Time (s)`),
               `End Time (s)` = max(`End Time (s)`)) |>
    mutate(Selection = row_number(),
           View = "Spectrogram 1",
           Channel = 1) |> 
    dplyr::select(Selection,View, Channel, `Begin Time (s)`, `End Time (s)`)
  
  path <- paste0("model.pred.", name, ".txt", sep = "")
  if(write){
    write_tsv(output, path)
  }
  output
}

validation_metrics_stft <- function(data,
                                    wavs,
                                    validation_idx,
                                    wide_grid, p_grid){
  results <- data.frame(filenumber = numeric(),
                        prediction = character(),
                        Total.Calls = numeric(),
                        Precision = numeric(),
                        Recall = numeric(),
                        Detected.Calls = numeric(),
                        False.Positives = numeric(),
                        Missed.Calls = numeric(),
                        len.avg = numeric())
  # loop through validation files
  for(f_name in wavs[validation_idx]){
    # read in selection table (stored in validation folder in R project)
    txt_name <- paste0(f_name, "-SS")
    truth <- grab_txt_files2(txt_name)
    # remove duplicate time annotations
    truth <- truth |> 
      dplyr::filter(View == "Spectrogram 1")
    
    # filter data frame to specific file
    predictions <- data |>
      filter(filename == f_name) |> 
      dplyr::select(filenumber, time_start, time_end, knn.pred.stft, rf.pred.stft)
    for(t in wide_grid){
      # make predictions
      wide_preds <- get_model_predictions_stft(predictions,t = t)
      
      # get metrics
      wide_metrics <- data.frame(get_metrics(wide_preds, truth))
      
      # format for output
      wide_metrics <- wide_metrics |> mutate(filename = f_name,
                                             prediction = "wide",
                                             len.avg = t)
      # add to data frame
      results <- rbind(results,wide_metrics)
    }
    for(t in p_grid){
      # make predictions
      precise_preds <- get_model_predictions_stft(predictions,t = t)
      
      # get metrics
      precise_metrics <- data.frame(get_metrics(precise_preds, truth))
      # format for output
      precise_metrics <- precise_metrics |> mutate(filename = f_name,
                                                   prediction = "precise",
                                                   len.avg = t)
      # add to data frame
      results <- rbind(results,precise_metrics)
    }
    
  }
  # format results
  results |> 
    dplyr::select(filename, prediction, len.avg,
                  Total.Calls, Detected.Calls, False.Positives, Missed.Calls,
                  Precision, Recall)
}

{
  stft_results <- read.csv("val_stft_pred.csv")
  wavs_names <- read.table("Wav file names", header = TRUE, sep = " ", colClasses = c("character", "numeric"))
  wavs <- as.list(wavs_names$Filename)
  validation <- c(17, 22, 32, 28, 23)
  wide_grid <- seq(100,600, by = 50)
  p_grid <- seq(5,50, by = 5)

selected_filenames <- wavs[validation] 
selected_filenames <- unlist(selected_filenames)
stft_results <- stft_results |> 
  mutate(filename = selected_filenames[filenumber])
}

stft_validation_metrics <- validation_metrics_stft(stft_results, wavs, validation, wide_grid, p_grid)

write.csv(as.data.frame(stft_validation_metrics), "validation_results_STFT_5_song_calls.csv", row.names = FALSE)


# ------------------- Getting Metrics for Melfcc after grouping -------------------------
results_melfcc <- read.csv("validation_results_melfcc_10_song_calls.csv")


results_melfcc1 <- results_melfcc |> 
  group_by(prediction, len.avg) |>
  summarise(mean.precision = mean(Precision),
            sd.precision = sd(Precision),
            mean.recall = mean(Recall),
            sd.recall = sd(Recall)) |> 
  slice_max(mean.recall, n = 10)

# ------------------- Getting Metrics for STFT after grouping -------------------------


results_STFT <- read.csv("validation_results_STFT_5_song_calls.csv")


results_STFT1 <- results_STFT |> 
  group_by(prediction, len.avg) |>
  summarise(mean.precision = mean(Precision),
            sd.precision = sd(Precision),
            mean.recall = mean(Recall),
            sd.recall = sd(Recall)) |> 
  slice_max(mean.recall, n = 10)


# ------------------------- Humpback Whale Testing -----------------------------

# use this one 671658014.180928183606 

melfcc_humpack <- read.csv(here('melfcc_180928183606.csv'))
table_melfcc_humpback <- read.table(here('table_melfcc_180928183606.txt'), header = TRUE )

melfcc_humpack$song <- as.factor(melfcc_humpack$song)
melfcc_humpack$rf.pred <- as.factor(melfcc_humpack$rf.pred)
precision(melfcc_humpack, truth = song, estimate = rf.pred)

table_melfcc_humpback |> 
  filter(time_interval %% 2 == 1 & n.rf.roc != 0) |> 
  count()

# 
# ------------------------ Breaking up the wav file 

# to is a c(,) and from is a c(,)


# this only works if splitting into two files
split_file <- function(name, start, end){
  for(i in 1:1) {
    training <- readWave(name, from = start[i], to = end[i], units = "minutes") # it breaks right here
    i <- i + 1
    testing <- readWave(name, from = start[i], to = end[i], units = "minutes")
  }
  return(c(training, testing)) # cannot return both 
  
}

# the for loop works 
# Error in readBin(con, int, n = N, size = bytes, signed = (bytes != 1), : invalid 'n' argument

both <- split_file(name, c(0,60), c(60,90))
training <- both[[1]]
testing <- both[[2]]


# there is a lot of fluxuations so then max freq should be a least 1000 or 1200


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












# --------------------- Tested Transforms that did not work --------------------
# -------------------- Depreciated STFT Reading Function
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

# -------------------------- Temporal Context

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