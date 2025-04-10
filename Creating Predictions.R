# ------------------------------- Setup ----------------------------------------
{
  source("Packages.R")
  source("AWS.R")
  source("Data Capstone Reading Wav Files.R")
  set.seed(1738)
  # library(keras)
  # library(tensorflow)
  library(here)
}



# do not run need to update the helper function accommodate the need rds fitted models

# -------------------- helper functions for the one below ----------------------
merging_data_stft <- function(audio, n = 16384, overlap = 6784, freq = 51, window = NULL){
  audio_data <- as.numeric(audio@left)
  audio4 <- specgram(audio_data, n = n, Fs = audio@samp.rate, overlap = overlap)
  
  # this is taking the magnitude in order to remove the imaginary numbers
  a <- Mod(audio4$S[1:freq,])
  a <- t(a)
  wav_file <- as.data.frame(a) %>%
    mutate(
      time_start =(seq(0, by = 0.1, length.out = nrow(a))),
      time_end = time_start + .1)
  print("Done with transformation")
  return (wav_file)
}

stft_pipeline <- function(audio, model){
  df <- merging_data_stft(audio)
  
  rf_stft <- readRDS("rf_final_stft.rds")
  df$rf.pred.stft <- predict(rf_stft, new_data = df)$.pred_class
  rm(rf_stft)
  knn_stft <- readRDS("knn_final_stft.rds")
  df$knn.pred.stft <- predict(knn_stft, new_data = df)$.pred_class
  rm(knn_stft)
  
  # This is for random forest
  if(model == "rf"){
    df <- df |> 
      dplyr::select("time_start","time_end","rf.pred.stft")
  }
  # This is for K Nearest Neighbors
  if(model == "knn"){
    df <- df |> 
      dplyr::select("time_start","time_end","knn.pred.stft")
  }
  if (model == 'both'){
    df <- df |> 
      dplyr::select("time_start","time_end","knn.pred.stft","rf.pred.stft")
  }
  print("STFT is complete")
  return(df)
}

merging_data_melfcc <- function(audio){
  audio2 <- melfcc(audio, numcep = 13, maxfreq = 600, wintime = .2, hoptime = .1)
  
  wav_file <- as.data.frame(audio2) %>%
    mutate(
      time_start =(seq(0, by = hoptime, length.out = nrow(audio2))),
      time_end = time_start + hoptime)
  print("meflcc transform done")
  return (wav_file)
}

melfcc_pipeline <- function(audio, model ){
  df <- merging_data_melfcc(audio)
  rf_melfcc <- readRDS("rf_final_melfcc.rds")
  df$rf.pred.mel <- predict(rf_melfcc, new_data = df)$.pred_class
  rm(rf_melfcc)
  knn_melfcc <- readRDS("knn_final_melfcc.rds")
  df$knn.pred.mel <- predict(knn_melfcc, new_data = df)$.pred_class
  rm(knn_melfcc)
  
  # This is for random forest 
  if ( model == "rf") {
    df <- df |> 
      dplyr::select("time_start","time_end","rf.pred.mel")
  }
  # This is for K Nearest Neighbors
  if (model == "knn"){
    df <- df |> 
      dplyr::select("time_start","time_end","knn.pred.mel")
  }
  if (model == 'both'){
    df <- df |> 
      dplyr::select("time_start","time_end","knn.pred.mel","rf.pred.mel")
  }
  print("MELFCC is complete")
  return(df)
}

# need to change this to make sure the second part works
to_selection_table <- function(df, name, t){
  df$total.pred <- 0
  possible_preds <- c("knn.pred.mel", "knn.pred.stft", "rf.pred.stft", "rf.pred.mel")
  for(pred in possible_preds) {
    if(pred %in% colnames(df)) {
      df$total.pred <- df$total.pred + as.numeric(levels(df[[pred]]))[df[[pred]]]
    }
  }
  for (i in t){
    predictions.v1 <- df |>
      # need to figure out how to only make it so the correct pred variables are used otherwise it will give error
      mutate(rolling.avg = frollmean(total.pred, n = t, fill = 0),
             final_pred = ifelse(rolling.avg>0, 1, 0),
             pred_number= rleid(final_pred),
             `Begin Time (s)` = time_start, - (t/10),
             `End Time (s)` = time_end - (t/10)) #adjust timeframe proportional to n (3 is too far)
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
    # this can be put straight into raven
    write_tsv(selection.table, paste0("preds.",(t/10),"sec.",name ,".txt")) 
  }

  # write_tsv(predictions.30, paste0("pred.30sec.",name ,".txt"))
}

# ----------------------- Function output selection tables ---------------------

create_selection_table <- function(name, # ex. "6805.230201090825.wav"
                                   # need to add a input of which model and which transformation
                                   model, transformation, 
                                   t){
  grab_wav_files(name)
  file_audio <- readWave(name)
  
  # This is for stft 
  if( transformation == "stft"){
    file_predictions <- stft_pipeline(file_audio, model)
  }
  # This is for melfcc
  if (transformation == "melfcc"){
    file_predictions <- melfcc_pipeline(file_audio, model)
  }
  if (transformation == "both"){
    file_audio_stft <- stft_pipeline(file_audio, model)
    file_audio_melfcc <- melfcc_pipeline(file_audio, numcep, maxfreq, wintime, hoptime, model)
    file_predictions <- merge(file_audio_stft, file_audio_melfcc, by = (c("time_start","time_end")), all.x = TRUE)
  }
  rm(file_audio)
  
  # i need to make sure that the predictions only happen for the specific predictions names
  file_predictions[is.na(file_predictions)] <- 0
  #write.csv(file_predictions, paste0("pred.",name, ".csv"))
  name_1 <- sub("\\.wav$", name)
  to_selection_table(file_predictions, name = name_1, t)
  return(file_predictions)
}
name <- "6805.230205170826.wav"

# mtry_n <- 15
# min_n_n <-  30
# 
# n <- 30

# model can be stft, melfcc, or both
# transformation can be rf, knn, or both
file_predictions <- create_selection_table(name = name, model = "", transformation = "",  
                                           t = c(50, 400))

# readWave(name, from = 0, to = 30, units = "minutes")
# use this for humpback whale files in order to cut down on the file size


# ---------------------- This is not needed to be run --------------------------

# melfcc_valid_predictions <- read.csv("melfcc_val_res_full.csv")
# melfcc_valid_predictions_checking <- melfcc_valid_predictions |> 
#   filter(filenumber == 2)
# 6805.230206100827
# get_model_predictions_mel <- function(preds,t = 100, write = T){
#   # extract filename for output
#   output <- preds |> 
#     # add other models into total.pred
#     mutate(total.pred = as.numeric(knn.pred) + 
#              as.numeric(rf.pred),
#            rolling.avg = frollmean(total.pred, n = t, fill = 0),
#            final_pred = ifelse(rolling.avg>0, 1, 0),
#            pred_number= rleid(final_pred),
#            `Begin Time (s)` = time_start, - (t/10),
#            `End Time (s)` = time_end - (t/10) ) |> 
#     filter(final_pred == 1) |> 
#     group_by(pred_number) |> 
#     summarise( `Begin Time (s)`= min(`Begin Time (s)`),
#                `End Time (s)` = max(`End Time (s)`)) |>
#     mutate(Selection = row_number(),
#            View = "Spectrogram 1",
#            Channel = 1) |> 
#     dplyr::select(Selection,View, Channel, `Begin Time (s)`, `End Time (s)`)
#   
#   path <- paste0("model.pred.6805.230206100827.txt", sep = "")
#   if(write){
#     write_tsv(output, path)
#   }
#   output
# }
# 
# get_model_predictions_mel(melfcc_valid_predictions)

# ------------------------ Setting up the Fitted models ------------------------

# {
#   df_train <- read.csv("train_melfcc.csv")
#   df_train <- df_train |> 
#     dplyr::select(-X, -X.1, -annotation_num, -time_interval)
#   df_train$song <- factor(df_train$song)
#   
#   # This is for Random Forest
#   whale_recipe <- recipe(song ~ ., data = df_train) |> 
#     step_rm(time_start, time_end)
#   
#   whale_rf <- rand_forest(mtry = 10,
#                           min_n = 33,
#                           trees = 1000) |>
#     set_engine("ranger") |>
#     set_mode("classification")
#   
#   whale_wrkf <- workflow() |>
#     add_recipe(whale_recipe) |>
#     add_model(whale_rf)
#   
#   rf_final_melfcc <- fit(whale_wrkf, data = df_train)
#   print("melfcc rf done")
# 
#   rm(final_model)
#   # This is for K Nearest Neighbors
#   whale_recipe_knn <- recipe(song~., data = df_train) |> 
#     step_rm(time_start, time_end) |> 
#     step_normalize(all_numeric_predictors())
#   
#   knn_final <- nearest_neighbor(neighbors = 30) |>
#     set_engine('kknn') |>
#     set_mode("classification")
#   knn_final_wf <- workflow() |>
#     add_recipe(whale_recipe_knn) |>
#     add_model(knn_final)
#   knn_final_melfcc <- knn_final_wf |> fit(df_train)
#   print("melfcc knn done")
#   
#   df_train <- read.csv("df_stft_train_10_files.csv")
#   df_train <- df_train |> 
#     dplyr::select(-X, -annotation_num, -time_interval, -filenumber)
#   df_train$song <- factor(df_train$song)
#   
#   # This is for Random Forest
#   whale_recipe <- recipe(song ~ ., data = df_train) |> 
#     step_rm(time_start, time_end)
#   
#   whale_rf <- rand_forest(mtry = 15,
#                           min_n = 30,
#                           trees = 1000) |>
#     set_engine("ranger") |>
#     set_mode("classification")
#   
#   whale_wrkf <- workflow() |>
#     add_recipe(whale_recipe) |>
#     add_model(whale_rf)
#   
#   rf_final_stft <- fit(whale_wrkf, data = df_train)
#   print("stft rf done")
# 
#   # This is for K Nearest Neighbors
#   whale_recipe_knn <- recipe(song~., data = df_train) |> 
#     step_rm(time_start, time_end) |> 
#     step_normalize(all_numeric_predictors())
#   
#   knn_final <- nearest_neighbor(neighbors = 30) |>
#     set_engine('kknn') |>
#     set_mode("classification")
#   knn_final_wf <- workflow() |>
#     add_recipe(whale_recipe_knn) |>
#     add_model(knn_final)
#   knn_final_stft <- knn_final_wf |> fit(df_train)
#   print("stft knn done")
# }

# this will save the final models so we dont have to load fit them everytime for a new file
  # {
  #   saveRDS(rf_final_melfcc, "rf_final_melfcc.rds")
  #   saveRDS(knn_final_melfcc, "knn_final_melfcc.rds")
  #   saveRDS(rf_final_stft, "rf_final_stft.rds")
  #   saveRDS(knn_final_stft,"knn_final_stft.rds")
  # }


# example
# knn_final_stft <- readRDS("knn_final_stft.rds")



