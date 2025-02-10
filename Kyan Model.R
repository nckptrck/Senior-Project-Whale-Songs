source("Packages.R")
source("AWS.R")
source("Data Capstone Reading Wav Files.R")

file <- "6805.230201090825.wav"   # need to change this to the files that you want to pull

df <- merging_data(file, 800, 50, annotated = TRUE)

df


library(tuneR)  # For reading WAV files
library(signal) # For signal processing
library(dplyr)
library(tibble)
library(tidyverse)
library(here)
library(tidymodels)

library(wavelets)




source("Packages.R")
source("AWS.R")
source("Data Capstone Reading Wav Files.R")


colnames(df)


library(wavelets)
library(ranger)
library(ggplot2)
library(rsample)

# WAVELET Transformation -------------------------------------------------------

# Define function to apply DWT on a row of amplitudes
apply_wavelet <- function(row) {
  row <- as.numeric(row)  # Ensure it's numeric
  row_length <- length(row)
  
  # Check for valid input
  if (row_length < 2) return(rep(NA, row_length)) 
  max_levels <- floor(log2(row_length))
  if (max_levels < 1) return(rep(NA, row_length))  
  
  # Perform Discrete Wavelet Transform
  dwt_result <- dwt(row, filter = "haar", n.levels = max_levels)
  
  # Extract wavelet coefficients
  unlist(c(dwt_result@W, dwt_result@V))
}

# Select only frequency columns
freq_columns <- grep("Hz", colnames(df), value = TRUE)

# Apply DWT row-wise
wavelet_transformed <- t(apply(df[, freq_columns], 1, apply_wavelet))

# Convert to a data frame
wavelet_df <- as.data.frame(wavelet_transformed)

# Combine with original dataset
df_transformed <- cbind(df[, c("time_start", "time_end", "song")], wavelet_df)

#Notes: 

# Notice the negative values in the detail coefficients—this means certain frequencies decreased at that time step.
# No longer directly correspond to specific frequency bins.
# Instead, represent a multi-scale decomposition of the frequency data.
# Provide a more compact, structured representation of the signal.
# So instead of seeing raw frequencies (e.g., 100 Hz, 200 Hz, etc.), you now have wavelet coefficients summarizing different levels of frequency variations in time.

df_transformed


# Model Setup ------------------------------------------------------------------

# train-test split
train_data <- df_transformed[0:13000,]
test_data <- df_transformed[13001:18000,]

# split train data for CV
set.seed(1738)
train_cvs <- vfold_cv(train_data, v = 5)

# Model 1: Random Forest -------------------------------------------------------



# Define the preprocessing recipe for wavelet-transformed data
wavelet_recipe <- recipe(song ~ ., data = df_transformed) |> 
  step_rm(time_start, time_end)

# Tune the Random Forest Model
rf_tune <- rand_forest(mtry = tune(), 
                       min_n = tune(),
                       trees = 100) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_grid <- grid_regular(mtry(c(1, 12)),
                        min_n(),
                        levels = 10)

rf_tune_wf <- workflow() |> 
  add_recipe(wavelet_recipe) |> 
  add_model(rf_tune)

rf_grid_search <- tune_grid(
  rf_tune_wf,
  resamples = train_cvs,
  grid = rf_grid,
  metrics = metric_set(f_meas, precision, recall)
)

# Evaluate tuning results
# rf_grid_search |> collect_metrics() |> filter(.metric == 'f_meas') |> slice_max(mean, n = 5)

# Fit final model using best parameters
rf_final <- rand_forest(mtry = 4,  # Adjust based on tuning results
                        min_n = 31,
                        trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_final_wf <- workflow() |> 
  add_recipe(wavelet_recipe) |> 
  add_model(rf_final)

rf_final_fit <- rf_final_wf |> 
  fit(train_data)

# Make predictions on the test set
test_data$rf_pred_wavelet <- predict(rf_final_fit, new_data = test_data)$.pred_class

# Compute F1-score and other metrics
f_meas(test_data, truth = song, estimate = rf_pred_wavelet)
precision(test_data, truth = song, estimate = rf_pred_wavelet)
recall(test_data, truth = song, estimate = rf_pred_wavelet)

# Confusion matrix
conf_mat(test_data, truth = song, estimate = rf_pred_wavelet)

# Visualization of errors
test_data |> 
  pivot_longer(cols = c(song, rf_pred_wavelet),
               names_to = 'type',
               values_to = 'value') |> 
  ggplot(aes(x = time_start, y = value)) +
  geom_point() +
  facet_wrap(~type)


test_data


# Plots Time Series ------------------------------------------------------------

# Filter data where the prediction is 1 for 'song'
predicted_ones <- test_data |>
  filter(rf_pred_wavelet == 1)

# Reshape data for plotting multiple wavelet coefficients
predicted_ones_long <- predicted_ones |>
  pivot_longer(cols = starts_with("V"),  # Assuming wavelet coefficients are named "V1", "V2", etc.
               names_to = "wavelet_coeff",
               values_to = "value")

# Plot wavelet coefficients for predicted '1' with time in minutes
ggplot(predicted_ones, aes(x = time_start_min, y = V1)) +   # Replace 'V1' with the desired wavelet coefficient column
  geom_line(color = "blue", size = 1) +
  theme_minimal() +
  labs(title = "Wavelet Coefficients for Predicted '1' (Song) Over Time (Minutes)",
       x = "Time (Minutes)",
       y = "Wavelet Coefficients") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# Plot multiple wavelet coefficients over time for the predicted '1'
ggplot(predicted_ones_long, aes(x = time_start, y = value, color = wavelet_coeff)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Wavelet Coefficients for Predicted '1' (Song) Over Time",
       x = "Time",
       y = "Wavelet Coefficients") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# Wavelet coefficient over time for single

ggplot(predicted_ones, aes(x = time_start, y = W11)) +   # Replace y = " " With coefficient
  geom_line(color = "blue") +
  geom_rect(aes(xmin = time_start, xmax = time_end, ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.2) +
  theme_minimal() +
  labs(title = "Wavelet Coefficients with Highlighted Prediction Time for '1' (Song)",
       x = "Time",
       y = "Wavelet Coefficients")



