source("Packages.R")
source("AWS.R")
source("Data Capstone Reading Wav Files.R")


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

print(train_data, width = Inf)

# Define recipe, checking for missing columns
svm_recipe <- recipe(song ~ ., data = train_data) |> 
  step_rm(any_of(c("time_start", "time_end", "annotation_num", "time_interval"))) |>  
  step_normalize(all_numeric_predictors())  # Normalize predictors

# Define SVM model
svm_model <- svm_rbf(cost = 1, rbf_sigma = 0.1) |> 
  set_engine("kernlab") |> 
  set_mode("classification")

# Create workflow
svm_wf <- workflow() |> 
  add_recipe(svm_recipe) |> 
  add_model(svm_model)

# Fit the model
svm_fit <- svm_wf |> fit(data = train_data)

# Make predictions on test set
test_data$svm_pred <- predict(svm_fit, new_data = test_data)$.pred_class

# Compute performance metrics
f_meas(test_data, truth = song, estimate = svm_pred)
precision(test_data, truth = song, estimate = svm_pred)
recall(test_data, truth = song, estimate = svm_pred)

# Confusion matrix
conf_mat(test_data, truth = song, estimate = svm_pred)















