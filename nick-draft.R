library(tuneR)  # For reading WAV files
library(signal) # For signal processing
library(dplyr)
library(tibble)
library(tidyverse)
library(here)
library(tidymodels)


source("Packages.R")
source("AWS.R")
source("Data Capstone Reading Wav Files.R")


df <- merging_data('6805.230201090825.wav', end_freq = 1600, step_size = 100, time_period = 0.1, annotated = T )

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
            sd.amp.100 = sd(`100-199Hz`),
            mean.amp.0 = mean(`0-99Hz`),
            sd.amp.0 = sd(`0-99Hz`))

# Create binary vector--is the amplitude at 100Hz higher than the mean of the 
# non whale songs + 3 SDs

2342 + 3*947


38506 + 3 *2405

df$binary <- ifelse(df$`0-99Hz` >= 45721 | df$`100-199Hz` >= 5183 , 1, 0)


df |> 
 # filter(time_start %% 1 == 0) |> 
  filter(time_start < 400) |>
  mutate(
    second_window = round(time_start)
  ) |>
  group_by(second_window) |>
  summarize(
    binary = as.numeric(any(binary ==1)),
    song = as.numeric(any(song == 1))
  ) |>
  ggplot(aes(x = second_window, y = binary, color = as.factor(song))) +
  geom_col() +
  scale_color_manual(values = c('red2', 'blue3'))


df |>
  filter(time_start < 400) |>
  ggplot(aes(fill = as.factor(binary), x = song)) +
  geom_bar(position = "fill")

# MODELING ---------------------------------------------------------------------

# Create data frame: 0-600 Hz, by 25 Hz
df2 <- merging_data('6805.230201090825.wav',
                    end_freq = 600,
                    step_size = 25,
                    time_period = 0.1,
                    annotated = T )
# train-test split
train_data <- df2[0:13000,]
test_data <- df2[13001:18000,]

# split train data for CV
set.seed(1738)
train_cvs <- vfold_cv(train_data, v = 5)


# Model 1: Random Forest -------------------------------------------------------

full_recipe <- recipe(song~., data = train_data) |> 
  step_rm(time_start, time_end, annotation_num)

# tune model
rf_tune <- rand_forest(mtry = tune(), 
                       min_n = tune(),
                       trees = 100) %>%
  set_engine("ranger") %>%
  set_mode("classification")


rf_grid <- grid_regular(mtry(c(1,12)),
                        min_n(),
                        levels = 4)

rf_tune_wf <- workflow() |> 
  add_recipe(full_recipe) |> 
  add_model(rf_tune)

rf_grid_search <- tune_grid(
  rf_tune_wf,
  resamples = train_cvs,
  grid = rf_grid,
  metrics = metric_set(roc_auc, precision, recall)
)

# evaluate grid search
rf_grid_search |> collect_metrics() |> filter(.metric == 'roc_auc') |> slice_max(mean, n = 5)


# fit best model and evaluate on test data

rf_final <- rand_forest(mtry = 8,
                        min_n = 27,
                        trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_final_wf <- workflow() |> 
  add_recipe(full_recipe) |> 
  add_model(rf_final)

rf_final_fit <- rf_final_wf |> 
  fit(train_data)


test_data$rf.pred <- predict(rf_final_fit, new_data = test_data)$.pred_class

# compute accuracy, precision, recall, and F1-score
accuracy(test_data, truth = song, estimate = rf.pred)
precision(test_data, truth = song, estimate = rf.pred)
recall(test_data, truth = song, estimate = rf.pred)
f_meas(test_data, truth = song, estimate = rf.pred)

# confusion matrix
conf_mat(test_data, truth = song, estimate = rf.pred)

# error evaluation graph
test_data |> 
  pivot_longer(cols = c(song, rf.pred),
               names_to = 'type',
               values_to = 'value') |> 
  ggplot(aes(x = time_start, y = value)) +
  geom_point() +
  facet_wrap(~type)

# Model 2: KNN -----------------------------------------------------------------

# tuning
knn_tune <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

neighbor_grid <- grid_regular(neighbors(c(70,100)),
                              levels = 20)

knn_tune_wf <- workflow() |> 
  add_recipe(full_recipe) |> 
  add_model(knn_tune)

knn_grid_search <- tune_grid(knn_tune_wf,
                             resamples = train_cvs,
                             grid = neighbor_grid,
                             metrics = metric_set(roc_auc, precision, recall))

knn_grid_search |> collect_metrics() |> filter(.metric == 'precision') |> slice_max(mean, n = 5)
  


# fitting best model
knn <- nearest_neighbor(neighbors = 70) %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_wf <- workflow() |> 
  add_recipe(full_recipe) |> 
  add_model(knn)

knn_fit <- knn_wf |> fit(train_data)


# predict on test set

test_data$knn.pred <- predict(knn_fit, new_data = test_data)$.pred_class

# Compute accuracy, precision, recall, and F1-score
accuracy(test_data, truth = song, estimate = knn.pred)
precision(test_data, truth = song, estimate = knn.pred)
recall(test_data, truth = song, estimate = knn.pred)
f_meas(test_data, truth = song, estimate = knn.pred)

# confusion matrix
conf_mat(test_data, truth = song, estimate = knn.pred)


# error evaluation graph
test_data |> 
  pivot_longer(cols = c(song, knn.pred),
               names_to = 'type',
               values_to = 'value') |> 
  ggplot(aes(x = time_start, y = value)) +
  geom_point() +
  facet_wrap(~type)

knn_errors <- test_data |> 
  filter(song != knn.pred)

# Model 3: Logistic Regression -------------------------------------------------


# proportion of each whale song being predicted as 1
test_data |> 
  mutate(knn.pred = ifelse(as.numeric(knn.pred) == 2,0,1),
         rf.pred = ifelse(as.numeric(rf.pred) == 2,0,1))  |> 
  group_by(annotation_num) |> 
  summarise(n.obs = n(),
            n.knn = sum(knn.pred),
            prop.knn = sum(knn.pred)/n(),
            n.rf = sum(rf.pred),
            prop.rf = sum(rf.pred)/n())
            