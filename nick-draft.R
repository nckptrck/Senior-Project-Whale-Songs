# source files
source("Packages.R")
source("AWS.R")
source("Data Capstone Reading Wav Files.R")


# EDA: full range from 0Hz - 1600 Hz

#NOTE: not the data we are using to model
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
  step_rm(time_start, time_end, annotation_num, time_interval)

# tune model
rf_tune <- rand_forest(mtry = tune(), 
                       min_n = tune(),
                       trees = 100) %>%
  set_engine("ranger") %>%
  set_mode("classification")


rf_grid <- grid_regular(mtry(c(1,12)),
                        min_n(),
                        levels = 10)

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
rf_grid_search |> collect_metrics() |> filter(.metric == 'precision') |> slice_max(mean, n = 5)
rf_grid_search |> collect_metrics() |> filter(.metric == 'recall') |> slice_max(mean, n = 5)


# fit best model and evaluate on test data

rf_final <- rand_forest(mtry = 4,
                        min_n = 31,
                        trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_final_wf <- workflow() |> 
  add_recipe(full_recipe) |> 
  add_model(rf_final)

rf_final_fit <- rf_final_wf |> 
  fit(train_data)


test_data$rf.pred <- predict(rf_final_fit, new_data = test_data)$.pred_class

# rf maximized precision
rf_prec <- rand_forest(mtry = 1,
                        min_n = 31,
                        trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_prec_wf <- workflow() |> 
  add_recipe(full_recipe) |> 
  add_model(rf_prec)

rf_prec_fit <- rf_prec_wf |> 
  fit(train_data)


test_data$rf.pred.precision <- predict(rf_prec_fit, new_data = test_data)$.pred_class

# rf maximized recall
rf_rec <- rand_forest(mtry = 12,
                       min_n = 6,
                       trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_rec_wf <- workflow() |> 
  add_recipe(full_recipe) |> 
  add_model(rf_rec)

rf_rec_fit <- rf_rec_wf |> 
  fit(train_data)


test_data$rf.pred.recall <- predict(rf_rec_fit, new_data = test_data)$.pred_class
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
recipe_norm <- recipe(song~., data = train_data) |> 
  step_rm(time_start, time_end, annotation_num, time_interval) |> 
  step_normalize(all_numeric_predictors())

# tuning
knn_tune <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

neighbor_grid <- grid_regular(neighbors(c(1,150)),
                              levels = 10)

knn_tune_wf <- workflow() |> 
  add_recipe(recipe_norm) |> 
  add_model(knn_tune)

knn_grid_search <- tune_grid(knn_tune_wf,
                             resamples = train_cvs,
                             grid = neighbor_grid,
                             metrics = metric_set(roc_auc, precision, recall))

knn_grid_search |> collect_metrics() |> filter(.metric == 'roc_auc') |> slice_max(mean, n = 5)
knn_grid_search |> collect_metrics() |> filter(.metric == 'precision') |> slice_max(mean, n = 5)
knn_grid_search |> collect_metrics() |> filter(.metric == 'recall') |> slice_max(mean, n = 5)

# fitting best model (Precision)
knn <- nearest_neighbor(neighbors = 67) %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_wf <- workflow() |> 
  add_recipe(recipe_norm) |> 
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
logit_grid <- grid_regular(penalty(),
                           mixture(),
                           levels = 10)

logit_tune <- logistic_reg(penalty = tune(),
                          mixture = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

logit_tune_wf <- workflow() |> 
  add_model(logit_tune) |> 
  add_recipe(recipe_norm)

logit_gs <- tune_grid(logit_tune_wf,
          resamples = train_cvs,
          grid = logit_grid,
          metrics = metric_set(roc_auc, precision, recall))


logit_gs |> collect_metrics() |> filter(.metric == 'roc_auc') |> slice_max(mean, n = 5)
logit_gs |> collect_metrics() |> filter(.metric == 'precision',
                                        mean != 1) |> slice_max(mean, n = 100)
logit_gs |> collect_metrics() |> filter(.metric == 'recall') |> slice_max(mean, n = 5)

# predict on test set
logit_mod <- logistic_reg(penalty = 0.0774,mixture = 0) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

logit_wf <- workflow() |> 
  add_model(logit_mod) |> 
  add_recipe(recipe_norm)

logit_fit <- logit_wf |> fit(train_data)

test_data$logit.pred <- predict(logit_fit, new_data = test_data)$.pred_class

# Compute accuracy, precision, recall, and F1-score
accuracy(test_data, truth = song, estimate = logit.pred)
precision(test_data, truth = song, estimate = logit.pred)
recall(test_data, truth = song, estimate = logit.pred)
f_meas(test_data, truth = song, estimate = logit.pred)

# confusion matrix
conf_mat(test_data, truth = song, estimate = logit.pred)

# Model 4: LDA -----------------------------------------------------------------
library(discrim)

lda_mod <- discrim_linear() %>%
  set_engine("MASS") %>%
  set_mode("classification")

lda_cv <- lda_mod |> 
  fit_resamples(recipe_norm,
                train_cvs,
                metrics = metric_set(precision,recall,roc_auc,f_meas))

lda_cv |> collect_metrics()

# fit model
lda_wf <- workflow() |> 
  add_model(lda_mod) |> 
  add_recipe(recipe_norm)

lda_fit <- lda_wf |> fit(train_data)

# evaluate on test set
test_data$lda.pred <- predict(lda_fit, new_data = test_data)$.pred_class

# Compute accuracy, precision, recall, and F1-score
accuracy(test_data, truth = song, estimate = lda.pred)
precision(test_data, truth = song, estimate = lda.pred)
recall(test_data, truth = song, estimate = lda.pred)
f_meas(test_data, truth = song, estimate = lda.pred)

# confusion matrix
conf_mat(test_data, truth = song, estimate = lda.pred)


# Model 5: SVM



# proportion of each whale song being predicted as 1
test_data |> 
  mutate(knn.pred = ifelse(as.numeric(knn.pred) == 2,0,1),
         rf.pred = ifelse(as.numeric(rf.pred) == 2,0,1),
         rf.pred.precision = ifelse(as.numeric(rf.pred.precision) == 2,0,1),
         logit.pred = ifelse(as.numeric(logit.pred) == 2,0,1))  |> 
  group_by(annotation_num) |> 
  summarise(n.obs = n(),
            n.knn = sum(knn.pred),
            prop.knn = sum(knn.pred)/n(),
            n.rf.roc = sum(rf.pred),
            prop.rf.roc = sum(rf.pred)/n(),
            n.rf.precision = sum(rf.pred.precision),
            prop.rf.precision = sum(rf.pred.precision)/n(),
            n.logit = sum(logit.pred),
            prop.logit = sum(logit.pred)/n()
            )

# table with time intervals
test_data |> 
  mutate(knn.pred = ifelse(as.numeric(knn.pred) == 2,0,1),
         rf.pred = ifelse(as.numeric(rf.pred) == 2,0,1),
         rf.pred.precision = ifelse(as.numeric(rf.pred.precision) == 2,0,1),
         logit.pred = ifelse(as.numeric(logit.pred) == 2,0,1))  |> 
  group_by(time_interval) |> 
  summarise(n.obs = n(),
            n.knn = sum(knn.pred),
            prop.knn = sum(knn.pred)/n(),
            n.rf.roc = sum(rf.pred),
            prop.rf.roc = sum(rf.pred)/n(),
            n.rf.precision = sum(rf.pred.precision),
            prop.rf.precision = sum(rf.pred.precision)/n(),
            n.logit = sum(logit.pred),
            prop.logit = sum(logit.pred)/n()
  )
# Principal Component Analysis -------------------------------------------------

# create matrix
df_pca <- df2 |> 
  dplyr::select(ends_with('Hz'))

df_mat <- as.matrix(df_pca) |> 
  scale()

# fit pca
pca <- prcomp(df_mat)

# explore components

pca$rotation |> 
  data.frame() |> 
  dplyr::select(PC1) |> 
  slice_max(abs(PC1), n = 18)

# By Whale Song
pca$x |> 
  as.data.frame() |> 
  mutate(song = df2$song) |> 
  ggplot(aes(x = PC1, y = PC2, color = song)) +
  geom_point() +
  facet_grid(rows = vars(song)) +
  labs(x = "Principal Component 1",
       y = "Principal Component 2",
       title = "Scatterplot: First Two Principal Components By Whale Song",
       subtitle = 'Colored by KNN Model Prediction',
       color = "Whale Song") +
  theme_classic() +
  scale_color_manual(values = c('red', 'blue'))

# By predictions
pca$x[13001:18000,] |> 
  as.data.frame() |> 
  mutate(song = test_data$song,
         pred = test_data$knn.pred) |> 
  ggplot(aes(x = PC1, y = PC2, color = pred)) +
  geom_point() +
  facet_grid(rows = vars(song)) +
  labs(x = "Principal Component 1",
       y = "Principal Component 2",
       title = "Scatterplot: First Two Principal Components By Whale Song",
       subtitle = 'Colored by KNN Model Prediction',
       color = "KNN Prediction") +
  theme_classic() +
  scale_color_manual(values = c('red', 'blue'))


cumsum(pca$sdev^2)/sum(pca$sdev^2)

# knn with pca

pca_recipe <- recipe(song~., data = train_data) |> 
  step_rm(time_start, time_end, annotation_num, time_interval) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_pca(num_comp = 15)
# tuning
knn_tune <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

neighbor_grid <- grid_regular(neighbors(c(1,150)),
                              levels = 10)

knn_tune_wf <- workflow() |> 
  add_recipe(pca_recipe) |> 
  add_model(knn_tune)

knn_grid_search <- tune_grid(knn_tune_wf,
                             resamples = train_cvs,
                             grid = neighbor_grid,
                             metrics = metric_set(roc_auc, precision, recall))

knn_grid_search |> collect_metrics() |> filter(.metric == 'roc_auc') |> slice_max(mean, n = 5)
knn_grid_search |> collect_metrics() |> filter(.metric == 'precision') |> slice_max(mean, n = 5)
knn_grid_search |> collect_metrics() |> filter(.metric == 'recall') |> slice_max(mean, n = 5)
            