###-----HYPER PARAMETER TUNING-----###
# Reusing code chunks from the lecture notes

# Setting up data sets
data_dict <- list(
  "train_idv" =  train_IDV,
  "test_idv" = test_IDV,
  "val_idv" = val_IDV,
  
  "train_dv" = train_DV,
  "test_dv" = test_DV,
  "val_dv" = val_DV,
  
  "w_no" = class_weights_values$weight_no,
  "w_yes" = class_weights_values$weight_yes
)

# Setting up the flaGs
FLAGS <- flags(
  flag_numeric("nodes", 512),
  flag_numeric("batch_size", 32),
  flag_string("activation", "sigmoid"),
  flag_numeric("learning_rate", 0.01)
)


# # We are not implementing Early Stopping as it can cause over fitting
# # Implementing Early Stopping
# callbacks <- list(callback_early_stopping(monitor = "val_loss", patience = 10, restore_best_weights = TRUE)) # No updates yet

# Tuning model
tuning_model <- keras_model_sequential()

tuning_model %>%
  layer_dense(units = FLAGS$nodes, activation = FLAGS$activation, input_shape = dim(data_dict$train_idv)[2]) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = FLAGS$nodes, activation = FLAGS$activation ) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = FLAGS$nodes, activation = FLAGS$activation ) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1, activation = FLAGS$activation)

tuning_model %>% compile(optimizer = optimizer_adam(learning_rate = FLAGS$learning_rate),
                         loss = 'binary_crossentropy',
                         metrics = c('accuracy'))

tuning_model %>% fit(data_dict$train_idv,
                     data_dict$train_dv,
                     batch_size = FLAGS$batch_size,
                     epochs = 30,
                     validation_data = list(data_dict$val_idv, data_dict$val_dv),
                     class_weight = list("0" = data_dict$w_no, "1" = data_dict$w_yes),
                     # callbacks = callbacks,
                     verbose = 1)
