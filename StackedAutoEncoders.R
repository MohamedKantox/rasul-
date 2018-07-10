autoEncode = function(data){
  data = setDT(as.data.frame(data))
  #Defining train and test sets
  train_size <- ceiling(0.8*nrow(data))
  test_index <- train_size+1
  
  train <- data[1:train_size,]
  test <- data[test_index:nrow(data),]
  
  # Gets descriptive statistics for every variable in the dataset.
  get_desc <- function(x) {
    map(x, ~list(
      min = min(.x),
      max = max(.x),
      mean = mean(.x),
      sd = sd(.x)
    ))
  } 
  
  #Given a dataset and normalization constants it will create a min-max normalized version of the dataset.
  normalization_minmax <- function(x, desc) {
    map2_dfc(x, desc, ~(.x - .y$min)/(.y$max - .y$min))
  }
  #Reverse function : unnormalise 
  denormalisation <- function(x, desc) {
    map2_dfc(x, desc, ~.x * (.y$max - .y$min) + .y$min)
  }
  
  
  
  #From train and test sets seperately, get everything but
  #daily return and normalise them 
  desc <- train %>% 
    select(-dailyReturn) %>% 
    get_desc()
  
  x_train <- train %>%
    select(-dailyReturn) %>%
    normalization_minmax(desc) %>%
    data.matrix()
  
  x_test <- test %>%
    select(-dailyReturn) %>%
    normalization_minmax(desc) %>%
    data.matrix()
  
  
  
  model_ae <- keras_model_sequential()
  model_ae %>%
    layer_dense(units = 15, activation = "tanh", input_shape = ncol(x_train)) %>%
    layer_dense(units = 10, activation = "tanh") %>%
    layer_dense(units = 15, activation = "tanh") %>%
    layer_dense(units = ncol(x_train))
  
  
  model_ae %>% compile(
    loss = "mse", 
    optimizer = "adam"
  )
  
  
  checkpoint_ae <- callback_model_checkpoint(
    filepath = "model_ae.hdf5", 
    save_best_only = TRUE, 
    period = 1,
    verbose = 1
  )
  
  early_stopping_ae <- callback_early_stopping(patience = 5)
  
  history = model_ae %>% fit(
    x = x_train, 
    y = x_train, 
    epochs = 10, 
    batch_size = 32,
    validation_data = list(x_test, x_test), 
    callbacks = list(checkpoint_ae, early_stopping_ae)
  )
  
  ####
  plot(history)
  ####
  #make predictions
  model_ae <- load_model_hdf5("model_ae.hdf5", compile = FALSE)
  
  pred_train <- predict(model_ae, x_train)
  mse_train <- apply((x_train - pred_train)^2, 1, sum)
  
  pred_test <- predict(model_ae, x_test)
  mse_test <- apply((x_test - pred_test)^2, 1, sum)
  
  
  #The output of AE
  trained_data <- rbind(pred_train,pred_test)
  trained_data <- cbind(trained_data, data[,c("dailyReturn")])
  
  data_den <- trained_data %>%
    select(-dailyReturn) %>%
    denormalisation(desc) 
  
  data_den = cbind(data_den, data[,c("dailyReturn")])
  setnames(data_den,names(data_den),names(data))
  
  #data_den contains what we want 
  return(data.matrix(data_den))
}