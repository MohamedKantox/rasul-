#Define and train a model that will work on the preprocessed data
samplesTest = NULL
#Create a generator :
generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 1) {
  if (is.null(max_index)) {max_index <- nrow(data)}
  i <- min_index + lookback
  #Smoothing the data
  data[min_index:max_index,] = apply(data[min_index:max_index,],2,waveletSmooth)
  #Applying Stacked AutoEncoders
  #data[min_index:max_index,] = autoEncode(data[min_index:max_index,])
  #print("Data has been smoothed")
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size, max_index))
      i <<- i + length(rows)
    }
    
    #For each prediction, we need lookback/step observations
    #The samples tensor has therefore the dimensions : length(rows),lookback/step, nb_features
    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows),1))
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                     length.out = dim(samples)[[2]])
      
      samples[j,,] <- data[indices,]
      targets[j,] <- data[rows[[j]] ,c("dailyReturn")]
    }
 
    #Normalize the data :
    #Avoiding data leakage : using only available data for scaling 
    mean <- apply(data[min_index:max_index,], 2, mean)
    std <- apply(data[min_index:max_index,], 2, sd)
    
    for (i in 1:lookback){
      samples[,i,] <- scale(samples[,i,], center = mean, scale = std)
      
    }
    
    targets = (targets - mean["dailyReturn"]) / std["dailyReturn"]
    
    list(samples, targets)
  }
}
#Define train/validation and test generators
lookback <- 30
step <- 1
delay <- 1
batch_size <- 60
train_max = ceiling(0.7*nrows)
val_max = ceiling(0.85*nrows)


train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = train_max,
  shuffle = FALSE,
  step = step,
  batch_size = batch_size
)
val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = train_max+1,
  max_index = val_max,
  step = step,
  batch_size = batch_size
)
test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = val_max+1,
  max_index = NULL,
  step = step,
  batch_size = 400
)
val_steps <- (val_max - train_max - lookback) / batch_size               
test_steps <- (nrow(data) - val_max+1 - lookback) / batch_size           

####Training a model
model <- keras_model_sequential() %>%
  layer_gru(units = 32,
            
            return_sequences = TRUE,
            input_shape = list(NULL, dim(data)[[-1]])) %>%
  layer_gru(units = 64, activation = "relu",
            
            recurrent_dropout = 0.5) %>%
  layer_dense(units = 1)
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)
history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 100,
  epochs = 5,
  validation_data = val_gen,
  validation_steps = val_steps
)
#####
plot(history)