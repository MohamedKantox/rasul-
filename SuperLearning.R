###STEP 1 : Choose a stop : Only data until this point will be used for training
trainingStop = 1500
#Preprocess the data 
#and make it ready to be fed to a model for learning
symbol = "EURUSD"
########CHANGE symbol and only run what is below###################

FXRates_W = FXRates[FXRates[,which(Symbol==symbol)]]

#uncomment if you wanna restrict the data

FXRates_W = FXRates_W[1:trainingStop]



FXRates_W = FXRates_W[,EndDate := lubridate::mdy(EndDate)]
FXRates_W = FXRates_W[,StartDate := lubridate::mdy(StartDate)]
FXRates_W[,c("StartTime","EndTime","X"):=NULL]


#Defining features for ML algorithm and adding them to FXRates_W table

#RSI
FXRates_W[,RSI:=RSI(FXRates_W[,Close])]

# #Stochastic Oscillator
stochFX = SMI(FXRates_W[,c("High","Low","Close")])
FXRates_W[,SMI:=stochFX[,"SMI"]]

#William's AD
FXRates_W[,WilliamsAD:=williamsAD(FXRates_W[,c("High","Low","Close")])]

#MACD value and signal
macd = MACD(FXRates_W[,Close],maType="EMA")
FXRates_W[,MACD :=macd[,"macd"]]
FXRates_W[,macd_signal :=macd[,"signal"]]

#Avergae Price RoC
FXRates_W[,CloseRoC:=ROC(FXRates_W[,Close],n=14)]


#Commodity Channel Index
FXRates_W[,CCI:=CCI(FXRates_W[,.SD,.SDcols=c("High","Low","Close")],maType ="EMA",n=15)]

#Average True Range
FXRates_W[,ATR:=ATR(FXRates_W[,.SD,.SDcols=c("High","Low","Close")],maType="EMA", n=15)[,"atr"]]

#BollingerBands
BB = BBands(FXRates_W[,.SD,.SDcols=c("High","Low","Close")],maType="EMA",n=15)
FXRates_W[,upperBollinger:=BB[,"up"]]
FXRates_W[,downBollinger:=BB[,"dn"]]

#MA5/MA10 on Close rate
FXRates_W[,MA5:=SMA(FXRates_W[,Close],n=5)]
FXRates_W[,MA10:=SMA(FXRates_W[,Close],n=10)]

#20 day Exponential Moving Average
FXRates_W[,EMA20:=EMA(FXRates_W[,Close],n=20)]

#Daily return
dailyReturn = log(FXRates_W[2:nrow(FXRates_W),Close])-log(FXRates_W[1:(nrow(FXRates_W)-1),Close])
dailyReturn = c(0,dailyReturn)
FXRates_W[,dailyReturn:=dailyReturn]


############################

#Prepare Data : Ignore all NA and get rid of non numericals
data = drop_na(FXRates_W)
dates = data[,StartDate]
data = data[,-c("Symbol","EndDate")]
dataWithDates = data
data = data[,-"StartDate"]

# ##########Smoothing the data
# for(i in 1:dim(data)[[2]]){
#   data[[i]] = waveletSmooth(as.numeric(data[[i]])) 
# }

nrows = dim(data)[[1]]
#Finally
data <- data.matrix(data)


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
lookback <- 25
step <- 1
delay <- 1
batch_size <- 60
train_max = ceiling(0.7*nrows)
#Val goes all the way to the end, we train on all available data !
val_max = ceiling(nrows)


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

val_steps <- (val_max - train_max - lookback) / batch_size               
           

####Training a model

model <- keras_model_sequential() %>%
  layer_gru(units = 128,
            
            return_sequences = TRUE,
            input_shape = list(NULL, dim(data)[[-1]])) %>%
  layer_gru(units = 64, activation = "relu",
            
            recurrent_dropout = 0) %>%
 
  layer_dense(units = 1)
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)


checkpoint <- callback_model_checkpoint(
  filepath = "model.hdf5", 
  save_best_only = TRUE, 
  period = 1,
  verbose = 1
)

early_stopping <- callback_early_stopping(patience = 5)



history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 100,
  epochs = 10,
  validation_data = val_gen,
  validation_steps = val_steps,
  callbacks = list(checkpoint, early_stopping)
)
#####
plot(history)

#Step 2: Save the model
save_model_hdf5(model,"intermediateModel.h5")

#Step 3: Loop through other currencies to better train de model
trainCurrencies=c("EURUSD","USDJPY","AUDUSD","USDCAD","EURJPY","EURGBP")
for(currencyPair in trainCurrencies){
  #3.1 Rebuild the table
  #Preprocess the data 
  #and make it ready to be fed to a model for learning
  symbol = currencyPair
  ########CHANGE symbol and only run what is below###################
  
  FXRates_W = FXRates[FXRates[,which(Symbol==symbol)]]
  
  #uncomment if you wanna restrict the data
  
  FXRates_W = FXRates_W[1:trainingStop]
  
  
  
  FXRates_W = FXRates_W[,EndDate := lubridate::mdy(EndDate)]
  FXRates_W = FXRates_W[,StartDate := lubridate::mdy(StartDate)]
  FXRates_W[,c("StartTime","EndTime","X"):=NULL]
  
  
  #Defining features for ML algorithm and adding them to FXRates_W table
  
  #RSI
  FXRates_W[,RSI:=RSI(FXRates_W[,Close])]
  
  # #Stochastic Oscillator
  stochFX = SMI(FXRates_W[,c("High","Low","Close")])
  FXRates_W[,SMI:=stochFX[,"SMI"]]
  
  #William's AD
  FXRates_W[,WilliamsAD:=williamsAD(FXRates_W[,c("High","Low","Close")])]
  
  #MACD value and signal
  macd = MACD(FXRates_W[,Close],maType="EMA")
  FXRates_W[,MACD :=macd[,"macd"]]
  FXRates_W[,macd_signal :=macd[,"signal"]]
  
  #Avergae Price RoC
  FXRates_W[,CloseRoC:=ROC(FXRates_W[,Close],n=14)]
  
  
  #Commodity Channel Index
  FXRates_W[,CCI:=CCI(FXRates_W[,.SD,.SDcols=c("High","Low","Close")],maType ="EMA",n=15)]
  
  #Average True Range
  FXRates_W[,ATR:=ATR(FXRates_W[,.SD,.SDcols=c("High","Low","Close")],maType="EMA", n=15)[,"atr"]]
  
  #BollingerBands
  BB = BBands(FXRates_W[,.SD,.SDcols=c("High","Low","Close")],maType="EMA",n=15)
  FXRates_W[,upperBollinger:=BB[,"up"]]
  FXRates_W[,downBollinger:=BB[,"dn"]]
  
  #MA5/MA10 on Close rate
  FXRates_W[,MA5:=SMA(FXRates_W[,Close],n=5)]
  FXRates_W[,MA10:=SMA(FXRates_W[,Close],n=10)]
  
  #20 day Exponential Moving Average
  FXRates_W[,EMA20:=EMA(FXRates_W[,Close],n=20)]
  
  #Daily return
  dailyReturn = log(FXRates_W[2:nrow(FXRates_W),Close])-log(FXRates_W[1:(nrow(FXRates_W)-1),Close])
  dailyReturn = c(0,dailyReturn)
  FXRates_W[,dailyReturn:=dailyReturn]
  
  
  ############################
  
  #Prepare Data : Ignore all NA and get rid of non numericals
  data = drop_na(FXRates_W)
  dates = data[,StartDate]
  data = data[,-c("Symbol","EndDate")]
  dataWithDates = data
  data = data[,-"StartDate"]
  
  # ##########Smoothing the data
   for(i in 1:dim(data)[[2]]){
     data[[i]] = waveletSmooth(as.numeric(data[[i]])) 
   }
  
  nrows = dim(data)[[1]]
  #Finally
  data <- data.matrix(data)
  
  #3.2: Load the model
  model = load_model_hdf5("intermediateModel.h5")
  
  
  #3.3: Fit the model again, keep callback to only keep the best model
  history <- model %>% fit_generator(
    train_gen,
    steps_per_epoch = 100,
    epochs = 20,
    validation_data = val_gen,
    validation_steps = val_steps,
    callbacks = list(checkpoint, early_stopping)
  )
  #####
  plot(history)
  
  #3.4: Save the model
  save_model_hdf5(model,"intermediateModel.h5")
  
  
}
