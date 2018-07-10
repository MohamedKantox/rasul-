accuracyResults = NULL
names_accuracyResults = NULL

for (currencyPair in currencyPairs) {
  if (nchar(currencyPair)==6){
    ###################################################################################
    #Preprocess the data 
    #and make it ready to be fed to a model for learning
    
    
    #Only Working on EURUSD for now
    FXRates_W = FXRates[FXRates[,which(Symbol==currencyPair)]]
    if (nrow(FXRates_W)>1000){
      #FXRates_W = FXRates_W[1:1794]
      
      
      
      
      FXRates_W = FXRates_W[,EndDate := lubridate::mdy(EndDate)]
      FXRates_W = FXRates_W[,StartDate := lubridate::mdy(StartDate)]
      FXRates_W[,c("StartTime","EndTime","X"):=NULL]
      
      
      #Defining features for ML algorithm and adding them to FXRates_W table
      
      #RSI
      FXRates_W[,RSI:=RSI(FXRates_W[,Close])]
      
      # #Stochastic Oscillator
      # stochFX = TTR::stoch(FXRates_W[,c("High","Low","Close")])
      # FXRates_W[,SMI:=stochFX[,"SMI"]]
      
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
      nrows = dim(data)[[1]]
      data <- data.matrix(data)
    ###################################################****MODEL*****########################  
      #Create a generator :
      generator <- function(data, lookback, delay, min_index, max_index,
                            shuffle = FALSE, batch_size = 128, step = 1) {
        if (is.null(max_index)) {max_index <- nrow(data)}
        i <- min_index + lookback
        #Smoothing the data
        data[min_index:max_index,] = apply(data[min_index:max_index,],2,waveletSmooth)
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
        validation_steps = val_steps,
        verbose =0
      )
 
      ############################Trading#############################
      #We start at the beginning of test
      startIndex = val_max+1
      
      startDate = dataWithDates[val_max+1,StartDate] 
      nb_trading_days = 80 
      
      endIndex = startIndex -1 + nb_trading_days
      #endIndex = val_max  + nb_trading_days 
      endDate = dataWithDates[endIndex,StartDate] 
      
      #This data has already been preprcessed to be centered and scaled    
      tradingData = tradingData = dataWithDates[startIndex:endIndex]
      
      #Getting rid of the date column 
      tradingData = tradingData[,-"StartDate"]
      
      trading_gen <- generator(
        data,
        lookback = lookback,
        delay = delay,
        min_index = startIndex - lookback,
        #min_index = val_max+1-lookback,
        max_index = startIndex + nb_trading_days-1, 
        #max_index = endIndex,
        step = step,
        batch_size = nrow(tradingData)
      )
      
      #Let's create our input
      input = trading_gen()[[1]]
      #Input[i,] contains information about the lookback days before the i-th prediction (including today)
      #We predict next day's daily return for each block of 30 days 
      predicted = predict(model,input)
      #Un-normalize predicted data
      
      mean <- apply(data[1:(startIndex-1),], 2, mean)
      std <- apply(data[1:(startIndex-1),], 2, sd) 
      
      for (i in 1:1){
        predicted[,i] = predicted[,i]*std[["dailyReturn"]] + mean[["dailyReturn"]]
      }
      #Accurate prediction rate
      accuratePredictionnRate = 0
      for (i in startIndex:endIndex){}
      for (i in 1:nb_trading_days){
        if (as.double(data[startIndex+i-1,"dailyReturn"])*predicted[i]>0) accuratePredictionnRate = accuratePredictionnRate+1
      }
      accuratePredictionnRate = accuratePredictionnRate/nb_trading_days
      cat("####################",currencyPair)
      cat(" Accurate Prediction Rate (APR) :", accuratePredictionnRate*100,"%" )
      accuracyResults=c(accuracyResults,accuratePredictionnRate)
      names_accuracyResults = c(names_accuracyResults,currencyPair)
    }
    }
}
