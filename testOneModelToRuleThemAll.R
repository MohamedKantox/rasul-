accuracyResults = NULL
names_accuracyResults = NULL

for (currencyPair in currencyPairs) {
  if (nchar(currencyPair)==6){
    ###################################################################################
    #Preprocess the data 
    #and make it ready to be fed to a model for learning
    
    
    ########CHANGE symbol and only run what is below###################
    
    FXRates_W = FXRates[FXRates[,which(Symbol==currencyPair)]]
    
    #uncomment if you wanna restrict the data
    
    FXRates_W = FXRates_W[1:2000]
    
    
    
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
    
      ############################Trading#############################
      #We start at the beginning of test
      startIndex = val_max+1
      
      startDate = dataWithDates[val_max+1,StartDate] 
      nb_trading_days = 100 
      
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

names(accuracyResults)=names_accuracyResults
sort(accuracyResults)

