accuracyResults = NULL
names_accuracyResults = NULL
ProfitsTradingStrategy = NULL
ProfitsRandom = NULL

testCurrencies=c("EURUSD","USDJPY","GBPUSD","AUDUSD","USDCAD","EURJPY","EURGBP","USDNZD","USDKRW","USDCNY")
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
    nb_trading_days = 365 
    
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
    
    #Now, we trade
    
    #A random walk is generated as a a benchmark strategy
    
    randomOrders = sample(c(-1,1), replace=TRUE, size=nb_trading_days)
    
    #initial wallet : 100 cur_1 and 100 worth of cur_1 of cur_2 at date StartDate-1
    #Function to calculate total wealth
    wealth = function(dateIndex,cur_1,cur_2) {
      cur_1 + cur_2/as.double(data[dateIndex,"Close"])
    }
    
    cur_1_Global = 100
    cur_2_Global= 100*as.double(data[startIndex-1,"Close"])
    
    #############Trading with predictions : Most basic strategy
    cur_1 =cur_1_Global
    cur_2 = cur_2_Global
    histcur_1 = c(cur_1)
    histcur_2 = c(cur_2)
    initialWealth = wealth(startIndex-1,cur_1,cur_2)
    histWealth = c(initialWealth)
    tradeDates = NULL
    
    threshold = sd(data[1:(startIndex-1),"dailyReturn"])
    
    for(i in 1:nb_trading_days){
      if (predicted[i]>threshold) {
        #if forecasted return is positive, buy cur_1 and sell cur_2
        cur_1 = cur_1 + (cur_2/1)/as.double(data[startIndex+i-2,"Close"])
        cur_2 = 0
      }
      if (predicted[i]<(-threshold)){
        #if forecasted return is negative, buy cur_2 and sell cur_1
        cur_2 = cur_2 + (cur_1/1)*as.double(data[startIndex+i-2,"Close"])
        cur_1 = 0
      }
      histcur_1 = c(histcur_1,cur_1)
      histcur_2 = c(histcur_2,cur_2)
      newWealth = wealth(startIndex+i-1,cur_1,cur_2)
      histWealth = c(histWealth,newWealth)
    }
    
    
    #Data for becnhmark
    cur_1B =cur_1_Global
    cur_2B =cur_2_Global
    histcur_1B = c(cur_1B)
    histcur_2B = c(cur_2B)
    initialWealthB = wealth(startIndex-1,cur_1B,cur_2B)
    histWealthB = c(initialWealthB)
    
    for(i in 1:nb_trading_days){
      if (randomOrders[i]>0) {
        #if forecasted return is positive, buy cur_1 and sell cur_2
        cur_1B = cur_1B + (cur_2B/1)/as.double(data[startIndex+i-2,"Close"])
        cur_2B = 0
      }
      if (randomOrders[i]<0){
        #if forecasted return is negative, buy cur_2 and sell cur_1
        cur_2B = cur_2B + (cur_1B/1)*as.double(data[startIndex+i-2,"Close"])
        cur_1B = 0
      }
      histcur_1B = c(histcur_1B,cur_1B)
      histcur_2B = c(histcur_2B,cur_2B)
      newWealthB = wealth(startIndex+i-1,cur_1B,cur_2B)
      histWealthB = c(histWealthB,newWealthB)
    }
    
    ############PERFECT TRADING#####################
    #Data for becnhmark
    cur_1P =cur_1_Global
    cur_2P =cur_2_Global
    histcur_1P = c(cur_1P)
    histcur_2P = c(cur_2P)
    initialWealthP = wealth(startIndex-1,cur_1P,cur_2P)
    histWealthP = c(initialWealthP)
    
    for(i in 1:nb_trading_days){
      if (as.double(data[startIndex+i-1,"dailyReturn"])>0) {
        #if forecasted return is positive, Puy cur_1 and sell cur_2
        cur_1P = cur_1P + (cur_2P/1)/as.double(data[startIndex+i-2,"Close"])
        cur_2P = 0
      }
      if (as.double(data[startIndex+i-1,"dailyReturn"])<0){
        #if forecasted return is negative, buy cur_2 and sell cur_1
        cur_2P = cur_2P + (cur_1P/1)*as.double(data[startIndex+i-2,"Close"])
        cur_1P = 0
      }
      histcur_1P = c(histcur_1P,cur_1P)
      histcur_2P = c(histcur_2P,cur_2P)
      newWealthP = wealth(startIndex+i-1,cur_1P,cur_2P)
      histWealthP = c(histWealthP,newWealthP)
    }
    
    
    
    
    #On trace 
    par(mfrow=c(4,1))
    
    #On trace l'historique de nos transactions
    graphics::plot.default(
      x = dates[(startIndex-1):endIndex],
      y = histWealth,
      type = "l",
      xlab = "Date",
      ylab = "TotalWealthIncur_1",
      main = currencyPair
    )
    
    graphics::plot.default(
      x = dates[(startIndex-1):endIndex],
      y = histWealthB,
      type = "l",
      xlab = "Date",
      ylab = "TotalWealthIncur_1",
      main = currencyPair
    )
    
    graphics::plot.default(
      x = dates[(startIndex-1):endIndex],
      y = histWealthP,
      type = "l",
      xlab = "Date",
      ylab = "TotalWealthIncur_1",
      main = currencyPair
    )
    #Plotting predicted and actual daily Returns
    plot(dataWithDates[which(StartDate %in% startDate:endDate)][,dailyReturn>0],type="p")
    points(predicted>0,col="red")
   
    #Save plot
    dev.copy(png,as.character(paste("Plots/",currencyPair,".png",sep="")),res=500,width=8,height=10,units="in")
    dev.off()
    
    #Metrics for trading strategy
    metrics <- function(nb_trading_days,histWealth,Profits) {
      
      
      nb_winning_trades = 0
      nb_losing_trades = 0
      nb_neutral_trades = 0
      biggest_win = 0 
      biggest_loss = 0 
      
      for (j in 1:(nb_trading_days)){
        
        
        if ( (histWealth[j+1]-histWealth[j]) >0) {
          nb_winning_trades = nb_winning_trades+1
          if ((histWealth[j+1]-histWealth[j])>biggest_win) {biggest_win = (histWealth[j+1]-histWealth[j])}
        }
        else if ( (histWealth[j+1]-histWealth[j]) <0) {
          nb_losing_trades = nb_losing_trades+1
          if ((histWealth[j+1]-histWealth[j])<biggest_loss) {biggest_loss = (histWealth[j+1]-histWealth[j])}
        }
        else (nb_neutral_trades = nb_neutral_trades+1)
      }
      
      net_profit = histWealth[nb_trading_days]-histWealth[1]
      
      if (Profits=="TS") {
        ProfitsTradingStrategy <<- c(ProfitsTradingStrategy,net_profit)}
      if (Profits=="Random") {ProfitsRandom <<- c(ProfitsRandom,net_profit)}
  
    }
    metrics(nb_trading_days,histWealth,"TS")
    metrics(nb_trading_days,histWealthB,"Random")
  }
}



