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
for(i in 1:nb_trading_days){
  if (predicted[i]>0) {
    #if forecasted return is positive, buy cur_1 and sell cur_2
    cur_1 = cur_1 + (cur_2/1)/as.double(data[startIndex+i-2,"Close"])
    cur_2 = 0
  }
  if (predicted[i]<0){
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
  main = symbol
)

graphics::plot.default(
  x = dates[(startIndex-1):endIndex],
  y = histWealthB,
  type = "l",
  xlab = "Date",
  ylab = "TotalWealthIncur_1",
  main = symbol
)

graphics::plot.default(
  x = dates[(startIndex-1):endIndex],
  y = histWealthP,
  type = "l",
  xlab = "Date",
  ylab = "TotalWealthIncur_1",
  main = symbol
)



#Metrics for trading strategy
metrics <- function(nb_trading_days,histWealth) {
  

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
  
  cat("Net profit : ", net_profit,"\n",
      "Profit % per month: ", 100*30*(net_profit/histWealth[1])/nb_trading_days,"\n",
      "Winning days : ", nb_winning_trades, " .Accouting for ",100*nb_winning_trades/nb_trading_days,"%\n",
      "Losing days : ", nb_losing_trades, " .Accouting for ",100*nb_losing_trades/nb_trading_days,"%\n",
      "Neutral days : ", nb_neutral_trades, " .Accouting for ",100*nb_neutral_trades/nb_trading_days,"%\n",
      "Biggest win : ",biggest_win,"\n",
      "Biggest loss : ",biggest_loss, sep="")
}

metrics(nb_trading_days,histWealth)
cat("########AND NOW THE DUMMY STATSs############")
metrics(nb_trading_days,histWealthB)
cat("########God stats############")
metrics(nb_trading_days,histWealthP)

#Accurate prediction rate
accuratePredictionnRate = 0
for (i in startIndex:endIndex){}
for (i in 1:nb_trading_days){
       if (as.double(data[startIndex+i-1,"dailyReturn"])*predicted[i]>=0) accuratePredictionnRate = accuratePredictionnRate+1
   }
accuratePredictionnRate = accuratePredictionnRate/nb_trading_days
cat("####################")
cat("Accurate Prediction Rate (APR) :", accuratePredictionnRate*100,"%" )

cat("####################")
accurateDummyRate = 0
for (i in startIndex:endIndex){}
for (i in 1:nb_trading_days){
  if (as.double(data[startIndex+i-1,"dailyReturn"])*randomOrders[i]>=0) accurateDummyRate = accurateDummyRate+1
}
accurateDummyRate = accurateDummyRate/nb_trading_days
cat("And this should be close to 50% (dummy) :", accurateDummyRate*100,"%" )


#Plotting predicted and actual daily Returns
plot(dataWithDates[which(StartDate %in% startDate:endDate)][,dailyReturn>0],type="p")
points(predicted>0,col="red")
