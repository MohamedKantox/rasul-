FXRates_W[,RSI:=RSI(FXRates_W[,Close])]

#Stochastic Oscillator
stochFX = stoch(FXRates2[,c("High","Low","Close")])
FXRates2[,SMI:=stochFX[,"SMI"]]

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
