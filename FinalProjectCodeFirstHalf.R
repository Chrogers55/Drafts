#' ---
#' title: "Advanced Hedge Funds Final Project: First Half"
#' author: Charles Rogers
#' date: "5/11/2019"
#' ---

library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(quantstrat)
## Creates Currencies Portfolio##
symbols <- c("CROC","DBV","USDU","FXB")
##### ignore code until you see "Pay Attention" ########################
currency('USD')
stock(symbols,
      currency = "USD",
      multiplier = 1)
## Set up investment strategy params
init_date <- "2014-05-01"
start_date <- "2014-05-01"
end_date <- "2019-05-01"
init_equity <- 1000000000 # $1,000,000,000 One Billion Dollars
adjustment <- TRUE
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date, 
           to = end_date,
           adjust = adjustment)
getSymbols(Symbols = "SPY",
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
stock(symbols,
      currency = "USD",
      multiplier = 1)
portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"
#########^^^^^^^^Ignore ^^^^^^^^^########################
#########################################################
############################################################
### Once you run above, to rerun after makning changes ###
### to your strategy below, just run from here to end ###
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st)
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date) 
strategy(strategy.st, store = TRUE)
#######################################################
#### Pay attention! Change indicators here ###########
# SMA speed can be modified here
fastSMA=60
slowSMA=180
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = fastSMA),
              label = "nFast")
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = slowSMA),
              label = "nSlow")
#### Pay attention! Change signals here ##########
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")
#### Pay attention! Change trade rules here ########### 
# Trade sizes for long/short
longSize=1000000
shortSize=1000000
### LONG Rules
# Enter Long
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = longSize,
                          ordertype = "market",
                          orderside = "long",
                          prefer = "High",
                          TxnFees = 0,
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")
# Exit Long
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")
#### SHORT rules
# Enter Short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -shortSize,
                          ordertype = "market",
                          orderside = "short",
                          replace = FALSE,
                          TxnFees = 0,
                          prefer = "Low"),
         type = "enter", 
         label = "EnterSHORT")
# Exit Short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2SHORT")
### Add stop loss rule #####
.stoploss <- .1 # <<<< Modify stop loss as % of entry price here
## Stop loss long
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long" ,
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain",
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = T)
## Stop loss short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          replace = FALSE, 
                          orderside = "short",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain",
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled =T)
#### End of Stop Loss code ################
## Now get results for signal/rules/strategies
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  file.remove(results_file)}
results <- applyStrategy(strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
for(symbol in symbols) {
  print(symbol)
  chart.Posn(portfolio.st, Symbol = symbol,
             TA = "add_SMA(n = fastSMA, col = 4); add_SMA(n = slowSMA, col = 2)")
}
### Tabulate Result Stats by Individual Asset
tstats <- tradeStats(portfolio.st)
knitr::kable(t(tstats))
### Now get Total Portfolio Results ####
final_acct <- getAccount(account.st)
end_eq <- final_acct$summary$End.Eq
returns <- Return.calculate(end_eq, method="log")
charts.PerformanceSummary(returns, colorset = bluefocus, main = "Strategy Performance")
# calculate accumulated portfolio return
(end_eq[1237,]-1000000000)/1000000000 
# calculate sharpe ratio
sharperatio_portfolio <- SharpeRatio.annualized(returns, Rf = 0, scale = NA, geometric = TRUE)
sharperatio_portfolio
# calculate information ratio
library(PerformanceAnalytics)
Currencies_returns = Return.calculate(end_eq, method="log")
SPYreturns = Return.calculate(SPY[,4], method="log")
InformationRatio(SPYreturns,Currencies_returns)
Currenciesreturns<- Return.calculate(end_eq, method="log") 


###### Creates MANAGED FUTURES Portfolio #####
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(quantstrat)

symbols <- c("FMF","WTMF")
##### ignore code until you see "Pay Attention" ########################
currency('USD')
stock(symbols,
      currency = "USD",
      multiplier = 1)
## Set up investment strategy params
init_date <- "2014-05-01"
start_date <- "2014-05-01"
end_date <- "2019-05-01"
init_equity <- 1000000000 # $1,000,000,000 One Billion Dollars
adjustment <- TRUE 
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
stock(symbols,
      currency = "USD",
      multiplier = 1)
portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"
#########^^^^^^^^Ignore ^^^^^^^^^########################
#########################################################
############################################################
### Once you run above, to rerun after makning changes ###
### to your strategy below, just run from here to end ###
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st)
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)
strategy(strategy.st, store = TRUE) 
#######################################################
#### Pay attention! Change indicators here ###########
# SMA speed can be modified here
fastSMA=60
slowSMA=180
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = fastSMA),
              label = "nFast")
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = slowSMA),
              label = "nSlow")
#### Pay attention! Change signals here ##########
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")
#### Pay attention! Change trade rules here ###########
# Trade sizes for long/short 
longSize=1000000
shortSize=1000000
### LONG Rules
# Enter Long
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = longSize,
                          ordertype = "market",
                          orderside = "long",
                          prefer = "High",
                          TxnFees = 0,
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")
# Exit Long
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")
#### SHORT rules
# Enter Short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -shortSize,
                          ordertype = "market",
                          orderside = "short",
                          replace = FALSE,
                          TxnFees = 0,
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT") 
# Exit Short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2SHORT")
### Add stop loss rule #####
.stoploss <- .1 # <<<< Modify stop loss as % of entry price here
## Stop loss long
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long" ,
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain",
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = T)
## Stop loss short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit", 
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain",
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled =T)
#### End of Stop Loss code ################
## Now get results for signal/rules/strategies
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  file.remove(results_file)}
results <- applyStrategy(strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
for(symbol in symbols) {
  print(symbol)
  chart.Posn(portfolio.st, Symbol = symbol,
             TA = "add_SMA(n = fastSMA, col = 4); add_SMA(n = slowSMA, col = 2)")
}
### Tabulate Result Stats by Individual Asset
tstats <- tradeStats(portfolio.st)
knitr::kable(t(tstats))

### Now get Total Portfolio Results ####
final_acct <- getAccount(account.st)
end_eq <- final_acct$summary$End.Eq
returns <- Return.calculate(end_eq, method="log")
charts.PerformanceSummary(returns, colorset = bluefocus, main = "Strategy Performance")

# calculate accumulated portfolio return
(end_eq[1258,]-1000000000)/1000000000 
# calculate sharpe ratio
sharperatio_portfolio <- SharpeRatio.annualized(returns, Rf = 0, scale = NA, geometric = TRUE)
sharperatio_portfolio

# calculate information ratio
library(PerformanceAnalytics )
getSymbols(Symbols = "SPY",
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)

Managedfutures_returns<- Return.calculate(end_eq, method="log")

########## Hedge Fund Replication
#### Set up packages we need to use below ##########
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(quantstrat)

symbols <- c("ALFA","CSM","HDG")
##ignore code until you see "Pay Attention"
currency('USD')
stock(symbols,
      currency = "USD",
      multiplier = 1)
## Set up investment strategy params
init_date <- "2014-05-01"
start_date <- "2014-05-01"
end_date <- "2019-05-01"
init_equity <- 1000000000 # $1,000,000,000
adjustment <- TRUE 
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
stock(symbols,
      currency = "USD",
      multiplier = 1)
portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"
#########^^^^^^^^Ignore ^^^^^^^^^########################
#########################################################
############################################################
### Once you run above, to rerun after makning changes ###
### to your strategy below, just run from here to end ###
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st)
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)
strategy(strategy.st, store = TRUE) 
#######################################################
#### Pay attention! Change indicators here ###########
# SMA speed can be modified here
fastSMA=60
slowSMA=180
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = fastSMA),
              label = "nFast")
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = slowSMA),
              label = "nSlow")
#### Pay attention! Change signals here ##########
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")
#### Pay attention! Change trade rules here ###########
# Trade sizes for long/short 
longSize=1000000
shortSize=1000000
### LONG Rules
# Enter Long
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = longSize,
                          ordertype = "market",
                          orderside = "long",
                          prefer = "High",
                          TxnFees = 0,
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")
# Exit Long
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")
#### SHORT rules
# Enter Short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -shortSize,
                          ordertype = "market",
                          orderside = "short",
                          replace = FALSE,
                          TxnFees = 0,
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT") 
# Exit Short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2SHORT")
### Add stop loss rule #####
.stoploss <- .1 # <<<< Modify stop loss as % of entry price here
## Stop loss long
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long" ,
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain",
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = T)
## Stop loss short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit", 
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain",
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled =T)
#### End of Stop Loss code ################
## Now get results for signal/rules/strategies
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  file.remove(results_file)}
results <- applyStrategy(strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
for(symbol in symbols) {
  print(symbol)
  chart.Posn(portfolio.st, Symbol = symbol,
             TA = "add_SMA(n = fastSMA, col = 4); add_SMA(n = slowSMA, col = 2)")
}
### Tabulate Result Stats by Individual Asset
tstats <- tradeStats(portfolio.st)
knitr::kable(t(tstats))
### Now get Total Portfolio Results ####
final_acct <- getAccount(account.st)
end_eq <- final_acct$summary$End.Eq
returns <- Return.calculate(end_eq, method="log")
charts.PerformanceSummary(returns, colorset = bluefocus, main = "Strategy Performance")
# calculate accumulated portfolio return
(end_eq[1258,]-1000000000)/1000000000 
# calculate sharpe ratio
sharperatio_portfolio <- SharpeRatio.annualized(returns, Rf = 0, scale = NA, geometric = TRUE)
sharperatio_portfolio
# calculate information ratio
library(PerformanceAnalytics)
getSymbols(Symbols = "SPY",
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
SPYreturns = Return.calculate(SPY[,6], method="log")
InformationRatio(returns,SPYreturns)
Hedgefunds_returns<- Return.calculate(end_eq, method="log")

########## Volatility Investing
#### Set up packages we need to use below ##########
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(quantstrat)

symbols <- c("SVXY","VIXM","VIIX",
             "VIXY","UVXY","TVIX","ZIV")
##ignore code until you see "Pay Attention"
currency('USD')
stock(symbols,
      currency = "USD",
      multiplier = 1)
## Set up investment strategy params
init_date <- "2014-05-01"
start_date <- "2014-05-01"
end_date <- "2019-05-01"
init_equity <- 1000000000 # $1,000,000,000
adjustment <- TRUE 
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
stock(symbols,
      currency = "USD",
      multiplier = 1)
portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"
#########^^^^^^^^Ignore ^^^^^^^^^########################
#########################################################
############################################################
### Once you run above, to rerun after makning changes ###
### to your strategy below, just run from here to end ###
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st)
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)
strategy(strategy.st, store = TRUE) 
#######################################################
#### Pay attention! Change indicators here ###########
# SMA speed can be modified here
fastSMA=60
slowSMA=180
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = fastSMA),
              label = "nFast")
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = slowSMA),
              label = "nSlow")
#### Pay attention! Change signals here ##########
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")
#### Pay attention! Change trade rules here ###########
# Trade sizes for long/short 
longSize=1000000
shortSize=1000000
### LONG Rules
# Enter Long
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = longSize,
                          ordertype = "market",
                          orderside = "long",
                          prefer = "High",
                          TxnFees = 0,
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")
# Exit Long
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")
#### SHORT rules
# Enter Short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -shortSize,
                          ordertype = "market",
                          orderside = "short",
                          replace = FALSE,
                          TxnFees = 0,
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT") 
# Exit Short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2SHORT")
### Add stop loss rule #####
.stoploss <- .1 # <<<< Modify stop loss as % of entry price here
## Stop loss long
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long" ,
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain",
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = T)
## Stop loss short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit", 
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain",
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled =T)
#### End of Stop Loss code ################
## Now get results for signal/rules/strategies
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  file.remove(results_file)}
results <- applyStrategy(strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
for(symbol in symbols) {
  print(symbol)
  chart.Posn(portfolio.st, Symbol = symbol,
             TA = "add_SMA(n = fastSMA, col = 4); add_SMA(n = slowSMA, col = 2)")
}
### Tabulate Result Stats by Individual Asset
tstats <- tradeStats(portfolio.st)
knitr::kable(t(tstats))
### Now get Total Portfolio Results ####
final_acct <- getAccount(account.st)
end_eq <- final_acct$summary$End.Eq
returns <- Return.calculate(end_eq, method="log")
charts.PerformanceSummary(returns, colorset = bluefocus, main = "Strategy Performance")
# calculate accumulated portfolio return
(end_eq[1258,]-1000000000)/1000000000 
# calculate sharpe ratio
sharperatio_portfolio <- SharpeRatio.annualized(returns, Rf = 0, scale = NA, geometric = TRUE)
sharperatio_portfolio
# calculate information ratio
library(PerformanceAnalytics)
getSymbols(Symbols = "SPY",
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
SPYreturns = Return.calculate(SPY[,6], method="log")
InformationRatio(returns,SPYreturns)
Volatility_returns<- Return.calculate(end_eq, method="log")


######### Active/ Event Driven Strategy
#### Set up packages we need to use below ##########
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(quantstrat)
## This is your universe of investable assets #####
## To remove an asset from the list, just delete #####
symbols <- c("IPO","FPX","MNA")
##### ignore code until you see "Pay Attention" ########################
currency('USD')
stock(symbols,
      currency = "USD",
      multiplier = 1)
## Set up investment strategy params
init_date <- "2014-05-01" 
start_date <- "2014-05-01"
end_date <- "2019-05-01"
init_equity <- 1000000000 # $1,000,000,000 One Billion Dollars
adjustment <- TRUE
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
stock(symbols,
      currency = "USD",
      multiplier = 1)
portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"
#########^^^^^^^^Ignore ^^^^^^^^^########################
#########################################################
############################################################
### Once you run above, to rerun after makning changes ###
### to your strategy below, just run from here to end ###
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st)
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
initOrders(portfolio = portfolio.st, 
           symbols = symbols,
           initDate = init_date)
strategy(strategy.st, store = TRUE)
#######################################################
#### Pay attention! Change indicators here ###########
# SMA speed can be modified here
fastSMA=60
slowSMA=180
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = fastSMA),
              label = "nFast")
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = slowSMA),
              label = "nSlow")
#### Pay attention! Change signals here ##########
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short") 
#### Pay attention! Change trade rules here ###########
# Trade sizes for long/short
longSize=1000000
shortSize=1000000
### LONG Rules
# Enter Long
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = longSize,
                          ordertype = "market",
                          orderside = "long",
                          prefer = "High",
                          TxnFees = 0,
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")
# Exit Long
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")
#### SHORT rules
# Enter Short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -shortSize,
                          ordertype = "market",
                          orderside = "short",
                          replace = FALSE,
                          TxnFees = 0, 
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT")
# Exit Short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2SHORT")
### Add stop loss rule #####
.stoploss <- .1 # <<<< Modify stop loss as % of entry price here
## Stop loss long
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long" ,
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain",
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = T)
## Stop loss short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short", 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain",
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled =T)
#### End of Stop Loss code ################
## Now get results for signal/rules/strategies
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  file.remove(results_file)}
results <- applyStrategy(strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
for(symbol in symbols) {
  print(symbol)
  chart.Posn(portfolio.st, Symbol = symbol,
             TA = "add_SMA(n = fastSMA, col = 4); add_SMA(n = slowSMA, col = 2)")
}
### Tabulate Result Stats by Individual Asset
tstats <- tradeStats(portfolio.st)
knitr::kable(t(tstats))
### Now get Total Portfolio Results ####
final_acct <- getAccount(account.st)
end_eq <- final_acct$summary$End.Eq
returns <- Return.calculate(end_eq, method="log")
charts.PerformanceSummary(returns, colorset = bluefocus, main = "Strategy Performance") 
# calculate accumulated portfolio return
(end_eq[1258,]-10000000)/10000000
# calculate sharpe ratio
sharperatio_portfolio <- SharpeRatio.annualized(returns, Rf = 0, scale = NA, geometric = TRUE)
sharperatio_portfolio
# calculate information ratio
library(PerformanceAnalytics )
getSymbols(Symbols = "SPY",
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
Eventdriven_returns = Return.calculate(end_eq, method="log")
InformationRatio(returns,Eventdriven_returns)
Eventdriven_returns<- Return.calculate(end_eq, method="log")

####### Realestate Hedging
#### Set up packages we need to use below ##########
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(quantstrat)
symbols <- c("DRN","URE","REZ","ICF","USRT",
             "VNQ","PSR","IYR","FRI","SCHH",
             "RWR","GRI","REM","MORT","REK")
##### ignore code until you see "Pay Attention" ########################
currency('USD')
stock(symbols,
      currency = "USD",
      multiplier = 1)
## Set up investment strategy params 
init_date <- "2014-05-01"
start_date <- "2014-05-01"
end_date <- "2019-05-01"
init_equity <- 1000000000 # $1,000,000,000 One Billion Dollars
adjustment <- TRUE
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
getSymbols(Symbols = "SPY",
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
stock(symbols,
      currency = "USD",
      multiplier = 1)
portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"
#########^^^^^^^^Ignore ^^^^^^^^^########################
#########################################################
############################################################
### Once you run above, to rerun after makning changes ###
### to your strategy below, just run from here to end ###
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st)
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date) 
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)
strategy(strategy.st, store = TRUE)
#######################################################
#### Pay attention! Change indicators here ###########
# SMA speed can be modified here
fastSMA=35
slowSMA=85
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = fastSMA),
              label = "nFast")
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = slowSMA),
              label = "nSlow")
#### Pay attention! Change signals here ##########
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long") 
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")
#### Pay attention! Change trade rules here ###########
# Trade sizes for long/short
longSize=1000000
shortSize=1000000
### LONG Rules
# Enter Long
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = longSize,
                          ordertype = "market",
                          orderside = "long",
                          prefer = "High",
                          TxnFees = 0,
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")
# Exit Long
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")
#### SHORT rules
# Enter Short
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -shortSize,
                          ordertype = "market",
                          orderside = "short",
                          replace = FALSE,
                          TxnFees = 0,
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT")
# Exit Short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = 0,
                          replace = TRUE),
         type = "exit",
         label = "Exit2SHORT")
### Add stop loss rule #####
.stoploss <- .17 # <<<< Modify stop loss as % of entry price here
## Stop loss long
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long" ,
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain",
         parent = "EnterLONG", 
         label = "StopLossLONG",
         enabled = T)
## Stop loss short
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = 0,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain",
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled =T)
#### End of Stop Loss code ################
## Now get results for signal/rules/strategies
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  file.remove(results_file)}
results <- applyStrategy(strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
for(symbol in symbols) {
  print(symbol)
  chart.Posn(portfolio.st, Symbol = symbol,
             TA = "add_SMA(n = fastSMA, col = 4); add_SMA(n = slowSMA, col = 2)")
}
### Tabulate Result Stats by Individual Asset
tstats <- tradeStats(portfolio.st)
knitr::kable(t(tstats)) 
### Now get Total Portfolio Results ####
final_acct <- getAccount(account.st)
end_eq <- final_acct$summary$End.Eq
returns <- Return.calculate(end_eq, method="log")
charts.PerformanceSummary(returns, colorset = bluefocus, main = "Strategy Performance")
# calculate accumulated portfolio return
(end_eq[1258,]-10000000)/10000000
# calculate sharpe ratio
sharperatio_portfolio <- SharpeRatio.annualized(returns, Rf = 0, scale = NA, geometric = TRUE)
sharperatio_portfolio
# calculate information ratio
library(PerformanceAnalytics)
Realestate_returns = Return.calculate(end_eq, method="log")
SPYreturns = Return.calculate(SPY[,4], method="log")
InformationRatio(SPYreturns,Realestate_returns)
Realestate_returns<- Return.calculate(end_eq, method="log")

