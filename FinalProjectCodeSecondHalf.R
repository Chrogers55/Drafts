
######## Choose Other Assets Here #########################
symbols <- c("GLD", "IAU", "SLV", "DBC", "USO",
             "PPLT", "DBE", "BNO", "UGAZ", "GLL",
             "PALL", "VXX", "VIXM", "QQQ",
             "EWJ", "EWG", "EWQ", "EWH", "CORN",
             "WEAT", "SOYB")
benchmark="SPY"
#####leave dates alone #####
## Set up investment strategy params
init_date <- "2014-05-01"
start_date <- "2014-05-01" 
end_date <- "2019-05-01"
init_equity <- 1000000000 # $1,000,000,000
### Get Market Data ##############################
### Market ####
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = init_date,
           to = end_date,
           adjust = TRUE)
getSymbols(Symbols = benchmark,
           src = "yahoo",
           index.class = "POSIXct",
           from = init_date,
           to = end_date,
           adjust = TRUE)
### Get Benchmark Data ############################
ff3=readRDS("ff3.rds")/100


########### Leave code below alone ####################################
########### Factor Matrix Build #######################################
### get regression coefs and return time series ###
dRange=paste0(start_date,"/",end_date)
clRange=paste0(init_date,"/",end_date)
clcl=ClCl(SPY)[clRange]
idx=intersect(as.Date(index(clcl)),as.Date(index(ff3)))
fit=lm(clcl[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
rets=clcl
colnames(rets)="SPY"
factorCoefs=data.frame("SPY",fit$coefficients[2],fit$coefficients[3],fit$coefficients[4])
names(factorCoefs)<-c("Symbol","Mkt","SMB","HML")
rownames(factorCoefs)=""

#1 now we'll do our CTA here 
idx=intersect(as.Date(index(Currenciesreturns)),as.Date(index(ff3)))
### now fama french fit ####
fit=lm(Currenciesreturns[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
factorCoefs=rbind(factorCoefs,data.frame(Symbol="CCY",Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
rets=cbind(rets,Currenciesreturns)
colnames(rets)=c(colnames(rets[,-ncol(rets)]),"CCY")

#2
# now we'll do our CTA here
idx=intersect(as.Date(index(Managedfutures_returns)),as.Date(index(ff3)))
### now fama french fit ####
fit=lm(Managedfutures_returns[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
factorCoefs=rbind(factorCoefs,data.frame(Symbol="MF",Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
rets=cbind(rets,Managedfutures_returns)
colnames(rets)=c(colnames(rets[,-ncol(rets)]),"MF")

#3
# now we'll do our CTA here
idx=intersect(as.Date(index(Hedgefunds_returns)),as.Date(index(ff3)))
### now fama french fit ####
fit=lm(Hedgefunds_returns[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
factorCoefs=rbind(factorCoefs,data.frame(Symbol="Hedge",Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
rets=cbind(rets,Hedgefunds_returns)
colnames(rets)=c(colnames(rets[,-ncol(rets)]),"Hedge")

#4
# now we'll do our CTA here
idx=intersect(as.Date(index(Volatility_returns)),as.Date(index(ff3)))
### now fama french fit ####
fit=lm(Volatility_returns[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
factorCoefs=rbind(factorCoefs,data.frame(Symbol="Volatility",Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
rets=cbind(rets,Volatility_returns)
colnames(rets)=c(colnames(rets[,-ncol(rets)]),"Volatility")

#5
# now we'll do our CTA here
idx=intersect(as.Date(index(Eventdriven_returns)),as.Date(index(ff3)))
### now fama french fit ####
fit=lm(Eventdriven_returns[as.Date(idx)]~ff3[as.Date(idx)][,1:3]) 
factorCoefs=rbind(factorCoefs,data.frame(Symbol="Event",Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
rets=cbind(rets,Eventdriven_returns)
colnames(rets)=c(colnames(rets[,-ncol(rets)]),"Event")

#6
# now we'll do our CTA here
idx=intersect(as.Date(index(Realestate_returns)),as.Date(index(ff3)))
### now fama french fit ####
fit=lm(Realestate_returns[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
factorCoefs=rbind(factorCoefs,data.frame(Symbol="Real",Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
rets=cbind(rets,Realestate_returns)
colnames(rets)=c(colnames(rets[,-ncol(rets)]),"Real")

for(symbol in symbols){
  
  clcl=ClCl(get(symbol))[clRange]
  idx=intersect(as.Date(index(clcl)),as.Date(index(ff3)))
  ### now fama french fit ####
  fit=lm(clcl[as.Date(idx)]~ff3[as.Date(idx)][,1:3])
  
  factorCoefs=rbind(factorCoefs,data.frame(Symbol=symbol,Mkt=fit$coefficients[2],SMB=fit$coefficients[3],HML=fit$coefficients[4]))
  rets=cbind(rets,clcl)
  colnames(rets)=c(colnames(rets[,-ncol(rets)]),symbol)
  
}
rownames(factorCoefs)=c()
rets=rets[complete.cases(rets)]
####^^^^^^^^^ Leave Alone^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^####

############################### Optimize ################################################
#### Now you have your return data for your chosen symbols and the factor coefs #######
#' Create portfolio object
pspec <- portfolio.spec(assets=colnames(rets))
#' Here we define individual constraint objects.
#' Leverage constraint.###This contsraint governs the amount of leverage you can use, max= 1.5###
lev_constr <- weight_sum_constraint(min_sum=1, max_sum=1.5)
#' Box constraint
lo_constr <- box_constraint(assets=pspec$assets, min=c(-15, rep(-10,6),rep(-5,ncol(rets)-7)),
                            max=c(0, rep(10,6), rep(5,ncol(rets)-7)))
#' Position limit constraint
pl_constr <- position_limit_constraint(assets=pspec$assets,
                                       max_pos=ncol(rets)-1,max_pos_short = 6)
#' FF3 exposure constraint.###The exposure constraint and group constraint are equivalent to test that they result in the same solution.
#' c(MKT,SMB,HML)
#' MKT cannot go above .1, everything else can change
lower <- c(-.2, -1.1, 1.3)
upper <- c(.1, 0.6, 2.0)
mf=as.matrix(factorCoefs[,-1])
rownames(mf)=factorCoefs$Symbol
exp_constr <- factor_exposure_constraint(assets=pspec$assets, B=mf, lower=lower,upper=upper)

#' Objective to minimize variance.
var_obj <- portfolio_risk_objective(name="var")

#' Objective to maximize return.
ret_obj <- return_objective(name="mean")

#' Run optimization on minimum variance portfolio with leverage, long only,and group constraints. 
opta <- optimize.portfolio(R=rets, portfolio=pspec,
                           constraints=list(lev_constr, lo_constr, pl_constr,exp_constr),
                           objectives=list(var_obj,ret_obj),
                           maxSR=TRUE,
                           optimize_method="ROI")
## look at fit ###
print(opta)
# Get returns of your strategy
myStratRets=xts(rets%*%opta$weights,order.by=index(rets))
# Get Sharpe (remember to get annualized to turn in)
SharpeRatio(myStratRets)
### And check ff3 contraint was met in data:mkt <.2 #####
idx=intersect(as.Date(index(myStratRets)),as.Date(index(ff3)))
fit=lm(myStratRets~ff3[as.Date(idx)][,1:3])
summary(fit)
#######
# Get Sharpe (remember to get annualized to turn in)
SharpeRatio(myStratRets)
# accumulated return
accumulated_return=sum(myStratRets[,1])
accumulated_return
#calcularte annualized return for our strategy and benchmark
Return.annualized(myStratRets)
Return.annualized(rets[,1])
write.table(myStratRets,file = "daily log return",col.names = "TRUE",row.names = TRUE) 