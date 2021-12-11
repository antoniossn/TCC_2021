
#instalação dos pacotes ( se for instalar tirar #)
#install.packages('quantmod')
#install.packages('fPortfolio')
#install.packages('PerformanceAnalytics')

#chamando os pacotes
library(quantmod)
library(fPortfolio)
library(PerformanceAnalytics)

#Inserindo base historica
base = read.csv("base_todos.csv", sep = ",")
base_indices = as.timeSeries(base)

#calculando retornos diarios
rets = na.omit(ROC(base_indices,type = "discrete"))
rets

#separando intervalos 12M
rets_2011 = rets[0:251,1:15]
rets_2012 = rets[252:502,1:15]
rets_2013 = rets[504:755,1:15]
rets_2014 = rets[756:1008,1:15]
rets_2015 = rets[1009:1258,1:15]
rets_2016 = rets[1259:1509,1:15]
rets_2017 = rets[1510:1758,1:15]
rets_2018 = rets[1759:2008,1:15]
rets_2019 = rets[2009:2261,1:15]
rets_2020 = rets[2262:2512,1:15]

#intervalo para analise de performance
rets_back = rets[252:2512, 1:15]
tail(rets_back,1)
head(rets_back,1)

# 6% DIARIO
pre_meta = ((1+0.06)^(1/252))-1
pre_meta

#meta atuarial  2011-2019
meta_2011 <- ((1+rets_2011[,"IPCA"]) * (1+pre_meta))-1
meta_2012 <- ((1+rets_2012[,"IPCA"]) * (1+pre_meta))-1
meta_2013 <- ((1+rets_2013[,"IPCA"]) * (1+pre_meta))-1
meta_2014 <- ((1+rets_2014[,"IPCA"]) * (1+pre_meta))-1
meta_2015 <- ((1+rets_2015[,"IPCA"]) * (1+pre_meta))-1
meta_2016 <- ((1+rets_2016[,"IPCA"]) * (1+pre_meta))-1
meta_2017 <- ((1+rets_2017[,"IPCA"]) * (1+pre_meta))-1
meta_2018 <- ((1+rets_2018[,"IPCA"]) * (1+pre_meta))-1
meta_2019 <- ((1+rets_2019[,"IPCA"]) * (1+pre_meta))-1
meta_2020 <- ((1+rets_2020[,"IPCA"]) * (1+pre_meta))-1

#meta de todo periodo
meta_back = ((1+rets_back[,"IPCA"]) * (1+pre_meta))-1


# target return 2011-2019 (meta atuarial 12M)

tr_2011 <- Return.cumulative(meta_2011)/252
tr_2012 <- Return.cumulative(meta_2012)/252 
tr_2013 <- 0.1045261763/252 #Return.cumulative(meta_2013)/252
tr_2014 <- Return.cumulative(meta_2014)/252 
tr_2015 <- Return.cumulative(meta_2015)/252 
tr_2016 <- Return.cumulative(meta_2016)/252 
tr_2017 <- Return.cumulative(meta_2017)/252 
tr_2018 <- Return.cumulative(meta_2018)/252 
tr_2019 <- Return.cumulative(meta_2019)/252 

#restricoes da resolução
restricoes = c("maxsumW[1:2]=0.40","maxW[10]=0.20","maxW[11]=0.20","maxW[12]=0.20","maxsumW[10:12]=0.30","maxsumW[13:15]=0.10","LongOnly")

#spec
spec = portfolioSpec()

#fronteiras eficientes
fronteira_2011 = portfolioFrontier(rets_2011,spec, constraints = restricoes)
fronteira_2012 = portfolioFrontier(rets_2012,spec, constraints = restricoes)
fronteira_2013 = portfolioFrontier(rets_2013,spec, constraints = restricoes)
fronteira_2014 = portfolioFrontier(rets_2014,spec, constraints = restricoes)
fronteira_2015 = portfolioFrontier(rets_2015,spec, constraints = restricoes)
fronteira_2016 = portfolioFrontier(rets_2016,spec, constraints = restricoes)
fronteira_2017 = portfolioFrontier(rets_2017,spec, constraints = restricoes)
fronteira_2018 = portfolioFrontier(rets_2018,spec, constraints = restricoes)
fronteira_2019 = portfolioFrontier(rets_2019,spec, constraints = restricoes)


#plotando as fronteiras

par(mfrow = c(3, 3))
tailoredFrontierPlot(fronteira_2011)
plot(fronteira_2011, c(1,2,3))
frontierPlot(fronteira_2011)
frontierPlot(fronteira_2012)
frontierPlot(fronteira_2013)
frontierPlot(fronteira_2014)
frontierPlot(fronteira_2015)
frontierPlot(fronteira_2016)
frontierPlot(fronteira_2017)
frontierPlot(fronteira_2018)
frontierPlot(fronteira_2019)

#carteiras otimas: Rc = ipca+6%

otima_2011 = efficientPortfolio(rets_2011,`setTargetReturn<-`(spec, tr_2011), constraints = restricoes)
otima_2012 = efficientPortfolio(rets_2012,`setTargetReturn<-`(spec, tr_2012), constraints = restricoes)
otima_2013 = efficientPortfolio(rets_2013,`setTargetReturn<-`(spec, tr_2013), constraints = restricoes)
otima_2014 = efficientPortfolio(rets_2014,`setTargetReturn<-`(spec, tr_2014), constraints = restricoes)
otima_2015 = efficientPortfolio(rets_2015,`setTargetReturn<-`(spec, tr_2015), constraints = restricoes)
otima_2016 = efficientPortfolio(rets_2016,`setTargetReturn<-`(spec, tr_2016), constraints = restricoes)
otima_2017 = efficientPortfolio(rets_2017,`setTargetReturn<-`(spec, tr_2017), constraints = restricoes)
otima_2018 = efficientPortfolio(rets_2018,`setTargetReturn<-`(spec, tr_2018), constraints = restricoes)
otima_2019 = efficientPortfolio(rets_2019,`setTargetReturn<-`(spec, tr_2019), constraints = restricoes)


#pesos das carteiras otimas

w_2011 =getWeights(otima_2011)                                
w_2012 =getWeights(otima_2012)
w_2013 =getWeights(otima_2013)
w_2014 =getWeights(otima_2014)
w_2015 =getWeights(otima_2015)
w_2016 =getWeights(otima_2016)
w_2017 =getWeights(otima_2017)
w_2018 =getWeights(otima_2018)
w_2019 =getWeights(otima_2019)

# pieplot dos pesos das carteiras otimas

par(mfrow = c(1,1))
weightsPie(otima_2011, col=rainbow(ncol(rets)))
weightsPie(otima_2012, col=rainbow(ncol(rets)))
weightsPie(otima_2013, col=rainbow(ncol(rets)))
weightsPie(otima_2014, col=rainbow(ncol(rets)))
weightsPie(otima_2015, col=rainbow(ncol(rets)))
weightsPie(otima_2016, col=rainbow(ncol(rets)))
weightsPie(otima_2017, col=rainbow(ncol(rets)))
weightsPie(otima_2018, col=rainbow(ncol(rets)))
weightsPie(otima_2019, col=rainbow(ncol(rets)))


#retorno esperado das carteiras
getTargetReturn(otima_2011) * 252
getTargetReturn(otima_2012) * 252
getTargetReturn(otima_2013) * 252
getTargetReturn(otima_2014) * 252
getTargetReturn(otima_2015) * 252
getTargetReturn(otima_2016) * 252
getTargetReturn(otima_2017) * 252
getTargetReturn(otima_2018) * 252
getTargetReturn(otima_2019) * 252

#retorno dos portfolios por ano
portfolio_2012 = Return.portfolio(rets_2012, weights = w_2011)
portfolio_2013 = Return.portfolio(rets_2013, weights = w_2012)
portfolio_2014 = Return.portfolio(rets_2014, weights = w_2013)
portfolio_2015 = Return.portfolio(rets_2015, weights = w_2014)
portfolio_2016 = Return.portfolio(rets_2016, weights = w_2015)
portfolio_2017 = Return.portfolio(rets_2017, weights = w_2016)
portfolio_2018 = Return.portfolio(rets_2018, weights = w_2017)
portfolio_2019 = Return.portfolio(rets_2019, weights = w_2018)
portfolio_2020 = Return.portfolio(rets_2020, weights = w_2019)


# peformance por ano

df_2012 = cbind(portfolio_2012, meta_2012)
charts.PerformanceSummary(df_2012)

df_2013 = cbind(portfolio_2013, meta_2013)
charts.PerformanceSummary(df_2013)

df_2014 = cbind(portfolio_2014, meta_2014)
charts.PerformanceSummary(df_2014)

df_2015 = cbind(portfolio_2015, meta_2015)
charts.PerformanceSummary(df_2015)

df_2016 = cbind(portfolio_2016, meta_2016)
charts.PerformanceSummary(df_2016)

df_2017 = cbind(portfolio_2017, meta_2017)
charts.PerformanceSummary(df_2017)

df_2018 = cbind(portfolio_2018, meta_2018)
charts.PerformanceSummary(df_2018)

df_2019 = cbind(portfolio_2019, meta_2019)
charts.PerformanceSummary(df_2019)

df_2020 = cbind(portfolio_2020, meta_2020)
charts.PerformanceSummary(df_2020)


# DF pesos todas as carteiras (todo periodo)
par(mfrow = c(1,1))
weights_df <- rbind(w_2011,w_2012,w_2013,w_2014,w_2015,w_2016,w_2017,w_2018,w_2019)
weights_df
      
#write.csv(weights_df, "df_pesos_2.csv")
pesos_rebal = read.csv("rebalanced.csv")
times_rebal = as.timeSeries(pesos_rebal)
times_rebal

###################### peformance da estrategia ########################


portfolio_back = Return.portfolio(rets_back, weights = times_rebal)
head(portfolio_back,5)

colnames(meta_back) <-"META"
df_back = cbind(portfolio_back, 
                    meta_back, 
                    rets_back[,"CDI"],
                    rets_back[,"IMA.B"],
                    rets_back[,"Ibovespa"])

charts.PerformanceSummary(df_back)


####################################################
