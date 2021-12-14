library(tidyverse)
library(vars)
library(mFilter)
library(tseries)
library(urca)
library(easyGplot2)
library(forecast)
library(rmarkdown)
library(tinytex)
library(stargazer)
library(sandwich)

#####varx

#loading data

library(readxl)
dados <- read_excel("C:/Users/joao.leite/Desktop/dados.xlsx", 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
View(dados)



#extracting gdp gaps with hp filter

hp<-hpfilter(dados$GDP, type = 'lambda', freq = 14400)
gap<-hp$cycle


#reshaping dataset
gap
gap1<-ts(gap,start=c(2000,02), freq=12)
dados1<-ts(dados[,-1], start=c(2000,02), freq=12)

dados1
model<- ts.intersect(gap1,dados1[,1],dados1[,2],dados1[,3],dados[,6],dados[,7], dados[,8], dados[,9])
model

colnames(model)<-c('GDP Gap',"Monetary Credibility Gap", "Fiscal Credibility Gap",
                   "SELIC", "FX", "Inflation","GET Index","FED Funds Rate")

plot.ts(model)

#ADF tests

summary(ur.df(y=model[,1],type="trend", selectlags = "AIC"))
summary(ur.df(y=model[,2],type="trend", selectlags = "AIC"))
summary(ur.df(y=model[,3],type="trend", selectlags = "AIC"))
summary(ur.df(y=model[,4],type="trend", selectlags = "AIC"))
summary(ur.df(y=model[,5],type="trend", selectlags = "AIC"))
summary(ur.df(y=model[,6],type="trend", selectlags = "AIC"))
summary(ur.df(y=model[,7],type="trend", selectlags = "AIC"))
summary(ur.df(y=model[,8],type="trend", selectlags = "AIC"))

##reshaping data

gdp_gap<-model[,1]
m_gap<-(model[,2])
f_gap<-(model[,3])
selic<-(model[,4])
FX<-diff(model[,5])
inflation<-model[,6]

hp2<-hpfilter(model[,7], type = 'lambda', freq = 14400)
geti_gap<-hp2$cycle

ts.plot(geti_gap)
fed_rates<-diff(model[,8])

model_var<-ts.intersect(gdp_gap,m_gap,f_gap,selic,FX,inflation, geti_gap, fed_rates)
model_var


#setting the seasonal dummies

dummies<-seasonaldummy(model_var[,1])

##selecting the var

exogen=ts.intersect(model_var[,7], model_var[,8])
geti_gap<-exogen[,1]
fed_rates<-exogen[,2]
exogenous<-ts.intersect(geti_gap, fed_rates)

Model_var<-ts.intersect(FX,selic,inflation,gdp_gap,f_gap, m_gap)

def=VARselect(Model_var,lag.max=12, season = 12, type = 'const', exogen = exogenous)
def


## estimating the var
var=VAR(Model_var,p=2,type = 'const',exogen = exogenous)
summary(var)
t1<-var$varresult$FX
t2<-var$varresult$selic
t3<-var$varresult$inflation
t4<-var$varresult$gdp_gap
t5<-var$varresult$m_gap
t6<-var$varresult$f_gap

stargazer(t1,
          t2,
          t3,
          t4,
          t5,
          t6)
         
          
)

a<-irf(var, impulse = "m_gap", response = c("gdp_gap", "selic", "FX", "inflation", "f_gap"), n.ahead = 24, ortho = TRUE,
       cumulative =FALSE , boot = TRUE, ci = 0.99, runs = 1000)
a

b<-irf(var, impulse = "f_gap", response = c("gdp_gap", "selic", "FX", "inflation", "m_gap"), n.ahead = 24, ortho = TRUE,
       cumulative = FALSE, boot = TRUE, ci = 0.99, runs = 1000)
b

c<-irf(var, impulse = "selic", response = c("gdp_gap", "FX", "inflation","m_gap","f_gap"), n.ahead = 24, ortho = TRUE,
          cumulative = FALSE, boot = TRUE, ci = 0.99, runs = 1000)
c

plot(a)
plot(b)
plot(c)


## Portmanteau test 
ser2<-serial.test(var)
ser2

##Jaque-Bera

norm2<-normality.test(var)
norm2

##VAR (2011-onwards)
geti_gap<-window(geti_gap,start=(2012))
geti_gap
fed_rates<-window(fed_rates,start=(2012))
en=window(Model_var,start=(2012))
en



ex<-ts.intersect(geti_gap,fed_rates)
ex

def=VARselect(en,lag.max=12, season = 12, type = 'const',exogen = ex)
def


## estimating the var
var2=VAR(en,p=2,type = 'const',exogen = ex)
summary(var2)

BQ(var2)

g<-irf(var2, impulse = "m_gap", response = c("gdp_gap", "selic", "FX", "inflation", "f_gap"), n.ahead = 24, ortho = TRUE,
       cumulative =FALSE , boot = TRUE, ci = 0.99, runs = 1000)
plot(g)

f<-irf(var2, impulse = "f_gap", response = c("gdp_gap", "selic", "FX", "inflation", "m_gap"), n.ahead = 24, ortho = TRUE,
       cumulative = FALSE, boot = TRUE, ci = 0.99, runs = 1000)
plot(f)

t1<-var2$varresult$FX
t2<-var2$varresult$selic
t3<-var2$varresult$inflation
t4<-var2$varresult$gdp_gap
t5<-var2$varresult$m_gap
t6<-var2$varresult$f_gap

stargazer(t1,
          t2,
          t3,
          t4,
          t5,
          t6)




## Portmanteau test 
ser2<-serial.test(var)
ser2

##Jaque-Bera

norm2<-normality.test(var)
norm2


#####FAVAR

#to quarter main variables
M_gap<-window(m_gap,start=c(2000,04))
M_gap<-aggregate(M_gap,nfrequency = 4,FUN = mean)
summary(ur.df(M_gap,type="trend",selectlags = "AIC"))

SELIC_Surprise <- read_excel("C:/Users/joao.leite/Desktop/FAVAR/SELIC_Surprise.xlsx", 
                             col_types = c("numeric"))
SELICSurprise<-ts(SELIC_Surprise,start = c(2001,01), frequency=12)
SELICSurprise
SELICSurprise<-aggregate(SELICSurprise, nfrequency = 4, FUN=mean)
summary(ur.df(SELICSurprise,type="trend",selectlags = "AIC"))
diff(SELICSurprise)

F_gap<-window(f_gap,start=c(2000,04))
F_gap<-aggregate(F_gap,nfrequency = 4,FUN = mean)
summary(ur.df(F_gap,type="trend",selectlags = "AIC"))
F_gap<-diff(F_gap)


USD_Spot<-ts(DOL_quart,start=c(2000,2),frequency = 4)
summary(ur.df(USD_Spot,type="trend",selectlags = "AIC"))
USD_Spot
USD_Spot<-diff(USD_Spot)
summary(ur.df(USD_Spot,type="trend",selectlags = "AIC"))

CPI<-window(inflation,start=c(2000,04))
CPI<- aggregate(CPI,nfrequency = 4,FUN=mean)


CPI
SELIC<-window(selic,start=c(2000,04))
SELIC<-aggregate(selic, nfrequency=4, FUN=mean)




GDP<-ts(PIB,start=c(2000,1),frequency = 4)
HP<-hpfilter(GDP, type = 'lambda', freq = 1600)
GDP_Gap<-HP$cycle
GDP_Gap


dummies=(seasonaldummy(CPI))


##VAR(1)
y<-ts.intersect(GDP_Gap,SELIC_Surprise,Investment,CPI,USD_Spot,M_gap,F_gap)
VARselect(y,lag.max=12)


v01=VAR(y,p=1)
summary(v01)

plot(irf(v01,impulse="SELIC_Surprise",n.ahead=12,ci=0.95))
Plot(irf(v01, impulse=)
plot(irf(v01,impulse="M_gap", n.ahead=12,ci=0.95))
plot(irf(v01,impulse="F_gap", n.ahead=12,ci=0.95))

plot.ts(M_gap)
plot.ts(F_gap)

##Other variables

#Political Variables
Political_variables <- read_excel("C:/Users/joao.leite/Desktop/FAVAR/To Quarter/Political variables.xlsx", 
                                  col_types = c("date", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric"))
View(Political_variables)

coal_idel<-ts(Political_variables[,2],start = c(2000,04),frequency = 12)
coal_idel<-aggregate(coal_idel,nfrequency = 4,FUN=mean)
summary(ur.df(coal_idel,type = "trend",selectlags = "AIC"))


dist_low<-ts(Political_variables[,3],start = c(2000,04),frequency = 12)
dist_low<-aggregate(dist_low,nfrequency = 4,FUN=mean)
summary(ur.df(dist_low,type = "trend",selectlags = "AIC"))
dist_low<-diff(dist_low)

eff_part<-ts(Political_variables[,4],start = c(2000,04),frequency = 12)
eff_part<-aggregate(eff_part,nfrequency = 4,FUN=mean)
summary(ur.df(eff_part,type = "trend",selectlags = "AIC"))
eff_part<-diff(eff_part)

dist_min<-ts(Political_variables[,5],start = c(2000,04),frequency = 12)
dist_min<-aggregate(dist_min,nfrequency = 4,FUN=mean)
summary(ur.df(dist_min,type = "trend",selectlags = "AIC"))

ideol_part<-ts(Political_variables[,6],start = c(2000,04),frequency = 12)
ideol_part<-aggregate(ideol_part,nfrequency = 4,FUN=mean)
summary(ur.df(ideol_part,type = "trend",selectlags = "AIC"))
ideol_part<-diff(ideol_part)

# International Commerce
FOB<-ts(BalancaComercial[,2],start=c(2000,04),frequency = 12)
FOB<-aggregate(FOB,nfrequency = 4,FUN=mean)
summary(ur.df(FOB,type="trend",selectlags = "AIC"))
FOB<-diff(FOB)

Exports<-ts(BalancaComercial[,3],start=c(2000,04),frequency = 12)
Exports<-aggregate(Exports,nfrequency = 4,FUN=mean)
summary(ur.df(Exports,type="trend",selectlags = "AIC"))
Exports<-diff(Exports)


# Macro Data

Macro_Quart <- read_excel("C:/Users/joao.leite/Desktop/FAVAR/Macro Quart.xlsx", 
                          col_types = c("numeric", "numeric"))
Consumption<-ts(Macro_Quart[,1],start = c(2000,2),frequency = 4)
Consumption
summary(ur.df(Consumption,type="trend",selectlags="AIC"))
Consumption<-diff(Consumption)

RealGDP<-ts(Macro_Quart[,2],start=c(2000,2),frequency=4)
RealGDP
summary(ur.df(RealGDP,type="trend",selectlags="AIC"))
RealGDP<-diff(RealGDP)


MacroMonth <- read_excel("C:/Users/joao.leite/Desktop/FAVAR/MacroMonth.xlsx", 
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric"))
MacroMonth

Public_Debt<-ts(MacroMonth[,2],start = c(2000,04),frequency = 12)
Public_Debt<-aggregate(Public_Debt,nfrequency = 4,FUN=mean)
summary(ur.df(Public_Debt,type="trend",selectlags = "AIC"))
Public_Debt<-diff(Public_Debt)
Public_Debt<-diff(Public_Debt)

Retail_Sales<-ts(MacroMonth[,3],start = c(2000,04),frequency = 12)
Retail_Sales<-aggregate(Retail_Sales,nfrequency = 4,FUN=mean)
summary(ur.df(Retail_Sales,type="trend",selectlags = "AIC"))
Retail_Sales<-diff(Retail_Sales)

NFSP<-ts(MacroMonth[,4],start = c(2000,04),frequency = 12)
NFSP<-aggregate(NFSP,nfrequency = 4,FUN=mean)
summary(ur.df(NFSP,type="trend",selectlags = "AIC"))
NFSP<-diff(NFSP)

NFSP_Subnational<-ts(MacroMonth[,5],start = c(2000,04),frequency = 12)
NFSP_Subnational<-aggregate(NFSP_Subnational,nfrequency = 4,FUN=mean)
summary(ur.df(NFSP_Subnational,type="trend",selectlags = "AIC"))
NFSP<-diff(NFSP_Subnational)

#Markets
Markets <- read_excel("C:/Users/joao.leite/Desktop/FAVAR/Markets.xlsx", 
                      col_types = c("numeric", "numeric", "numeric", "numeric"))
View(Markets)

IBOVESPA<-ts(Markets[,1],,start = c(2000,04),frequency = 12)
IBOVESPA<-aggregate(IBOVESPA,nfrequency = 4,FUN=mean)
summary(ur.df(IBOVESPA,type="trend",selectlags = "AIC"))
IBOVESPA<-diff(IBOVESPA)

IGP<-ts(Markets[,2]start = c(2000,04),frequency = 12)
IGP<-aggregate(IGP,nfrequency = 4,FUN=mean)
summary(ur.df(IGP,type="trend",selectlags = "AIC"))


EMBI<-ts(Markets[,3],start = c(2000,04),frequency = 12)
EMBI<-aggregate(EMBI,nfrequency = 4,FUN=mean)
summary(ur.df(EMBI,type="trend",selectlags = "AIC"))
EMBI<-diff(EMBI)

IIEBr_Media<-ts(Markets[,4],start = c(2000,04),frequency = 12)
IIEBr_Media<-aggregate(IIEBr_Media,nfrequency = 4,FUN=mean)
summary(ur.df(IIEBr_Media,type="trend",selectlags = "AIC"))
IIEBr_Media<-diff(IIEBr_Media)

#International variables

GETI<-window(model[,7],start=c(2000,04))
GETI<-aggregate(GETI,nfrequency = 4,FUN=mean)
summary(ur.df(GETI,type="trend",selectlags = "AIC"))
GeTI<-diff(GETI)

FEDFundsRate<-window(model[,8],start=c(2000,04)
FEDFundsRate<-aggregate(FEDFundsRate,nfrequency = 4,FUN=mean)
summary(ur.df(FEDFundsRate,type="trend",selectlags = "AIC"))
model)
FEDFundsRate<-diff(FEDFundsRate)

China_Uncertainty <- read_excel("C:/Users/joao.leite/Desktop/FAVAR/China Uncertainty.xlsx", 
                                col_types = c("numeric"))
China_Uncertainty<-ts(China_Uncertainty,start=c(2000,04),frequency = 12)
China_Uncertainty<-aggregate(China_Uncertainty,nfrequency = 4, FUN=mean)
summary(ur.df(China_Uncertainty,type = "trend",selectlags = "AIC"))
China_Uncertainty<-diff(China_Uncertainty)

#Combining series

dadosA<-ts.intersect(coal_idel,dist_low,eff_part,dist_min,ideol_part,FOB,Exports,Consumption,RealGDP,
                    Public_Debt,Retail_Sales,NFSP,NFSP_Subnational,IBOVESPA,IGP,
                    EMBI,IIEBr_Media,GETI,FEDFundsRate,China_Uncertainty, SELICSurprise)
cp<-prcomp(dadosA,,center = TRUE,scale=TRUE)
cp
f1<-cp$x[,1]
f1
f1<-ts(f1,start=c(2001,2),frequency = 4)
f1

yf<-ts.intersect(USD_Spot,SELIC,CPI,Investment,GDP_gap)


v02<-VAR(yf,p=2)


summary(v02)

plot(irf(v02,impulse = "F_gap",n.ahead = 12,ci=0.99,runs = 1000))
plot(irf(v02,impulse="M_gap",n.ahead = 12,ci=0.99,runs = 1000))
plot(irf(v02,impulse="SELIC_Surprise",n.ahead = 12,ci=0.99,runs = 1000))
