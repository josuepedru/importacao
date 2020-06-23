install.packages("RcppArmadillo")
install.packages("ggplot2")
install.packages('Rcpp')
install.packages('xts')
install.packages("devtools")
install.packages("tidyverse")
install.packages("scales")
install.packages("lubridate")
install.packages('gcookbook')
install.packages('scales')
install.packages('corrplot')


library(RcppArmadillo)
library(Rcpp)
library(xts)
library(ggplot2)
library(devtools)
library(tidyverse)
library(scales)
library(dplyr) 
library(forecast)
library(magrittr)
library(lubridate)
library(ggplot2)
library(xts)
library(corrplot)


############ Baixando o pacote BMR
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
library(devtools)

install_github("kthohr/BMR")

##############
################ Baixando o pacote rbcb 
devtools::install_github('wilsonfreitas/rbcb')
library(rbcb)
cambio <- get_series(10813,start_date = '2019-01-01')
colnames(cambio) <- c('date', 'cambio')
cambio$date <- as.Date(cambio$date, format='%d/%m/%Y')
cambio <- xts(cambio$cambio, order.by = cambio$date)
#cambio <- apply.monthly(cambio, FUN=mean) só pra se eu quiser ver mensalmente.
autoplot(cambio)


##############
################ Installing ipeadatar package
install.packages('ipeadatar')
library(ipeadatar)
View(available_series(language = "br")) #seaching for avaliable series 
ipca=ipeadata("PRECOS12_IPCAG12",language=c("en","br"))[,-c(1,2,4,5)]
datas=seq(as.Date("1980-01-01"),length=nrow(ipca),by="month")
ipca=xts(ipca,order.by=datas)
autoplot(ipca)
autoplot(ipca["19960101/2020301"]) #cut the series to starting in january 96 till now  

### Baixando indicador mensal do IPEA para FBCF dessazonalizado 
fbcf=ipeadata("GAC12_INDFBCFDESSAZ12")[,-c(1,2,4,5)]
datas=seq(as.Date("1996-01-01"),length=nrow(fbcf),by="month")
fbcf=xts(fbcf,order.by=datas)
autoplot(fbcf)


##############
################ Installing the sidrar package 
install.packages("sidrar")
library(sidrar)    
info_sidra(1621) #serie encadeada do indice de volume trimestral com ajuste sazonal 
cnt=get_sidra(1621,variable=584,period=c("last"=8)) #last 8 quartel
series=c(90687,90691,90692,90693,90695,90694,90696,90697,90698,90699,90700,90702,90701,90703,90705,90707,93404,93405,93406,93407,93408)
names=c('Agropecuária', "Indústria","Indústrias extrativas","Indústrias de transformação",
        "Eletricidade/Gás/Esgoto","Construção","Serviços","Comércio","Transporte","Informação",
        "Atividades financeiras","Atividades imobiliárias","Outras serviços",
        "Administração/Educação/Saúde Pública","Valor Adicionado","Pib a pm",
        "Despesa de Consumo das Famílias","Despesa Adm Pública","FBCF","Exportações","Importações")
cnt1=matrix(NA, ncol=length(series), nrow=(nrow(cnt)/length(series)))

for (i in 1:length(names))
{cnt1[,i] = cnt$Valor[cnt$`Setores e subsetores (Código)`==series[i]] #Para colocar na coluna o valor de cada setor baseado no seu número. 
#cnt1 = ts(cnt1, start=c(2018,01), freq=4)
colnames(cnt1) = names
cnt1=data.frame(cnt1)
}

trim=c("2018 T1","2018 T2","2018 T3","2018 T4","2019 T1","2019 T2","2019 T3","2019 T4")
cnt1=cbind(trim,cnt1)
ggplot(cnt1,aes(x=trim, y=FBCF,group = 1))+geom_line()+geom_point(size=3) +
  labs(x = "Fonte: CNT, IBGE. Elaboração Própria.", y = "", 
       title = "Índice encadeado da FBCF (1995=100)")

##serie encadeada do indice de volume trimestral (1995=100)
cnt=get_sidra(1620,variable=583, period = "All") 
series=c(90687,90691,90692,90693,90695,90694,90696,90697,90698,90699,90700,90702,90701,90703,90705,90706,90707,93404,93405,93406,93407,93408)
names=c('Agropecuária', "Indústria","Indústrias extrativas","Indústrias de transformação","Eletricidade/Gás/Esgoto","Construção","Serviços","Comércio","Transporte","Informação","Atividades financeiras","Atividades imobiliárias","Outras serviços","Administração/Educação/Saúde Pública","Valor Adicionado","Impostos líquidos ","Pib a pm",
        "Despesa de Consumo das Famílias","Despesa Adm Pública","FBCF","Exportações","Importações")
cnt1=matrix(NA, ncol=length(series), nrow=(nrow(cnt)/length(series)))
for (i in 1:length(series))
{cnt1[,i] = cnt$Valor[cnt$`Setores e subsetores (Código)`==series[i]]
colnames(cnt1) = names
cnt1=ts(cnt1,freq=4,start=c(1996,1))
}

##############
################ Installing the quantmod package
install.packages("quantmod")
library(quantmod)

startDate = as.Date("1995-12-30")   
endDate = as.Date("2019-12-31")  
tickers<-c('^BVSP','BTCUSD=X')
getSymbols(tickers,src="yahoo",from=startDate,to=endDate)

#obtendo os retornos diarios ou a variacao diaria da acao/moeda
BVSP_RET <- dailyReturn(BVSP)  
BTC_RET <- dailyReturn(`BTCUSD=X`)
autoplot(BVSP_RET)
autoplot(BTC_RET)

## mensalizando a serie diaria 
BVSP<-na.omit(BVSP) #retirando as NAs 
ep <- endpoints(BVSP$BVSP.Close,on="month") 
BVSP<-period.apply(BVSP,INDEX=ep,FUN=mean) 
autoplot(BVSP$BVSP.Close)
monthplot(BVSP$BVSP.Close)

#######################
#obtendo os retornos diarios ou a variacao diaria do real em termos de dolar
startDate = as.Date("2019-01-01")   
tickers<-c("BRL=X")
getSymbols(tickers,src="yahoo",from=startDate)

#obtendo os retornos diarios ou a variacao diaria da acao/moeda
BRL_RET <- dailyReturn(`BRL=X`)
autoplot(BRL_RET)
