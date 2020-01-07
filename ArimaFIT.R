#Transformando dados em Time Series .
tsdataC <- ts(db$Conservador, start = 1992 , end = 2017)
tsdataA <- ts(db$Arrojado, start = 1992 , end = 2017)

#Arima com trace para ver as possibilidades
#Para Conservador

auto.arima(tsdataC,trace=TRUE)

# ARIMA(2,1,2) with drift         : Inf
# ARIMA(0,1,0) with drift         : 626.6413
# ARIMA(1,1,0) with drift         : 623.255
# ARIMA(0,1,1) with drift         : 618.5636
# ARIMA(0,1,0)                    : 624.3275
# ARIMA(1,1,1) with drift         : Inf
# ARIMA(0,1,2) with drift         : Inf
# ARIMA(1,1,2) with drift         : Inf
# ARIMA(0,1,1)                    : 616.6056
# ARIMA(1,1,1)                    : 619.1905
# ARIMA(0,1,2)                    : 619.1872
# ARIMA(1,1,0)                    : 620.7515
# ARIMA(1,1,2)                    : 622.0586


fit1C <- Arima(tsdataC,order = c(2,1,2),include.drift = TRUE )
fit2C <- Arima(tsdataC,order = c(0,1,0),include.drift = TRUE )
fit3C <- Arima(tsdataC,order = c(1,1,0),include.drift = TRUE)
fit4C <- Arima(tsdataC,order = c(0,1,1),include.drift = TRUE)
fit5C <- Arima(tsdataC,order = c(0,1,0),include.drift = FALSE)
fit6C <- Arima(tsdataC,order = c(1,1,1),include.drift = TRUE)
fit7C <- Arima(tsdataC,order = c(0,1,2),include.drift = TRUE)
fit8C <- Arima(tsdataC,order = c(1,1,2),include.drift = TRUE)
fit9C <- Arima(tsdataC,order = c(0,1,1),include.drift = FALSE)
fit10C <- Arima(tsdataC,order = c(1,1,1),include.drift = FALSE)
fit11C <- Arima(tsdataC,order = c(0,1,2),include.drift = FALSE)
fit12C <- Arima(tsdataC,order = c(1,1,0),include.drift = FALSE)
fit13C <- Arima(tsdataC,order = c(1,1,2),include.drift = FALSE)


# Verificação por plot. Todos no mesmo tempo


# plot(tsdataC,lwd = 4)
# lines(fit1C$fitted, col = "red",lwd = 2)
# lines(forecast(fit1c,h=20))
# lines(fit2C$fitted, col = "green", lwd = 2)
# lines(fit3C$fitted, col = "darkred", lwd = 2)
# lines(fit4C$fitted, col = "gold", lwd = 2)
# lines(fit5C$fitted, col = "lightpink", lwd = 2)
# lines(fit6C$fitted, col = "purple", lwd = 2)
# legend("topleft",lty=1,bty = "n",
#        col=c("red","green","darkred","gold","lightpink","purple"),
#        c("2,1,2","0,1,0","1,1,0","0,1,1","0,1,0","1,1,0"))



## Antes + Depois -> Todos os modelos. Previsão 20 períodos


plot(forecast(fit1C,h=20),sub = "Cenário Conservador")
lines(tsdataC)
lines(fit1C$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))


plot(forecast(fit2C,h=20),sub = "Cenário Conservador")
lines(tsdataC)
lines(fit2C$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit3C,h=20),sub = "Cenário Conservador")
lines(tsdataC)
lines(fit3C$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit4C,h=20),sub = "Cenário Conservador")
lines(tsdataC)
lines(fit4C$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit5C,h=20),sub = "Cenário Conservador")
lines(tsdataC)
lines(fit5C$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit6C,h=20),sub = "Cenário Conservador")
lines(tsdataC)
lines(fit6C$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit7C,h=20),sub = "Cenário Conservador")
lines(tsdataC)
lines(fit7C$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit8C,h=20),sub = "Cenário Conservador")
lines(tsdataC)
lines(fit8C$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit9C,h=20),sub = "Cenário Conservador")
lines(tsdataC)
lines(fit9C$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit10C,h=20),sub = "Cenário Conservador")
lines(tsdataC)
lines(fit10C$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit11C,h=20),sub = "Cenário Conservador")
lines(tsdataC)
lines(fit11C$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))


plot(forecast(fit13C,h=20),sub = "Cenário Conservador")
lines(tsdataC)
lines(fit13C$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))


#Para Arrojado

auto.arima(tsdataA,trace=TRUE)

# ARIMA(2,1,2) with drift         : 640.1425
# ARIMA(0,1,0) with drift         : 635.0963
# ARIMA(1,1,0) with drift         : 633.2479
# ARIMA(0,1,1) with drift         : 631.0223
# ARIMA(0,1,0)                    : 632.8176
# ARIMA(1,1,1) with drift         : 633.8777
# ARIMA(0,1,2) with drift         : 633.8767
# ARIMA(1,1,2) with drift         : 636.9361
# ARIMA(0,1,1)                    : 628.6772
# ARIMA(1,1,1)                    : 631.2662
# ARIMA(0,1,2)                    : 631.2608
# ARIMA(1,1,0)                    : 630.8077
# ARIMA(1,1,2)                    : 633.9094


fit1A <- Arima(tsdataA,order = c(2,1,1),include.drift = TRUE)
fit2A <- Arima(tsdataA,order = c(0,1,0),include.drift = TRUE)
fit3A <- Arima(tsdataA,order = c(1,1,0),include.drift = TRUE)
fit4A <- Arima(tsdataA,order = c(0,1,1),include.drift = TRUE)
fit5A <- Arima(tsdataA,order = c(0,1,0),include.drift = FALSE)
fit6A <- Arima(tsdataA,order = c(1,1,1),include.drift = TRUE)
fit7A <- Arima(tsdataA,order = c(0,1,2),include.drift = TRUE)
fit8A <- Arima(tsdataA,order = c(1,1,2),include.drift = TRUE)
fit9A <- Arima(tsdataA,order = c(0,1,1),include.drift = FALSE)
fit10A <- Arima(tsdataA,order = c(1,1,1),include.drift = FALSE)
fit11A <- Arima(tsdataA,order = c(0,1,2),include.drift = FALSE)
fit12A <- Arima(tsdataA,order = c(1,1,0),include.drift = FALSE)
fit13A <- Arima(tsdataA,order = c(1,1,2),include.drift = FALSE)



# Verificação por plot. Todos no mesmo tempo

# 
# plot(tsdataA,lwd = 4)
# lines(fit1A$fitted, col = "red",lwd = 2)
# lines(forecast(fit1c,h=20))
# lines(fit2A$fitted, col = "green", lwd = 2)
# lines(fit3A$fitted, col = "darkred", lwd = 2)
# lines(fit4A$fitted, col = "gold", lwd = 2)
# lines(fit5A$fitted, col = "lightpink", lwd = 2)
# lines(fit6A$fitted, col = "purple", lwd = 2)
# legend("topleft",lty=1,bty = "n",
#        col=c("red","green","darkred","gold","lightpink","purple"),
#        c("0,1,1","1,1,1","0,1,2","0,1,1","0,1,0","1,1,0"))



## Antes + Depois -> Todos os modelos. Previsão 20 períodos


plot(forecast(fit1A,h=20),sub = "Cenário Arrojado")
lines(tsdataA)
lines(fit1A$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))


plot(forecast(fit2A,h=20),sub = "Cenário Arrojado")
lines(tsdataA)
lines(fit2A$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit3A,h=20),sub = "Cenário Arrojado")
lines(tsdataA)
lines(fit3A$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit4A,h=20),sub = "Cenário Arrojado")
lines(tsdataA)
lines(fit4A$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit5A,h=20),sub = "Cenário Arrojado")
lines(tsdataA)
lines(fit5A$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit6A,h=20),sub = "Cenário Arrojado")
lines(tsdataA)
lines(fit6A$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit7A,h=20),sub = "Cenário Arrojado")
lines(tsdataA)
lines(fit7A$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit8A,h=20),sub = "Cenário Arrojado")
lines(tsdataA)
lines(fit8A$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit9A,h=20),sub = "Cenário Arrojado")
lines(tsdataA)
lines(fit9A$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit10A,h=20),sub = "Cenário Arrojado")
lines(tsdataA)
lines(fit10A$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit11A,h=20),sub = "Cenário Arrojado")
lines(tsdataA)
lines(fit11A$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit12A,h=20),sub = "Cenário Arrojado")
lines(tsdataA)
lines(fit12A$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))

plot(forecast(fit13A,h=20),sub = "Cenário Arrojado")
lines(tsdataA)
lines(fit13A$fitted, col = "green", lwd = 2)
legend("topleft", 
       lty = 1,lwd = 2, bty = "n", col = c("Black","Green"),
       c("Dado Original","Previsto"))


## Dados backtest para o modelo escolhido

table <- data.frame("ConservadorAntesPrevisto"= fit4C$fitted, "ArrojadoAntesPrevisto" = fit4A$fitted,
                    row.names = FALSE)
table$Ano <- c(1992:2017)
View(table)



#Gravar tabela em csv 

write.csv2(table,"AreaUtil_antes.csv")


#Impressão forecast escolhido

FitConservador <- forecast(fit4C,h=20)
FitCtable <- data.frame("Ano" = c(2018:2037),"Conservador Previsto" = FitConservador$mean)
FitC <- cbind(c(2018:2037),FitConservador$lower,FitConservador$upper,FitConservador$mean)

FitArrojado <- forecast(fit4A,h = 20)
FitAtable <- data.frame("Ano" = c(2018:2037), "Arrojado" = FitArrojado$mean)
FitA <- cbind(c(2018:2037),FitArrojado$lower,FitArrojado$upper,FitArrojado$mean)


write.xlsx2(FitCtable,"FitCTable_xlsx.xlsx",row.names = FALSE)

write.csv(FitCtable,"2_FitConservador_AreaUtil.csv",row.names = FALSE,qmethod = "double")
write.csv(FitAtable,"2_FitArrojado_AreaUtil.csv",row.names = FALSE,dec = ",")
