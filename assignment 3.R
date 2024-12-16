ixmor.df <-timeseries_5065623
str(ixmor.df)

# Overview of the essentials ----
summary(ixmor.df)
summary(ixmor.df[,c(4,5,7,6,8)])
plot(ixmor.df[,c(4,5,7,6,8)])
plot(ixmor.df[,c(3)],ixmor.df[,c(4)], type="l", col="red", lwd=5, xlab="weeks", ylab="lnsales", main="sales")
plot(ixmor.df[,c(3)],ixmor.df[,c(6)], type="l", col="red", lwd=5, xlab="weeks", ylab="LnPrice", main="price")
plot(ixmor.df[,c(3)],ixmor.df[,c(8)], type="l", col="red", lwd=5, xlab="weeks", ylab="lncompprice", main="competitor price")
barplot(ixmor.df[,c(5)], main = "advertisement", col = "red")
barplot(ixmor.df[,c(7)], main = "competitor advertisement", col = "red")
barhelp.data = data.matrix(ixmor.df[,c(5,7)])
barhelp2.data = t(barhelp.data)
barplot(barhelp2.data, main = "adv (red) and competitor adv (grey) investment", col = c("red","grey"), xlab = "weeks")

# For Loop for Granger causality tests----
install.packages("vars")
library(vars)
install.packages("lmtest")
library(lmtest)
## dv: sales----
p_sales_adv <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnSales~LnAdvertising, order = i, data = ixmor.df)
  p_sales_adv[i] <- pvalue[2,4]
}
min(p_sales_adv)
which.min(p_sales_adv)

p_sales_compadv <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnSales~LnCompAdvertising, order = i, data = ixmor.df)
  p_sales_compadv[i] <- pvalue[2,4]
}
min(p_sales_compadv)
which.min(p_sales_compadv)

p_sales_price <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnSales~LnPrice, order = i, data = ixmor.df)
  p_sales_price[i] <- pvalue[2,4]
}
min(p_sales_price)
which.min(p_sales_price)

p_sales_comp_price <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnSales~LnCompPrice, order = i, data = ixmor.df)
  p_sales_comp_price[i] <- as.numeric(format(pvalue[2,4], digits = 10))
}
min(p_sales_comp_price)
which.min(p_sales_comp_price)

## dv: adv ----
p_adv_sales <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnAdvertising~LnSales, order = i, data = ixmor.df)
  p_adv_sales[i] <- pvalue[2,4]
}
min(p_adv_sales)
which.min(p_adv_sales)

p_adv_compadv <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnAdvertising~LnCompAdvertising, order = i, data = ixmor.df)
  p_adv_compadv[i] <- pvalue[2,4]
}
min(p_adv_compadv)
which.min(p_adv_compadv)

p_adv_price <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnAdvertising~LnPrice, order = i, data = ixmor.df)
  p_adv_price[i] <- pvalue[2,4]
}
min(p_adv_price)
which.min(p_adv_price)

p_adv_comp_price <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnAdvertising~LnCompPrice, order = i, data = ixmor.df)
  p_adv_comp_price[i] <- pvalue[2,4]
}
min(p_adv_comp_price)
which.min(p_adv_comp_price)


## dv: competitor adv----
p_compadv_sales <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnCompAdvertising~LnSales, order = i, data = ixmor.df)
  p_compadv_sales[i] <- pvalue[2,4]
}
min(p_compadv_sales)
which.min(p_compadv_sales)

p_compadv_adv <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnCompAdvertising~LnAdvertising, order = i, data = ixmor.df)
  p_compadv_adv[i] <- pvalue[2,4]
}
min(p_compadv_adv)
which.min(p_compadv_adv)

p_compadv_price <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnCompAdvertising~LnPrice, order = i, data = ixmor.df)
  p_compadv_price[i] <- pvalue[2,4]
}
min(p_compadv_price)
which.min(p_compadv_price)

p_compadv_comp_price <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnCompAdvertising~LnCompPrice, order = i, data = ixmor.df)
  p_compadv_comp_price[i] <- pvalue[2,4]
}
min(p_compadv_comp_price)
which.min(p_compadv_comp_price)

## dv: price ----
p_price_sales <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnPrice~LnSales, order = i, data = ixmor.df)
  p_price_sales[i] <- pvalue[2,4]
}
min(p_price_sales)
which.min(p_price_sales)

p_price_adv <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnPrice~LnAdvertising, order = i, data = ixmor.df)
  p_price_adv[i] <- pvalue[2,4]
}
min(p_price_adv)
which.min(p_price_adv)

p_price_comp_adv <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnPrice~LnCompAdvertising, order = i, data = ixmor.df)
  p_price_comp_adv[i] <- pvalue[2,4]
}
min(p_price_comp_adv)
which.min(p_price_comp_adv)

p_price_comp_price <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnPrice~LnCompPrice, order = i, data = ixmor.df)
  p_price_comp_price[i] <- pvalue[2,4]
}
min(p_price_comp_price)
which.min(p_price_comp_price)

## dv: competitor price----
p_comp_price_sales <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnCompPrice~LnSales, order = i, data = ixmor.df)
  p_comp_price_sales[i] <- pvalue[2,4]
}
min(p_comp_price_sales)
which.min(p_comp_price_sales)

p_comp_price_adv <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnCompPrice~LnAdvertising, order = i, data = ixmor.df)
  p_comp_price_adv[i] <- pvalue[2,4]
}
min(p_comp_price_adv)
which.min(p_comp_price_adv)

p_comp_price_comp_adv <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnCompPrice~LnCompAdvertising, order = i, data = ixmor.df)
  p_comp_price_comp_adv[i] <- pvalue[2,4]
}
min(p_comp_price_comp_adv)
which.min(p_comp_price_comp_adv)

p_comp_price_price <- numeric(13)
x <- c(1:13)
for (i in x) {
  pvalue <- grangertest(LnCompPrice~LnPrice, order = i, data = ixmor.df)
  p_comp_price_price[i] <- pvalue[2,4]
}
min(p_comp_price_price)
which.min(p_comp_price_price)


#Testing for unit roots----
install.packages("aTSA")
library(aTSA)

pp.test(ixmor.df[,c(4)], output = TRUE)

pp.test(ixmor.df[,c(5)], output = TRUE)

pp.test(ixmor.df[,c(7)], output = TRUE)

pp.test(ixmor.df[,c(6)], output = TRUE)

pp.test(ixmor.df[,c(8)], output = TRUE)


#Determining lag length of the endogenous variables----
ixmorendo = ixmor.df[,c(4,5,7,6,8)]
ixmorexo = ixmor.df[,c(9,10,11)]    
VARselect(ixmorendo,lag.max = 4, type = "both", exogen = ixmorexo)


#Estimating the VAR model, and reporting results for the individual equations----
ixmorest <- VAR(ixmorendo, p=1, type = "both", exogen = ixmorexo)
summary(ixmorest,"LnSales")
summary(ixmorest,"LnAdvertising")
summary(ixmorest,"LnCompAdvertising")
summary(ixmorest,"LnPrice")
summary(ixmorest,"LnCompPrice")


#Generating the IRFs----
sales_ime_irf <- irf(ixmorest, impulse = NULL, response = "LnSales", n.ahead = 12,
                ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68,
                runs = 500)  
plot(sales_ime_irf)

sales_cum_irf <- irf(ixmorest, impulse = NULL, response = "LnSales", n.ahead = 12,
                ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68,
                runs = 500)  
plot(sales_cum_irf)

adv_ime_irf <- irf(ixmorest, impulse = NULL, response = "LnAdvertising", n.ahead = 12,
                ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68,
                runs = 500)  
plot(adv_ime_irf)

adv_cum_irf <- irf(ixmorest, impulse = NULL, response = "LnAdvertising", n.ahead = 12,
                ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68,
                runs = 500)  
plot(adv_cum_irf)

comp_adv_ime_irf <- irf(ixmorest, impulse = NULL, response = "LnCompAdvertising", n.ahead = 12,
                ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68,
                runs = 500)  
plot(comp_adv_ime_irf)

comp_adv_cum_irf <- irf(ixmorest, impulse = NULL, response = "LnCompAdvertising", n.ahead = 12,
                ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68,
                runs = 500) 
plot(comp_adv_cum_irf)

price_ime_irf <- irf(ixmorest, impulse = NULL, response = "LnPrice", n.ahead = 12,
                     ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68,
                     runs = 500)  
plot(price_ime_irf)

price_cum_irf <- irf(ixmorest, impulse = NULL, response = "LnPrice", n.ahead = 12,
                     ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68,
                     runs = 500)  
plot(price_cum_irf)

comp_price_ime_irf <- irf(ixmorest, impulse = NULL, response = "LnCompPrice", n.ahead = 12,
                     ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68,
                     runs = 500)  
plot(comp_price_ime_irf)

comp_price_cum_irf <- irf(ixmorest, impulse = NULL, response = "LnCompPrice", n.ahead = 12,
                     ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68,
                     runs = 500)  
plot(comp_price_cum_irf)


#Generating the FEVDs----
ixmorfevd1 <-fevd(ixmorest,n.ahead = 12)
ixmorfevd1
barbasis1 = ixmorfevd1[1]
barbasis1
barbasis2 = matrix(unlist(barbasis1),ncol =5, byrow = TRUE)
barbasis2

bartry = Reduce(rbind,ixmorfevd1)
bartry
bartry2 = t(bartry)
bartry2
bartry2 = bartry2[,c(12,24,36,48,60)]
library(gplots)
library(RColorBrewer)

barplot(bartry2, col = c("red", "grey", "white", "blue","green"), xlab = "sales - adv - comp_adv - price - comp_price") 
barplot(bartry2, names.arg = c("sales","adv","comp_adv","price","comp_price"), 
        xlab = "sales - adv - comp_adv - price - comp_price",  ylab = "sales - adv - comp_adv - price - comp_price" ) 
barplot(bartry2,  col = brewer.pal(5, "YlOrRd"), 
        names.arg = c("sales","adv","comp_adv","price","comp_price"), 
        xlab = "sales - adv - comp_adv - price - comp_price",  ylab = "sales - adv - comp_adv - price - comp_price" ) 


