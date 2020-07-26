install.packages("ggplot2")
library(ggplot2)
library(scales)

#disable e notation for real numbers
options(scipen=999)

#converter from stupid to smart measurement
f2_to_m2 = (0.3048)^2

N = length(kc_house_data$id)

#dropping rows with 0 bedrooms or 0 bathrooms
indexes_wo_ano = c()
for (i in 1:N){
  if (kc_house_data$bedrooms[i] > 0 && kc_house_data$bathrooms[i] > 0.5)
  {
    indexes_wo_ano = c(indexes_wo_ano, i)
  }
}
house_data_prep = kc_house_data[indexes_wo_ano,]

N = length(house_data_prep$id)

#creating columns for squared meters and getting rid of the stupid measurement
house_data_prep$sqm_living = house_data_prep$sqft_living*f2_to_m2
house_data_prep$sqm_lot = house_data_prep$sqft_lot*f2_to_m2
house_data_prep$sqm_above = house_data_prep$sqft_above*f2_to_m2
house_data_prep$sqm_basement = house_data_prep$sqft_basement*f2_to_m2
house_data_prep$sqm_living15 = house_data_prep$sqft_living15*f2_to_m2
house_data_prep$sqm_lot15 = house_data_prep$sqft_lot15*f2_to_m2
house_data_prep = subset(house_data_prep, select=-c(sqft_living, sqft_lot, sqft_above, sqft_basement, sqft_living15, sqft_lot15))
#dimnames(house_data_prep)

#histogram of prices and percentege of those with price bellow 1m
hist(house_data_prep$price, xlab="Cena nekretnine", ylab = "Broj nekretnina", main = NULL)
#ggplot(house_data_prep, aes(x=price)) + geom_histogram()
n_lt_1m = length(house_data_prep[house_data_prep$price<1000000,1])
perc_n_lt_1m = n_lt_1m/N*100
perc_n_lt_1m

#minimum and maximum price
min(house_data_prep$price)
max(house_data_prep$price)

#all houses with more than 4 bedrooms categorized together
bedrooms = house_data_prep$bedrooms
for (i in 1:length(bedrooms)){
  if (bedrooms[i] > 4){
    bedrooms[i] = 5
  }
}
mytable = table(bedrooms)
lbls = paste(names(mytable), "=", round(mytable/sum(mytable)*100, digits = 2),"%", sep=" ")
lbls[5] = paste("5+ ", "=", round(mytable[5]/sum(mytable)*100, digits = 2),"%", sep=" ")
pie(mytable, labels = lbls, 
    main=NULL)

bathrooms = house_data_prep$bathrooms
for (i in 1:length(bathrooms)){
  bathrooms[i] = round(bathrooms[i])
  if (bathrooms[i] > 3){
    bathrooms[i] = 4
  }
}
mytable = table(bathrooms)
lbls = paste(names(mytable), "=", round(mytable/sum(mytable)*100, digits = 2),"%", sep=" ")
lbls[4] = paste("4+ ", "=", round(mytable[4]/sum(mytable)*100, digits = 2),"%", sep=" ")
pie(mytable, labels = lbls, 
    main=NULL)


house_data_prep$sqm_living_bin = cut(house_data_prep$sqm_living, breaks = c(0,100,200,300,400,1300), labels = c("0-100", "100-200", "200-300", "300-400", "400-1300"))
bins = house_data_prep$sqm_living_bin
mytable = table(bins)
lbls = paste(names(mytable), "=", round(mytable/sum(mytable)*100, digits = 2),"%", sep=" ")
pie(mytable, labels = lbls, 
    main=NULL)

plot(house_data_prep$price, house_data_prep$sqm_living, xlab = "prices", ylab = "sqm")
cor(house_data_prep$price, house_data_prep$sqm_living)

n = ceiling(qchisq(0.95, 7)*N*0.5*0.5/(0.05^2*(N-1)) + qchisq(0.95, 7)*0.5*0.5)
n

set.seed(6)
sample_psu = sample(1:N, n)
length(house_data_prep$price[sample_psu])
xn_ocena_psu = mean(house_data_prep$price[sample_psu])
mean(house_data_prep$price[sample_psu])
sum(is.na(house_data_prep$price[sample_psu]))
s2_psu = var(house_data_prep$price[sample_psu])
d_ocena_psu = s2_psu/n*(1-n/N)
xn_ocena_psu
d_ocena_psu
var(house_data_prep$price)/n*(1-n/N)

aplha = 0.05
z = qnorm(1 - aplha/2)
interval = c(xn_ocena_psu - z*sqrt(d_ocena_psu), xn_ocena_psu + z*sqrt(d_ocena_psu))
interval

uzorak_za_reg = sample(1:N, n)
ro = cor(house_data_prep$price[uzorak_za_reg], house_data_prep$sqm_living[uzorak_za_reg])
b = ro*sqrt(var(house_data_prep$price[uzorak_za_reg]))/sqrt(var(house_data_prep$sqm_living[uzorak_za_reg]))
xn_lr = mean(house_data_prep$price[uzorak_za_reg])+b*(mean(house_data_prep$sqm_living[uzorak_za_reg] - mean(house_data_prep$sqm_living[uzorak_za_reg])))
se = 1/(n-1)*sum((house_data_prep$price[uzorak_za_reg] - (mean(house_data_prep$price[uzorak_za_reg]) + b*(house_data_prep$sqm_living[uzorak_za_reg] - mean(house_data_prep$sqm_living[uzorak_za_reg]))))^2)
d_ocena_reg = se/n*(1-n/N)
interval_reg = c(xn_lr - z*sqrt(d_ocena_reg), xn_lr + z*sqrt(d_ocena_reg))
xn_lr
d_ocena_reg
mean(kc_house_data$price)

interval_reg = c(xn_lr - z*sqrt(d_ocena_reg), xn_lr + z*sqrt(d_ocena_reg))
interval_reg

house_data_prep$bathrooms_rounded = round(house_data_prep$bathrooms)
for (i in 1:N){
  if (house_data_prep$bathrooms_rounded[i] > 4){
    house_data_prep$bathrooms_rounded[i] = 4
  }
}
unique(house_data_prep$bathrooms_rounded)

stratumi = list()
stratumi[[1]] = house_data_prep[house_data_prep$bathrooms_rounded == 1,]
stratumi[[2]] = house_data_prep[house_data_prep$bathrooms_rounded == 2,]
stratumi[[3]] = house_data_prep[house_data_prep$bathrooms_rounded == 3,]
stratumi[[4]] = house_data_prep[house_data_prep$bathrooms_rounded == 4,]
stratumi[[1]]

disprezije_stratuma = c(sqrt(var(stratumi[[1]]$price)), sqrt(var(stratumi[[2]]$price)), sqrt(var(stratumi[[3]]$price)), sqrt(var(stratumi[[4]]$price)))
N_stratuma = c(length(stratumi[[1]]$price), length(stratumi[[2]]$price), length(stratumi[[3]]$price), length(stratumi[[4]]$price))
n_stratuma = round(n*(N_stratuma*disprezije_stratuma)/sum(N_stratuma*disprezije_stratuma))

n_stratuma
sum(n_stratuma) == n

indeksi_strat = c()
indeksi_strat[[1]] = sample(1:N_stratuma[1], n_stratuma[1], replace = F)
indeksi_strat[[2]] = sample(1:N_stratuma[2], n_stratuma[2], replace = F)
indeksi_strat[[3]] = sample(1:N_stratuma[3], n_stratuma[3], replace = F)
indeksi_strat[[4]] = sample(1:N_stratuma[4], n_stratuma[4], replace = F)

xn_strat = c()
xn_strat[1] = mean(house_data_prep$price[indeksi_strat[[1]]])
xn_strat[2] = mean(house_data_prep$price[indeksi_strat[[2]]])
xn_strat[3] = mean(house_data_prep$price[indeksi_strat[[3]]])
xn_strat[4] = mean(house_data_prep$price[indeksi_strat[[4]]])

uzoracka_disperzija_stratuma = c()
uzoracka_disperzija_stratuma[1] = var(house_data_prep$price[indeksi_strat[[1]]])
uzoracka_disperzija_stratuma[2] = var(house_data_prep$price[indeksi_strat[[2]]])
uzoracka_disperzija_stratuma[3] = var(house_data_prep$price[indeksi_strat[[3]]])
uzoracka_disperzija_stratuma[4] = var(house_data_prep$price[indeksi_strat[[4]]])

xn_st = 1/N*sum(N_stratuma*xn_strat)
xn_st
d_ocena_st = 1/N^2*sum((N_stratuma^2*uzoracka_disperzija_stratuma)/n_stratuma*(1-n_stratuma/N_stratuma))
d_ocena_st

interval_strat = c(xn_st - z*sqrt(d_ocena_st), xn_st + z*sqrt(d_ocena_st))
interval_strat

mean(house_data_prep$price)



cor(kc_house_data$price, kc_house_data$sqft_lot*(0.3048)^2)
cor(kc_house_data$price, kc_house_data$sqft_above*(0.3048)^2)
cor(kc_house_data$price, kc_house_data$sqft_basement*(0.3048)^2)
cor(kc_house_data$price, kc_house_data$view)
cor(kc_house_data$sqft_lot, kc_house_data$sqft_lot15)

sum(is.na(kc_house_data$price))
sum(is.na(kc_house_data$sqft_living))

hist(kc_house_data$price)

hist(kc_house_data$sqft_living*(0.3048)^2)

hist(kc_house_data$bedrooms)

hist(kc_house_data$bathrooms)

mean(kc_house_data$price)
length(kc_house_data$price)

indexes = sample(1:21613, 1000)
mean(kc_house_data$price[indexes])

N = length(kc_house_data$price)
s2 = var(kc_house_data$price)
s2
z = qnorm(0.95)
d = 10000
n = ceiling(1/(d^2/(z^2*s2) +1/N))
n
N

indexes = sample(1:21613, n)
mean(kc_house_data$price[indexes])



indexes = sample(1:21613, s)
mean(kc_house_data$price[indexes])





unique(kc_house_data$view)



mean(kc_house_data$price)