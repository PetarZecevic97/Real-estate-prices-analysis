install.packages("ggplot2")
library(ggplot2)
library(scales)

#otklanjamo e notifikaciju sa realnih brojeva
options(scipen=999)

#konvertujemo stope kvadratne u metre kvadtarne
f2_to_m2 = (0.3048)^2

N = length(kc_house_data$id)

#izbacujemo redove koji imaju 0 soba ili 0 kupatila jer su to verovatno greske
indexes_wo_ano = c()
for (i in 1:N){
  if (kc_house_data$bedrooms[i] > 0 && kc_house_data$bathrooms[i] > 0.5)
  {
    indexes_wo_ano = c(indexes_wo_ano, i)
  }
}
house_data_prep = kc_house_data[indexes_wo_ano,]

N = length(house_data_prep$id)

#pravimo kolone za metre kvadratne umesto kolona za stope kvadratne
house_data_prep$sqm_living = house_data_prep$sqft_living*f2_to_m2
house_data_prep$sqm_lot = house_data_prep$sqft_lot*f2_to_m2
house_data_prep$sqm_above = house_data_prep$sqft_above*f2_to_m2
house_data_prep$sqm_basement = house_data_prep$sqft_basement*f2_to_m2
house_data_prep$sqm_living15 = house_data_prep$sqft_living15*f2_to_m2
house_data_prep$sqm_lot15 = house_data_prep$sqft_lot15*f2_to_m2
house_data_prep = subset(house_data_prep, select=-c(sqft_living, sqft_lot, sqft_above, sqft_basement, sqft_living15, sqft_lot15))
#dimnames(house_data_prep)

#histogram cena
hist(house_data_prep$price, xlab="Cena nekretnine", ylab = "Broj nekretnina", main = NULL)
#ggplot(house_data_prep, aes(x=price)) + geom_histogram()

hist(house_data_prep$sqm_living, xlab="Broj metara kvadratnih", ylab = "Broj nekretnina", main = NULL)

#procenat nekretnina koje kostaju manje od 1,000,000 dolara
n_lt_1m = length(house_data_prep[house_data_prep$price<1000000,1])
perc_n_lt_1m = n_lt_1m/N*100
perc_n_lt_1m

#minimalna i maksimalna cena
min(house_data_prep$price)
max(house_data_prep$price)

#racunamo cene jednog kvadrata svake nekretnine kao i njihovu prosecnu ocenu
house_data_prep$price_of_m2 = house_data_prep$price/house_data_prep$sqm_living
house_data_prep$price_of_m2
hist(house_data_prep$price_of_m2, xlab = "Cena kvadrata", ylab = "Broj nekretnina", main = NULL)

#sve kuce sa 4 ili vise soba kategorisemo zajedno
bedrooms = house_data_prep$bedrooms
for (i in 1:length(bedrooms)){
  if (bedrooms[i] > 4){
    bedrooms[i] = 5
  }
}

#pravimo pie chart za broj soba
mytable = table(bedrooms)
lbls = paste(names(mytable), "=", round(mytable/sum(mytable)*100, digits = 2),"%", sep=" ")
lbls[5] = paste("5+ ", "=", round(mytable[5]/sum(mytable)*100, digits = 2),"%", sep=" ")
pie(mytable, labels = lbls, 
    main=NULL)

#sve kuce sa 3 ili vise kupatila kategorisemo zajedno
bathrooms = house_data_prep$bathrooms
for (i in 1:length(bathrooms)){
  bathrooms[i] = round(bathrooms[i])
  if (bathrooms[i] > 3){
    bathrooms[i] = 4
  }
}

#pravimo pie chart za broj kupatila
mytable = table(bathrooms)
lbls = paste(names(mytable), "=", round(mytable/sum(mytable)*100, digits = 2),"%", sep=" ")
lbls[4] = paste("4+ ", "=", round(mytable[4]/sum(mytable)*100, digits = 2),"%", sep=" ")
pie(mytable, labels = lbls, 
    main=NULL)

#diskretizujemo kolonu sa metrima kvadratnim
house_data_prep$sqm_living_bin = cut(house_data_prep$sqm_living, breaks = c(0,100,200,300,400,1300), labels = c("0-100", "100-200", "200-300", "300-400", "400-1300"))

#pravimo pie chart sa diskretizovanim podacima
bins = house_data_prep$sqm_living_bin
mytable = table(bins)
lbls = paste(names(mytable), "=", round(mytable/sum(mytable)*100, digits = 2),"%", sep=" ")
pie(mytable, labels = lbls, 
    main=NULL)

#pravimo grafik korelacije izmedju cene i metara kvadratnih nekretnine
plot(house_data_prep$price, house_data_prep$sqm_living, xlab = "prices", ylab = "sqm")
cor(house_data_prep$price, house_data_prep$sqm_living)

#racunamo idealnu velicinu uzorka
n = ceiling(qchisq(0.95, 7)*N*0.5*0.5/(0.05^2*(N-1)) + qchisq(0.95, 7)*0.5*0.5)
n
procenat_velicine_uzorka = n/N*100

#postavljamo seed na fiksnu vrednost zbog vise pokretanja
set.seed(6)

#Vadjenje prostog slucajnog uzorka
sample_psu = sample(1:N, n)

#Ocena srednje vrednosti prostog slucajnog uzorka
xn_ocena_psu = mean(house_data_prep$price[sample_psu])

#Ocena disprezije ocene srednje vrednosti prostog slucajnog uzorka
s2_psu = var(house_data_prep$price[sample_psu])
d_ocena_psu = s2_psu/n*(1-n/N)

xn_ocena_psu
d_ocena_psu

#Disperzija srednje vrednosti prostog slucajnog uzorka
var(house_data_prep$price)/n*(1-n/N)

#Racunjanje intervala poverenja za psu
aplha = 0.05

#Posto je n>30 onda koristimo normalnu raspodelu
z = qnorm(1 - aplha/2)
interval = c(xn_ocena_psu - z*sqrt(d_ocena_psu), xn_ocena_psu + z*sqrt(d_ocena_psu))
interval

#Regresiono ocenjivanje gde je pomocna promenljiva broj metara kvadratnih zatvorenog prostora
uzorak_za_reg = sample(1:N, n)

#Racunanje pomocnih promenljivih ro i b
ro = cor(house_data_prep$price[uzorak_za_reg], house_data_prep$sqm_living[uzorak_za_reg])
b = ro*sqrt(var(house_data_prep$price[uzorak_za_reg]))/sqrt(var(house_data_prep$sqm_living[uzorak_za_reg]))

#Racunanje ocene srednje vrednosti regresionim ocenjivanjem
xn_lr = mean(house_data_prep$price[uzorak_za_reg])+b*(mean(house_data_prep$sqm_living[uzorak_za_reg] - mean(house_data_prep$sqm_living[uzorak_za_reg])))

#Racunanje ocene disperzije srednje vrednosti regresionim ocenjivanjem
se = 1/(n-1)*sum((house_data_prep$price[uzorak_za_reg] - (mean(house_data_prep$price[uzorak_za_reg]) + b*(house_data_prep$sqm_living[uzorak_za_reg] - mean(house_data_prep$sqm_living[uzorak_za_reg]))))^2)
d_ocena_reg = se/n*(1-n/N)

#Racunanje inervala poverenja za regresiono ocenjivanje
interval_reg = c(xn_lr - z*sqrt(d_ocena_reg), xn_lr + z*sqrt(d_ocena_reg))
interval_reg

xn_lr
d_ocena_reg

#Prava srednja vrednost
mean(house_data_prep$price)

#Pravimo stratume od obelezja bathrooms pa hocemo da diskretizujemo to obelezje
#Takodje sve nekretnine sa 4 ili vise kupatila svrstavamo u jedan stratum
house_data_prep$bathrooms_rounded = round(house_data_prep$bathrooms)
for (i in 1:N){
  if (house_data_prep$bathrooms_rounded[i] > 4){
    house_data_prep$bathrooms_rounded[i] = 4
  }
}
unique(house_data_prep$bathrooms_rounded)

#Pravimo listu stratuma
stratumi = list()
stratumi[[1]] = house_data_prep[house_data_prep$bathrooms_rounded == 1,]
stratumi[[2]] = house_data_prep[house_data_prep$bathrooms_rounded == 2,]
stratumi[[3]] = house_data_prep[house_data_prep$bathrooms_rounded == 3,]
stratumi[[4]] = house_data_prep[house_data_prep$bathrooms_rounded == 4,]
stratumi[[1]]

#Cuvamo disperzije svakog stratuma i velicine
disprezije_stratuma = c(sqrt(var(stratumi[[1]]$price)), sqrt(var(stratumi[[2]]$price)), sqrt(var(stratumi[[3]]$price)), sqrt(var(stratumi[[4]]$price)))
N_stratuma = c(length(stratumi[[1]]$price), length(stratumi[[2]]$price), length(stratumi[[3]]$price), length(stratumi[[4]]$price))

#Nejmanovom rapodelom odredjujemo velicine uzorka iz svakog stratuma
n_stratuma = round(n*(N_stratuma*disprezije_stratuma)/sum(N_stratuma*disprezije_stratuma))

#Posto smo odmah dobili zeljeni ukupni broj elemenata u uzoraku, ne moramo da dodajemo ili oduzimamo elemente
n_stratuma
sum(n_stratuma) == n

#Uzimamo prost slucajan uzorak iz svakog stratuma
indeksi_strat = c()
indeksi_strat[[1]] = sample(1:N_stratuma[1], n_stratuma[1], replace = F)
indeksi_strat[[2]] = sample(1:N_stratuma[2], n_stratuma[2], replace = F)
indeksi_strat[[3]] = sample(1:N_stratuma[3], n_stratuma[3], replace = F)
indeksi_strat[[4]] = sample(1:N_stratuma[4], n_stratuma[4], replace = F)

#Racunamo ocene srednje vrednosti svakog stratuma
xn_strat = c()
xn_strat[1] = mean(house_data_prep$price[indeksi_strat[[1]]])
xn_strat[2] = mean(house_data_prep$price[indeksi_strat[[2]]])
xn_strat[3] = mean(house_data_prep$price[indeksi_strat[[3]]])
xn_strat[4] = mean(house_data_prep$price[indeksi_strat[[4]]])

#Racunamo ocene uzoracke disperzije svakog stratuma
uzoracka_disperzija_stratuma = c()
uzoracka_disperzija_stratuma[1] = var(house_data_prep$price[indeksi_strat[[1]]])
uzoracka_disperzija_stratuma[2] = var(house_data_prep$price[indeksi_strat[[2]]])
uzoracka_disperzija_stratuma[3] = var(house_data_prep$price[indeksi_strat[[3]]])
uzoracka_disperzija_stratuma[4] = var(house_data_prep$price[indeksi_strat[[4]]])

#Racunamo ocenu srednje vrednosti dobijenu stratifikovanim uzorkovanjem
xn_st = 1/N*sum(N_stratuma*xn_strat)
xn_st

#Racunamo ocenu disperzije te ocene
d_ocena_st = 1/N^2*sum((N_stratuma^2*uzoracka_disperzija_stratuma)/n_stratuma*(1-n_stratuma/N_stratuma))
d_ocena_st

#Racunamo interval poverenja te ocene
interval_strat = c(xn_st - z*sqrt(d_ocena_st), xn_st + z*sqrt(d_ocena_st))
interval_strat

#Aproskimativno delimo nekretnine na one koje su u Sijetlu i one koje su van njega
lat_min = 47.501221
long_max = -122.238907
lat_max = 47.735211
long_min = -122.443828
house_data_prep$is_seattle = ifelse(house_data_prep$lat > lat_min & house_data_prep$lat < lat_max & house_data_prep$long > long_min & house_data_prep$long < long_max, 1, 0)
unique(house_data_prep$is_seattle)

mytable = table(house_data_prep$is_seattle)
lbls = paste(c("in Seattle", "not in Seattle"), "=", round(mytable/sum(mytable)*100, digits = 2),"%", sep=" ")
pie(mytable, labels = lbls, 
    main=NULL)

#Zelimo da vidimo da li postoji korelacije izmedju cene i toga da li se nekretnina nalazi u Sijetlu
#Ispostavlja se da nema korelacije
cor(house_data_prep$price, house_data_prep$is_seattle)

#Isto to samo za obelezje cena metra kvadatnog
cor(house_data_prep$price_of_m2, house_data_prep$is_seattle)

#U ovom delu koda ocenjijemo srednju vrednost za obeleyje cena metra kvadratnog
#Koristimo PSU i straifikovano uzorkovanje po broju kupatila
#Veoma slicno kao do sad tako da nece biti komentara
sample_psu_2 = sample(1:N, n)

xn_ocena_psu_2 = mean(house_data_prep$price_of_m2[sample_psu_2])

s2_psu_2 = var(house_data_prep$price_of_m2[sample_psu_2])
d_ocena_psu_2 = s2_psu_2/n*(1-n/N)

xn_ocena_psu_2
d_ocena_psu_2
mean(house_data_prep$price_of_m2)

interval_psu_2 = c(xn_ocena_psu_2 - z*sqrt(d_ocena_psu_2), xn_ocena_psu_2 + z*sqrt(d_ocena_psu_2))
interval_psu_2

indeksi_strat_2 = c()
indeksi_strat_2[[1]] = sample(1:N_stratuma[1], n_stratuma[1], replace = F)
indeksi_strat_2[[2]] = sample(1:N_stratuma[2], n_stratuma[2], replace = F)
indeksi_strat_2[[3]] = sample(1:N_stratuma[3], n_stratuma[3], replace = F)
indeksi_strat_2[[4]] = sample(1:N_stratuma[4], n_stratuma[4], replace = F)

xn_strat_2 = c()
xn_strat_2[1] = mean(house_data_prep$price_of_m2[indeksi_strat_2[[1]]])
xn_strat_2[2] = mean(house_data_prep$price_of_m2[indeksi_strat_2[[2]]])
xn_strat_2[3] = mean(house_data_prep$price_of_m2[indeksi_strat_2[[3]]])
xn_strat_2[4] = mean(house_data_prep$price_of_m2[indeksi_strat_2[[4]]])

uzoracka_disperzija_stratuma_2 = c()
uzoracka_disperzija_stratuma_2[1] = var(house_data_prep$price_of_m2[indeksi_strat_2[[1]]])
uzoracka_disperzija_stratuma_2[2] = var(house_data_prep$price_of_m2[indeksi_strat_2[[2]]])
uzoracka_disperzija_stratuma_2[3] = var(house_data_prep$price_of_m2[indeksi_strat_2[[3]]])
uzoracka_disperzija_stratuma_2[4] = var(house_data_prep$price_of_m2[indeksi_strat_2[[4]]])

xn_st_2 = 1/N*sum(N_stratuma*xn_strat_2)
xn_st_2

d_ocena_st_2 = 1/N^2*sum((N_stratuma^2*uzoracka_disperzija_stratuma_2)/n_stratuma*(1-n_stratuma/N_stratuma))
d_ocena_st_2

interval_strat = c(xn_st_2 - z*sqrt(d_ocena_st_2), xn_st_2 + z*sqrt(d_ocena_st_2))
interval_strat