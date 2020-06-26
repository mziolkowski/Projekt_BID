# PROJEKT COVID-19 JAKUB CIESLIK::MACIEJ ZIOLKOWSK::JAKUB WOJCIK
attach(COVID_19)

danePL <- subset(COVID_19[COVID_19$geoId=='PL',], cases > 0 )
danePL
daneSE <- subset(COVID_19[COVID_19$geoId=='SE',], cases > 0 )
daneSE
daneIT <- subset(COVID_19[COVID_19$geoId=='IT',], cases > 0 )
daneIT

################################################################################
################################################################################
danePL <- na.omit(danePL)
danePL<- danePL[seq(dim(danePL)[1],1),]
danePL$nextDay <- seq.int(nrow(danePL))
danePL$nextDay

daneSE <- na.omit(daneSE)
daneSE<- daneSE[seq(dim(daneSE)[1],1),]
daneSE$nextDay <- seq.int(nrow(daneSE))
daneSE$nextDay

daneIT <- na.omit(daneIT)
daneIT<- daneIT[seq(dim(daneIT)[1],1),]
daneIT$nextDay <- seq.int(nrow(daneIT))
daneIT$nextDay

plot(danePL$nextDay, danePL$cases, xlab = 'Kolejne dni', ylab = 'Przypadki' ,main = 'Liczba przypadków dla Polski')
plot(daneSE$nextDay, daneSE$cases, xlab = 'Kolejne dni', ylab = 'Przypadki' ,main = 'Liczba przypadków dla Szewcji')
plot(daneIT$nextDay, daneIT$cases, xlab = 'Kolejne dni', ylab = 'Przypadki' ,main = 'Liczba przypadków dla Włoch')

################################################################################
################################################################################
reg.pl <- lm(cases~nextDay, data = danePL) #44% dopasowanie modelu
regkwadrat.pl <- lm(cases~nextDay + I(nextDay^2), data = danePL) # 59% dopasowanie modelu
regszwscian.pl <-lm(cases~nextDay + I(nextDay^2) + I(nextDay^3), data = danePL) # 62% dopasowanie modelu
summary(reg.pl)
summary(regkwadrat.pl)
summary(regszwscian.pl)

reg.se <- lm(cases~nextDay, data = daneSE) # 63% dopasowanie modelu
regkwadrat.se <- lm(cases~nextDay + I(nextDay^2), data = daneSE) # 65% dopasowanie modelu
regszwscian.se <-lm(cases~nextDay + I(nextDay^2) + I(nextDay^3), data = daneSE) # 66% dopasowanie modelu
summary(reg.se)
summary(regkwadrat.se)
summary(regszwscian.se)

reg.it <- lm(cases~nextDay, data = daneIT) # 0.3 dopasowanie modelu
regkwadrat.it <- lm(cases~nextDay + I(nextDay^2), data = daneIT) # 46% dopasowanie modelu
regszwscian.it <-lm(cases~nextDay + I(nextDay^2) + I(nextDay^3), data = daneIT) # 52% dopasowanie modelu
summary(reg.it)
summary(regkwadrat.it)
summary(regszwscian.it)

################################################################################
dane <- danePL
dane <- daneSE
dane <- daneIT # Dla Włoch nie ma to sensu

## Predykcja, linear modulation
length <- nrow(dane)
count.treningowy <- floor(length * 0.8)

treningowy <- dane[1:count.treningowy,]
testowy <- dane[(count.treningowy+1):length,]

reg.treningowy <- lm(cases~nextDay, data = treningowy)
summary(reg.treningowy)
predykcja <- predict(reg.treningowy, newdata = data.frame(nextDay = testowy$nextDay), interval = "prediction") 
plot(testowy$nextDay, testowy$cases, xlab = 'Kolejne dni', ylab = 'Przypadki', main = 'Liczba przypadków (zbiór testowy)')
abline(reg.treningowy$coefficients, col = "red", lwd=2)
segments(testowy$nextDay, predykcja[,1], testowy$nextDay, testowy$cases)

## Funkcja kwadratowa 
reg.treningowy.kwadrat <- lm(cases~nextDay + I(nextDay^2), data = treningowy) 
summary(reg.treningowy.kwadrat)
predykcja.kwadrat <- predict(reg.treningowy.kwadrat, newdata = data.frame(nextDay = testowy$nextDay), interval = "prediction") 
plot(testowy$nextDay, testowy$cases, xlab = 'Kolejne dni', ylab = 'Przypadki', main = 'Liczba przypadków (zbiór testowy)')
lines(testowy$nextDay, predykcja.kwadrat[,1], col="red", lwd=2)
segments(testowy$nextDay, predykcja.kwadrat[,1], testowy$nextDay, testowy$cases)

# Wielomian 3-ciego stopnia
reg.treningowy.3ciapotega <- lm(cases~nextDay + I(nextDay^2) + I(nextDay^3), data = treningowy) 
summary(reg.treningowy.3ciapotega)
predykcja.3ciapotega<- predict(reg.treningowy.3ciapotega, newdata = data.frame(nextDay = testowy$nextDay), interval = "prediction") 
plot(testowy$nextDay, testowy$cases, xlab = 'Kolejne dni', ylab = 'Przypadki', main = 'Liczba przypadków (zbiór testowy)')
lines(testowy$nextDay, predykcja.3ciapotega[,1], col="red", lwd=2)
segments(testowy$nextDay, predykcja.3ciapotega[,1], testowy$nextDay, testowy$cases)

# Model predykujący początek pandemii
plot(danePL$nextDay, danePL$cases, xlab = 'Kolejne dni', ylab = 'Przypadki' ,main = 'Liczba przypadków dla Polski')
plot(daneSE$nextDay, daneSE$cases, xlab = 'Kolejne dni', ylab = 'Przypadki' ,main = 'Liczba przypadków dla Szewcji')
# plot(daneIT$nextDay, daneIT$cases, xlab = 'Kolejne dni', ylab = 'Przypadki' ,main = 'Liczba przypadków dla Włoch')
# poczatek.dane <- data.frame(nextDay = c(danePL$nextDay[1:30], daneSE$nextDay[1:30], daneIT$nextDay[1:30]),
#                             cases = c(danePL$cases[1:30], daneSE$cases[1:30], daneIT$cases[1:30]))
poczatek.dane <- data.frame(nextDay = c(danePL$nextDay[1:30], daneSE$nextDay[1:30]),
                           cases = c(danePL$cases[1:30], daneSE$cases[1:30]))
plot(poczatek.dane$nextDay, poczatek.dane$cases, xlab = 'Kolejne dni', ylab = 'Przypadki')

reg.treningowy.kwadrat <- lm(cases~nextDay + I(nextDay^2), data = poczatek.dane) 
summary(reg.treningowy.kwadrat)
kolejne.dni <- data.frame(nextDay = c(31:44))
predykcja.kwadrat <- predict(reg.treningowy.kwadrat, newdata = kolejne.dni, interval = "prediction") 
plot(c(1:44), c(danePL$cases[1:30],predykcja.kwadrat[,1]), xlab = 'Kolejne dni', ylab = 'Przypadki', main = 'Przedykcja rozwoju')
segments(c(31:44), predykcja.kwadrat[,1], c(31:44), danePL$cases[31:44], col="red")
points(c(31:44),danePL$cases[31:44], col="red")

## NALEZY:
# 1. zbudować model dla danych PL
# 2. wczytac dane <- daneSE i zainicjalizować zbiór testowy
predykcja.se <- predict(reg.treningowy, newdata = data.frame(nextDay = testowy$nextDay), interval = "prediction") 
predykcja.se
plot(daneSE$nextDay, daneSE$cases, xlab = 'Kolejne dni', ylab = 'Przypadki')
lines(testowy$nextDay, predykcja.se[,1], col="red")
segments(testowy$nextDay, predykcja.se[,1], testowy$nextDay, testowy$cases)
################################################################################
################################################################################
install.packages("tree")
library(tree)

dane <- danePL
dane <- daneSE
dane <- daneIT

treningowy <- dane[1:count.treningowy,]
testowy <- dane[(count.treningowy+1):length,]

# Przypadek jeżeli dziś jest 10 przypadków to dziś jest X śmierci
drzewo.reg <- tree(deaths~cases, treningowy)
summary(drzewo.reg)
plot(drzewo.reg)
text(drzewo.reg, pretty = 0)

# Jeżeli dziś jest 10 przypadków to za 7 dni będzie X śmierci
length <- nrow(dane)
count.treningowy <- floor(length * 0.8)

treningowy <- dane[1:count.treningowy,]
testowy <- dane[(count.treningowy+1):length,]

a <- data.frame(
  cases=dane$cases[1:count.treningowy],
  deaths=dane$deaths[8:(count.treningowy+7)]
)
a

drzewo.reg <- tree(deaths~cases, a)
summary(drzewo.reg)
plot(drzewo.reg)
text(drzewo.reg, pretty = 0)

prognoza.tree <-predict(drzewo.reg, newdata = data.frame(cases = dane$cases[(count.treningowy+8):length]))
prognoza.tree
wyniki.przyciete <- table(prognoza.tree, dane$deaths[(count.treningowy+8):length])
wyniki.przyciete
sum(diag(wyniki.przyciete)/sum(wyniki.przyciete))

################################################################################

# Jeżeli dziś jest 10 przypadków to za 14 dni będzie X śmierci
b <- data.frame(
  cases=dane$cases[1:count.treningowy],
  deaths=dane$deaths[14:(count.treningowy+13)]
)
b

drzewo.reg.b <- tree(deaths~cases, b)
summary(drzewo.reg.b)
plot(drzewo.reg.b)
text(drzewo.reg.b, pretty = 0)

prognoza.tree.b <-predict(drzewo.reg.b, newdata = data.frame(cases = dane$cases[(count.treningowy+8):length]))
prognoza.tree.b
wyniki.przyciete.b <- table(prognoza.tree.b, dane$deaths[(count.treningowy+8):length])
wyniki.przyciete.b
sum(diag(wyniki.przyciete.b)/sum(wyniki.przyciete.b))
################################################################################
################################################################################



