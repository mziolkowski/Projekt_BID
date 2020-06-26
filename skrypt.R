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
plot(daneIT$nextDay, daneIT$cases, xlab = 'Kolejne dni', ylab = 'Przypadki' ,main = 'Liczba przypadków dla Włoch')
poczatek.dane <- data.frame(nextDay = c(danePL$nextDay[1:30], daneSE$nextDay[1:30], daneIT$nextDay[1:30]),
                            cases = c(danePL$cases[1:30], daneSE$cases[1:30], daneIT$cases[1:30]))
plot(poczatek.dane$nextDay, poczatek.dane$cases)

## NALEZY wczytac przed tym dane <- daneSE
predykcja.se <- predict(reg.treningowy, newdata = data.frame(nextDay = testowy$nextDay), interval = "prediction") 
predykcja.se
################################################################################
################################################################################
install.packages("tree")
library(tree)

# Przypadek jeżeli dziś jest 10 przypadków to dziś jest X śmierci
drzewo.reg.pl <- tree(deaths~cases, treningowy.pl)
summary(drzewo.reg.pl)
plot(drzewo.reg.pl)
text(drzewo.reg.pl, pretty = 0)

# Jeżeli dziś jest 10 przypadków to za 7 dni będzie X śmierci
lengthPL <- nrow(danePL)
count.treningowy.pl <- floor(lengthPL * 0.8)

treningowy.pl <- danePL[1:count.treningowy.pl,]
testowy.pl <- danePL[(count.treningowy.pl+1):lengthPL,]

a <- data.frame(
  cases=danePL$cases[1:count.treningowy.pl],
  deaths=danePL$deaths[8:(count.treningowy.pl+7)]
)
a

drzewo.reg.pl <- tree(deaths~cases, a)
summary(drzewo.reg.pl)
plot(drzewo.reg.pl)
text(drzewo.reg.pl, pretty = 0)

prognoza.tree.pl <-predict(drzewo.reg.pl, newdata = data.frame(cases = danePL$cases[(count.treningowy.pl+8):lengthPL]))
prognoza.tree.pl
wyniki.przyciete.pl <- table(prognoza.tree.pl, danePL$deaths[(count.treningowy.pl+8):lengthPL])
wyniki.przyciete.pl
sum(diag(wyniki.przyciete.pl)/sum(wyniki.przyciete.pl))

################################################################################

# Jeżeli dziś jest 10 przypadków to za 14 dni będzie X śmierci
b <- data.frame(
  cases=danePL$cases[1:count.treningowy.pl],
  deaths=danePL$deaths[14:(count.treningowy.pl+13)]
)
b

drzewo.reg.pl.b <- tree(deaths~cases, b)
summary(drzewo.reg.pl.b)
plot(drzewo.reg.pl.b)
text(drzewo.reg.pl.b, pretty = 0)

prognoza.tree.pl.b <-predict(drzewo.reg.pl.b, newdata = data.frame(cases = danePL$cases[(count.treningowy.pl+8):lengthPL]))
prognoza.tree.pl.b
wyniki.przyciete.pl.b <- table(prognoza.tree.pl.b, danePL$deaths[(count.treningowy.pl+8):lengthPL])
wyniki.przyciete.pl.b
sum(diag(wyniki.przyciete.pl.b)/sum(wyniki.przyciete.pl.b))
################################################################################
################################################################################

# Przypadek jeżeli dziś jest 10 przypadków to dziś jest X śmierci
drzewo.reg.se <- tree(deaths~cases, treningowy.se)
summary(drzewo.reg.se)
plot(drzewo.reg.se)
text(drzewo.reg.se, pretty = 0)

# Jeżeli dziś jest 10 przypadków to za 7 dni będzie X śmierci
lengthSE <- nrow(daneSE)
count.treningowy.se <- floor(lengthSE * 0.8)

treningowy.se <- daneSE[1:count.treningowy.se,]
testowy.se <- daneSE[(count.treningowy.se+1):lengthSE,]

a <- data.frame(
  cases=daneSE$cases[1:count.treningowy.se],
  deaths=daneSE$deaths[8:(count.treningowy.se+7)]
)
a

drzewo.reg.se <- tree(deaths~cases, a)
summary(drzewo.reg.se)
plot(drzewo.reg.se)
text(drzewo.reg.se, pretty = 0)

prognoza.tree.se <-predict(drzewo.reg.se, newdata = data.frame(cases = daneSE$cases[(count.treningowy.se+8):lengthSE]))
prognoza.tree.se
wyniki.przyciete.se <- table(prognoza.tree.se, daneSE$deaths[(count.treningowy.se+8):lengthSE])
wyniki.przyciete.se
sum(diag(wyniki.przyciete.se)/sum(wyniki.przyciete.se))

################################################################################

# Jeżeli dziś jest 10 przypadków to za 14 dni będzie X śmierci
b <- data.frame(
  cases=daneSE$cases[1:count.treningowy.se],
  deaths=daneSE$deaths[14:(count.treningowy.se+13)]
)
b

drzewo.reg.se.b <- tree(deaths~cases, b)
summary(drzewo.reg.se.b)
plot(drzewo.reg.se.b)
text(drzewo.reg.se.b, pretty = 0)

prognoza.tree.se.b <-predict(drzewo.reg.se.b, newdata = data.frame(cases = daneSE$cases[(count.treningowy.se+8):lengthSE]))
prognoza.tree.se.b
wyniki.przyciete.se.b <- table(prognoza.tree.se.b, daneSE$deaths[(count.treningowy.se+8):lengthSE])
wyniki.przyciete.se.b
sum(diag(wyniki.przyciete.pl.b)/sum(wyniki.przyciete.pl.b))


################################################################################
################################################################################
# Przypadek jeżeli dziś jest 10 przypadków to dziś jest X śmierci
drzewo.reg.it <- tree(deaths~cases, treningowy.it)
summary(drzewo.reg.it)
plot(drzewo.reg.it)
text(drzewo.reg.it, pretty = 0)

# Jeżeli dziś jest 10 przypadków to za 7 dni będzie X śmierci
lengthIT <- nrow(daneIT)
count.treningowy.it <- floor(lengthIT * 0.8)

treningowy.it <- daneIT[1:count.treningowy.it,]
testowy.it <- daneIT[(count.treningowy.it+1):lengthIT,]

a <- data.frame(
  cases=daneIT$cases[1:count.treningowy.it],
  deaths=daneIT$deaths[8:(count.treningowy.it+7)]
)
a

drzewo.reg.it <- tree(deaths~cases, a)
summary(drzewo.reg.it)
plot(drzewo.reg.it)
text(drzewo.reg.it, pretty = 0)

prognoza.tree.it <-predict(drzewo.reg.it, newdata = data.frame(cases = daneIT$cases[(count.treningowy.it+8):lengthIT]))
prognoza.tree.it
wyniki.przyciete.it <- table(prognoza.tree.it, daneIT$deaths[(count.treningowy.se+8):lengthIT])
wyniki.przyciete.it
sum(diag(wyniki.przyciete.it)/sum(wyniki.przyciete.it))

################################################################################

# Jeżeli dziś jest 10 przypadków to za 14 dni będzie X śmierci
b <- data.frame(
  cases=daneIT$cases[1:count.treningowy.it],
  deaths=daneIT$deaths[14:(count.treningowy.it+13)]
)
b

drzewo.reg.it.b <- tree(deaths~cases, b)
summary(drzewo.reg.it.b)
plot(drzewo.reg.it.b)
text(drzewo.reg.it.b, pretty = 0)

prognoza.tree.it.b <-predict(drzewo.reg.it.b, newdata = data.frame(cases = daneIT$cases[(count.treningowy.it+8):lengthIT]))
prognoza.tree.it.b
wyniki.przyciete.it.b <- table(prognoza.tree.it.b, daneIT$deaths[(count.treningowy.it+8):lengthIT])
wyniki.przyciete.it.b
sum(diag(wyniki.przyciete.it.b)/sum(wyniki.przyciete.it.b))





