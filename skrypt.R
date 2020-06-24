# PROJEKT COVID-19 JAKUB CIESLIK::MACIEJ ZIOLKOWSK::JAKUB WOJCIK
attach(COVID_19_FULL)

#danePL <- COVID_19_FULL[COVID_19_FULL$geoId=='PL',]
#danePL
#daneSE <- COVID_19_FULL[COVID_19_FULL$geoId=='SE',]
#daneSE
#daneIT <- COVID_19_FULL[COVID_19_FULL$geoId=='IT',]
#daneIT

danePL <- subset(COVID_19_FULL[COVID_19_FULL$geoId=='PL',], cases > 0 )
danePL
daneSE <- subset(COVID_19_FULL[COVID_19_FULL$geoId=='SE',], cases > 0 )
daneSE
daneIT <- subset(COVID_19_FULL[COVID_19_FULL$geoId=='IT',], cases > 0 )
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
################################################################################
lengthPL <- nrow(danePL)
count.treningowy.pl <- floor(lengthPL * 0.8)

treningowy.pl <- danePL[1:count.treningowy.pl,]
testowy.pl <- danePL[(count.treningowy.pl+1):lengthPL,]

reg.pl.treningowy <- lm(cases~nextDay, data = treningowy.pl) #55% dopasowanie modelu
summary(reg.pl.treningowy)

################################################################################
lengthSE <- nrow(daneSE)
count.treningowy.se <- floor(lengthSE * 0.8)

treningowy.se <- daneSE[1:count.treningowy.se,]
testowy.se <- daneSE[(count.treningowy.se+1):lengthSE,]

reg.se.treningowy <- lm(cases~nextDay, data = treningowy.se) #74% dopasowanie modelu
summary(reg.se.treningowy)

################################################################################
lengthIT <- nrow(daneIT)
count.treningowy.it <- floor(lengthIT * 0.8)

treningowy.it <- daneIT[1:count.treningowy.it,]
testowy.it <- daneIT[(count.treningowy.it+1):lengthIT,]

reg.it.treningowy <- lm(cases~nextDay, data = treningowy.it) #33% dopasowanie modelu
summary(reg.it.treningowy)

################################################################################
################################################################################

regkwadrat.pl <-lm(cases~nextDay + I(nextDay^2), data = danePL) # 59% dopasowanie modelu
regszwscian.pl <-lm(cases~nextDay + I(nextDay^2) + I(nextDay^3), data = danePL) # 62% dopasowanie modelu
summary(regkwadrat.pl)
summary(regszwscian.pl)

regkwadrat.se <-lm(cases~nextDay + I(nextDay^2), data = daneSE) # 65% dopasowanie modelu
regszwscian.se <-lm(cases~nextDay + I(nextDay^2) + I(nextDay^3), data = daneSE) # 66% dopasowanie modelu
summary(regkwadrat.se)
summary(regszwscian.se)

regkwadrat.it <-lm(cases~nextDay + I(nextDay^2), data = daneIT) # 46% dopasowanie modelu
regszwscian.it <-lm(cases~nextDay + I(nextDay^2) + I(nextDay^3), data = daneIT) # 52% dopasowanie modelu
summary(regkwadrat.it)
summary(regszwscian.it)

################################################################################
################################################################################
testowy.pl$nextDay <- seq.int(nrow(testowy.pl))
predykcja.pl <- predict(reg.pl.treningowy, newdata = data.frame(nextDay = testowy.pl$nextDay), interval = "prediction") 
predykcja.pl 

testowy.se$nextDay <- seq.int(nrow(testowy.se))
predykcja.se <- predict(reg.se.treningowy, newdata = data.frame(nextDay = testowy.se$nextDay), interval = "prediction") 
predykcja.se

testowy.it$nextDay <- seq.int(nrow(testowy.it))
predykcja.it <- predict(reg.it.treningowy, newdata = data.frame(nextDay = testowy.it$nextDay), interval = "prediction") 
predykcja.it
################################################################################
################################################################################

plot(testowy.pl$nextDay, testowy.pl$cases)
abline(reg.pl.treningowy$coefficients, col="red", lwd=2)
segments(testowy.pl$nextDay,fitted.values(reg.pl.treningowy), testowy.pl$nextDay, testowy.pl$cases)

plot(testowy.se$nextDay, testowy.se$cases)
abline(reg.se.treningowy$coefficients, col="red", lwd=2)
segments(testowy.se$nextDay,fitted.values(reg.se.treningowy), testowy.se$nextDay, testowy.se$cases)

plot(testowy.it$nextDay, testowy.it$cases)
abline(reg.it.treningowy$coefficients, col="red", lwd=2)
segments(testowy.it$nextDay,fitted.values(reg.it.treningowy), testowy.it$nextDay, testowy.it$cases)

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


