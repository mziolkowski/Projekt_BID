# PROJEKT COVID-19 JAKUB CIEAŚLIK::MACIEJ ZIOLKOWSK::JAKUB WOJCIK
attach(COVID_19_FULL)

danePL <- COVID_19_FULL[COVID_19_FULL$geoId=='PL',]
danePL
daneSE <- COVID_19_FULL[COVID_19_FULL$geoId=='SE',]
daneSE
daneIT <- COVID_19_FULL[COVID_19_FULL$geoId=='IT',]
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

plot(danePL$nextDay, danePL$cases) 
plot(daneSE$nextDay, daneSE$cases) 
plot(daneIT$nextDay, daneIT$cases)

################################################################################
################################################################################
reg.pl <- lm(danePL$cases~danePL$nextDay) #43% dopasowanie modelu
regkwadrat.pl <- lm(danePL$cases~danePL$nextDay + I(danePL$nextDay^2)) # 60% dopasowanie modelu
regszwscian.pl <-lm(danePL$cases~danePL$nextDay + I(danePL$nextDay^2) + I(danePL$nextDay^3)) # 62% dopasowanie modelu
summary(reg.pl)
summary(regkwadrat.pl)
summary(regszwscian.pl)

reg.se <- lm(daneSE$cases~daneSE$nextDay) # 63% dopasowanie modelu
regkwadrat.se <- lm(daneSE$cases~daneSE$nextDay + I(daneSE$nextDay^2)) # 65% dopasowanie modelu
regszwscian.se <-lm(daneSE$cases~daneSE$nextDay + I(daneSE$nextDay^2) + I(daneSE$nextDay^3)) # 66% dopasowanie modelu
summary(reg.se)
summary(regkwadrat.se)
summary(regszwscian.se)

reg.it <- lm(daneIT$cases~daneIT$nextDay) # 0.4% dopasowanie modelu
regkwadrat.it <- lm(daneIT$cases~daneIT$nextDay + I(daneIT$nextDay^2)) # 46% dopasowanie modelu
regszwscian.it <-lm(daneIT$cases~daneIT$nextDay + I(daneIT$nextDay^2) + I(daneIT$nextDay^3)) # 52% dopasowanie modelu
summary(reg.it)
summary(regkwadrat.it)
summary(regszwscian.it)

################################################################################
################################################################################
lengthPL <- nrow(danePL)
count.treningowy.pl <- floor(lengthPL * 0.8)

treningowy.pl <- danePL[1:count.treningowy.pl,]
testowy.pl <- danePL[(count.treningowy.pl+1):lengthPL,]

reg.pl.treningowy <- lm(treningowy.pl$cases~treningowy.pl$nextDay) #55% dopasowanie modelu
summary(reg.pl.treningowy)

################################################################################
lengthSE <- nrow(daneSE)
count.treningowy.se <- floor(lengthSE * 0.8)

treningowy.se <- daneSE[1:count.treningowy.se,]
testowy.se <- daneSE[(count.treningowy.se+1):lengthSE,]

reg.se.treningowy <- lm(treningowy.se$cases~treningowy.se$nextDay) #73% dopasowanie modelu
summary(reg.se.treningowy)

################################################################################
lengthIT <- nrow(daneIT)
count.treningowy.it <- floor(lengthIT * 0.8)

treningowy.it <- daneIT[1:count.treningowy.it,]
testowy.it <- daneIT[(count.treningowy.it+1):lengthIT,]

reg.it.treningowy <- lm(treningowy.it$cases~treningowy.it$nextDay) #33% dopasowanie modelu
summary(reg.it.treningowy)

################################################################################
################################################################################

regkwadrat.pl <-lm(danePL$cases~danePL$nextDay + I(danePL$nextDay^2)) # 60% dopasowanie modelu
regszwscian.pl <-lm(danePL$cases~danePL$nextDay + I(danePL$nextDay^2) + I(danePL$nextDay^3)) # 62% dopasowanie modelu
summary(regkwadrat.pl)
summary(regszwscian.pl)

regkwadrat.se <-lm(daneSE$cases~daneSE$nextDay + I(daneSE$nextDay^2)) # 64% dopasowanie modelu
regszwscian.se <-lm(daneSE$cases~daneSE$nextDay + I(daneSE$nextDay^2) + I(daneSE$nextDay^3)) # 66% dopasowanie modelu
summary(regkwadrat.se)
summary(regszwscian.se)

regkwadrat.it <-lm(daneIT$cases~daneIT$nextDay + I(daneIT$nextDay^2)) # 46% dopasowanie modelu
regszwscian.it <-lm(daneIT$cases~daneIT$nextDay + I(daneIT$nextDay^2) + I(daneIT$nextDay^3)) # 52% dopasowanie modelu
summary(regkwadrat.it)
summary(regszwscian.it)


#NA TYM ETAPIE SKONCZONE. NIE DZIALA PREDYKCJA 'envir' fail
testowy.pl$nextDay <- seq.int(nrow(testowy.pl))
predykcja.pl <- predict(reg.pl.treningowy, testowy.pl$nextDay, interval = "prediction") 
predykcja.pl 
testowy.pl
#dokonczyc dla reszty

plot(testowy.pl$nextDay, testowy.pl$cases)
abline(reg.pl.treningowy$coefficients, col="red", lwd=2)
segments(testowy.pl$nextDay,fitted.values(predykcja.pl), testowy.pl$nextDay, testowy.pl$cases)


