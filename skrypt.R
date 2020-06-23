# Regresja liniowa dla 
attach(COVID_19)
regresja.cases <- lm(cases~day, data = COVID_19)
summary(regresja.cases)

# Wykresy dla Polski, Szwecji i Włoch
#COVID_19$countriesAndTerritories <- factor(COVID_19$countriesAndTerritories)
#COVID_19$geoId <- factor(COVID_19$geoId)
#COVID_19$countryterritoryCode <- factor(COVID_19$countryterritoryCode)
#COVID_19$continentExp <- factor(COVID_19$continentExp)
#COVID_19$dateRep <- factor(COVID_19$dateRep)

as.numeric(COVID_19_FULL$dateRep) #wykonuje to zmiane dla dateRep na to co chcielismy czyli days (id dla kazdego dnia dla kazdego kraju) 
nextDays <- as.numeric(COVID_19_FULL$dateRep) 
nextDays

danePL <- COVID_19[COVID_19$geoId=='PL',]
danePL
danePL$id
daneSE <- COVID_19[COVID_19$geoId=='SE',]
daneSE
daneIT <- COVID_19[COVID_19$geoId=='IT',]
daneIT

plot(nextDays, COVID_19_FULL$cases) 
reg.all <- lm(COVID_19_FULL$cases~nextDays) 
abline(reg.all$coefficients, col='red', lwd = 2) 
cor.test(nextDays, COVID_19_FULL$cases) #brak zaleznosci liniowej

plot(danePL$days, danePL$cases) 
plot(daneSE$days, daneSE$cases) 
plot(daneIT$days, daneIT$cases)

reg.pl <- lm(danePL$cases~danePL$days)
reg.se <- lm(daneSE$cases~daneSE$days)
reg.it <- lm(daneIT$cases~daneIT$days)

summary(reg.pl)
summary(reg.se)
summary(reg.it)

#treningowy.pl <- danePL[1:(length(danePL)*0.8),]
treningowy.pl <- danePL[1:80,]
treningowy.pl.2 <- danePL[1:20,]
treningowy.pl
summary(reg.pl)
abline(reg.pl$coefficients, col='red', lwd=2)
?predict
#predykcja.pl <- predict(reg.pl, treningowy.pl, typ = "response") # nie wiem czy jest OK bo mamy response a nie interval jak w innych zadaniach z regresji liniowej 
predykcja.pl <- predict(reg.pl, danePL, interval = "prediction") 
predykcja.pl 
przypadkiPredykcji <- predykcja.pl[,1] 
plot(days[1:108], przypadkiPredykcji)
predykcja.se <- predict(reg.se, daneSE, interval = "prediction") 
predykcja.se 
przypadkiPredykcjiSe <- predykcja.se[,1] plot(days[109:282], przypadkiPredykcjiSe) # zaczynaja sie od -200 xD nie ogarniam tego

