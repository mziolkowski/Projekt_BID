getwd()
?read.table
Covid<- read.csv2("C:/Users/jwojcik.ALFA/Desktop/WIT/Zaliczenia sem 2/zaliczenie 2 - big data/Projekt/Covid-19.csv")
str(Covid)


Covid$countriesAndTerritories <- factor(Covid$countriesAndTerritories)
Covid$geoId <- factor(Covid$geoId)
Covid$countryterritoryCode <- factor(Covid$countryterritoryCode)
Covid$continentExp <- factor(Covid$continentExp)
str(Covid)

reglog.deaths <- glm(deaths~., data=Covid)
summary(reglog.deaths)




plot(1:length(Covid$deaths),Covid$month,
     type = "l",xlab = "śmierć", ylab = "miesiące",main = "Wykres śmierć/miesiąc")



regresja.deaths.month <- lm(deaths~month, data = Covid)
regresja.deaths.month

names(regresja.deaths.month)
abline(regresja.deaths.month$coefficients, col='red', lwd=2)
segments(Covid$deaths, fitted(regresja.deaths.month), Covid$deaths, Covid$month)


nowy.zestaw.covid <- data.frame(month=5,
                                year=2020,
                                countriesAndTerritories='Poland')

nowy.zestaw.covid


regresja.Covid.All <- lm(deaths ~., data = Covid)
regresja.Covid.All

# Regresja liniowa dla 
attach(COVID_19)
regresja.cases <- lm(cases~day, data = COVID_19)
summary(regresja.cases)

# Wykresy dla Polski, Szwecji i Włoch
COVID_19$countriesAndTerritories <- factor(COVID_19$countriesAndTerritories)
COVID_19$geoId <- factor(COVID_19$geoId)
COVID_19$countryterritoryCode <- factor(COVID_19$countryterritoryCode)
COVID_19$continentExp <- factor(COVID_19$continentExp)
COVID_19$dateRep <- factor(COVID_19$dateRep)


?subset()
dane <- (geoId="PL")


danePL <- COVID_19[COVID_19$geoId=='PL',]
danePL
danePL$id
daneSE <- COVID_19[COVID_19$geoId=='SE',]
daneSE
daneIT <- COVID_19[COVID_19$geoId=='IT',]
daneIT

?convert()
plot(danePL$days, danePL$cases)
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
predykcja.pl <- predict(reg.pl, treningowy.pl, interval = "prediction")
predykcja.pl
