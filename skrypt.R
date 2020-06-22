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