# install.packages("Hmisc", dependencies = TRUE)
# install.packages("psych", dependencies = TRUE)
library(Hmisc)
library(psych)
library(plotly)

#Chargement du dataset
vins <- read.csv("./winequality-white.csv", sep = ';')
#Recuperation des colonnes numeriques
vins <- vins[ , unlist(lapply(vins, is.numeric))]  

nb <- data.frame(table(vins[, "quality"]))
plot_ly(data=nb, x=~Var1, y=~Freq, type="bar")


# pairs.panels(vins, col="red")

str(vins)
vins.lm <- lm(formula=quality~.,data=vins)
summary(vins.lm)

life.dt <- data.frame(summary(vins.lm)[["coefficients"]])
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$Pr...t..), decreasing = FALSE)])
plot_ly(life.dt, x=~names, y=~Pr...t.., type="bar")
extractAIC(vins.lm)

#On supprime les non significatifs
vins.lm <- update(vins.lm,.~.-citric.acid -chlorides -total.sulfur.dioxide)

#On recommence 
summary(vins.lm)
life.dt <- data.frame(summary(vins.lm)[["coefficients"]])
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$Pr...t..), decreasing = FALSE)])
plot_ly(life.dt, x=~names, y=~Pr...t.., type="bar")
extractAIC(vins.lm)

vins.lm <- update(vins.lm,.~.-fixed.acidity)

summary(vins.lm)
# Moins bien donc on garde fixed.acidity
vins.lm <- update(vins.lm,.~.+fixed.acidity)

plot(vins.lm)

# On supprime les valeurs extreme
vins <- vins[!rownames(vins) %in% c("2782", "4746"),]
vins.lm <- update(vins.lm,.~.-citric.acid -chlorides -total.sulfur.dioxide)

life.dt <- data.frame(summary(vins.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
paste("Variables gardées :")
life.dt$names

summary(vins.lm)
plot(vins.lm)

Res <- residuals(vins.lm)
hist(Res, freq=FALSE, nclass=10, col="yellow",main="histogramme des résidus")
x=seq(-3,3,by=0.1)
lines(x, dnorm(x,0,1), type="l", col="red", lwd=2.5)

expectedResult <- vins[rownames(vins) == "1418", "quality"]
expectedResult
individu <- vins[rownames(vins) == "1418", as.character(life.dt$names)]
individu
predict(vins.lm, individu, interval="prediction", level=0.99)

yn=rnorm(50,0,1)
qqplot(Res,yn,main="droite de Henri",col="blue")
z=seq(-2,2,by=0.1)
zz=z
lines(z,zz,type="l",col="red")
#
# ind?pendance des r?sidus
#
acf(Res,ci=0.99)
#
# Test de normalit? de Shapiro
#
shapiro.test(Res)


/*========================================================================================================*/

#Chargement du dataset
vins <- read.csv("./winequality-white.csv", sep = ';')
#Recuperation des colonnes numeriques
vins <- vins[ , unlist(lapply(vins, is.numeric))]  

nb <- data.frame(table(vins[, "quality"]))
plot_ly(data=nb, x=~Var1, y=~Freq, type="bar")

vins$volatile.acidity <- log10(vins$volatile.acidity)
vins$residual.sugar <- log10(vins$residual.sugar)
vins$chlorides <- log10(vins$chlorides)
vins$free.sulfur.dioxide <- log10(vins$free.sulfur.dioxide)
vins$density <- log10(vins$density)

# vins$citric.acid <- log10(vins$citric.acid + min(vins$citric.acid) + 0.000000001)

# pairs.panels(vins, col="red")

str(vins)
vins.lm <- lm(formula=quality~.,data=vins)
summary(vins.lm)

life.dt <- data.frame(summary(vins.lm)[["coefficients"]])
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$Pr...t..), decreasing = FALSE)])
plot_ly(life.dt, x=~names, y=~Pr...t.., type="bar")
extractAIC(vins.lm)

#On supprime les non significatifs
vins.lm <- update(vins.lm,.~.-fixed.acidity -citric.acid)

#On recommence 
summary(vins.lm)
life.dt <- data.frame(summary(vins.lm)[["coefficients"]])
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$Pr...t..), decreasing = FALSE)])
plot_ly(life.dt, x=~names, y=~Pr...t.., type="bar")
extractAIC(vins.lm)

vins.lm <- update(vins.lm,.~.-chlorides)

summary(vins.lm)
# Moins bien donc on garde chlorides
vins.lm <- update(vins.lm,.~.+chlorides)

plot(vins.lm)

# On supprime les valeurs extreme
vins <- vins[!rownames(vins) %in% c("2782", "4746", "1418"),]
vins.lm <- update(vins.lm,.~. -fixed.acidity -citric.acid)
summary(vins.lm)
plot(vins.lm)

vins <- vins[!rownames(vins) %in% c("1654"),]
vins.lm <- update(vins.lm,.~. -fixed.acidity -citric.acid)
summary(vins.lm)
plot(vins.lm)

life.dt <- data.frame(summary(vins.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
paste("Variables gardées :")
life.dt$names



hist(residuals(vins.lm), freq=FALSE, nclass=10, col="yellow",main="histogramme des résidus")
x=seq(-3,3,by=0.1)
lines(x, dnorm(x,0,1), type="l", col="red", lwd=2.5)

expectedResult <- vins[rownames(vins) == "252", "quality"]
expectedResult
individu <- vins[rownames(vins) == "252", as.character(life.dt$names)]
individu
predict(vins.lm, individu, interval="prediction", level=0.99)


Res <- residuals(vins.lm)
hist(Res, freq=FALSE, nclass=10, col="yellow",main="histogramme des résidus")
x=seq(-3,3,by=0.1)
lines(x, dnorm(x,0,1), type="l", col="red", lwd=2.5)

expectedResult <- vins[rownames(vins) == "1418", "quality"]
expectedResult
individu <- vins[rownames(vins) == "1418", as.character(life.dt$names)]
individu
predict(vins.lm, individu, interval="prediction", level=0.99)

yn = rnorm(50,0,1)
qqplot(Res, yn,main="droite de Henri", col="blue")
z=seq(-2,2,by=0.1)
zz=z
lines(z,zz,type="l",col="red")
#
# ind?pendance des r?sidus
#
acf(Res,ci=0.99)
#
# Test de normalit? de Shapiro
#
shapiro.test(Res)






