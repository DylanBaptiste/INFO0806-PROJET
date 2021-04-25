#script de la r?gression lin?aire multiple sur des donn?es state.x77
#

install.packages("Hmisc", dependencies = TRUE)
install.packages("psych", dependencies = TRUE)
library(Hmisc)
library(psych)
library(plotly)

#Chargement du dataset
vins <- read.csv("./winequality-white.csv", sep = ';')
#Recuperation des colonnes numeriques
vins <- vins[ , unlist(lapply(vins, is.numeric))]  


### Check for non-linearity (visually) and transform vars
# pairs.panels(vins, col="red")

# Optionel
vins$volatile.acidity <- log10(vins$volatile.acidity)
vins$residual.sugar <- log10(vins$residual.sugar)
vins$chlorides <- log10(vins$chlorides)
vins$free.sulfur.dioxide <- log10(vins$free.sulfur.dioxide)
vins$density <- log10(vins$density)

# Que faire ?
# https://blogs.sas.com/content/iml/2011/04/27/log-transformations-how-to-handle-negative-data-values.html
# https://blogs.sas.com/content/iml/2018/08/27/on-the-assumptions-and-misconceptions-of-linear-regression.html
#vins$citric.acid <- log10(vins$citric.acid + min(vins$citric.acid) + 0.000000001)

#pairs.panels(vins, col="red")
1+1
set.seed(666)
datasize <- length(vins$quality)
train.size <- 0.99 # 1000 / datasize # prise aleatoire de 1000 individus
train.index <- sample.int(length(vins$quality), round(length(vins$quality) * train.size))
vins.train <- vins[train.index,]
vins.test  <- vins[-train.index,]
nrow(vins.test)
nrow(vins.train)
str(vins)




vins.lm <- lm(formula=quality~.,data=vins.train)

summary(vins.lm)
life.dt <- data.frame(summary(vins.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$Pr...t..), decreasing = FALSE)])
plot_ly(life.dt, x=~names, y=~Pr...t.., type="bar")
extractAIC(vins.lm)
#
# On soustrait ? pr?sent les variables dont le coefficient n'est pas 
# significativement diff?rent de z?ro.
#
vins.lm <- update(vins.lm,.~.-citric.acid-chlorides-total.sulfur.dioxide)
vins.lm <- update(vins.lm,.~.-citric.acid-fixed.acidity)
vins.lm <- update(vins.lm,.~.+chlorides)

extractAIC(vins.lm)
summary(vins.lm)
life.dt <- data.frame(summary(vins.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$Pr...t..), decreasing = FALSE)])

plot_ly(life.dt, x=~names, y=~Pr...t.., type="bar")
#
# On soustrait la variable 'Population' et on regarde si le mod?le est meilleur
#
vins.lm <- update(vins.lm,.~.-fixed.acidity)
extractAIC(vins.lm)
summary(vins.lm)
life.dt <- data.frame(summary(vins.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$Pr...t..), decreasing = FALSE)])
plot_ly(life.dt, x=~names, y=~Pr...t.., type="bar")

vins.lm <- update(vins.lm,.~.+fixed.acidity)

life.dt <- data.frame(summary(vins.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
paste("Variables gardées :")
life.dt$names

plot(vins.lm)

vins.train[which(rownames(vins.train) %in% c("2782", "4746")),]
vins.train.minusextrem <- vins.train[!rownames(vins.train) %in% c("2782", "4746"),]
nrow(vins.train)
nrow(vins.train.minusextrem)


vins.lm <- lm(formula=quality~.,data=vins.train.minusextrem)

#◙pas log 10
vins.lm <- update(vins.lm,.~.-citric.acid-chlorides-total.sulfur.dioxide)

# log10
vins.lm <- update(vins.lm,.~.-citric.acid-fixed.acidity)


extractAIC(vins.lm)
summary(vins.lm)
life.dt <- data.frame(summary(vins.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$Pr...t..), decreasing = FALSE)])
plot_ly(life.dt, x=~names, y=~Pr...t.., type="bar")

plot(vins.lm)



Res=residuals(vins.lm)
hist(Res, freq=FALSE, nclass=10, col="yellow",main="histogramme des r?sidus")
# 
# On ajute la courbe en cloche ? l'histogramme pour voir si les r?sidus
# peuvent suivre une loi normle
#
x=seq(-3,3,by=0.1)
y=dnorm(x,0,1)
lines(x, y, type="l",col="red",lwd=2.5)
#
# Pr?diction
#
nb <- data.frame(table(vins.train[, "quality"]))
plot_ly(data=nb, x=~Var1, y=~Freq, type="bar")

summary(vins.lm)

expectedResult <- vins.train[2, "quality"]
individu <- vins.train[2, as.character(life.dt$names)]
predict(vins.lm, individu, interval="prediction", level=0.99)

expectedResult <- vins.train[252, "quality"]
individu <- vins.train[252, as.character(life.dt$names)]
predict(vins.lm, individu, interval="prediction", level=0.99)

expectedResult <- vins.train[rownames(vins.train) == "1418", "quality"]
individu <- vins.train[2, as.character(life.dt$names)]
predict(vins.lm, individu, interval="prediction", level=0.99)

# données biaisé car nb de donné != pour chaque type de qualité


#
# Etude des r?sidus
#
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


# conclusion
# - notation discrete et non on notation continue (il aurait fallu que l'etudeai permise des notation au demi, quart... de points)

