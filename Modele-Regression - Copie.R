#script de la r?gression lin?aire multiple sur des donn?es state.x77
#

install.packages("Hmisc", dependencies = TRUE)
install.packages("psych", dependencies = TRUE)
library(Hmisc)
library(psych)
library(plotly)

#Chargement du dataset
happyness2020 <- read.csv("./winequality-white.csv", sep = ';')
#Recuperation des colonnes numeriques
happyness2020 <- happyness2020[ , unlist(lapply(happyness2020, is.numeric))]  


### Check for non-linearity (visually) and transform vars
# pairs.panels(happyness2020, col="red")

# Optionel
happyness2020$volatile.acidity <- log10(happyness2020$volatile.acidity)
happyness2020$residual.sugar <- log10(happyness2020$residual.sugar)
happyness2020$chlorides <- log10(happyness2020$chlorides)
happyness2020$free.sulfur.dioxide <- log10(happyness2020$free.sulfur.dioxide)
happyness2020$density <- log10(happyness2020$density)

# Que faire ?
# https://blogs.sas.com/content/iml/2011/04/27/log-transformations-how-to-handle-negative-data-values.html
# https://blogs.sas.com/content/iml/2018/08/27/on-the-assumptions-and-misconceptions-of-linear-regression.html
#happyness2020$citric.acid <- log10(happyness2020$citric.acid + min(happyness2020$citric.acid) + 0.000000001)

#pairs.panels(happyness2020, col="red")
1+1
set.seed(666)
datasize <- length(happyness2020$quality)
train.size <- 0.99 # 1000 / datasize # prise aleatoire de 1000 individus
train.index <- sample.int(length(happyness2020$quality), round(length(happyness2020$quality) * train.size))
happyness2020.train <- happyness2020[train.index,]
happyness2020.test  <- happyness2020[-train.index,]
nrow(happyness2020.test)
nrow(happyness2020.train)
str(happyness2020)




happyness.lm <- lm(formula=quality~.,data=happyness2020.train)

summary(happyness.lm)
life.dt <- data.frame(summary(happyness.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$Pr...t..), decreasing = FALSE)])
plot_ly(life.dt, x=~names, y=~Pr...t.., type="bar")
extractAIC(happyness.lm)
#
# On soustrait ? pr?sent les variables dont le coefficient n'est pas 
# significativement diff?rent de z?ro.
#
happyness.lm <- update(happyness.lm,.~.-citric.acid-chlorides-total.sulfur.dioxide)
happyness.lm <- update(happyness.lm,.~.-citric.acid-fixed.acidity)
happyness.lm <- update(happyness.lm,.~.+chlorides)

extractAIC(happyness.lm)
summary(happyness.lm)
life.dt <- data.frame(summary(happyness.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$Pr...t..), decreasing = FALSE)])

plot_ly(life.dt, x=~names, y=~Pr...t.., type="bar")
#
# On soustrait la variable 'Population' et on regarde si le mod?le est meilleur
#
happyness.lm <- update(happyness.lm,.~.-fixed.acidity)
extractAIC(happyness.lm)
summary(happyness.lm)
life.dt <- data.frame(summary(happyness.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$Pr...t..), decreasing = FALSE)])
plot_ly(life.dt, x=~names, y=~Pr...t.., type="bar")

happyness.lm <- update(happyness.lm,.~.+fixed.acidity)

life.dt <- data.frame(summary(happyness.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
paste("Variables gardées :")
life.dt$names

plot(happyness.lm)

happyness2020.train[which(rownames(happyness2020.train) %in% c("2782", "4746")),]
happyness2020.train.minusextrem <- happyness2020.train[!rownames(happyness2020.train) %in% c("2782", "4746"),]
nrow(happyness2020.train)
nrow(happyness2020.train.minusextrem)


happyness.lm <- lm(formula=quality~.,data=happyness2020.train.minusextrem)

#◙pas log 10
happyness.lm <- update(happyness.lm,.~.-citric.acid-chlorides-total.sulfur.dioxide)

# log10
happyness.lm <- update(happyness.lm,.~.-citric.acid-fixed.acidity)


extractAIC(happyness.lm)
summary(happyness.lm)
life.dt <- data.frame(summary(happyness.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$Pr...t..), decreasing = FALSE)])
plot_ly(life.dt, x=~names, y=~Pr...t.., type="bar")

plot(happyness.lm)



Res=residuals(happyness.lm)
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
nb <- data.frame(table(happyness2020.train[, "quality"]))
plot_ly(data=nb, x=~Var1, y=~Freq, type="bar")

summary(happyness.lm)

expectedResult <- happyness2020.train[2, "quality"]
individu <- happyness2020.train[2, as.character(life.dt$names)]
predict(happyness.lm, individu, interval="prediction", level=0.99)

expectedResult <- happyness2020.train[252, "quality"]
individu <- happyness2020.train[252, as.character(life.dt$names)]
predict(happyness.lm, individu, interval="prediction", level=0.99)

expectedResult <- happyness2020.train[rownames(happyness2020.train) == "1418", "quality"]
individu <- happyness2020.train[2, as.character(life.dt$names)]
predict(happyness.lm, individu, interval="prediction", level=0.99)

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

