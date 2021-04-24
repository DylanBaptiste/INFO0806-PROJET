#script de la r?gression lin?aire multiple sur des donn?es state.x77
#
library(plotly)
happyness2020 <- read.csv("./winequality-white.csv", sep = ';')
str(happyness2020)

num_cols <- unlist(lapply(happyness2020, is.numeric))
happyness2020 <- happyness2020[ , num_cols]  
str(happyness2020)
happyness.lm=lm(formula=quality~.,data=happyness2020)

summary(happyness.lm)
life.dt <- data.frame(summary(happyness.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$t.value), decreasing = FALSE)])
plot_ly(life.dt, x=~names, y=~abs(t.value), type="bar")
extractAIC(happyness.lm)
#
# On soustrait ? pr?sent les variables dont le coefficient n'est pas 
# significativement diff?rent de z?ro.
#
happyness.lm <- update(happyness.lm,.~.-citric.acid)
extractAIC(happyness.lm)
summary(happyness.lm)
life.dt <- data.frame(summary(happyness.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$t.value), decreasing = FALSE)])
plot_ly(life.dt, x=~names, y=~abs(t.value), type="bar")
#
# On soustrait la variable 'Population' et on regarde si le mod?le est meilleur
#
happyness.lm <- update(happyness.lm,.~.-chlorides)
extractAIC(happyness.lm)
summary(happyness.lm)
life.dt <- data.frame(summary(happyness.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$t.value), decreasing = FALSE)])
plot_ly(life.dt, x=~names, y=~abs(t.value), type="bar")

happyness.lm <- update(happyness.lm,.~.-total.sulfur.dioxide)
extractAIC(happyness.lm)
summary(happyness.lm)
life.dt <- data.frame(summary(happyness.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
life.dt$names <- factor(life.dt$names, levels = unique(life.dt$names)[order(abs(life.dt$t.value), decreasing = FALSE)])
plot_ly(life.dt, x=~names, y=~abs(t.value), type="bar")

happyness.lm <- update(happyness.lm,.~.-fixed.acidity)
extractAIC(happyness.lm)

happyness.lm <- update(happyness.lm,.~.+fixed.acidity)
extractAIC(happyness.lm)
summary(happyness.lm)

life.dt <- data.frame(summary(happyness.lm)[["coefficients"]])[-1, ]
life.dt$names <- rownames(life.dt)
paste("Variables gardées :")
life.dt$names

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
nb <- data.frame(table(happyness2020[, "quality"]))
plot_ly(data=nb, x=~Var1, y=~Freq, type="bar")

happyness2020[2, life.dt$names]
expectedResult <- happyness2020[2, "quality"]
predict(happyness.lm,data.frame(happyness2020[1, life.dt$names]),interval="prediction",level=0.99)

happyness2020[252, life.dt$names]
expectedResult <- happyness2020[252, "quality"]
predict(happyness.lm,data.frame(happyness2020[1, life.dt$names]),interval="prediction",level=0.99)

happyness2020[775, life.dt$names]
expectedResult <- happyness2020[775, "quality"]
predict(happyness.lm,data.frame(happyness2020[1, life.dt$names]),interval="prediction",level=0.99)

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




