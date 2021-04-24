#script de la r?gression lin?aire multiple sur des donn?es state.x77
#
library(plotly)
happyness2020 <- read.csv("./Fish.csv")
str(happyness2020)

num_cols <- unlist(lapply(happyness2020, is.numeric))
happyness2020 <- happyness2020[ , num_cols]  
str(happyness2020)
happyness.lm=lm(formula=Weight~.,data=happyness2020)

summary(happyness.lm)
life.dt <- data.frame(summary(happyness.lm)[["coefficients"]])
life.dt$names <- names(happyness2020)
plot_ly(life.dt, x=~names, y=~abs(t.value), type="bar")
extractAIC(happyness.lm)
#
# On soustrait ? pr?sent les variables dont le coefficient n'est pas 
# significativement diff?rent de z?ro.
#
happyness.lm<- update(happyness.lm,.~.-Length2)
summary(happyness.lm)
extractAIC(happyness.lm)
#
# On soustrait la variable 'Population' et on regarde si le mod?le est meilleur
#
happyness.lm<- update(happyness.lm,.~.-Width)
summary(happyness.lm)
extractAIC(happyness.lm)

#
# On reprend le deuxi?me mod?le
#
life.lm<-lm(formula=Weight~Length1+Length3+Height, data=happyness2020)
summary(life.lm)
Res=residuals(life.lm)
hist(Res,freq=FALSE,nclass=10, col="yellow",main="histogramme des r?sidus")
# 
# On ajute la courbe en cloche ? l'histogramme pour voir si les r?sidus
# peuvent suivre une loi normle
#
x=seq(-3,3,by=0.1)
y=dnorm(x,0,1)
lines(x,y,type="l",col="red",lwd=2.5)
#
# Pr?diction
#
predict(life.lm,data.frame(Length1=23.2,Length3=30,Height=11.52),interval="prediction",level=0.99)
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




