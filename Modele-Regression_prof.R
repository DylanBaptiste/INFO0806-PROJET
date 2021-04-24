#script de la r?gression lin?aire multiple sur des donn?es state.x77
#
data(state)
stateUSA<-data.frame(state.x77,row.names=state.abb)
attach(stateUSA)
life.lm=lm(formula=Life.Exp~.,data=stateUSA)
summary(life.lm)
extractAIC(life.lm)
#
# On soustrait ? pr?sent les variables dont le coefficient n'est pas 
# significativement diff?rent de z?ro.
#
life.lm<- update(life.lm,.~.-Area-Illiteracy-Income)
summary(life.lm)
extractAIC(life.lm)
#
# On soustrait la variable 'Population' et on regarde si le mod?le est meilleur
#
life.lm<-update(life.lm,.~.-Population)
summary(life.lm)
extractAIC(life.lm)
#
# On reprend le deuxi?me mod?le
#
life.lm<-lm(formula=Life.Exp~Murder+HS.Grad+Frost+Population,data=stateUSA)
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
predict(life.lm,data.frame(Murder=8,HS.Grad=75,Frost=80,Population=4250),interval="prediction",level=0.99)
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





#script de la r?gression lin?aire multiple sur des donn?es state.x77
#
data(state)
stateUSA<-data.frame(state.x77,row.names=state.abb)
attach(stateUSA)
life.lm=lm(formula=Life.Exp~.,data=stateUSA)
summary(life.lm)
extractAIC(life.lm)
#
# On soustrait ? pr?sent les variables dont le coefficient n'est pas 
# significativement diff?rent de z?ro.
#
life.lm<- update(life.lm,.~.-Area-Illiteracy-Income)
summary(life.lm)
extractAIC(life.lm)
#
# On soustrait la variable 'Population' et on regarde si le mod?le est meilleur
#
life.lm<-update(life.lm,.~.-Population)
summary(life.lm)
extractAIC(life.lm)
#
# On reprend le deuxi?me mod?le
#
life.lm<-lm(formula=Life.Exp~Murder+HS.Grad+Frost+Population,data=stateUSA)
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
predict(life.lm,data.frame(Murder=8,HS.Grad=75,Frost=80,Population=4250),interval="prediction",level=0.99)
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


