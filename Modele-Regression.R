#script de la régression linéaire multiple sur des données state.x77
#
data(state)
stateUSA<-data.frame(state.x77,row.names=state.abb)
attach(stateUSA)
life.lm=lm(formula=Life.Exp~.,data=stateUSA)
summary(life.lm)
extractAIC(life.lm)
#
# On soustrait à présent les variables dont le coefficient n'est pas 
# significativement différent de zéro.
#
life.lm<- update(life.lm,.~.-Area-Illiteracy-Income)
summary(life.lm)
extractAIC(life.lm)
#
# On soustrait la variable 'Population' et on regarde si le modèle est meilleur
#
life.lm<-update(life.lm,.~.-Population)
summary(life.lm)
extractAIC(life.lm)
#
# On reprend le deuxième modèle
#
life.lm<-lm(formula=Life.Exp~Murder+HS.Grad+Frost+Population,data=stateUSA)
summary(life.lm)
Res=residuals(life.lm)
hist(Res,freq=FALSE,nclass=10, col="yellow",main="histogramme des résidus")
# 
# On ajute la courbe en cloche à l'histogramme pour voir si les résidus
# peuvent suivre une loi normle
#
x=seq(-3,3,by=0.1)
y=dnorm(x,0,1)
lines(x,y,type="l",col="red",lwd=2.5)
#
# Prédiction
#
predict(life.lm,data.frame(Murder=8,HS.Grad=75,Frost=80,Population=4250),interval="prediction",level=0.99)
#
# Etude des résidus
#
yn=rnorm(50,0,1)
qqplot(Res,yn,main="droite de Henri",col="blue")
z=seq(-2,2,by=0.1)
zz=z
lines(z,zz,type="l",col="red")
#
# indépendance des résidus
#
acf(Res,ci=0.99)
#
# Test de normalité de Shapiro
#
shapiro.test(Res)

