setwd("C:/Users/LISSETTE/Desktop/Maestria met.Invest/Diseño Experimental")
df=read.csv("TABLEROSELECT.csv",sep = ";")
df

df$Centigrado=factor(df$Centigrado)
str(df)

boxplot(I~Centigrado,data=df)

modelo=aov(I~Centigrado,data=df)
summary(modelo)

tk=TukeyHSD(modelo)
tk

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

library("car")
leveneTest(I~Centigrado,data=df)


plot(modelo$residuals)
abline(h=0)

plot(modelo$fitted.values, modelo$residuals)
abline(h=0)
