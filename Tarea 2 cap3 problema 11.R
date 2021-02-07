setwd("C:/Users/LISSETTE/Desktop/Maestria met.Invest/Diseño Experimental")
df=read.csv("MATAMOSCA.csv",sep = ";")
df

df$Marca=factor(df$Marca)
str(df)

boxplot(y~Marca,data=df)

modelo=aov(y~Marca,data=df)
summary(modelo)

tk=TukeyHSD(modelo)
tk

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

library("car")
leveneTest(y~Marca,data=df)


plot(modelo$residuals)
abline(h=0)

plot(modelo$fitted.values, modelo$residuals)
abline(h=0)
