setwd("C:/Users/LISSETTE/Desktop/Maestria met.Invest/Diseño Experimental")
df=read.csv("PRODUCTOS.csv",sep = ";")
df

df$TRATAMIENTO=factor(df$TRATAMIENTO)
str(df)

boxplot(D~TRATAMIENTO,data=df)

modelo=aov(D~TRATAMIENTO,data=df)
summary(modelo)

tk=TukeyHSD(modelo)
tk

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

library("car")
leveneTest(D~TRATAMIENTO,data=df)


plot(modelo$residuals)
abline(h=0)

plot(modelo$fitted.values, modelo$residuals)
abline(h=0)
