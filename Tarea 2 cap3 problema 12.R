setwd("C:/Users/LISSETTE/Desktop/Maestria met.Invest/Diseño Experimental")
df=read.csv("FRIJOLES.csv",sep = ";")
df

df$Tratamiento=factor(df$Tratamiento)
str(df)

boxplot(M~Tratamiento,data=df)

modelo=aov(M~Tratamiento,data=df)
summary(modelo)

tk=TukeyHSD(modelo)
tk

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

library("car")
leveneTest(M~Tratamiento,data=df)


plot(modelo$residuals)
abline(h=0)

plot(modelo$fitted.values, modelo$residuals)
abline(h=0)
