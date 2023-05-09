library("lmtest")
library("GGally")
library("car")

data = swiss
help(swiss)

#Первая модель
plot(data$Catholic, data$Fertility)
plot(data$Fertility, data$Catholic)

#Вторая модель
plot(data$Catholic, data$Examination)
plot(data$Examination, data$Catholic)

#Дисперсия и СКО
var(data$Catholic)
sd(data$Catholic)
var(data$Fertility)
sd(data$Fertility)
var(data$Examination)
sd(data$Examination)

#Среднее значение переменных
mean(data$Catholic)
mean(data$Fertility)
mean(data$Examination)

#Линейная регрессия

#Модель плохая, т.к. R^2 < 30%
#Взаимосвязь между объясняемой и объясняющей переменной плохая, практически отсутствует
model1 = lm(Catholic ~ Fertility, data)
model1
summary(model1)

model2 = lm(Fertility ~ Catholic, data)
model2
summary(model2)

#Модель хорошая, т.к. R^2 > 30%
#Взаимосвязь между объясняемой и объясняющей переменной присутствует
model3 = lm(Catholic ~ Examination, data)
model3
summary(model3)

model4 = lm(Examination ~ Catholic, data)
model4
summary(model4)
