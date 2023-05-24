library("lmtest")
library("GGally")
library("car")

data = mtcars
summary(data)
ggpairs(data)

#Пункт 1: Проверка на линейную зависимость 
model1 = lm(wt ~ qsec + hp + drat, data)
model1
summary(model1)
#R^2 = 0.69 => следует исключить wt

model2 = lm(qsec ~ wt + hp + drat, data)
model2
summary(model2)
#R^2 = 0.62 => следует исключить qsec

model3 = lm(hp ~ wt + qsec + drat, data)
model3
summary(model3)
#R^2 = 0.78 => следует исключить hp

model4 = lm(drat ~ wt + qsec + hp, data)
model4
summary(model4)
#R^2 = 0.46 => следует исключить drat

#Пункт 2:

model5 = lm(mpg ~ wt + hp, data)
model5
summary(model5)
#Из модели можем исключить drat и qsec(если заместо qsec убрать hp, то у него параметры Std. Error и P-статистика будут хуже, чем у hp)
#R^2 снизится менее, чем на 5%
#R^2 = 0.81 => модель хороша
#Так как количество звездочек в последнем столбце равно 3, то вероятность того, что посчитанные в моделях коэффициенты отстою от реального значения практически равна 0
#Следовательно, между объясняемыми и объясняющими переменными есть сильная причинно-следственная связь 

#Пункт 3:
model6 = lm(mpg ~ wt + qsec + hp + drat + log(wt) + log(qsec) + log(hp) + log(drat), data)
model6
summary(model6)
#R^2 = 0.8787
vif(model6)

model7 = lm(mpg ~ wt + qsec + hp + drat + log(wt) + log(hp) + log(drat), data)
model7
summary(model7)
# R^2 = 0.8836
vif(model7)

model8 = lm(mpg ~ wt + qsec + hp + log(wt) + log(hp) + log(drat), data)
model8
summary(model8)
# R^2 = 0.8769
vif(model8)

model9 = lm(mpg ~ wt + qsec + hp + log(wt) + log(drat), data)
model9
summary(model9)
#R^2 = 0.8671
vif(model9)

model10 = lm(mpg ~ wt + qsec + hp + log(drat), data)
model10
summary(model10)
# R^2 = 0.8223
vif(model10)

model11 = lm(mpg ~ wt + qsec + log(drat), data)
model11
summary(model11)
# R^2 = 0.8188
vif(model11)

# Модель № 11 - наилучшая, т.к. имеет высокий R^2 и макс. кол-во звездочек у регрессеров из всех моделей

#Пункт 4:

#Произведение пар регрессоров
mtcars_sq = mtcars
mtcars_sq$wt_qsec = mtcars$wt * mtcars$qsec
mtcars_sq$wt_hp = mtcars$wt * mtcars$hp
mtcars_sq$wt_drat = mtcars$wt * mtcars$drat
mtcars_sq$qsec_hp = mtcars$qsec * mtcars$hp
mtcars_sq$qsec_drat = mtcars$qsec * mtcars$drat
mtcars_sq$hp_drat = mtcars$hp * mtcars$drat

model13 = lm(mpg ~ wt + qsec + hp + drat + I(wt^2) + I(qsec^2) + I(hp^2) + I(drat^2) + wt_qsec + wt_hp + wt_drat + qsec_hp + qsec_drat + hp_drat, data = mtcars_sq)
model13
summary(model13)
vif(model13)
# R^2 = 0.8347

model14 = lm(mpg ~ wt + qsec + hp + drat + I(wt^2) + I(qsec^2) + I(hp^2) + I(drat^2) + wt_hp + wt_drat + qsec_hp + qsec_drat + hp_drat, data = mtcars_sq)
model14
summary(model14)
vif(model14)
#R^2 = 0.8428

model15 = lm(mpg ~ wt + qsec + drat + I(wt^2) + I(qsec^2) + I(hp^2) + I(drat^2) + wt_hp + wt_drat + qsec_hp + qsec_drat + hp_drat, data = mtcars_sq)
model15
summary(model15)
vif(model15)
#R^2 = 0.8463

model16 = lm(mpg ~ wt + qsec + I(wt^2) + I(qsec^2) + I(hp^2) + I(drat^2) + wt_hp + wt_drat + qsec_hp + qsec_drat + hp_drat, data = mtcars_sq)
model16
summary(model16)
vif(model16)
#R^2 = 0.8539

model17 = lm(mpg ~ qsec + I(wt^2) + I(qsec^2) + I(hp^2) + I(drat^2) + wt_hp + wt_drat + qsec_hp + qsec_drat + hp_drat, data = mtcars_sq)
model17
summary(model17)
vif(model17)
#R^2 = 0.8582

model18 = lm(mpg ~ I(wt^2) + I(qsec^2) + I(hp^2) + I(drat^2) + wt_hp + wt_drat + qsec_hp + qsec_drat + hp_drat, data = mtcars_sq)
model18
summary(model18)
vif(model18)
#R^2 = 0.8646

model19 = lm(mpg ~ I(wt^2) + I(qsec^2) + I(hp^2) + I(drat^2) + wt_drat + qsec_hp + qsec_drat + hp_drat, data = mtcars_sq)
model19
summary(model19)
vif(model19)
#R^2 = 0.8693

model20 = lm(mpg ~ I(wt^2) + I(qsec^2) + I(hp^2) + I(drat^2) + wt_drat + qsec_hp + hp_drat, data = mtcars_sq)
model20
summary(model20)
vif(model20)
#R^2 = 0.8367

model21 = lm(mpg ~ I(wt^2) + I(qsec^2) + I(hp^2) + I(drat^2) + wt_drat + qsec_hp, data = mtcars_sq)
model21
summary(model21)
vif(model21)
#R^2 = 0.8427

model22 = lm(mpg ~ I(wt^2) + I(qsec^2) + I(drat^2) + wt_drat + qsec_hp, data = mtcars_sq)
model22
summary(model22)
vif(model22)
#R^2 = 0.8389

model23 = lm(mpg ~ I(qsec^2) + I(drat^2) + wt_drat + qsec_hp, data = mtcars_sq)
model23
summary(model23)
vif(model23)

model24 = lm(mpg ~ I(qsec^2) + I(drat^2) + wt_drat, data = mtcars_sq)
model24
summary(model24)
vif(model24)
#R^2 = 0.8359
#Модель 24 - лучшая, т.к. не имеет зависимости между регрессорами, а регрессоры зависят от объясняющей переменной
help(mtcars)
#Пункт 2.1

t_critical = qt(0.975, df = 28)
summary(t_critical) #t = 2.064
# ЧСС = DF(32) - Число строк(4) = 28


#I(qsec^2)(Estimate) = 0.023, I(qsec^2)(Std. Error) = 0.006
#Доверительный интервал: [0.023 - 2.06 * 0.006, 0.023 + 2.064 * 0.006]
#[0.01064, 0.035384]

#I(drat^2)(Estimate) = 0.691, I(drat^2)(Std, Error) = 0.119
#Доверительный интервал: [0.691 - 2.064 * 0.119, 0.691 + 2.064 * 0.119]
#[0.445, 0,936]

#wt_drat(Estimate) = -1.332, wt_drat(Std. Error) = 0.187
#Доверительный интервал: [-1.332 - 2.064 * 0.187, -1.332 + 2.064 * 0.187]
#[-1.717, -0.946]

#Пункт 2.2

#Если невозможно отвергнуть статистическую гипотезу о том, что коэффициент равен 0, то можно сделать вывод, что регрессор практически не связан с объясняемой переменной.
#Иначе, если 0 не входит в доверительный интервал, то гипотезу можно отвергнуть, следовательно, коэффициент будет иметь значение для модели.
#Доверительные интервалы коэффициентов не содержат 0, значит можно сделать вывод об отвержении статистической гипотезы о том, что коэффициенты равны 0.

#Пункт 2.3

model25 = lm(mpg ~ wt + qsec + hp + drat, data = mtcars_sq)

new.data = data.frame(wt = 20, qsec = 15, hp = 10, drat = 5)
predict(model25, new.data, interval = "confidence")
#Точечный прогноз для mpg при wt = 20, qsec = 15, hp = 10, drat = 5 равен -38.87466
#Нижняя граница доверительного интервала = -75.24395, а верхняя граница = -2.505379

# Доверительные интервалы коэффициента не содержит 0, значит можно сделать вывод об отвержении статистической гипотезы о том, что коэффициент равен 0.
# Cледовательно, коэффициент будет иметь значение для модели.
