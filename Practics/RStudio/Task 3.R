install.packages("devtools")
devtools::install_github("bdemeshev/rlms")

library("lmtest")
library("dplyr")
library("GGally")
library(car)
library(sandwich)
library("devtools")

data = read.csv("C:\\Users\\ivcbibl13\\Documents\\R\\r15i_os26c.csv")
glimpse(data)
data2 = select(data, kj13.2, kh5, k_marst, k_diplom,  k_age, status, kj6.2)
data2 = na.omit(data2)
data2
glimpse(data2)

#Зарплата c элементами нормализации
sal1 = as.character(data2$kj13.2)
sal2 = lapply(sal1, as.integer)
sal = as.numeric(unlist(sal2))
mean(sal)
data2["salary"] = (sal - mean(sal)) / sqrt(var(sal))
data2["salary"]

#Пол
data2["sex"] = data2$kh5
data2["sex"] = lapply(data2["sex"], as.character)
data2$sex[which(data2$sex=='2')] <- 0
data2$sex[which(data2$sex=='1')] <- 1
data2$sex = as.numeric(data2$sex)
data2["sex"]

#Семейное положение + дамми-переменные, которые связаны с данным параметром(k_marst)
data2["wed"]= data2$k_marst
data2["wed"] = lapply(data2["wed"], as.character)
data2$wed1 = 0
data2$wed1[which(data2$wed=='2')] <- 1
data2$wed1[which(data2$wed=='6')] <- 1
data2$wed1 = as.numeric(data2$wed1)
data2["wed1"]

data2["wed2"] = lapply(data2["wed"], as.character)
data2$wed2 = 0
data2$wed2[which(data2$wed=='4')] <-1
data2$wed2[which(data2$wed=='5')] <-1
data2$wed2 = as.numeric(data2$wed2)
data2["wed2"]

data2["wed3"] = data2$k_marst
data2$wed3 = 0
data2$wed3[which(data2$wed=='1')] <- 1
data2$wed3 = as.numeric(data2$wed3)
data2["wed3"]

#Образование
data2["higher_educ1"] = data2$k_diplom
data2["higher_educ1"] = lapply(data2["higher_educ1"], as.character)
data2["higher_educ2"] = 0
data2$higher_educ2[which(data2$higher_educ1=='6')] <- 1
data2$higher_educ2 = as.numeric(data2$higher_educ2)
data2["higher_educ2"]


#Возраст с элементами нормализации
age1 = as.character(data2$k_age)
age2 = lapply(age1, as.integer)
age3 = as.numeric(unlist(age2))
data2["age"]= (age3 - mean(age3)) / sqrt(var(age3))
data2["age"]


#Населенный пункт + дамми-переменная, которая связана с данным параметром(status)
data2["citystatus1"]=data2$status
data2["citystatus1"] = lapply(data2["citystatus1"], as.character)
data2["citystatus2"] = 0
data2$citystatus2[which(data2$citystatus1=='1')] <- 1
data2$citystatus2[which(data2$citystatus1=='2')] <- 1
data2$citystatus2 = as.numeric(data2$citystatus2)
data2["citystatus2"]

#Длительность рабочей недели с элементами нормализации
dur1 = as.character(data2$kj6.2)
dur2 = lapply(dur1, as.integer)
dur3 = as.numeric(unlist(dur2))
mean(dur3)
data2["dur"] = (dur3 - mean(dur3)) / sqrt(var(dur3))

data3 = select(data2, salary, age, sex, citystatus2, higher_educ2, wed1, wed2, wed3, kj6.2)
data3

#График парных зависимостей
ggpairs(data3)

#Модель на все параметры
model1 = lm(salary ~ age + sex + citystatus2 + higher_educ2 + wed1 + wed2 + wed3 + kj6.2, data = data3)
summary(model1)
vif(model1)
#R^2 = 0.0216
#Сильная зависимость от параметров age, kj6.2 и средняя зависимость от параметров sex, higher_educ2 

#Попробуем исключить из модели незначимые параметры

model2 = lm(salary ~ age + sex + citystatus2 + higher_educ2 + wed1 + wed2 + kj6.2, data = data3)
summary(model2)
vif(model2)
#R^2 = 0.02168 (меняется менее, чем на 5%) => можно исключить wed3

model3 = lm(salary ~ age + sex + citystatus2 + higher_educ2 + wed1 + kj6.2, data = data3)
summary(model3)
vif(model3)
#R^2 = 0.02183 (меняется менее, чем на 5%) => можно исключить wed2

model4 = lm(salary ~ age + sex + higher_educ2 + wed1 + kj6.2, data = data3)
summary(model4)
vif(model4)
#R^2 = 0.0218 (меняется менее, чем на 5%) => можно исключить citystatus2

model5 = lm(salary ~ age + sex + higher_educ2 + kj6.2, data = data3)
summary(model5)
vif(model5)
#R^2 = 0.02188 (меняется менее, чем на 5%) => можно исключить wed1
# Модель с наивысшим R^2 со значимыми регрессорами + отсутствие сильной зависимости регрессоров

#Полученная наилучшая модель 5

#Логарифмы для факторных переменных

model6 = lm(salary ~ log(age + 5) + sex + higher_educ2 + log(kj6.2 + 5), data = data3)
summary(model6)
vif(model6)
#R^2 не имеет значительных изменений

#Введение степеней
model7 = lm(salary ~ I((age + 5)^0.1) + sex + higher_educ2 + I((kj6.2 + 5)^0.1), data = data3)
summary(model7)
vif(model7)
#R^2 = 0.02197

model8 = lm(salary ~ I((age + 5)^0.2) + sex + higher_educ2 + I((kj6.2 + 5)^0.2), data = data3)
summary(model8)
vif(model8)
#R^2 = 0.02197

model9 = lm(salary ~ I((age + 5)^0.3) + sex + higher_educ2 + I((kj6.2 + 5)^0.3), data = data3)
summary(model9)
vif(model9)
#R^2 = 0.02196

model10 = lm(salary ~ I((age + 5)^0.4) + sex + higher_educ2 + I((kj6.2 + 5)^0.4), data = data3)
summary(model10)
vif(model10) 
#R^2 = 0.02194

model11 = lm(salary ~ I((age + 5)^0.5) + sex + higher_educ2 + I((kj6.2 + 5)^0.5), data = data3)
summary(model11)
vif(model11)
#R^2 = 0.02193

model12 = lm(salary ~ I((age + 5)^0.6) + sex + higher_educ2 + I((kj6.2 + 5)^0.6), data = data3)
summary(model12)
vif(model12)
#R^2 = 0.02192

model13 = lm(salary ~ I((age + 5)^0.7) + sex + higher_educ2 + I((kj6.2 + 5)^0.7), data = data3)
summary(model13)
vif(model13)
#R^2 = 0.02191

model14 = lm(salary ~ I((age + 5)^0.8) + sex + higher_educ2 + I((kj6.2 + 5)^0.8), data = data3)
summary(model14)
vif(model14)
#R^2 = 0.0219

model15 = lm(salary ~ I((age + 5)^0.9) + sex + higher_educ2 + I((kj6.2 + 5)^0.9), data = data3)
summary(model15)
vif(model15)
# R^2 = 0.02189

model16 = lm(salary ~ I((age + 5)^0.7) + sex + higher_educ2 + I((kj6.2 + 5)^0.7), data = data3)
summary(model16)
vif(model16)
#R^2 = 0.02191

model17 = lm(salary ~ I((age + 5)^0.8) + sex + higher_educ2 + I((kj6.2 + 5)^0.8), data = data3)
summary(model17)
vif(model17)
#R^2 = 0.0219

model18 = lm(salary ~ I((age + 5)^0.9) + sex + higher_educ2 + I((kj6.2 + 5)^0.9), data = data3)
summary(model18)
vif(model18)
#R^2 = 0.02189

model19 = lm(salary ~ I((age + 5)^1.0) + sex + higher_educ2 + I((kj6.2 + 5)^1.0), data = data3)
summary(model19)
vif(model19)
#R^2 = 0.02188

model20 = lm(salary ~ I((age + 5)^1.1) + sex + higher_educ2 + I((kj6.2 + 5)^1.1), data = data3)
summary(model20)
vif(model20)
#R^2 = 0.02187

model21 = lm(salary ~ I((age + 5)^1.2) + sex + higher_educ2 + I((kj6.2 + 5)^1.2), data = data3)
summary(model21)
vif(model21)
#R^2 = 0.02185

model22 = lm(salary ~ I((age + 5)^1.3) + sex + higher_educ2 + I((kj6.2 + 5)^1.3), data = data3)
summary(model22)
vif(model22)
#R^2 = 0.02184

model23 = lm(salary ~ I((age + 5)^1.4) + sex + higher_educ2 + I((kj6.2 + 5)^1.4), data = data3)
summary(model23)
vif(model23)
#R^2 = 0.02183

model24 = lm(salary ~ I((age + 5)^1.5) + sex + higher_educ2 + I((kj6.2 + 5)^1.5), data = data3)
summary(model24)
vif(model24)
#R^2 = 0.02181

model25 = lm(salary ~ I((age + 5)^1.6) + sex + higher_educ2 + I((kj6.2 + 5)^1.6), data = data3)
summary(model25)
vif(model25)
#R^2 = 0.0218

model26 = lm(salary ~ I((age + 5)^1.7) + sex + higher_educ2 + I((kj6.2 + 5)^1.7), data = data3)
summary(model26)
vif(model26)
#R^2 = 0.02179

model27 = lm(salary ~ I((age + 5)^1.8) + sex + higher_educ2 + I((kj6.2 + 5)^1.8), data = data3)
summary(model27)
vif(model27)
#R^2 = 0.02177

model28 = lm(salary ~ I((age + 5)^1.9) + sex + higher_educ2 + I((kj6.2 + 5)^1.9), data = data3)
summary(model28)
vif(model28)
#R^2 = 0.02176

model29 = lm(salary ~ I((age + 5)^2.0) + sex + higher_educ2 + I((kj6.2 + 5)^2.0), data = data3)
summary(model29)
vif(model29)
#R^2 = 0.02174

#У всех моделей R^2 не сильно отличается, лучшими можно назвать модели 6, 7 и 8
# У данных моделей наивысший среди остальный R^2, присутствуют значимые регрессоры, а также, отсутствует сильная зависимость регрессоров

#Модель с произведением регрессоров(без степеней)
model30 = lm(salary ~ age + sex + higher_educ2 + kj6.2 + I(age*kj6.2), data = data3)
summary(model30)
vif(model30)
#R^2 = 0.02168, R^2 (меняется менее, чем на 5%)

#Модель с произведением регрессоров
model31 = lm(salary ~ I((age + 5)^0.1) + sex + higher_educ2 + I((kj6.2 + 5)^0.1) + I(age*kj6.2), data = data3)
summary(model31)
vif(model31)
#R^2 = 0.02168, R^2 (меняется менее, чем на 5%)

#Лучшая модель - model7
#Из нее видно, что зарплата имеет отрицательную зависимость от параметра age, и положительные зависимости от параметров higher_educ2 и Kj6.2
#Иными словами, зарабатывают больше люди моложе, которые больше работают и имеют высшее образование

#Женатые, не из города
data4 = subset(data2, citystatus2==0 & wed1==1)
data4


#Разведённые или не вступавшие в брак, с высшим образованием
data5 = subset(data2, (wed2==1 & higher_educ2==1) | (wed3==1 & higher_educ2==1))
data5

#Линейная регрессия для data4
model32 = lm(salary ~ age + sex + higher_educ2 + kj6.2, data = data4)
summary(model32)
vif(model32)
#R^2 имеет значение выше, чем у модели 33, но все равно маленькое, также есть два значимых параметра с максимальным уровнем значимости
#Модель лучше, но по прежнему не является хорошей

#Линейная регрессия для data5
#Регрессоры higher_educ2 является мультиколлинеарным, линейную регрессию построить нельзя, уберём его из модели
model33 = lm(salary ~ age + sex + kj6.2, data = data5)
summary(model33)
vif(model33)
#R^2 имеет маленькое значение => модель плоха

#Введем операторы умножения и логарифма в две предыдущие полученные модели

model34 = lm(salary ~ age + sex + higher_educ2 + I(kj6.2*age), data = data4)
summary(model34)
vif(model34)
#R^2 = 0.005489

model35 = lm(salary ~ log(age) + sex + higher_educ2 + kj6.2, data = data4)
summary(model35)
vif(model35)
#R^2 = 0.01882

model36 = lm(salary ~ age + sex + I(kj6.2*age), data = data5)
summary(model36)
vif(model36)
#R^2 = 0.003222

model37 = lm(salary ~ age + sex + log(kj6.2), data = data5)
summary(model37)
vif(model37)
#R^2 = 0.003765

model38 = lm(salary ~ log(age) + sex + kj6.2, data = data5)
summary(model38)
vif(model38)
#R^2 = -0.01414


# Наилучшей можно считать модель 35, т.к. у неё самый высокий R^2, отсутствует сильная зависимость регрессоров, низкие значения vif, которые указывают на
# отсутствие мультиколлинеарности и присутствуют низкие p-значения
