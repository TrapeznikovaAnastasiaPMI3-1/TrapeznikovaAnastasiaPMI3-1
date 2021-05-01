# Данные - Auto {ISLR}	
# Непрерывный Y - mpg
# Объясняющие переменные - остальные, кроме name
# Метод подгонки моделей - дерево с обрезкой ветвей

# набор данных Auto
library('ISLR')
library('GGally')            # матричный график разброса ggpairs()
library('tree')              # деревья tree()
attach(Auto)
data(Auto)


# Деревья решений
# Загрузим таблицу с данными по параметрам автомобилей и добавим к ней переменную high.mpg – “высокий расход топлива” со значениями:
# 1 если mpg >= 29
# 0 если mpg < 29

head(Auto)

# новая переменная
high.mpg <- ifelse(Auto$mpg >= 29, 1, 0)
high.mpg <- factor(high.mpg, labels = c('yes', 'no'))
Auto$high.mpg <- high.mpg 
# матричные графики разброса переменных
p <- ggpairs(Auto[, c(10, 1:5)], aes(color = high.mpg))
suppressMessages(print(p))

p <- ggpairs(Auto[, c(10, 6:8)], aes(color = high.mpg))
suppressMessages(print(p))



# модель бинарного  дерева без переменных mpg и name
tree.auto <- tree(high.mpg ~ (.-name-mpg), Auto)
summary(tree.auto)

# график результата
plot(tree.auto)              # ветви
text(tree.auto, pretty = 0)  # подписи

# посмотреть всё дерево в консоли
tree.auto                    



# Теперь построим дерево на обучающей выборке и оценим ошибку на тестовой.

# ядро генератора случайных чисел по номеру варианта
my.seed <- 9
set.seed(my.seed)

# обучающая выборка
train <- sample(1:nrow(Auto), 200)

# тестовая выборка
auto.test <- Auto[-train,]
high.mpg.test <- high.mpg[-train]

# строим дерево на обучающей выборке
tree.auto <- tree(high.mpg ~ (.-name-mpg), Auto, subset = train)
summary(tree.auto)

# делаем прогноз
tree.pred <- predict(tree.auto, auto.test, type = "class")

# матрица неточностей
tbl <- table(tree.pred, high.mpg.test)
tbl

# ACC на тестовой
acc.test <- sum(diag(tbl))/sum(tbl)
names(acc.test)[length(acc.test)] <- 'Auto.class.tree.all'
acc.test

# Обобщённая характеристика точности: доля верных прогнозов: 0.921875 

# Теперь обрезаем дерево, используя в качестве критерия частоту ошибок классификации. Функция cv.tree() проводит кросс-валидацию для выбора лучшего дерева, аргумент prune.misclass означает, что мы минимизируем ошибку классификации.

set.seed(my.seed)
cv.auto <- cv.tree(tree.auto, FUN = prune.misclass)
# имена элементов полученного объекта
names(cv.auto)

# сам объект
cv.auto

# графики изменения параметров метода по ходу обрезки дерева ###################

# 1. ошибка с кросс-валидацией в зависимости от числа узлов
par(mfrow = c(1, 2))
plot(cv.auto$size, cv.auto$dev, type = "b",
     ylab = 'Частота ошибок с кросс-вал. (dev)',
     xlab = 'Число узлов (size)')
# размер дерева с минимальной ошибкой
opt.size <- cv.auto$size[cv.auto$dev == min(cv.auto$dev)]
abline(v = opt.size, col = 'red', 'lwd' = 2)     # соотв. вертикальная прямая
mtext(opt.size, at = opt.size, side = 1, col = 'red', line = 1)

# 2. ошибка с кросс-валидацией в зависимости от штрафа на сложность
plot(cv.auto$k, cv.auto$dev, type = "b",
     ylab = 'Частота ошибок с кросс-вал. (dev)',
     xlab = 'Штраф за сложность (k)')

# Как видно на графике слева, минимум частоты ошибок достигается при числе узлов 6.
# Оценим точность дерева с 6 узлами.


# дерево с 6 узлами
prune.auto <- prune.misclass(tree.auto, best = 6)

# визуализация
plot(prune.auto)
text(prune.auto, pretty = 0)

# прогноз на тестовую выборку
tree.pred <- predict(prune.auto, auto.test, type = "class")

# матрица неточностей
tbl <- table(tree.pred, high.mpg.test)
tbl

# ACC на тестовой
acc.test <- c(acc.test, sum(diag(tbl))/sum(tbl))
names(acc.test)[length(acc.test)] <- 'Auto.class.tree.6'
acc.test

# Точность этой модели не изменилась и составляет 0.921.
# Увеличив количество узлов, получим точно такое же дерево.


# дерево с 7 узлами
prune.auto <- prune.misclass(tree.auto, best = 7)

# визуализация
plot(prune.auto)
text(prune.auto, pretty = 0)

# прогноз на тестовую выборку
tree.pred <- predict(prune.auto, auto.test, type = "class")

# матрица неточностей
tbl <- table(tree.pred, high.mpg.test)
tbl

# ACC на тестовой
acc.test <- c(acc.test, sum(diag(tbl))/sum(tbl))
names(acc.test)[length(acc.test)] <- 'Carseats.class.tree.15'
acc.test

# сбрасываем графические параметры
par(mfrow = c(1, 1))



