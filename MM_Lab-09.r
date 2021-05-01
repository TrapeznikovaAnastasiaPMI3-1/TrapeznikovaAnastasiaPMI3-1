# Зависимая переменная	
# default (Yes – наличие признака, No – отсутствие)	
# 
# Объясняющие переменные	
# все остальные	
# 
# Методы
# LDA, QDA

library('ISLR')
library(MASS)
library('e1071')     # SVM
data(Default)

# Отложить 25% наблюдений в тестовую выборку (ядро генератора случайных чисел указано в варианте к упражнению №3).

# Зададим ядро генератора случайных чисел и объём обучающей выборки.
my.seed <- 345
train.percent <- 0.75
options("ggmatrix.progress.bar" = FALSE)

# ?Default   # справка по набору данных
head(Default)
str(Default)

# Отбираем наблюдения в обучающую выборку
set.seed(my.seed)
inTrain <- sample(seq_along(Default$default),nrow(Default)*train.percent)
df_train <- Default[inTrain, ]
df_test <- Default[-inTrain, ]

Факт <- df_train$default


# На обучающей выборке (оставшихся 75% наблюдений) сравнить несколько видов ядер SVM по точности модели (AUC) методом сеточного поиска.

# Сеточный поиск:

# обучающие данные
dat <- data.frame(x = df_train[,-1], y = df_train[,1])

# тестовые данные
dat.te <- data.frame(x = df_test[,-1], y = df_test[,1])

# параметры алгоритма
kernel.grid <- c('linear', 'polynomial')
cost.grid <- seq(1, 20, by = 0.5)

AUC <- matrix(0, length(kernel.grid), length(cost.grid))
colnames(AUC) <- paste0('cost = ', cost.grid)
rownames(AUC) <- paste0('kernel = ', kernel.grid)

# SVM 
for (i in 1:length(kernel.grid)) {
  print(paste0('Starting ', kernel.grid[i], ' kernel'))
  for (j in 1:length(cost.grid)) {
    out <- svm(y ~ ., data = dat, kernel = kernel.grid[i], cost = cost.grid[j])
    # прогноз на тестовой выборке
    pred.te <- predict(out, newdata = dat.te)
    # матрица неточностей
    tbl <- table(pred.te, dat.te$y)
    AUC[i, j] <- sum(diag(tbl)) / sum(tbl)
  }
}

AUC
View(t(AUC))
# Максимальное значение AUC (area under ROC curve) равно 0.9708 при значении параметров:
# kernel = 'polynomial'
# cost = 6

# Для оптимальной формы ядерной функции на обучающей выборке подобрать 
# оптимальное значение настроечных параметров по минимальной ошибке с перекрёстной проверкой (функция tune).

# делаем перекрёстную проверку, изменяя штраф (аргумент cost)
set.seed(my.seed)
tune.out <- tune(svm, y ~ ., data = dat,
                 kernel = "polynomial",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

# лучшая модель -- с минимальной ошибкой
bestmod <- tune.out$best.model
summary(bestmod)

# делаем прогноз по лучшей модели
ypred <- predict(bestmod, dat.te)

# матрица неточностей
table(Прогноз = ypred, Факт = dat.te$y)

# прогноз по модели с cost = 0.01
svmfit <- svm(y ~ ., data = dat, kernel = "polynomial", cost = .01, scale = FALSE)
ypred <- predict(svmfit, dat.te)
# матрица неточностей
table(Прогноз = ypred, Факт = dat.te$y)

##############################################
# ROC-кривые
##############################################

# функция построения ROC-кривой: pred -- прогноз, truth -- факт
rocplot <- function(pred, truth, ...){
  require(ROCR)
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}

# последняя оптимальная модель
svmfit.opt <- svm(y ~ ., data = dat.te, kernel = "polynomial", gamma = 2, cost = 1, probability = T)

# матрица неточностей на обучающей (p = 0.5)
table(Прогноз = predict(svmfit.opt, dat.tr), Факт = dat.tr$y)

# прогноз вероятностей, на основе которых присваивается класс
fitted.prob <- predict(svmfit.opt, dat.tr, type = "prob",probability = TRUE)
fitted.prob <- attr(fitted.prob, "probabilities")[, 2]


# график для обучающей выборки
par(mfrow = c(1, 2))
# ROC-кривая для первой модели
rocplot(fitted.prob, dat.tr[, "y"], main = "Training Data")

predob = prediction(fitted.prob, dat.tr[, "y"])
perf = performance(predob, "tpr", "fpr")

# более гибкая модель (gamma выше)
svmfit.flex = svm(y ~ ., data = dat.tr, kernel = "radial", gamma = 10, cost = 1, probability = T)

# матрица неточностей на обучающей (p = 0.5)
table(Прогноз = predict(svmfit.flex, dat.tr), Факт = dat.tr$y)

# прогноз вероятностей, на основе которых присваивается класс
fitted.prob <- predict(svmfit.flex, dat.tr, type = "prob",probability = TRUE)
fitted.prob <- attr(fitted.prob, "probabilities")[, 2]
# ROC-кривая для второй модели
rocplot(fitted.prob, dat.tr[, "y"], add = T, col = "red")



# график для тестовой выборки
fitted.prob <- predict(svmfit.opt, dat.te[, ], type = "prob",probability = TRUE)
fitted.prob <- attr(fitted.prob, "probabilities")[, 2]

# матрица неточностей на тестовой (p = 0.5)
table(Прогноз = predict(svmfit.opt, dat.tr), Факт = dat.tr$y)

rocplot(fitted.prob, dat.te[, "y"], main = "Test Data")
fitted.prob <- predict(svmfit.flex, dat.te, type = "prob",probability = TRUE)
fitted.prob <- attr(fitted.prob, "probabilities")[, 2]

# матрица неточностей на тестовой (p = 0.5)
table(Прогноз = predict(svmfit.flex, dat.te), Факт = dat.te$y)

rocplot(fitted.prob, dat.te[, "y"], add = T, col = "red")