# ��������� ����������	
# default (Yes � ������� ��������, No � ����������)	
# 
# ����������� ����������	
# ��� ���������	
# 
# ������
# LDA, QDA

library('ISLR')
library(MASS)
library('e1071')     # SVM
data(Default)

# �������� 25% ���������� � �������� ������� (���� ���������� ��������� ����� ������� � �������� � ���������� �3).

# ������� ���� ���������� ��������� ����� � ����� ��������� �������.
my.seed <- 345
train.percent <- 0.75
options("ggmatrix.progress.bar" = FALSE)

# ?Default   # ������� �� ������ ������
head(Default)
str(Default)

# �������� ���������� � ��������� �������
set.seed(my.seed)
inTrain <- sample(seq_along(Default$default),nrow(Default)*train.percent)
df_train <- Default[inTrain, ]
df_test <- Default[-inTrain, ]

���� <- df_train$default


# �� ��������� ������� (���������� 75% ����������) �������� ��������� ����� ���� SVM �� �������� ������ (AUC) ������� ��������� ������.

# �������� �����:

# ��������� ������
dat <- data.frame(x = df_train[,-1], y = df_train[,1])

# �������� ������
dat.te <- data.frame(x = df_test[,-1], y = df_test[,1])

# ��������� ���������
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
    # ������� �� �������� �������
    pred.te <- predict(out, newdata = dat.te)
    # ������� �����������
    tbl <- table(pred.te, dat.te$y)
    AUC[i, j] <- sum(diag(tbl)) / sum(tbl)
  }
}

AUC
View(t(AUC))
# ������������ �������� AUC (area under ROC curve) ����� 0.9708 ��� �������� ����������:
# kernel = 'polynomial'
# cost = 6

# ��� ����������� ����� ������� ������� �� ��������� ������� ��������� 
# ����������� �������� ����������� ���������� �� ����������� ������ � ����������� ��������� (������� tune).

# ������ ����������� ��������, ������� ����� (�������� cost)
set.seed(my.seed)
tune.out <- tune(svm, y ~ ., data = dat,
       kernel = "polynomial",
       ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

# ������ ������ -- � ����������� �������
bestmod <- tune.out$best.model
summary(bestmod)

# ������ ������� �� ������ ������
ypred <- predict(bestmod, dat.te)

# ������� �����������
table(������� = ypred, ���� = dat.te$y)

# ������� �� ������ � cost = 0.01
svmfit <- svm(y ~ ., data = dat, kernel = "polynomial", cost = .01, scale = FALSE)
ypred <- predict(svmfit, dat.te)
# ������� �����������
table(������� = ypred, ���� = dat.te$y)

##############################################
# ROC-������
##############################################

# ������� ���������� ROC-������: pred -- �������, truth -- ����
rocplot <- function(pred, truth, ...){
  require(ROCR)
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}

# ��������� ����������� ������
svmfit.opt <- svm(y ~ ., data = dat.te, kernel = "polynomial", gamma = 2, cost = 1, probability = T)

# ������� ����������� �� ��������� (p = 0.5)
table(������� = predict(svmfit.opt, dat.tr), ���� = dat.tr$y)

# ������� ������������, �� ������ ������� ������������� �����
fitted.prob <- predict(svmfit.opt, dat.tr, type = "prob",probability = TRUE)
fitted.prob <- attr(fitted.prob, "probabilities")[, 2]


# ������ ��� ��������� �������
par(mfrow = c(1, 2))
# ROC-������ ��� ������ ������
rocplot(fitted.prob, dat.tr[, "y"], main = "Training Data")

predob = prediction(fitted.prob, dat.tr[, "y"])
perf = performance(predob, "tpr", "fpr")

# ����� ������ ������ (gamma ����)
svmfit.flex = svm(y ~ ., data = dat.tr, kernel = "radial", gamma = 10, cost = 1, probability = T)

# ������� ����������� �� ��������� (p = 0.5)
table(������� = predict(svmfit.flex, dat.tr), ���� = dat.tr$y)

# ������� ������������, �� ������ ������� ������������� �����
fitted.prob <- predict(svmfit.flex, dat.tr, type = "prob",probability = TRUE)
fitted.prob <- attr(fitted.prob, "probabilities")[, 2]
# ROC-������ ��� ������ ������
rocplot(fitted.prob, dat.tr[, "y"], add = T, col = "red")



# ������ ��� �������� �������
fitted.prob <- predict(svmfit.opt, dat.te[, ], type = "prob",probability = TRUE)
fitted.prob <- attr(fitted.prob, "probabilities")[, 2]

# ������� ����������� �� �������� (p = 0.5)
table(������� = predict(svmfit.opt, dat.tr), ���� = dat.tr$y)

rocplot(fitted.prob, dat.te[, "y"], main = "Test Data")
fitted.prob <- predict(svmfit.flex, dat.te, type = "prob",probability = TRUE)
fitted.prob <- attr(fitted.prob, "probabilities")[, 2]

# ������� ����������� �� �������� (p = 0.5)
table(������� = predict(svmfit.flex, dat.te), ���� = dat.te$y)

rocplot(fitted.prob, dat.te[, "y"], add = T, col = "red")