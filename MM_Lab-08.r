# ������ - Auto {ISLR}	
# ����������� Y - mpg
# ����������� ���������� - ���������, ����� name
# ����� �������� ������� - ������ � �������� ������

# ����� ������ Auto
library('ISLR')
library('GGally')            # ��������� ������ �������� ggpairs()
library('tree')              # ������� tree()
attach(Auto)
data(Auto)


# ������� �������
# �������� ������� � ������� �� ���������� ����������� � ������� � ��� ���������� high.mpg � �������� ������ �������� �� ����������:
# 1 ���� mpg >= 29
# 0 ���� mpg < 29

head(Auto)

# ����� ����������
high.mpg <- ifelse(Auto$mpg >= 29, 1, 0)
high.mpg <- factor(high.mpg, labels = c('yes', 'no'))
Auto$high.mpg <- high.mpg 
# ��������� ������� �������� ����������
p <- ggpairs(Auto[, c(10, 1:5)], aes(color = high.mpg))
suppressMessages(print(p))

p <- ggpairs(Auto[, c(10, 6:8)], aes(color = high.mpg))
suppressMessages(print(p))



# ������ ���������  ������ ��� ���������� mpg � name
tree.auto <- tree(high.mpg ~ (.-name-mpg), Auto)
summary(tree.auto)

# ������ ����������
plot(tree.auto)              # �����
text(tree.auto, pretty = 0)  # �������

# ���������� �� ������ � �������
tree.auto                    



# ������ �������� ������ �� ��������� ������� � ������ ������ �� ��������.

# ���� ���������� ��������� ����� �� ������ ��������
my.seed <- 9
set.seed(my.seed)

# ��������� �������
train <- sample(1:nrow(Auto), 200)

# �������� �������
auto.test <- Auto[-train,]
high.mpg.test <- high.mpg[-train]

# ������ ������ �� ��������� �������
tree.auto <- tree(high.mpg ~ (.-name-mpg), Auto, subset = train)
summary(tree.auto)

# ������ �������
tree.pred <- predict(tree.auto, auto.test, type = "class")

# ������� �����������
tbl <- table(tree.pred, high.mpg.test)
tbl

# ACC �� ��������
acc.test <- sum(diag(tbl))/sum(tbl)
names(acc.test)[length(acc.test)] <- 'Auto.class.tree.all'
acc.test

# ���������� �������������� ��������: ���� ������ ���������: 0.921875 

# ������ �������� ������, ��������� � �������� �������� ������� ������ �������������. ������� cv.tree() �������� �����-��������� ��� ������ ������� ������, �������� prune.misclass ��������, ��� �� ������������ ������ �������������.

set.seed(my.seed)
cv.auto <- cv.tree(tree.auto, FUN = prune.misclass)
# ����� ��������� ����������� �������
names(cv.auto)

# ��� ������
cv.auto

# ������� ��������� ���������� ������ �� ���� ������� ������ ###################

# 1. ������ � �����-���������� � ����������� �� ����� �����
par(mfrow = c(1, 2))
plot(cv.auto$size, cv.auto$dev, type = "b",
     ylab = '������� ������ � �����-���. (dev)',
     xlab = '����� ����� (size)')
# ������ ������ � ����������� �������
opt.size <- cv.auto$size[cv.auto$dev == min(cv.auto$dev)]
abline(v = opt.size, col = 'red', 'lwd' = 2)     # �����. ������������ ������
mtext(opt.size, at = opt.size, side = 1, col = 'red', line = 1)

# 2. ������ � �����-���������� � ����������� �� ������ �� ���������
plot(cv.auto$k, cv.auto$dev, type = "b",
     ylab = '������� ������ � �����-���. (dev)',
     xlab = '����� �� ��������� (k)')

# ��� ����� �� ������� �����, ������� ������� ������ ����������� ��� ����� ����� 6.
# ������ �������� ������ � 6 ������.


# ������ � 6 ������
prune.auto <- prune.misclass(tree.auto, best = 6)

# ������������
plot(prune.auto)
text(prune.auto, pretty = 0)

# ������� �� �������� �������
tree.pred <- predict(prune.auto, auto.test, type = "class")

# ������� �����������
tbl <- table(tree.pred, high.mpg.test)
tbl

# ACC �� ��������
acc.test <- c(acc.test, sum(diag(tbl))/sum(tbl))
names(acc.test)[length(acc.test)] <- 'Auto.class.tree.6'
acc.test

# �������� ���� ������ �� ���������� � ���������� 0.921.
# �������� ���������� �����, ������� ����� ����� �� ������.


# ������ � 7 ������
prune.auto <- prune.misclass(tree.auto, best = 7)

# ������������
plot(prune.auto)
text(prune.auto, pretty = 0)

# ������� �� �������� �������
tree.pred <- predict(prune.auto, auto.test, type = "class")

# ������� �����������
tbl <- table(tree.pred, high.mpg.test)
tbl

# ACC �� ��������
acc.test <- c(acc.test, sum(diag(tbl))/sum(tbl))
names(acc.test)[length(acc.test)] <- 'Carseats.class.tree.15'
acc.test

# ���������� ����������� ���������
par(mfrow = c(1, 1))



