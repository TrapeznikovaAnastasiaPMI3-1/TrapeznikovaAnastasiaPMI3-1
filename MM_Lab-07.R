# набор данных Auto
library('ISLR')
attach(Auto)
data(Auto)

# Зависимая переменная displacement (объём двигателя в кубических дюймах)
# Объясняющая переменная horsepowers
# Вероятность для второй модели P(displacement>280)

# нарезаем предиктор horsepower на 4 равных интервала
table(cut(horsepower, 4))

# подгоняем линейную модель на интервалах
fit <- lm(displacement ~ cut(horsepower, 4), data = Auto)
round(coef(summary(fit)), 2)

horsepowerlims=range(horsepower)
horsepower.grid=seq(from=horsepowerlims[1],to=horsepowerlims[2])

# прогноз -- это средние по `displacement` на каждом интервале
preds.cut <- predict(fit, newdata = list(horsepower = horsepower.grid), se = T)

# интервальный прогноз
se.bands.cut <- cbind(lower.bound = preds.cut$fit - 2*preds.cut$se.fit,
                      upper.bound = preds.cut$fit + 2*preds.cut$se.fit)



# наблюдения
plot(horsepower, displacement, xlim = horsepowerlims, cex = 0.5, col = 'darkgrey')

# модель
lines(horsepower.grid, preds.cut$fit, lwd = 2, col = 'darkgreen')

# доверительные интервалы прогноза
matlines(x = horsepower.grid, y = se.bands.cut, lwd = 1, col = 'darkgreen', lty = 3)

# заголовок
title('Ступенчатая функция')



# Правая часть графика, для вероятности того, что объём двигателя выше 280.

fit <- glm(I(displacement > 280) ~ cut(horsepower, 4), data = Auto, family = 'binomial')

# прогнозы
preds <- predict(fit, newdata = list(horsepower = horsepower.grid), se = T)

# пересчитываем доверительные интервалы и прогнозы в исходные ЕИ
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(lower.bound = preds$fit - 2*preds$se.fit,
                        upper.bound = preds$fit + 2*preds$se.fit)
se.bands <- exp(se.bands.logit)/(1 + exp(se.bands.logit))

# результат - доверительный интервал для вероятности события  "Объём двигателя выше 280".   
round(head(se.bands), 3)


# сетка для графика (изображаем вероятности, поэтому интервал изменения y мал)
plot(horsepower, I(displacement > 280), xlim = horsepowerlims, type = 'n',
     ylab = 'P(displacement > 280 | horsepower)')

# фактические наблюдения показываем засечками
points(jitter(horsepower), I((displacement > 280) / 5), cex = 0.5, pch = '|', col = 'darkgrey')

# модель
lines(horsepower.grid, pfit, lwd = 2, col = 'darkgreen')

# доверительные интервалы
matlines(horsepower.grid, se.bands, lwd = 1, col = 'darkgreen', lty = 3)

# заголовок
title('Ступенчатая функция')

