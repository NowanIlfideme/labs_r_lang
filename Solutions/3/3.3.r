# Anatoly Makarevich
# Variant 10 == 30 %% 20

Nvar = 30 %% 20  # == 10


# 3. Из файла Lab2Task3Var[x].scv загрузить данные. Данные содержат как значения
# зависимых переменных, так и независимых в модели множественной линейной регрессии.
# В случайно выбранные 10 значений y внести пропуски. По полностью наблюдаемым
# значениям оценки коэффициентов регрессии, определить какие из них статистически
# значимые, а какие нет. Кроме этого провести "пошаговую оценку коэффициентов
# регрессии" как с добавлением переменных, так и с удалением. Выберете на ваш взгляд
# наиболее адекватную модель (если модели получились различные) и спрогнозируйте те
# значения y, в которые были внесены пропуски, сравните с исходными значениями.

base_path = "Tasks/for lab3/"
#base_path = paste0("../", base_path)
df <- read.csv(paste0(base_path, "Lab3Task3Var", Nvar, ".csv"))
y_true <- df$y


# Dirty the data

idx.rem <- sample(length(df$y), 10, replace=F)
df$y[idx.rem] <- NA


# Split into train, test data

train <- na.omit(df)
test.idx <- is.na(df$y)
test <- df[test.idx,]


# Fit multiple models

mdl1 <- lm(y ~ .-X, data=train, na.action=drop)
summary(mdl1)

mdl2 <- lm(
    y ~ x.1 + x.2 + x.3 + x.5 + x.7 + x.9 + x.10,
    data=train, na.action=drop
)
summary(mdl2)

mdl3 <- lm(
    y ~ x.1 + x.2 + x.7 + x.10,
    data=train, na.action=drop
)
summary(mdl3)


# Predict

y_pred <- predict(mdl3, test, )
y_pred <- as.array(y_pred)

y_test <- y_true[test.idx]

# Prediction
y_pred
y_test

# Error
eps <- y_pred - y_test

eps
eps / y_test

# Pretty good :)
