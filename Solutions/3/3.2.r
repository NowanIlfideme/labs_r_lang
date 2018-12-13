# Anatoly Makarevich
# Variant 10 == 30 %% 20

Nvar = 30 %% 20  # == 10


# 2. Из файла Lab3Task2Var[x].scv загрузить данные. Вместо [x] необходимо подставить
# ваш номер варианта. Данные содержат как значения зависимых переменных, так и
# независимых. Вид зависимости известен и задан в таблице. Однако кроме коэффициентов
# регрессии неизвестен и коэффициент α. Предложите метод оценивания всех неизвестных
# коэффициентов с использованием функции lm, и оцените их. Приведите графическую
# иллюстрацию полученных результатов (график рассеяния с полученной линией
# регрессии). Воспользуйтесь функцией nlm, сравните полученные результаты.

base_path = "Tasks/for lab3/"
#base_path = paste0("../", base_path)
df <- read.csv(paste0(base_path, "Lab3Task2Var", Nvar, ".csv"))

# Function: y = beta0 + beta1 / (x - alpha) + eps
# Generally:
# y = b0 + b1 * f(x, a) + eps 
# We need to find a, b0, b1


plot(df$x, df$y)
# Uhhhhh, what? This data is terrible. :P



# 1. Using nls (nonlinear least squares)

f <- function(a, x) {
    return(1/(x-a))
}
nl_model <- nls(
    y ~ b0 + b1 * f(a, x), 
    df, 
    start=list(a=1, b0=1, b1=1)
)

summary(nl_model)

params1 <- coef(nl_model)
pred1 <- predict(nl_model)


# 2. Using nlm (nonlinear minimization)

a_pred <- function(params, x) {
    a <- params[1]
    b0 <- params[2]
    b1 <- params[3]
    y_hat <- b0 + b1 * f(a, x) 
    return(y_hat)
}

mse <- function(params, df) {
    y_hat <- a_pred(params, df$x)
    dlt <- y_hat - df$y
    return(sum(dlt*dlt))
}

res <- nlm(mse, c(1, 1, 1), df=df)
params2 <- res$estimate
pred2 <- a_pred(params2, df$x)

# NOTE: We don't know the error bounds


# RMSE
mse(params1, df) ^ 0.5
mse(params2, df) ^ 0.5

# 
plot(df$x, df$y)
points(df$x, pred2, col="green")
points(df$x, pred1, col="red")
