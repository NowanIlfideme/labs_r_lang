# Anatoly Makarevich
# Variant 6 == 30 %% 12

Nvar = 30 %% 12  # == 6

# 1. Сгенерировать выборку из N=100 4-х мерных векторов из нормального закона
# распределения с ненулевым вектором математического ожидания и недиагональной
# ковариационной матрицей. Ковариационная матрица должна генерироваться случайно
# перед генерацией выборки и должна удовлетворять всем свойствам ковариационной
# матрицы.

library(mvtnorm)

n_dim <- 4
N <- 100

mu <- runif(n_dim, min=-3, max=3)
sigma <- matrix(
    runif(n_dim^2, min=-3, 3), 
    nrow = n_dim, ncol = n_dim
)
sigma <- sigma %*% t(sigma)

X <- rmvnorm(N, mu, sigma)

i1 = c(1)
i2 = setdiff(seq(1, n_dim), i1)

# Затем, считая первые компоненты элементов сгенерированной выборки зависимыми
# переменными, а остальные компоненты -- независимыми в модели линейной регрессии,
# найти оценки коэффициентов регрессии и дисперсии случайных ошибок. Проверить
# остатки модели на нормальность.

mdl <- lm(X[,i1] ~ 1 + X[,i2])
summary(mdl)

coeffs.fit <- mdl$coefficients[2:length(mdl$coefficients)]
intercept.fit <- mdl$coefficients[1]

resids <- mdl$residuals
shapiro.test(resids)
qqnorm(resids); qqline(resids)



# Найти истинные значения коэффициентов регрессии (см. доп. файл) и сравнить их с
# полученными оценками.

coeffs.true <- sigma[i1, i2] %*% solve(sigma[i2, i2])
intercept.true <- mu[i1] - coeffs.true %*% mu[i2]

mu_1.2 <- function(x2) {
    # res <- mu[i1] + sigma[i1, i2] %*% solve(sigma[i2, i2]) %*% (x2 - mu[i2])
    # res <- (
    #    (mu[i1] - sigma[i1, i2] %*% solve(sigma[i2, i2]) %*% mu[i2])
    # ) + sigma[i1, i2] %*% solve(sigma[i2, i2]) %*% x2
    res <- BIAS + COEFFS %*% x2
    return(res)
}

c(intercept.fit, coeffs.fit)
c(intercept.true, coeffs.true)

