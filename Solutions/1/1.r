# Anatoly Makarevich
# Variant 30

# hack to get russian to work
Sys.setlocale("LC_CTYPE", "russian")

Nvar = 30
Nfam = nchar("Makarevich")

# 1
v1 = seq(Nvar, 6.5, length.out=Nfam)
print(v1)

v2.0 = abs(13-Nvar)+2
v2.1 = 100
v2.g = (v2.1/v2.0)^(1/(Nfam-1))
v2 = v2.0 * v2.g^(0:(Nfam-1))
print(v2)

v3 = sample(c(v1, v2), 3, replace = FALSE)
print(v3)


# 2
# (30 - 1) %% 6 + 1 = 6

conv <- function(word, j) {
    az = letters # alphabet
    n.j = match(substr(word, j, j), az)
    new_k = ((j * n.j) %% length(az) + 1)
    new = az[new_k]
    return(new)
}
conv2 <- function(word) {
    res = c()
    for (i in 1:nchar(word)){
        res = paste(res, conv(word, i), sep="")
    }
    return(res)
}
conv2("beef")


# 3
# Make a 13x13 matrix 'm'. Fill with elements of v1.
m = matrix(v1, nrow=13, ncol=13)
# By row, if v3[1] >10, otherwise by column
if (v3[1]<=10) m = t(m)

#mons = c("Jan", "Feb", "Mar", "Apr", 
#            "May", "Jun", "Jul", "Aug", 
#            "Sep", "Oct", "Nov", "Dec")
mons = month.name

# set names to months
k.3 = (Nvar-1) %% 12 + 1
nms = array(c(mons[k.3:12],mons[1:(k.3-1)]), 13)
nms -> rownames(m) -> colnames(m)

# filter out by names
is_ok.3 = (substring(nms, 1, 1) > "F") # not A-F
m1 = m[is_ok.3, is_ok.3]

# results
det(m1) # ==0
vv = eigen(m1)
vv$values
vv$vectors
diag(m1)
m1 %*% m1
m1 ^ 2


#  4 
# Создать произвольную таблицу данных, в которой должны присутствовать 
# данные следующих типов: числовые, текстовые, условные, факторы. 

# numeric, text, conditional, factor
t.4 <- data.frame(
    num=c(1,2,3,5,8),
    txt=c('ar','too','dee','too','three'),
    cond=c(T,F,F,F,T),
    fac=as.factor(c('a','b','b','a','b')),
    stringsAsFactors=FALSE
)

# Вывести все такие текстовые значения для заданного фактора, 
# для которых числовые значения больше заданного значения.
print(
    t.4['txt'][t.4['fac']=='b' & t.4['num']>2]
)


#  5
# (Таблица) Изначально это задание необходимо выполнить без 
# 1) использования циклов; 
# 2) использования функций типа _apply, 
# 3) создания своих функций.

# Создать произвольную таблицу (можно прочитать из файла), 
# состоящую из трех переменных (x1, x2, x3) 
# (переменная = столбец) и n наблюдений (строк).

paste('Number for 5th exercise:', (Nvar-1)%%7 + 1)
# SUM{i,1,N}[ ((i-1)%5+1) * x_{(i-1)%3+1, i}^{i} ]
# rows and columns are actually switched in the task

mp <- function(a, b) {
    return(((a-1) %% b) + 1)
}

t.5 <- data.frame(
    x1=c(1,2,3,5,8,13),
    x2=c(1,1,2,3,5,8),
    x3=rep(c(7,3), times=3) + 1:6
)


# FIRST: Without cycles or anything
f5.a <- function(tbl) {
    n = nrow(tbl)
    mults = mp(seti, 5)  # multipliers
    cls = paste("x", mp(seti, 3), sep="")  # columns
    rws = 1:n  # rows
    pws = 1:n  # powers
    mtx = mults * diag(as.matrix(t.5[rws, cls]))^pws
    return(sum(mtx))  # the sum
}

# THEN: 
# Реализовать вычисление этой же суммы с использованием 
# циклов, функций и пр. Сравнить полученные результаты.

f5.b <- function(tbl) {
    res = 0
    for(i in 1:nrow(tbl)) {
        m1 = mp(i, 5)
        xi = paste("x", mp(i, 3), sep="")
        m2 = t.5[i, xi]
        res = res + m1 * m2^i
    }
    return(res)
}
f5.a(t.5)
f5.b(t.5)

