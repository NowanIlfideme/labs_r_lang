# Anatoly Makarevich
# Variant 6 == 30 %% 12

Nvar = 30 %% 12  # == 6

# Написать рекурсивную функцию для нахождения корня
# нелинейного уравнения f(x) = 0, используя метод деления 
# отрезка пополам.
# Функцию f(x) передавать в качестве аргумента решающей функции.


FindRoot <- function(f, a=-100, b=100+1e-3, tol=1e-5) {
    # Make sure it's [a, b] and not the other way
    if (a>b) {
        a -> tmp; b -> a; tmp -> b
    }

    # Quick return if we're at root
    z <- f((a+b)/2 ->m)
    if (abs(z)<tol) {
        return(m)
    }

    # Find signs
    sa <- sign(f(a))
    sb <- sign(f(b))

    # Sanity check
    if (sa == sb) {
        # NOTE: Could try "extending" like this:
        # d <- b - a;  a <- a - d;  b <- b + d
        print("Roots same sign at ends, set different a and b.")
        return(NaN)
    }

    # Do the actual segment division
    if (sign(z) * sb > 0) {
        return(FindRoot(f, a, m, tol=tol))
    }
    return(FindRoot(f, m, b, tol=tol))    
}

f1 <- function(x) {
    return(x^3 + x - 5)
}
f2 <- function(x) {
    return(sin(x)+0.1*x)
}

rt1 <- FindRoot(f1)
paste(rt1, f1(rt1))

rt2 <- FindRoot(f2, 0, 4)
paste(rt2, f2(rt2))
