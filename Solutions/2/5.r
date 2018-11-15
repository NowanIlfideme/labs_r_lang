
# Anatoly Makarevich
# Variant 6 == 30 %% 12

Nvar = 30 %% 12  # == 6

# На C++ реализовать задание лабораторной работы №2. 
# Встроить этот код в R. 
# С помощью следующего пакета (по вариантам) 
# сравнить время выполнения реализации на C++ с 
# реализацией на R (2 лабораторная работа):
# 1) Library tictoc;
# 2) Library rbenchmark;
# 3) Library microbenchmark. <- ~ 6


library('Rcpp')


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

# NOTE: Doing in-place because it's a simiple function
# Though, this means it'll be slower because we call R to C to R...
src <- '
double CFindRoot(
    Function f, double a=-100, 
    double b=100+1e-3, double tol=0.00001) 
{
    //printf("%f %f \\n", a, b);
    
    // Swap if needed
    if (a>b) {
        double tmp = a;
        a = b; b = tmp;
    }

    double m = (a+b)/2;
    double fm = as<double>(f(m));

    // Quick return if at root
    if (fabs(fm) < tol) {
        return m;
    }
    
    double fa = as<double>(f(a));
    double fb = as<double>(f(b));

    // Sanity check
    if (fa * fb > 0) {
        // TODO: Convert to NAN or exception or smth
        printf("The a and b are the same sign, set new ones.\\n");
        return -999;
    }
    
    // In left segment?
    if (fa * fm < 0) {
        return CFindRoot(f, a, m, tol);
    }

    // In right segment now
    return CFindRoot(f, m, b, tol);
}
'
#
cppFunction(src)


f1 <- function(x) {
    return(x^3 + x - 5)
}

f2 <- function(x) {
    return(sin(x)+0.1*x)
}

fcs <- c(f1, f2)
for (i in 1:length(fcs)) {
    f <- fcs[[i]]
    rt1 <- FindRoot(f)
    rc1 <- CFindRoot(f)
    print(paste("Function", i))
    print(paste(rt1, '->', f(rt1)))
    print(paste(rc1, '->', f(rc1)))
}

# Using this lib because I think 3 is my number :P
library('microbenchmark')
mr <- microbenchmark(
    FindRoot(f1), CFindRoot(f1),
    FindRoot(f2), CFindRoot(f2)
); print(mr)
