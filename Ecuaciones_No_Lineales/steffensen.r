steffensen <- function(a, b, tol, iter_max){
    if (f(a) * f(b) > 0){
        print('No se cumple el teorema de Bolzano')
        return()
    }

    x_k <- (a + b) / 2;
    error <- 0
    
    for (k in 1:iter_max){
        x_k <- x_k - f(x_k)^2 / (f(x_k + f(x_k)) - f(x_k))

        error <- abs(f(x_k))
        if (error < tol){
            break
        }
    }
    return(list(x_k, error, k))
}

f <- function(x){
    imagen <- x^2 - 4*x - 10 + 30*cos(x)
    return (imagen)
}

# solucion <- steffensen(-5, -3, 10e-10, 1500)
# print(format(solucion, nsmall = 12))