falsa_posicion <- function(a, b, tol, iter_max){
    x_k_anterior <- a
    x_k <- b
    error <- 0
    
    for (k in 1:iter_max){
        if (f(a) * f(b) < 0){
            tmp <- x_k
            x_k <- x_k - f(x_k) * (x_k - x_k_anterior) / (f(x_k) - f(x_k_anterior))
            x_k_anterior <- tmp

            error <- abs(f(x_k))
            if (error < tol){
                break
            }

            if (f(a) * f(x_k) < 0){
                b <- x_k
            }
            else if (f(x_k) * f(b) < 0){
                a <- x_k
            }
            else{
                print('No se cumple el teorema de Bolzano')
                return()
            }
        }
        else{
            print('No se cumple el teorema de Bolzano')
            return()
        }
    }
    return(list(x_k, error, k))
}

f <- function(x){
    imagen <- x^2 - 4*x - 10 + 30*cos(x)
    return (imagen)
}

# solucion <- falsa_posicion(-5, -3, 10e-10, 1500)
# print(format(solucion, nsmall = 12))