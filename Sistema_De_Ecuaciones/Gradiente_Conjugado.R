# Metodo del Gradiente Conjugado: Resuelve el sistema Ax = b
#     Entrada:
#         A: Matriz
#         b: vector
#         x0: vector con valores iniciales
#         tol: error maximo del resultado
#         iterMax: iteraciones máximas
    # Salida:
    #     xk: Vector solución
    #     k: numero de iteraciones
    #     error: error del resultado
gradiente_conjugado <- function(A, b, x0, tol, itermax){ # nolint
    if (SDP(A)){
        xk <- x0
        for (k in seq(1, itermax, by = 1)){
            rk <- b - A%*%xk
            alphak <- as.vector((t(rk)%*%rk)/(t(rk)%*%A%*%rk))
            xksig <- xk+alphak*rk
            xk <- xksig
            error <- norm(b-A%*%xksig)

            if (error <= tol){
                break
            }
        }
        k <- k+1

    } else {
       stop("La matriz A no es simetrica y definida positiva")
    }
    resultado <- list(xk, error, k)
    return(resultado)
}


# Retorna booleano indicando si la matriz es simetrica definida positiva
# Entrada:
#     A: matriz
# Salida:
#     result: booleano resultante
SDP <- function(A){
    result = TRUE
    if (isSymmetric(A)){
        for (i in seq(1, nrow(A), by = 1)){
            if (det(A[1:i, 1:i]) <= 0){
                result = FALSE
            }
        }
    }
    return(result)
}

r1 <- c(5, 1, 0)
r2 <- c(1, 5, 1)
r3 <- c(0, 1, 5)
mat_A <- rbind(r1, rbind(r2, r3)) #nolint

x0 <- c(0, 0, 0)
b <- c(6, 7, 6)

tol <- 10^-10
itermax <- 1000
resultados <- gradiente_conjugado(mat_A, b, x0, tol, itermax)
xk <- resultados[[1]]
i <- resultados[[2]]
error <- resultados[[3]]
print(xk)
print(i)
print(error)