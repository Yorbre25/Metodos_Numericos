# Metodo Gauss Seidel: Resuelve el sistema Ax = b
#     Entrada:
#         mat: Matriz
#         b: vector
#         x0: vector con valores iniciales
#         tol: error mínimo del resultado
#         iterMax: iteraciones máximas
    # Salida:
    #     xk: Vector solución
    #     i: numero de iteraciones
    #     error: error del resultado
gauss_seidel <- function(mat, b, x0, tol, itermax){ # nolint
    ldu <- getLDU(mat)
    lower <- ldu[[1]]
    D <- ldu[[2]] #nolint
    upper <- ldu[[3]]
    xk <- x0

    y <- forwardsolve(lower + D, b)

    for (i in seq(1, itermax, by = 1)) {
        z <- forwardsolve(lower + D, upper %*% xk)
        xk <- -z + y

        error <- norm(mat %*% xk - b, type = "2")
        if (error < tol) {
            break
        }
    }
    resultado <- list(xk, error, i)
    return(resultado)
}

# Retorna las matrices U, D y L. Que cumplen mat = U + D + L
# Entrada:
#     mat: matriz
# Salida:
#     ldu: [inferior, diagonal, superior]
getLDU <- function(mat) { #nolint
    d <- diag(mat)
    D <- diag(d) #nolint
    upper <- mat
    upper[lower.tri(upper)] <- 0
    upper <- upper - D
    lower <- mat - D - upper
    ldu <- list(lower, D, upper)
    return(ldu)
}


r1 <- c(5, 1, 0)
r2 <- c(1, 5, 1)
r3 <- c(0, 1, 5)
mat_A <- rbind(r1, rbind(r2, r3)) #nolint

x0 <- c(0, 0, 0)
b <- c(6, 7, 6)

tol <- 10^-10
itermax <- 1000
resultados <- gauss_seidel(mat_A, b, x0, tol, itermax)
xk <- resultados[[1]]
i <- resultados[[2]]
error <- resultados[[3]]
print(xk)
print(i)
print(error)