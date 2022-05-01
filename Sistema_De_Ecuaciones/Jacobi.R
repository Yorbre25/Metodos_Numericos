
# Metodo Jacobi: Resulve el sistema Ax = b,
# y retorna una lista que contiene xk, i y e lerror
#     Entrada:
#         matriz_a: Matriz
#         b: vector
#         x0: vector con valores iniciales
#         tol: error mínimo del resultado
#         iterMax: iteraciones máximas
    # Salida:
    #     xk: Vector solución
    #     i: numero de iteraciones
    #     error: error del resultado
jacobi <- function(matriz_a, b, x0, tol, itermax) {
    d <- diag(matriz_a) #obtener la diagonal
    matriz_d <- diag(d) #Hacer una matriz con la diagonal

    #inversa de la matriz diagonal
    d_inv <- 1 / d
    matriz_d_inv <- diag(d_inv)
    
    l_mas_u <- matriz_a - matriz_d
    z <- matriz_d_inv %*%  b
    m <-  -matriz_d_inv %*% l_mas_u
    xk <- x0

    for (i in seq(1, itermax, by = 1)) {
        xk <- m %*% xk + z
        error <- norm(matriz_a %*% xk - b, type = "2")
        if (error < tol) {
            break
        }
    }
    print(xk)
    print(i)
    print(error)
    lista <- list(xk, i , error)
    return(lista)
}


r1 <- c(5, 1, 0)
r2 <- c(1, 5, 1)
r3 <- c(0, 1, 5)
matriz_a <- rbind(r1, rbind(r2, r3))

x0 <- c(0, 0, 0)
b <- c(6, 7, 6)
tol <- 10^-10
itermax <- 1000

resultados <- jacobi(matriz_a, b, x0, tol, itermax)
xk <- resultados[[1]]
i <- resultados[[2]]
error <- resultados[[3]]
print(xk)
print(i)
print(error)