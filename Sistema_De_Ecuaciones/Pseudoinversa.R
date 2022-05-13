# Metodo de la Pseudoinversa: Resuelve el sistema Ax = b
#     Entrada:
#         mat: Matriz
#         b: vector
#         tol: error mínimo del resultado
#         iterMax: iteraciones máximas
    # Salida:
    #     xk: Vector solución
    #     i: numero de iteraciones
    #     error: error del resultado
pseudoinversa <- function(A, b, tol, itermax){ # nolint
    x <- (1/norm(A, type = "2")**2)*t(A) 
    xk <- x%*%b
    I<-diag(nrow(A))

    for (i in seq(1, itermax, by = 1)) {
        x <- x%*%(2*I-A%*%x)
        xk_n <- x%*%b

        error <- norm(xk_n-xk, type = "2")/ norm(xk_n, type = "2")
        xk <- xk_n
        if (error < tol) {
            break
        }
    }
    resultado <- list(xk, error, i)
    return(resultado)
}

tol <- 10^-10
itermax <- 1000

A = matrix(
    datos <- c(4,1,0,0,1,4,1,0,0,1,4,1,0,0,1,4),
    nrow = 4,
    ncol = 4,
    byrow = TRUE
)

b = as.vector(matrix(
    datos <- c(-10,-12,-12,-10),
    nrow = 4,
    ncol = 1,
    byrow = TRUE
))

resultados <- pseudoinversa(A, b, tol, itermax)
xk <- resultados[[1]]
i <- resultados[[3]]
error <- resultados[[2]]
print(xk)
print(i)
print(error)