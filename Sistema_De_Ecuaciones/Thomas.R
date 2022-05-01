
# Metodod de Thomas: Resuelve el sistema Ax = b
#    Entrada:
#       mat: Matriz tridiagonal
        # d: vector
    # Salida:
    #     x: vector solución
thomas <- function(mat, d){ # nolint
    m <- nrow(mat)
    cba <- get_tridiagonal(mat)

    c_vec <- cba[[1]]
    b_vec <- cba[[2]]
    a_vec <- cba[[3]]

    x <- rep(0, m)
    p <- rep(0, m)
    q <- rep(0, m)

    #valores iniciales de p y q
    p[1] <- c_vec[1] / b_vec[1]
    q[1] <- d[1] / b_vec[1]

    for (i in seq(2, m - 1, by = 1)) {
        denom <- b_vec[i] - p[i - 1] * a_vec[i - 1]
        #Calculo de p
        p[i] <- c_vec[i] / denom
        #Calculo de q (menos el valor final)
        num <- d[i] - q[i - 1] * a_vec[i - 1]
        q[i] <- num / denom
    }
    #Calculo de valor final de q
    num <- d[m] - q[m - 1] * a_vec[m - 1]
    denom <- b_vec[m] - p[m - 1] * a_vec[m - 1]
    q[m] <- num / denom

    # #Valor final de X
    x[m] <- q[m]
    #El resto de valores de X
    for (j in seq(m - 1, 0, by = -1)) {
        x[j] <- q[j] - p[j] * x[j + 1]
    }
    return(x)
}

# Retorna una lista cba con 3 vectores correspondientes
#  a la diagonal, subdiagonal y supradiagonal
#     Entrada:
#         mat: Matriz tridiagonal
#     Salida:
#         cba: Lista con 
#         [diagonal superior, diagonal principal, diagonal inferior]
get_tridiagonal <- function(mat) {
    m <- nrow(mat)

    c_vec <- c()
    b_vec <- diag(mat)
    a_vec <- c()

    for (i in seq(1, m - 1, by = 1)) {
        c_vec[i] <- mat[i, i + 1]
        a_vec[i] <- mat[i + 1, i]
    }
    for (i in seq(1, m - 1, by = 1)) {
        d[i] <- mat[i, i]
    }

    cba <- list(c_vec, b_vec, a_vec)
    return(cba)
    }

r1 <- c(5, 1, 0, 0)
r2 <- c(1, 5, 1, 0)
r3 <- c(0, 1, 5, 1)
r4 <- c(0, 0, 1, 5)

mat <- rbind(r1, rbind(r2, rbind(r3, r4))) #nolint
d <- c(-12, -14, -14, -12)
x <- thomas(mat, d)
print(x)
