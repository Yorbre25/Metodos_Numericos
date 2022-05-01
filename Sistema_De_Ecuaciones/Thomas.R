
thomas <- function(mat_A, d){ # nolint
    m <- nrow(mat_A)
    a_vec <- diag(mat_A, 1)
    b_vec <- diag(mat_A)
    c_vec <- diag(mat_A, -1)

    x <- c()
    p <- c()
    q <- c()

    #valores iniciales de p y q
    p[1] <- c_vec[1] / b_vec[1]
    q[1] <- d[1] / b_vec[1]

    for (i in seq(2, m - 1, by = -1)) {
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

    print(p)
    print(q)

    # #Valor final de X
    # x[m] <- q[m]
    # #El resto de valores de X
    # for (j in seq(m - 1, 0, by = -1)) {
    #     x[j] <- q[j] - p[j] * x[j + 1]
    # }
    # print(x)
}

get_diagonal <- function(mat, offset) {
    m <- nrow(mat)
    # n <- ncol(mat)
    d <- c()
    # for (i in seq(1, m, by = 1)) {
    #     d[i] <- mat[i, i + offset]
    # }
    for (i in seq(1, m - 1, by = 1)) {
        d[i] <- mat[i, i]
    }
    print(d)
    }

r1 <- c(5, 1, 0, 0) 
r2 <- c(1, 5, 1, 0)
r3 <- c(0, 1, 5, 1)
r4 <- c(0, 0, 1, 5)
mat_A <- rbind(r1, rbind(r2, rbind(r3, r4))) #nolint
d <- c(-12, -14, -14, -12)
get_diagonal(mat_A, -1)
# thomas(mat_A, d)

