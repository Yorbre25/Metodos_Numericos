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
    print(xk)
    print(error)
    print(i)
}

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
gauss_seidel(mat_A, b, x0, tol, itermax)