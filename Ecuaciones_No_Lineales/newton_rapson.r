newton_rapson <- function(x0,tol,iterMax) {
  f1 <- function(x) {
    a <-x**3-x**2-1
    return(a)
  }
  df1 <- function(x) {
    a <-3*x**2-2*x
    return(a)
  }
  xn<-x0

    for (k in 1:iterMax) {
      x<-xn-(f1(xn)/df1(xn))
      xn<-x
      error<-abs(f1(x))
      if (error <tol) {
        break
      }
    }

  ele_sal<- list(x, error,k)
  return (ele_sal)
}

# solnr<-newton_raphson(2,1e-6,1000)
# el_error<-solnr[2]
# el_k<-solnr[3]
# el_x<-solnr[1]
# print(el_x)
# print(el_k)
# print(el_error)