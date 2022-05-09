secante <- function(x0,x1,tol,iterMax) {
  f1 <- function(x) {
    a <-exp(-x**2)-x
    return(a)
  }
  xk<-x1;
  xk_1<-x0;
    for (k in 1:iterMax) {
      x<-xk-((xk-xk_1)*f1(xk))/(f1(xk)-f1(xk_1));   
      xk_1<-x;
      error<-abs(f1(x))
      if (error <tol) {
        break
      }
    }
  ele_sal<- list(x, error,k)
  return (ele_sal)
}


# sols<-secante(0,1,1e-3,100)
# el_error<-sols[2]
# el_k<-sols[3]
# el_x<-sols[1]
# print(el_x)
# print(el_k)
# print(el_error)