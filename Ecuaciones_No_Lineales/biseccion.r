biseccion <- function(a,b,tol,iterMax) {
  f1 <- function(x) {
    a <-  -2*x +exp(x)  -10
    return(a)
  }
if (f1(b) * f1(a)<0 ) {
  
	for (k in 1:iterMax) {
		x<-(a+b)/2
		if (f1(a) * f1(x)<0) {
	 	 b<-x
		} else {
 		 a<-x
		}
		error<-abs(f1(x))
		if (error <tol) {
		  break
		}
	}
} else {
  print ("No cumple el teorema de Bolzano")
}

ele_sal<- list(x, error,k)
  return (ele_sal)
}


# solb<-biseccion(2,3,1e-6,1000)
# el_error<-solb[2]
# el_k<-solb[3]
# el_x<-solb[1]
# print(el_x)
# print(el_k)
# print(el_error)
