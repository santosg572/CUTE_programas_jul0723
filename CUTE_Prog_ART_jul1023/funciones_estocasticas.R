EncIndices <- function(x=0, y=0){
	# Busca las posiciones de los valores de "y" en el vector "x"
	x = unique(sort(x))
	y = unique(sort(y))
	T = sort(unique(c(x,y)))
	ny = length(y)
	nt = length(T)
	ii = c()
	j = 1
	i = 1
	
	while (j <= ny){
		while  (i <= nt){
			if (y[j] == T[i]){
				ii = c(ii, i)
				break
			}
			i = i+1
		}
		j = j+1
	}
	ret = list(T,ii)
}

K <- function(z=0){
	res = (TETA* z)^2 / (.5+(TETA*z)^2)
}

K_old <- function(z=0){
	res = sign(z)* z^2 / (1+z^2)
}

K4 <- function(z=0, t=0){
	res = z^2 * exp(z/t)
}

Solucion <- function(){
	del = .01
	t = seq(0, 50,del)

	npun = length(t)
	
	x = rep(0,npun)
	x1 = 2.5
	x[1] = x1
	
	for (i in 2:npun){
		x2 = x1 + del * (.8 - .2*x1)
		x[i] = x2
		x1 = x2
	}
	res = list(t, x)	
}

SolucionEsto_Ejemplo1 <- function(){
	del = .01
	t = seq(0, 50,del)
	ss = funcion_poisson(50, .5)
	tt = ss[[1]]

    res = EncIndices(t, tt)
	
	ii = res[[2]]
	T = res[[1]]
	npun = length(T)
	
	x = rep(0,npun)
	x1 = 2.5
	x[1] = x1
	
	for (i in 2:npun){
		z = 0
		if (length(which(i == ii)) > 0){
			z = K(rnorm(1))
		}
		x2 = x1 + del * x1*(.8 - .2*x1) + z
		x[i] = x2
		x1 = x2
	}
	res = list(T, x)	
}

funcion_poisson <- function(T=0, lambda=0) {
	set.seed(10)
	t <- 0
	N <- 0
	
	while (max(t) <= T) {
		x <- rexp(1, lambda)
		t <- c(t, max(t)+x)
		N <- c(N, max(N)+1)
	}
	tk <- t[1:(length(t) -1)]
	Nk <- N[1:(length(N)-1)]
	res = list(tk, Nk)
}

SolucionEsto_Ejemplo2 <- function(){
	del = .01
	t = seq(0, 50,del)
	ss = funcion_poisson(50, .5)
	tt = ss[[1]]

    res = EncIndices(t, tt)
	
	ii = res[[2]]
	T = res[[1]]
	npun = length(T)
	
	del = T[2]-T[1]
	B = sqrt(del)*rnorm(npun)
	
	x = rep(0,npun)
	x1 = 2.5
	x[1] = x1
	
	for (i in 2:npun){
		z = 0
		if (length(which(i == ii)) > 0){
			z = K(rnorm(1))
		}
		x2 = x1 + del * x1*(.8 - .2*x1) + .03*x1*B[i-1] +  z
		x[i] = x2
		x1 = x2
	}
	res = list(T, x)	
}

SolucionEsto_Ejemplo3 <- function(){
	del = .01
	t = seq(0, 50,del)
	ss = funcion_poisson(50, .5)
	tt = ss[[1]]

    res = EncIndices(t, tt)
	
	ii = res[[2]]
	T = res[[1]]
	npun = length(T)
	
	del = T[2]-T[1]
	B = sqrt(del)*rnorm(npun)
	
	x = rep(0,npun)
	x1 = 2.5
	x[1] = x1
	
	for (i in 2:npun){
		z = 0
		if (length(which(i == ii)) > 0){
			z = K(rnorm(1))
		}
		x2 = x1 + del * x1*(.8 - .2*x1) + .03*x1*B[i-1] +  x1 * z
		x[i] = x2
		x1 = x2
	}
	res = list(T, x)	
}
SolucionEsto_Ejemplo4 <- function(){
	del = .01
	t = seq(0, 50,del)
	ss = funcion_poisson(50, .5)
	tt = ss[[1]]

    res = EncIndices(t, tt)
	
	ii = res[[2]]
	T = res[[1]]
	npun = length(T)
	
	del = T[2]-T[1]
	B = sqrt(del)*rnorm(npun)
	
	x = rep(0,npun)
	x1 = 2.5
	x[1] = x1
	
	for (i in 2:npun){
		z = 0
		if (length(which(i == ii)) > 0){
			z = K4(rnorm(1), x1)
		}
		x2 = x1 + del * x1*(.8 - .2*x1) + .03*x1*B[i-1] +  z/x1
		x[i] = x2
		x1 = x2
	}
	res = list(T, x)	
}




