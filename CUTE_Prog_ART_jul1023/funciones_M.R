
Solucion <- function(x1=0, x2=0, del=0){
	npun = GenNP(x1, x2, del)
	t = seq(x1, x2, length.out = npun)
	cat('Solucion, numero de puntos:', npun, '\n')

	del = t[2] - t[1]
	
	x = rep(0,npun)
	x1 = 2.5
	x[1] = x1
	
	for (i in 2:npun){
		x2 = x1 + del * x1*(.8 - .2*x1)
		x[i] = x2
		x1 = x2
	}
	res = list(t, x)	
}


SolucionEsto_Ejemplo2 <- function(x1=0, x2=0, del=0, parpois=0, fac_ruido=0){
	npun = GenNP(x1, x2, del)
	t = seq(x1, x2,length.out = npun)
	del = t[2]-t[1]

	cat('SolucionEsto_Ejemplo2, npun: ', npun,'\n')
	cat('    parametro de Poisson: ', parpois,'\n')
	cat('    valor de del: ', del, '\n')
	
	ss = funcion_poisson(x2, parpois)
	tt = as.vector(ss[[1]])

    res = EncIndices(t, tt)
	
	ii = as.vector(res[[2]])
	TT = as.vector(res[[1]])
	npun = length(TT)
	cat('.   intervalo de TT: ',  TT[1], TT[npun], '\n')
	cat('.    npun TT:', npun, '\n')
	
	B = fac_ruido*sqrt(del)*rnorm(npun)
	
	x = rep(0,npun)
	x1 = 2.5
	x[1] = x1
	for (i in 2:npun){
		z = 0
		if (length(which(i == ii)) > 0){
			z = K(rnorm(1))
		}
		x2 = x1 + (TT[i]-TT[i-1]) * x1*(.8 - .2*x1) + x1*B[i-1] +  z
		x[i] = x2
		x1 = x2
	}
	res = list(TT, x)	
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
	res = sign(z) * (TETA*z)^2 / (1+(TETA*z)^2)
}

GenNP <- function(x1=0, x2=0, del=0){
	np = round((x2-x1)/del + 1)
}


