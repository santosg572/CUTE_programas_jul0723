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
	TT = sort(unique(c(x,y)))
	ny = length(y)
	nt = length(TT)
	ii = c()
	j = 1
	i = 1
	
	while (j <= ny){
		while (i <= nt){
			if (y[j] == TT[i]){
				ii = c(ii, i)
				break
			}
			i = i+1
		}
		j = j+1
	}
	ret = list(TT,ii)
}


K <- function(z=0){
	res = sign(z) * (TETA*z)^2 / (1+(TETA*z)^2)
}


