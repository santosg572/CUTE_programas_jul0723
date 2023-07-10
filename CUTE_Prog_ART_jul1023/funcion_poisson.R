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
