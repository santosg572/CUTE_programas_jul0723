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