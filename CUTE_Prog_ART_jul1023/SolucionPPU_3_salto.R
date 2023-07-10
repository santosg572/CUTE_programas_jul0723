SolucionPPU_3_salto <- function(del=0, xx=0, uu=0){ 
	ss = dim(xx)
	n = ss[1]
	
	dwp = DWP*sqrt(del)*runif(n)
	
	pp = matrix(rep(0,3*n), ncol=3)
	
	pp[n,] = c(0,0,0)
	
	p2 = pp[n,]
	u2 = uu[n,]
	w2 = dwp[n]
	x2 = xx[n,]	
	
	for (i in (n-1):1){
		r = funP_ART2(x2, p2,u2)
		z = c(0,0,0)
		if (length(which(i == II)) > 0){
			z = K(rnorm(3))
		}
		p11 = p2[1] - (TT[i+1]-TT[i]) * r[1] - p2[1] * w2 + u2[1] * x2[1]*z[1]
		p12 = p2[2] - (TT[i+1]-TT[i]) * r[2] - p2[2] * w2 + u2[2] * x2[2]*z[2]
		p13 = p2[3] - (TT[i+1]-TT[i]) * r[3] - p2[3] * w2 + u2[3] * x2[3]*z[3]
		p2 = c(p11, p12, p13)
		pp[i,] = p2
		u2 = uu[i,]
		w2 = dwp[i]
		x2 = xx[i,]
	}
	res = pp
}

