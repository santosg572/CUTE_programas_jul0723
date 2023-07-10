SolucionXPU_3_salto <- function(del=0, pp=0){
   ss = dim(pp)

   n = ss[1]
   
   xx = matrix(rep(0,3*n), ncol=3)

   dwx1 = DWX1 *sqrt(del)*runif(n)
   dwx2 = DWX2 *sqrt(del)*runif(n)
   dwx3 = DWX3 *sqrt(del)*runif(n)
   
   	x1 = c(.7, .7,.5)
   	xx[1,] = x1
	p1 = pp[1,]
	w1 = dwx1[1]
	w2 = dwx2[1]
	w3 = dwx3[1]
	
	u = Calu1u2u3(x1, p1,1)
	
   	for (i in 2:n){
   		r = funX_ART2 (x1, u)
   		z = c(0,0,0)
		if (length(which(i == II)) > 0){
			z = K(rnorm(3))
		}

   		x21 = x1[1] + (TT[i]-TT[i-1]) * r[1] + w1+ x1[1]*u[1]*z[1]
   		x22 = x1[2] + (TT[i]-TT[i-1]) * r[2] + w2+ x1[2]*u[2]*z[2]
   		x23 = x1[3] + (TT[i]-TT[i-1]) * r[3] + w3+ x1[3]*u[3]*z[3]
   		x1 = c(x21, x22, x23)
	  	xx[i,] = x1
		p1 = pp[i,]
		w1 = dwx1[i]
		w2 = dwx2[i]
		w3 = dwx3[i]
		u = Calu1u2u3(x1, p1, i )
   }
   
   uu = matrix(rep(0,3*n), ncol=3)
   
   for (i in 1:n){
   		pi = pp[i,]
   		xi = xx[i,]
        uu[i, ] = Calu1u2u3(xi,pi, i)
   }
   
  
   res = list(xx=xx, uu=uu)
}

