rm(list=ls())

args <- commandArgs(trailingOnly = TRUE)
print(args)
print(typeof(args))

if (length(args) == 3){
	beta1      <<- as.numeric(args[1])
	delta1     <<- as.numeric(args[2]) 
	epsilon1 <<- as.numeric(args[3]) 
} else {
	beta1 =.2
	delta1 = .2
	epsilon1 = .2
}

# lo principal que cambie del programa anterior fue que
# el contro -u- lo reemplace por -p- al inicio del programa

source('CreaFolder.R')
source('SolucionXPU_3.R')
source('SolucionPPU_3.R')
source('funciones_modelos_A.R')
source('SalvaGrafica.R')
   
fac= 12

npun = round(fac *5001)
tiempo = seq(0,fac*5, length.out = npun)
del = tiempo[2]-tiempo[1]

cc = rep(0, npun)
pp =  matrix(rep(c(1, 0, 0), c(npun, npun, npun)), ncol=3)

param = matrix(c(.15, .15, .15, 12, .1,.1,.1,26, .01,.01,.1,12, .04,.01,.15,24, .07,.03,.05,18, 0, 0, 0, 25), ncol=6)

m = 6
dwx1 = param[1,m]
dwx2 = param[2,m]
dwx3 = param[2,m]
dwp =  param[3,m]
kk =   param[4,m]


A1 <<- .4
B1 <<- .4
C1 <<- .2

fig        = 'fig_'
prefijo = paste('fac_',fac,'_dwx1_',dwx1,'_dwx2_',dwx2,'_dwp_',dwp,':b', beta1,'d', delta1,'e',epsilon1, sep='')
pat      = getwd()

CreaFolder(prefijo)

for (jj in 1:25){
	cat('iter= ', jj, '\n')
	
	R <- SolucionXPU_3(del, pp, wx1=dwx1, wx2=dwx2, wx3=dwx3)
	
	if (jj == jj){
        SalvaGrafica(tiempo, R$xx[,1], pat, fig, prefijo, jj,kk,'t', 'X1')
        SalvaGrafica(tiempo, R$xx[,2], pat, fig, prefijo, jj,kk,'t', 'X2')
		SalvaGrafica(tiempo, R$xx[,3], pat, fig, prefijo, jj,kk,'t', 'X3')
		
		library("scatterplot3d")
		
		fig_namet3D = paste(fig, prefijo, '_', jj,'_3D_',kk, '.jpeg', sep='')
		jpeg(file = file.path(pat, prefijo,fig_namet3D))
		par(mai=c(1,1,.5,.5))
		scatterplot3d(R$xx[,1], R$xx[,2], R$xx[,3], type='l', box=F, lwd=3, color='blue', main=prefijo)
		dev.off()

		SalvaGrafica(R$xx[,1], R$xx[,2], pat, fig, prefijo, jj,kk,'X1', 'X2')
		SalvaGrafica(R$xx[,2], R$xx[,3], pat, fig, prefijo, jj,kk,'X2', 'X3')
		SalvaGrafica(R$xx[,1], R$xx[,3], pat, fig, prefijo, jj,kk,'X1', 'X3')
		
	    SalvaGrafica(tiempo, R$uu[,1], pat, fig, prefijo, jj,kk,'t', 'U1')
	    SalvaGrafica(tiempo, R$uu[,2], pat, fig, prefijo, jj,kk,'t', 'U2')
	    SalvaGrafica(tiempo, R$uu[,3], pat, fig, prefijo, jj,kk,'t', 'U3')	    
	}
	
	pp <- SolucionPPU_3(del, R$xx, R$uu, wp=dwp)	

	if (jj == jj){
		SalvaGrafica(tiempo, pp[,1], pat, fig, prefijo, jj,kk,'t', 'U1')
		SalvaGrafica(tiempo, pp[,2], pat, fig, prefijo, jj,kk,'t', 'U2')
		SalvaGrafica(tiempo, pp[,3], pat, fig, prefijo, jj,kk,'t', 'U3')
	}	
}
