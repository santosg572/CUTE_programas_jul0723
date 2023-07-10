rm(list=ls())

source('CreaFolder.R')
source('SolucionXPU_3_salto.R')
source('SolucionPPU_3_salto.R')
source('funciones_modelos_A.R')
source('SalvaGrafica.R')
source('funciones_M.R')
   
beta1 =.2
delta1 = .2
epsilon1 = .2

del = .001
x2 = 60

npun = GenNP(0, x2, del)
cat(' npun: ', npun,'\n')

tiempo = seq(0, x2, length.out = npun)
del = tiempo[2]-tiempo[1]

cat(' del: ', del,'\n')

parpois = 0.4
#TETA = 0.001
#TETA = 0.01
# TETA = 0.1 #bien
TETA = .15

ss = funcion_poisson(60, parpois)
tt = as.vector(ss[[1]])

res = EncIndices(tiempo, tt)
II = as.vector(res[[2]])
print(II)

TT = as.vector(res[[1]])
npun = length(TT)
cat(' npun: ', npun,'\n')

cc = rep(0, npun)
pp =  matrix(rep(c(1, 0, 0), c(npun, npun, npun)), ncol=3)

param = matrix(c(.15, .15, .15, 12, .15, .2, .25, 26, .01,.01,.1,12, .04,.01,.15,24, .07,.03,.05,18, 0, 0, 0, 25), ncol=6)

m = 1
DWX1 = param[1,m]
DWX2 = param[2,m]
DWX3 = param[2,m]
DWP =  param[3,m]
kk =   param[4,m]


A1 <<- .4
B1 <<- .4
C1 <<- .2

fig        = 'fig_'
prefijo = paste('fac','_dwx1_',DWX1,'_dwx2_', DWX2,'_dwp_',DWP,':b', beta1,'d', delta1,'e',epsilon1, sep='')
pat      = getwd()

CreaFolder(prefijo)

for (jj in 1:15){
	cat('iter= ', jj, '\n')
	
	R <- SolucionXPU_3_salto(del, pp)
	
	if (jj == jj){
        SalvaGrafica(TT, R$xx[,1], pat, fig, prefijo, jj,kk,'t', 'X1')
        SalvaGrafica(TT, R$xx[,2], pat, fig, prefijo, jj,kk,'t', 'X2')
		SalvaGrafica(TT, R$xx[,3], pat, fig, prefijo, jj,kk,'t', 'X3')
		
		library("scatterplot3d")
		
		fig_namet3D = paste(fig, prefijo, '_', jj,'_3D_',kk, '.jpeg', sep='')
		jpeg(file = file.path(pat, prefijo,fig_namet3D))
		par(mai=c(1,1,.5,.5))
		scatterplot3d(R$xx[,1], R$xx[,2], R$xx[,3], type='l', box=F, lwd=3, color='blue', main=prefijo)
		dev.off()

		SalvaGrafica(R$xx[,1], R$xx[,2], pat, fig, prefijo, jj,kk,'X1', 'X2')
		SalvaGrafica(R$xx[,2], R$xx[,3], pat, fig, prefijo, jj,kk,'X2', 'X3')
		SalvaGrafica(R$xx[,1], R$xx[,3], pat, fig, prefijo, jj,kk,'X1', 'X3')
		
	    SalvaGrafica(TT, R$uu[,1], pat, fig, prefijo, jj,kk,'t', 'U1')
	    SalvaGrafica(TT, R$uu[,2], pat, fig, prefijo, jj,kk,'t', 'U2')
	    SalvaGrafica(TT, R$uu[,3], pat, fig, prefijo, jj,kk,'t', 'U3')	    
	}
	
	pp <- SolucionPPU_3_salto(del, R$xx, R$uu)	

	if (jj == jj){
		SalvaGrafica(TT, pp[,1], pat, fig, prefijo, jj,kk,'t', 'P1')
		SalvaGrafica(TT, pp[,2], pat, fig, prefijo, jj,kk,'t', 'P2')
		SalvaGrafica(TT, pp[,3], pat, fig, prefijo, jj,kk,'t', 'P3')
	}	
}
