SalvaGrafica <- function(x=0, y=0, pat1='', fig1='', prefijo1='', jj1=0, kk1 =0, xl1='', yl1=''){
	fig_nametX1 = paste(fig1, prefijo1, '_', jj1,'_',xl1,yl1,'_',kk1, '.jpeg', sep='')
	jpeg(file = file.path(pat1, prefijo1, fig_nametX1))
	par(mai=c(1,1,.5,.5))
	plot(x, y, type='l',xlab=xl1, ylab=yl1,cex.axis=2, cex.lab=2)
	dev.off()
}

