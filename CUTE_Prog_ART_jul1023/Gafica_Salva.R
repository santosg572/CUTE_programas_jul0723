Gafica_Salva <- function(tg=0, xg=0, graf = 0){
	if (graf == 1){	
		fig_nametX1 = paste(fig, prefijo, '_', jj,'_tX1_',kk, '.jpeg', sep='')
		jpeg(file = file.path(pat, prefijo,fig_nametX1))
		par(mai=c(1,1,.5,.5))
		plot(tg, xg, type='l',xlab='t', ylab='X1',cex.axis=2, cex.lab=2)
		dev.off()
	} else {
		plot(tg, xg, type='l',xlab='t', ylab='X1',cex.axis=2, cex.lab=2)
	}
}