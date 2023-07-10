library(devEMF)

  # open file "bar.emf" for graphics output
  emf("bar.emf")
  # produce the desired graph(s)
 x = seq(0, 4*pi, .1)
 y = x*cos(x)
 #par(mar = c(1, 1, 1, 1) + 0.1)
    plot(x,y)
  dev.off() #tur 
