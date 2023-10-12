
data <- read.table("/data/106/wna3/xmm/pg1244/events/1flx_lag_300_700_1200_4000.dat")




# -------------------------------------------


## make nice plot of psd and best model fit

  #png("bayes_fit.png", width = 960, height = 960)
  postscript("/xpc9_data1/data/wna/R/bayes/bayes_fit.ps", family="Times",
		horizontal=TRUE, height=7, pointsize=10)

  par(cex=1.5, lwd=1.5, ps=15, las=1)



      layout(matrix(c(1,2)), heights=c(1.5,1))

      par(oma=c(0,3,3,2))
      par(mar=c(1,6,0,0))
      par(mgp=c(4,1,0))

  

      x.plot <- c(x[1]-dx/2,x)

      y.plot <- model(theta.map, x.plot, mod=mod)


## this is the good plot!!
     ylab <- expression(paste("Power density [(rms/mean)"^2, " Hz"^-1, "]"))

      plot(x-dx/2, y, log="xy", ylim=c(4e-3,8e2), type="s", xlab="", ylab=ylab, xaxt="n",
		bty="l", cex.lab=1.2)

      axis(1, labels=FALSE)

      lines(x.plot, y.plot, lty=1, lwd=4, col="red")



      par(mar=c(6,6,0,0))

      plot(x-dx/2, rat/2, log="xy", ylim=c(1e-3,10), type="s", xlab="Frequency", 
		ylab="data/model", bty="l", cex.lab=1.2)

      lines(range(x)-dx/2,c(1,1), lwd=4, col="red")
  dev.off(which = dev.cur())


# -------------------------------------------
## nice plot of LRT

  postscript("/xpc9_data1/data/wna/R/bayes/bayes_lrt.ps", family="Times",
		horizontal=TRUE, height=7, pointsize=10)

  par(cex=1.5, lwd=1.5, ps=15, las=1)

 #     par(oma=c(1,1,1,1))
##order: bottom, left, top, and right
 #     par(mar=c(2,6,0,0))
##i.e. xlab and ylab in plot
  #    par(mgp=c(2,1,0))

        par(oma=c(1,2,2,2))
##order: bottom, left, top, and right
     par(mar=c(5,6,3,2))
##The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
      par(mgp=c(3.5,1,0))

  a<-lrt.obs
  ptext <- expression(paste("p = ", a))
  r <- hist(lrt.sim, prob=TRUE, main="", xlab="LRT statistic", ylab="Density", col="blue")
  md <- max(r$density) * 0.75
  abline(v=lrt.obs, lwd=4)
  text(lrt.obs, md, ptext, pos=4)


  dev.off(which = dev.cur())



# -------------------------------------------
## nice plot of T_R stat

  postscript("/xpc9_data1/data/wna/R/bayes/bayes_T.ps", family="Times",
		horizontal=TRUE, height=7, pointsize=10)

  par(cex=1.5, lwd=1.5, ps=15, las=1)


  xlab <- expression("T"[R], " statistic")
  a<-T.obs
  ptext <- expression(paste("p = ", a))
#  hist(T.sim, prob=TRUE, main="", xlim=c(-5,50), xlab=xlab, ylab="Density", col="blue")
  r <- hist(T.sim, prob=TRUE, main="", xlab="test", ylab="Density", col="blue")
  md <- max(r$density) * 0.75
  abline(v=T.obs, lwd=4)
  text(T.obs, md, ptext, pos=4)



  dev.off(which = dev.cur())







# end of BAYES plot routine



  }







