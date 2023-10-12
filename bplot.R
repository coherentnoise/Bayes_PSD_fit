
## plot stat reults from bayes.R

bplot <- function(ps=TRUE) {

  nbreaks <- 20

# -------------------------------------------
### LRT

   p.lrt <- read.table(file = "/xpc9_data1/data/wna/R/bayes/res_plrt.dat")
   lrt.obs <- read.table(file = "/xpc9_data1/data/wna/R/bayes/res_lrtobs.dat")
   lrt.sim <- as.vector(read.table(file = "/xpc9_data1/data/wna/R/bayes/res_lrtsim.dat"))
   lrt.sim <- lrt.sim$V1

### T_R stat

   p.T <- read.table(file = "/xpc9_data1/data/wna/R/bayes/res_pT.dat")
   T.obs <- read.table(file = "/xpc9_data1/data/wna/R/bayes/res_Tobs.dat")
   T.sim <- read.table(file = "/xpc9_data1/data/wna/R/bayes/res_Tsim.dat")
   T.sim <- T.sim$V1

### T_SSE stat

   p.sse <- read.table(file = "/xpc9_data1/data/wna/R/bayes/res_psse.dat")
   sse.obs <- read.table(file = "/xpc9_data1/data/wna/R/bayes/res_sseobs.dat")
   sse.sim <- read.table(file = "/xpc9_data1/data/wna/R/bayes/res_ssesim.dat")
   sse.sim <- sse.sim$V1

# -------------------------------------------
## nice plot of LRT

  postscript("/xpc9_data1/data/wna/R/bayes/bayes_lrt.ps", family="Times",
		horizontal=TRUE, height=7, pointsize=10)

     par(cex=1.5, lwd=1.5, ps=15, las=1)
##order: bottom, left, top, and right
     par(oma=c(1,2,2,2))
     par(mar=c(5,7,3,2))
##The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
     par(mgp=c(3.5,1,0))



  ptext <- paste("p = ", p.lrt)

  take <- (lrt.sim > 0)
  lrt.sim <- lrt.sim[take]
  take <- (lrt.sim < 50)
  lrt.sim <- lrt.sim[take]


  r <- hist(lrt.sim, breaks=nbreaks, prob=TRUE, main="", xlab="LRT statistic", ylab="Density", col="blue")
  md <- max(r$density) * 0.75
  abline(v=lrt.obs, lwd=4)
  text(11, md, ptext, pos=4)


  dev.off(which = dev.cur())



# -------------------------------------------
## nice plot of T_R stat

  postscript("/xpc9_data1/data/wna/R/bayes/bayes_T.ps", family="Times",
		horizontal=TRUE, height=7, pointsize=10)

     par(cex=1.5, lwd=1.5, ps=15, las=1)
##order: bottom, left, top, and right
     par(oma=c(1,2,2,2))
     par(mar=c(5,7,3,2))
##The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
     par(mgp=c(3.5,1,0))



  xlab <- expression(paste("T"[R], " statistic"))
  ptext <- paste("p = ", p.T)
  r <- hist(T.sim, breaks=nbreaks, prob=TRUE, main="", xlab=xlab, ylab="Density", col="blue")
  md <- max(r$density) * 0.75
  abline(v=T.obs, lwd=4)
  text(T.obs, md, ptext, pos=4)



  dev.off(which = dev.cur())


# -------------------------------------------
## nice plot of SSE stat

  postscript("/xpc9_data1/data/wna/R/bayes/bayes_sse.ps", family="Times",
		horizontal=TRUE, height=7, pointsize=10)

     par(cex=1.5, lwd=1.5, ps=15, las=1)
##order: bottom, left, top, and right
     par(oma=c(1,2,2,2))
     par(mar=c(5,7,3,2))
##The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
     par(mgp=c(4,1,0))



  xlab <- expression(paste("T"[SSE], " statistic"))
  ptext <- paste("p = ", p.sse)
  r <- hist(sse.sim, breaks=nbreaks, prob=TRUE, main="", xlab=xlab, ylab="Density", col="blue")
  md <- max(r$density) * 0.75
  abline(v=sse.obs, lwd=4)
  text(sse.obs, md, ptext, pos=4)



  dev.off(which = dev.cur())

# -------------------------------------------
# end of BAYES plot routine



  }



