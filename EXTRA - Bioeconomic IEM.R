#####################################################################
# MSE Course
# Estimator explained hands-on (session 03, day03)
# 20191119 (EJ)
#####################################################################

#====================================================================
# libraries, code and input data
#====================================================================
library(FLa4a)
library(mse)
library(FLAssess)
library(ggplotFL)
library(FLBRP)
library(doParallel)
load("mse_setup.RData")
load("datarich.RData")

#--------------------------------------------------------------------
# generic arguments to be passed to the MP
#--------------------------------------------------------------------

mpargs <- list(fy=fy, y0=y0, iy=iy, nsqy=nsqy, seed = 1234)

#====================================================================
# estimators
#====================================================================

#--------------------------------------------------------------------
# bio-economic iem
#--------------------------------------------------------------------

my.iem <- function (ctrl, args, tracking, price, income_lim, ...){
	v <- ctrl@trgtArray[,'val',,drop=FALSE]*price<income_lim
	ctrl@trgtArray[v] <- income_lim/price
	list(ctrl = ctrl, tracking = tracking)
}

iem2 <- FLiem(method=my.iem, args=list(price=2.53, income_lim=250000))

#--------------------------------------------------------------------
# run
#--------------------------------------------------------------------

ctrl <- mpCtrl(list(
	est = mseCtrl(method=sca.sa),
	phcr = mseCtrl(method=movingF.phcr, args=list(interval=5, frp='msy')),
	hcr = mseCtrl(method=movingF.hcr),
	isys = mseCtrl(method=tac.is)))

cl <- makeCluster(10)
clusterEvalQ(cl = cl, expr = {library(FLa4a)})
registerDoParallel(cl)
res.be <- mp(stk.om, oem, iem2, ctrl=ctrl, args=mpargs)
stopCluster(cl)

plot(stk.om, bioecon=res.be)


