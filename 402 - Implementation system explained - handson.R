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

#--------------------------------------------------------------------
# generic arguments to be passed to the MP
#--------------------------------------------------------------------

mpargs <- list(fy=fy, y0=y0, iy=iy, nsqy=nsqy, seed = 1234)

#====================================================================
# estimators
#====================================================================

#--------------------------------------------------------------------
# estimator function skeleton
#--------------------------------------------------------------------

my.isys <- function (stk, ctrl, args, tracking, ...){
    ay <- args$ay
    nsqy <- args$nsqy
    iy <- args$iy
    mlag <- args$management_lag
    it <- dim(stk)[6]

	browser()
	# code that runs a stock assessment, creates a new stock object with the estimations and updates the tracking matrix 
		
	mult <- tracking['F.est',ac(ay-mlag)]/ctrl@trgtArray[,'val',]
	ctrl <- getCtrl(tracking['metric.fb', ac(ay-mlag)]*mult, "catch", ay + mlag, it)

	# output must have a fwdControl object
	list(ctrl = ctrl.is, tracking = tracking)
}

#--------------------------------------------------------------------
# run
#--------------------------------------------------------------------

ctrl <- mpCtrl(list(
	est = mseCtrl(method=sca.sa),
	phcr = mseCtrl(method=movingF.phcr, args=list(interval=5, frp='msy')),
	hcr = mseCtrl(method=movingF.hcr),
	isys = mseCtrl(method=my.isys)))

mp(stk.om, oem, iem, ctrl=ctrl, args=mpargs)

#====================================================================
# assignment
# groups of 3
# 1 coder, 1 raporteur, 1 modeler
#====================================================================

#--------------------------------------------------------------------
# (1)
# develop an input control implementation system
# hint: use the function getCtrl
# hint: use changes in F to mimic changes in effort or capacity
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# (2)
# develop an output control implementation system
# hint: use the function getCtrl
# hint: use changes in F to scale catch for next year
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# (3)
# develop an mixed control implementation system
#--------------------------------------------------------------------


