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

my.est <- function (stk, idx, args, tracking, ...){

	browser()
	# code that runs a stock assessment, creates a new stock object with the estimations and updates the tracking matrix 

	# output must have a stock object
	list(stk = stk.est, tracking = tracking)
}

#--------------------------------------------------------------------
# run
#--------------------------------------------------------------------

ctrl <- mpCtrl(list(
	est = mseCtrl(method=my.est),
	phcr = mseCtrl(method=movingF.phcr, args=list(interval=5, frp='msy')),
	hcr = mseCtrl(method=movingF.hcr),
	isys = mseCtrl(method=tac.is)))

mp(stk.om, oem, iem, ctrl=ctrl, args=mpargs)

#====================================================================
# assignment
# groups of 3
# 1 coder, 1 raporteur, 1 modeler
#====================================================================

#--------------------------------------------------------------------
# (1)
# develop a biased assessment
# hint: simulate the stock assessment
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# (2)
# develop a separable assessment
# hint: use a4a
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# (3)
# develop a XSA assessment
# hint: aaarrrggghhhhhhhhh
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# (4)
# develop a model ensemble assessment
# hint: use a4a fmodels 
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# (5)
# develop a production model assessment
# hint: will need to change the OEM and HCR also 
#--------------------------------------------------------------------

