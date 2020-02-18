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
# function skeleton
#--------------------------------------------------------------------

my.iem <- function (ctrl, args, tracking, ...){

	# code that runs a stock assessment, creates a new stock object with the estimations and updates the tracking matrix 
	
	browser()

	# output must have a fwdControl object
	list(ctrl = ctrl, tracking = tracking)
}

#--------------------------------------------------------------------
# run
#--------------------------------------------------------------------

iem2 <- FLiem(method=my.iem)
mp(stk.om, oem, iem2, ctrl=mpCtrl(), args=mpargs)

#====================================================================
# assignment
# groups of 3
# 1 coder, 1 raporteur, 1 modeler
#
# IEM used in the cod example
# iem <- FLiem(method=noise.iem, 
#	args=list(fun="rlnorm", mean=0, sd=0.2, multiplicative=TRUE))
#====================================================================

#--------------------------------------------------------------------
# (1)
# develop an IEM that uses alternative distributions
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# (2)
# develop an IEM that accounts for IUU
# hint: the distribution mean doesn't need to be 0 
#--------------------------------------------------------------------


