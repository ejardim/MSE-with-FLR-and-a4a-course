#####################################################################
# MSE Course
# Writing your own functions for mp() (session 01, day03)
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
# API
#====================================================================

#--------------------------------------------------------------------
# foo function
#--------------------------------------------------------------------

foo <- function(...){
	browser()
	# args <- list(...)
	# names(args)
}

#--------------------------------------------------------------------
# estimator (est)
#--------------------------------------------------------------------

ctrl <- mpCtrl(list(est = mseCtrl(method=foo)))
mp(stk.om, oem, iem, ctrl=ctrl, args=mpargs)

#--------------------------------------------------------------------
# parametrization of the HCR (phcr)
#--------------------------------------------------------------------

ctrl <- mpCtrl(list(phcr = mseCtrl(method=foo)))
mp(stk.om, oem, iem, ctrl=ctrl, args=mpargs)

#--------------------------------------------------------------------
# HCR (hcr)
#--------------------------------------------------------------------

ctrl <- mpCtrl(list(hcr = mseCtrl(method=foo)))
mp(stk.om, oem, iem, ctrl=ctrl, args=mpargs)

#--------------------------------------------------------------------
# implementation system (is)
#--------------------------------------------------------------------

ctrl <- mpCtrl(list(isys = mseCtrl(method=foo)))
mp(stk.om, oem, iem, ctrl=ctrl, args=mpargs)

#--------------------------------------------------------------------
# technical measures (tm)
#--------------------------------------------------------------------

ctrl <- mpCtrl(list(tm = mseCtrl(method=foo)))
mp(stk.om, oem, iem, ctrl=ctrl, args=mpargs)

#--------------------------------------------------------------------
# OM projections
#--------------------------------------------------------------------

stk.om2 <- stk.om
projection(stk.om2) <- mseCtrl(method=foo)
mp(stk.om2, oem, iem, ctrl=mpCtrl(), args=mpargs)

#--------------------------------------------------------------------
# OEM
#--------------------------------------------------------------------

oem2 <- oem
method(oem2) <- foo
mp(stk.om, oem2, iem, ctrl=mpCtrl(), args=mpargs)

#--------------------------------------------------------------------
# IEM
#--------------------------------------------------------------------

iem2 <- iem
method(iem2) <- foo
mp(stk.om, oem, iem2, ctrl=mpCtrl(), args=mpargs)






