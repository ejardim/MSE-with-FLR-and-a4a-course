#####################################################################
# MSE Course
# The a4a framework elements and classes (session 05, day01)
# Tracking decisions with the 'tracking' matrix
# 20191117 (EJ)
#####################################################################

#====================================================================
# libraries, code and input data
#====================================================================
load("datarich.RData")
library(mse)
library(ggplotFL)
library(reshape2)

facunsorted <- function(x) factor(x, levels=as.character(unique(x)))

#====================================================================
# The tracking matrix
#====================================================================

trk_flq <- tracking(res.dr)

#====================================================================
# Diagnostics
#====================================================================
#--------------------------------------------------------------------
# Convergence of MP's estimator
#--------------------------------------------------------------------
# fraction of non-convergence
noconv_perc <- iterMeans(trk_flq['conv.est']>1)

# which iters and years didn't converge
df0 <- as.data.frame(trk_flq['conv.est']>1)
noconv_iter <- subset(df0, data==TRUE)[,c('iter', 'year')]

#--------------------------------------------------------------------
# Check decision is being implemented properly
# Note this example is an output system so check is on catch
#--------------------------------------------------------------------
impl_diagnostic <- trk_flq['metric.fb',-ncol(trk_flq)]/trk_flq['C.om',-1]
all.equal(c(impl_diagnostic), rep(1,length(impl_diagnostic)))

#--------------------------------------------------------------------
# Assessment bias
#--------------------------------------------------------------------
ass_bias <- trk_flq[c('F.est', 'B.est'),-1]/trk_flq[c('F.om', 'B.om'),-ncol(trk_flq)]
bwplot(data~factor(year)|metric, data=ass_bias)
xyplot(data~factor(year)|metric, group=iter, data=iter(ass_bias, 1), type='l')

#--------------------------------------------------------------------
# Implementation error model effect
#--------------------------------------------------------------------
iem_effect <- trk_flq[c('metric.is', 'metric.iem')]
bwplot(data~facunsorted(metric)|year, data=iem_effect)

#====================================================================
# Performance evaluation
#====================================================================
#--------------------------------------------------------------------
# Overview of changes
#--------------------------------------------------------------------
flq <- trk_flq[,-1]/trk_flq[,-ncol(trk_flq)]
df0 <- subset(as.data.frame(flq), metric!='conv.est')
xyplot(facunsorted(df0$metric)~data|year, data=df0, type='l', groups=iter)
bwplot(facunsorted(df0$metric)~data|year, data=df0)

#--------------------------------------------------------------------
# Intended management results
#--------------------------------------------------------------------
mng_objective <- trk_flq['F.om',-1]/trk_flq['metric.hcr',-ncol(trk_flq)]
bwplot(data~factor(year), data=mng_objective)
xyplot(data~factor(year), group=iter, data=mng_objective, type='l')


