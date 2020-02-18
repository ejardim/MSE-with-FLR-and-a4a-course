#####################################################################
# MSE Course
# Estimator explained hands-on (session 03, day03)
# 20191119 (EJ)
#####################################################################

#====================================================================
# libraries, code and input data
#====================================================================

rm(list = ls())
library(FLa4a)
library(mse)
library(FLAssess)
library(ggplotFL)
library(FLBRP)
library(doParallel)

#--------------------------------------------------------------------
# data and preprocessing
#--------------------------------------------------------------------
attach("cod.RData")
stk <- stk
idxs <- idxs
detach()

#--------------------------------------------------------------------
# MSE setup
#--------------------------------------------------------------------

it <- 25 # iterations
y0 <- range(stk)["minyear"] # initial OM year
dy <- range(stk)["maxyear"] # final OM year
fy <- dy + 2 # final year
iy <- dy  # initial year of projection (also intermediate)
nsqy <- 3 # number of years to compute status quo metrics
vy <- ac(iy:fy) # vector of years to be projected

#====================================================================
# OM conditioning
#====================================================================

#--------------------------------------------------------------------
# Assessment
#--------------------------------------------------------------------
# submodels
fmod <- ~te(age, year, k = c(3, 25)) 
fit <- sca(stk, idxs, fmodel=fmod)
fit <- simulate(fit, it)
stk <- stk + fit
plot(stk)

#--------------------------------------------------------------------
# S/R
# use medians to avoid fitting to each iter
#--------------------------------------------------------------------

stk0 <- qapply(stk, iterMedians)
gsr  <- as.FLSR(stk0, model = "bevholt")
gsr <- fmle(gsr)

#--------------------------------------------------------------------
# Set residuals for the projections period using residuals sd for
# rlnorm
#--------------------------------------------------------------------
res_gsr <- window(rec(stk), end=fy)
res_gsr <- rlnorm(res_gsr, 0, sd(residuals(gsr)))
residuals(gsr) <- res_gsr

#--------------------------------------------------------------------
# reference points
#--------------------------------------------------------------------
brp_gsr <- brp(FLBRP(stk0, gsr))

#--------------------------------------------------------------------
# extend object for fwd, set up future assumptions - means of nsqy years
#--------------------------------------------------------------------
stk <- stf(stk, fy-dy, nsqy, nsqy)

#--------------------------------------------------------------------
# build OM object
#--------------------------------------------------------------------
# set projection method for OM
proj <- mseCtrl(method=fwd.om, args=list(maxF=3))
# build object
stk.om <- FLom(stock=stk, sr=gsr, refpts=refpts(brp_gsr), projection=proj)#, fleetBehaviour=fb)

#====================================================================
# OEM
#====================================================================
#--------------------------------------------------------------------
# deviances for indices using q estimated by the model
#--------------------------------------------------------------------
idcs <- FLIndices()
for (i in 1:length(idxs)){
	i.q0 <- predict(fit)$qmodel[[i]]
	i.q <- window(i.q0, end=fy)
	i.q[,ac((iy):fy)] <- i.q[,ac(dy)]
	i.fit <- window(index(fit)[[i]], end=fy) 
	idx_temp <- FLIndex(index=i.fit, index.q=i.q) 
	range(idx_temp)[c("startf", "endf")] <- range(idxs[[i]])[6:7]
	idcs[[i]] <- idx_temp
}
names(idcs) <- names(idxs)

#--------------------------------------------------------------------
# deviances for catches
#--------------------------------------------------------------------

set.seed(0)
# build log residuals
catch.dev <- log(catch.n(stk))
catch.dev <- catch.dev-iterMeans(catch.dev)
# compute varcov for multivariate normal randomization
Sig <- apply(catch.dev[,ac(y0:dy),1,1,,drop=TRUE], 3, function(x) cov(t(x)))
Sig <- apply(Sig, 1, mean)
Sig <- matrix(Sig, ncol=dim(catch.dev)[1])
# randomize
catch.dev[,ac(vy)][] <- t(mvrnorm((it) * length(vy), rep(0, nrow(Sig)), Sig))
# exponentiate for OEM object
catch.dev <- exp(catch.dev)

#--------------------------------------------------------------------
# build OEM object
#--------------------------------------------------------------------

idxDev <- lapply(idcs, index.q)
names(idxDev) <- c("index.q", "index.q")
stkDev <- FLQuants(catch.n=catch.dev)

# deviances
dev <- list(idx=idxDev, stk=stkDev)
# observations
# WARNING: note we're selecting one index only
obs <- list(idx=idcs, stk=stk)

# OEM
oem <- FLoem(method=sampling.oem, observations=obs, deviances=dev)

#====================================================================
# IEM
# not much to say, just assuming our own ignorance ...
#====================================================================
#iem <- FLiem(method=noise.iem, args=list(fun="rlnorm", mean=0, sd=0.2, multiplicative=TRUE))

#====================================================================
# MP
#====================================================================

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
	stk0 <- stk
	fit <- sca(stk, idx, 
		fmodel = ~s(age, k=3) + s(year, k=10))
	stk <- stk + fit
	save(fit, stk, stk0, file=paste0('fit', args$ay, '.RData'))
	# output must have a stock object
	list(stk = stk, tracking = tracking)
}

#--------------------------------------------------------------------
# run
#--------------------------------------------------------------------

ctrl <- mpCtrl(list(est = mseCtrl(method=my.est)))
res <- mp(stk.om, oem, ctrl=ctrl, args=mpargs)

load('fit2016.RData')

fls <- FLStocks(om=window(stock(stk.om), end=2016), omproject=window(stock(res), end=2017), fit=stk, oem=stk0)

plot(fls)

flqs <- lapply(fls, harvest)

wireframe(data~year+age|qname, data=as.data.frame(flqs))


