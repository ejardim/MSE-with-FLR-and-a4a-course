# hcr.R - DESC
# /hcr.R

# Copyright European Union, 2019
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# ices.hcr

ices.hcr <- function(stk, fmin, ftrg, blim, bsafe, args, tracking){
	ay <- args$ay
	ssb_lag <- ifelse(is.null(args$ssb_lag), 1, args$ssb_lag)
	# rule
	ssb <- ssb(stk)[, ac(ay-ssb_lag)]
	fout <- FLQuant(fmin, dimnames=list(iter=dimnames(ssb)$iter))
	fout[ssb >= bsafe] <- ftrg
	inbetween <- (ssb < bsafe) & (ssb > blim)
	gradient <- (ftrg - fmin) / (bsafe - blim)
	fout[inbetween] <- (ssb[inbetween] - blim) * gradient + fmin
	# create control file
	ctrl <- getCtrl(c(fout), "f", ay+args$management_lag, dim(fout)[6])
	# return
	list(ctrl=ctrl, tracking=tracking)
}

# movingF.hcr

movingF.hcr <- function(stk, hcrpars, args, tracking){
	ay <- args$ay
	# rule 
	if(!is(hcrpars, "FLQuant"))
    hcrpars <- FLQuant(hcrpars, dimnames=list(iter=dimnames(stk@catch)$iter))
	
  # create control file
	ctrl <- getCtrl(c(hcrpars), "f", ay+args$management_lag, dim(hcrpars)[6])
	
  # return
	list(ctrl=ctrl, tracking=tracking)
}

# moving.phcr

movingF.phcr <- function(stk, frp="f0.1", model="missing", interval, args, hcrpars, tracking) {
	ay <- args$ay
	iy <- args$iy
	if(ay==iy | (ay-iy)%%interval==0){
		if(!missing(model)){
			sr0 <- fmle(as.FLSR(stk, model=model))
			hcrpars <- c(refpts(brp(FLBRP(stk, sr0)))[frp,"harvest"])
		} else {
			hcrpars <- c(refpts(brp(FLBRP(stk)))[frp,"harvest"])
		}
	}
	list(hcrpars=hcrpars, tracking=tracking)	
}

