# oem.R - DESC
# /oem.R

# Copyright European Union, 2019
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

# INSPECT perfect.oem

perfect.oem

# DEVELOP a biased OEM

biased.oem <- function(stk, deviances, observations, args, tracking){
  
  dataYears <- args$y0:args$dy
	mxy <- ac(max(dataYears))
	say <- args$ay

  # OBSERVE stock

  stk0 <- window(stk, end=max(dataYears))

  # ADD bias in catch.n

  catch.n(stk0) <- catch.n(stk0) %*% FLQuant(seq(0.6, 1, length=6), dim=c(6,1))
  catch.n(observations$stk)[,mxy] <- catch.n(stk0)[,mxy]

  # indices
		idx0 <- observations$idx
		for (idx_count in 1:length(observations$idx)){
			TS <- mean(range(observations$idx[[idx_count]])[c("startf", "endf")])
			ages <- dimnames(observations$idx[[idx_count]])$age
			i0 <- (stock.n(stk)[,mxy] * exp((-m(stk)[,mxy] - harvest(stk)[,mxy]) * TS))[ages]
			i0 <- i0 * deviances$idx[[idx_count]][,mxy]
			if(any(i0==0)) i0[i0==0] <- min(i0[i0>0])/2
			index(observations$idx[[idx_count]])[,mxy] <- i0
			idx0[[idx_count]] <- observations$idx[[idx_count]][,ac(range(observations$idx[[idx_count]])['minyear']:mxy)]
		}
	
  list(stk=stk0, idx=idx0, observations=observations, tracking=tracking)
}

# ALTER data-rich oem
load("mse_setup.RData")
method(oem) <- biased.oem


