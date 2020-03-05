# quickFLR.R - DESC
# /quickFLR.R

# Copyright European Union, 2019
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(FLCore)

# --- FLQuant

# CREATE

FLQuant(matrix(2, ncol=10, nrow=8))

FLQuant(rnorm(20))

nums <- matrix(unlist(lapply(runif(12, 800, 2000),
  function(x) x * exp(-0.3 * 1:10))), nrow=10, ncol=12)

flq <- FLQuant(nums)

# dimnames and units

dimnames(flq)

units(flq)

flq <- FLQuant(nums, dimnames=list(age=1:10, year=2008:2019), units="1000")

# quant

quant(flq)

# uoms

FLQuant(1286, units="1000") * FLQuant(34.5, units="kg")

FLQuant(1286, units="thousands") * FLQuant(34.5, units="kg")

FLQuant(0.2, units="m") + FLQuant(0.34, units="f")

# subsetting

flq[, 1]

flq[, '2009']

# collapsing

yearMeans(flq)

quantSums(flq)

# inspect

summary(flq)

dims(flq)

dim(flq)


# --- FLStock

data(ple4)

# inspect

summary(ple4)

ple4

# access and replace

stock(ple4)

m(ple4)

m(ple4) <- m(ple4) * 2

m(ple4) <- 0.1

# subset

ple4[, ac(1990:2000)]

window(ple4, start=1990, end=2000)

# calculations

ssb(ple4)

fbar(ple4)


# --- ggplotFL

library(ggplotFL)

# Standard plots

plot(ple4)

plot(catch(ple4))

plot(rnorm(300, catch(ple4), catch(ple4) * 0.3))

# FLQuants

plot(FLQuants(CA=catch(ple4), B=stock(ple4)))

# adding and modifying elements

plot(catch(ple4)) + ylab(paste0("Catch (", units(catch(ple4)), ")"))

# --- Some recent methods

# metrics

metrics(ple4)

metrics(ple4, SSB=ssb, B=stock)

metrics(ple4, SSB=ssb, B=stock, F=function(x) quantMeans(harvest(x)))

# verify

verify(ple4)

# residuals

residuals(flq, flq * 0.90)

rlogstandard(flq, flq * 0.90)

# append

append(window(flq, end=2011), window(flq, start=2012))

# combine

combine(rlnorm(10, log(flq), 0.2), rlnorm(10, log(flq), 0.4))

# update

newstk <- update(ple4, list(catch=catch(ple4) * 10))

# standardUnits

standardUnits(ple4)

stk <- FLStock(catch.n=flq)
units(stk) <- standardUnits(stk)
stk


# --- TUTORIALS

# <http://www.flr-project.org/doc/>
