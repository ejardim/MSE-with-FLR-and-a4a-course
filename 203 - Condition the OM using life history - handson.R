# conditioning_life_history.R - DESC
# /conditioning_life_history.R

# Copyright European Union, 2019
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(FLife)
library(ggplotFL)

# LOAD teleost dataset

data(teleost)

# An FLPar object with 145 spp.

teleost

dimnames(teleost)

# GET cod

codlh <- teleost[, "Gadus morhua"]
codlh['t0'] <- -codlh['t0']

# linf, k, t0: VB growth params
# l50 length at 50% mature
# a, b: scaling factor and exponent of length weight relationship

# GET growth curve

vonB(1:25, codlh)

ggplot(data.frame(age=1:25, len=vonB(1:25, codlh)), aes(x=age, y=len)) +
  geom_line() + geom_point() + xlab("Age") + ylab("Length (cm)")

# CREATE complete set of params

codpars <- lhPar(codlh)

# CHECK weight-at-age
ggplot(data.frame(age=1:25, wt=len2wt(vonB(1:25, codlh), codpars)/1000), aes(x=age, y=wt)) +
  geom_line() + geom_point() + xlab("Age") + ylab("Weight (kg)")

# Selectivity: double normal, sel1=3, sel2=1, sr=45

codpars <- lhPar(codlh, sel1=3, sel2=1, sr=45)

# dnormal: a1, age at maximum; sl, sr: SD for left / right

ggplot(dnormal(1:25, lhPar(codlh, sel1=3, sel2=1, sel3=200)), aes(x=age, y=data)) +
  geom_line() + geom_point() + xlab("Age") + ylab("Selectivity") + ylim(c(0,1))

ggplot(dnormal(1:25, lhPar(codlh, sel1=3, sel2=1, sel3=25)), aes(x=age, y=data)) +
  geom_line() + geom_point() + xlab("Age") + ylab("Selectivity") + ylim(c(0,1))

# Steepness: s=0.9

codpars <- lhPar(codlh, sel1=3, sel2=1, sel3=25, s=0.75)

# Virgin biomass: v=1000

codpars <- lhPar(codlh, sel1=4, sel2=1, sel3=25, s=0.75, v=2e5)

# GENERATE equilibirum population according to pars

eql <- lhEql(codpars, range=c(min=1, max=25, minfbar=3, maxfbar=6, plusgroup=25))

# eql DEFAULT args

# growth = vonB
# m = "gislason"
# mat = logistic
# sel = dnormal
# sr = "bevholt"
# range = c(min = 0,
# spwn = 0
# fish = 0.5
# midyear = 0.5

# gislason's M

m(eql)[,1]

# RETURNS an FLBRP

eql

plot(eql)

# PLOT m, selex, mat and wt at age
ggplot(metrics(eql, list(M=m, Selex=catch.sel, Maturity=mat, Weigth=catch.wt))) +
  geom_line(aes(age, data)) + facet_wrap(~qname, scale="free")+
  scale_x_continuous(limits=c(1, 25))

# CREATE population at all Fbar levels

cod <- as(eql, "FLStock")

plot(fbar(eql))

# POPULATION at F=0

ggplot(stock.n(cod)[, 1], aes(x=age, y=data)) + geom_line() +
  xlab("Age") + ylab("Abundance (1000s)")

# CHOOSE initial F level (not 0, better Fbar=0.03)

cod <- cod[, 2]

cod <- transform(cod, catch.wt=catch.wt/1000, catch.wt=catch.wt/1000,
  landings.wt=landings.wt/1000, discards.wt=discards.wt/1000)

units(cod) <- standardUnits(cod)

# EXTEND stock for 100 y

om <- fwdWindow(cod, eql, end=101)

# GET SRR

srr <- list(model=model(eql), params=params(eql))

# CREATE trajectories and forward

library(FLash)

Pfmsy <- fwd(om, sr=srr,
  control=fwdControl(data.frame(year=3:101, val=c(fmsy(eql)), quantity="f")))

plot(Pfmsy)

# FORECAST with SRR uncertainty

om <- propagate(om, 300)

lndev06 <- rlnorm(300, FLQuant(0, dimnames=list(year=3:101)), 0.6)

Pfmsy <- fwd(om, sr=srr, sr.residuals=lndev06,
  control=fwdControl(data.frame(year=3:101, val=c(fmsy(eql)), quantity="f")))

plot(Pfmsy)

# ADD structural uncertainty on K

simcod <- function(v) {

  pars <- lhPar(codlh, sel1=4, sel2=1, sel3=25, s=0.75, v=v)
  
  eql <- lhEql(pars, range=c(min=1, max=25, minfbar=3, maxfbar=6, plusgroup=25))

  stk <- as(eql, "FLStock")[,2]
  stk <- transform(stk, catch.wt=catch.wt/1000, catch.wt=catch.wt/1000,
  landings.wt=landings.wt/1000, discards.wt=discards.wt/1000)

  units(stk) <- standardUnits(stk)

  stk <- fwdWindow(stk, eql, end=101)
  srr <- list(model=model(eql), params=params(eql))

  return(list(stock=stk, sr=srr))
}

# APPLY for v = U(1e5, 2e5)

sim <- lapply(runif(50, 1e5, 3e5), simcod)

# COMBINE into single FLStock

om <- Reduce(combine, lapply(sim, "[[", "stock"))

# GET srr. SORRY, ugly hack

srr <- list(model=model(eql),
  params=as(Reduce(combine, lapply(sim, function(x)
    as.FLQuant(x$sr$params))), "FLPar"))

# FWD

lndev06 <- rlnorm(50, FLQuant(0, dimnames=list(year=3:101)), 0.6)

omA <- fwd(om, sr=srr, sr.residuals=lndev06,
  control=fwdControl(data.frame(year=3:101, val=c(fmsy(eql)), quantity="f")))

plot(omA)

# NOW, What other uncertainties would you introduce?
