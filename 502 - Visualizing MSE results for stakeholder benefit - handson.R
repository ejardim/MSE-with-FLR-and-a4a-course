# mseviz.R - DESC
# /mseviz.R

# Copyright European Union, 2019
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mseviz)
library(mse)

# LOAD performance table, mse runs and OM.

load('perf.RData')
load('mse_setup.RData')
load('datarich.RData')
load('datalimited.RData')

# BOXPLOTS of selected indicators

plotBPs(perf, indicators=c("C", "FMSY", "SBMSY"),
  target=list(FMSY=1), limit=c(SBMSY=0.20))

# TRADE-OFF plots across all indicators

plotTOs(perf)

# TRADE-OFF plots across all indicators

plotTOs(perf, x="C", y=c("FMSY", "Ftarget", "SBMSY"))

# KOBE plot of performance

kobeMPs(perf, x="SBMSY", y="FMSY")

# RUNS over three indicators

# SSB

plotOMruns(window(ssb(stock(stk.om)), end=2015),
  FLQuants(DR=ssb(stock(res.dr)),
  DL=ssb(stock(res.dr))))

# F

plotOMruns(window(fbar(stock(stk.om)), end=2015),
  FLQuants(DR=fbar(stock(res.dr)),
  DL=fbar(stock(res.dr))))

# CATCH

plotOMruns(window(catch(stock(stk.om)), end=2015),
  FLQuants(DR=catch(stock(res.dr)),
  DL=catch(stock(res.dr))))

# SUMMARY table

summTable(perf)

# THINK about other plots you might want to see

# - TOs across other indicators

# - COMPARISON in the short, medium and long term (needs new perf)

