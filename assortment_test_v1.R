# Testing script for harvested assortment implementation in prebas
# based on Vuokila & Väliaho (1980) equations for spruce and pine
# jh, 2021-07-07


# Fortran subroutine in fortsub_v2.f90; if changed, recompile by something like
# R CMD SHLIB fortsub_v2.f90
# in the terminal

library(data.table)
library(ggplot2)

dyn.load("fortsub_v2.so") # fortran subroutine
load("prebRuns_sprucesubs.Rdata") # input (from transectRuns' prebRuns$spruce[[3]])
load("prebRuns_pinesubs.Rdata") # input (from transectRuns' prebRuns$pine[[3]])


# SPRUCE
spec <- 1
v  <- prebRuns_sprucesubs$multiOut[5,,30,1,1]
n <-  prebRuns_sprucesubs$multiOut[5,,17,1,1]
h <- prebRuns_sprucesubs$multiOut[5,,11,1,1]
age <- prebRuns_sprucesubs$multiOut[5,,7,1,1]



# loop trough simulation years
aggtab_s<- data.table(year=as.numeric(), sawn=as.numeric(), energy=as.numeric(), pulp=as.numeric(), vol=as.numeric())

for(i in 1:150){
  vol = v[i]
  nx = n[i]
  hx = h[i]
  agex = age
  sawnratio = 99
  enerratio = 99
  pulpratio = 99
  result <- .Fortran("harvAssort1", spec, vol, nx, hx, agex, sawnratio, enerratio, pulpratio)
  rowx <- data.table(year=i, sawn=result[[6]], energy=result[[7]], pulp=result[[8]], vol=vol)
  aggtab_s <- rbind(aggtab_s, rowx)
}

aggtab_s[50:100]
aggtab_s[, sum(sawn, energy, pulp)] # sum of ratios = simyears?


p_ftest1_s<- ggplot()+
  geom_line(data=aggtab_s, aes(x=year, y=sawn, col="sawn")) + 
  geom_line(data=aggtab_s, aes(x=year, y=energy, col="energy")) + 
  geom_line(data=aggtab_s, aes(x=year, y=pulp, col="pulp")) + 
  ylab(paste(expression("ratio of assortment")))+
  ggtitle("Spruce")+
  scale_y_continuous(limits = c(0, 1), breaks=seq(0, 1, 0.2))

p_ftest1_s


### PINE (and others)
spec <- 2
v  <- prebRuns_pinesubs$multiOut[5,,30,1,1]
n <-  prebRuns_pinesubs$multiOut[5,,17,1,1]
h <- prebRuns_pinesubs$multiOut[5,,11,1,1]
age <- prebRuns_pinesubs$multiOut[5,,7,1,1]

aggtab_p<- data.table(year=as.numeric(), sawn=as.numeric(), energy=as.numeric(), pulp=as.numeric(), vol=as.numeric())

for(i in 1:150){
  vol = v[i]
  nx = n[i]
  hx = h[i]
  agex = age
  sawnratio = 99
  enerratio = 99
  pulpratio = 99
  result <- .Fortran("harvAssort1", spec, vol, nx, hx, agex, sawnratio, enerratio, pulpratio)
  rowx <- data.table(year=i, sawn=result[[6]], energy=result[[7]], pulp=result[[8]], vol=vol)
  aggtab_p <- rbind(aggtab_p, rowx)
}

aggtab_p[50:100]
aggtab_p[, sum(sawn, energy, pulp)] # sum of ratios = simyears?


p_ftest1_p<- ggplot()+
  geom_line(data=aggtab_p, aes(x=year, y=sawn, col="sawn")) + 
  geom_line(data=aggtab_p, aes(x=year, y=energy, col="energy")) + 
  geom_line(data=aggtab_p, aes(x=year, y=pulp, col="pulp")) + 
  ylab(paste(expression("ratio of assortment")))+
  ggtitle("Pine (et al.)")+
  scale_y_continuous(limits = c(0, 1), breaks=seq(0, 1, 0.2))

p_ftest1_p





