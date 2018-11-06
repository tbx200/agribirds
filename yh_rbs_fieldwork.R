##############
## Packages ##
##############
install.packages(c("ggplot2", "tidyverse", "tibble", "lubridate", 
                   "lme4", "sparklyr", "reshape2", "Hmisc", "car", 
                   "MuMIn", "glmmTMB", "matrixStats", "hexbin", "DHARMa",
                   "sandwich", "sjstats", "pscl"))
                 
# tidyverse
library(tidyverse)

# models
library(lme4)
library(glmmTMB)
library(MuMIn)
library(car)
library(DHARMa)
library(vcdExtra)
library(sandwich)
library(sjstats)
library(pscl)

# data management
library(lubridate)
library(reshape2)
library(Hmisc)
library(matrixStats)

# Plotting
library(RColorBrewer)
library(hexbin)

############
## Set WD ##
############
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Agribirds Sweden iCloud/stats/AgriBirds")

##################
## Loading data ##
##################
obs <- read_delim("YH_RBS_observations.txt", 
                                  "\t", 
                                  escape_double = FALSE, 
                                  col_types = cols(areasize = col_number(), 
                                                   bare = col_number(), 
                                                   birch = col_number(), 
                                                   branches = col_number(), 
                                                   cuttingdate = col_date(format = "%d/%m/%Y"), 
                                                   date = col_date(format = "%d/%m/%Y"), 
                                                   grass = col_number(), 
                                                   objectID = col_character(),
                                                   raspberry = col_number(), 
                                                   shrikes = col_number(), 
                                                   shrubs = col_number(),
                                                   spontaneous = col_factor(levels = c("0","1")), 
                                                   spruce = col_number(), 
                                                   stones = col_number(), 
                                                   stubs = col_factor(levels = c("yes","no")),
                                                   total = col_skip(), 
                                                   trees = col_factor(levels = c("yes","no")), 
                                                   type_lvl1 = col_factor(levels = c("agriculture","forest")), 
                                                   vegheight = col_number(), 
                                                   yellowhammers = col_number()), 
                                  trim_ws = TRUE)

str(obs)
######################
## Data preparation ##
######################

obs$yh_occ <- as.numeric(ifelse(obs$yellowhammers > 0, 
                                     c("1"), 
                                     c("0")))
obs$rbs_occ <- as.numeric(ifelse(obs$shrikes > 0, 
                                     c("1"), 
                                     c("0")))

obs$heightcat <- cut(obs$vegheight,
                                     breaks = c(0,1,2,3,4,5,Inf),
                                     labels = c("0-1","1-2","2-3","3-4","4-5",">5"),
                                     right = FALSE)

str(obs)
obs$farmland_250 <- as.numeric(obs$farmland_250) 
obs$clearcuts250 <- as.numeric(obs$clearcuts250)
obs$type1_num <- as.numeric(factor(obs$type_lvl1))
obs$year <- year(obs$cuttingdate)

# select only the variables that will be used in the Yellowhammer model
yhobs <- obs[,c(1,2,3,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,26,27,28,31,32,33,36,37)]
yhobs$yh_dens <- ((yhobs$yellowhammers / yhobs$areasize) * 10000)

# select only the variables that will be used in the Red-backed shrike model
rbsobs <- obs[,c(1,2,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,26,27,28,31,32,34,36,37)]
rbsobs$rbs_dens <- ((rbsobs$shrikes / rbsobs$areasize) * 10000)

yhobs_f <- yhobs[which(yhobs$type_lvl1 == "forest"),]
YH_fp <- yhobs_f[which(yhobs_f$yh_occ=="1"),]
YH_fa <-  yhobs_f[which(yhobs_f$yh_occ=="0"),]

yhobs_a <- yhobs[which(yhobs$type_lvl1 == "agriculture"),]
YH_ap <- yhobs_a[which(yhobs_a$yh_occ=="1"),]
YH_aa <-  yhobs_a[which(yhobs_a$yh_occ=="0"),]

rbsobs_f <- rbsobs[which(rbsobs$type_lvl1 == "forest"),]
RBS_fp <- rbsobs_f[which(rbsobs_f$rbs_occ=="1"),]
RBS_fa <-  rbsobs_f[which(rbsobs_f$rbs_occ=="0"),]

rbsobs_a <- rbsobs[which(rbsobs$type_lvl1 == "agriculture"),]
RBS_ap <- rbsobs_a[which(rbsobs_a$rbs_occ=="1"),]
RBS_aa <-  rbsobs_a[which(rbsobs_a$rbs_occ=="0"),]

###################
## Global labels ## 
###################
areax <- xlab(label = "Clear-cut size (ha)")

####################
# Lists of variables
####################
varlist <- c("areasize", "edges", "spruce", "grass", "shrubs", "birch", "raspberry", 
             "branches", "bare", "stones", "trees", "stubs", "vegheight", "distfl10ha",
             "distcc", "propfl_250", "propcc_250")
varlist2 <- c("areasize", "edges", "spruce", "grass", "shrubs", "birch", "raspberry", 
              "branches", "bare", "stones", "vegheight", "distfl10ha",
              "distcc", "propfl_250", "propcc_250")
varlist3 <- c("yellowhammers","areasize", "edges", "spruce", "grass", "shrubs", "birch", "raspberry", 
              "branches", "bare", "stones", "vegheight", "distfl10ha",
              "distcc","propfl_250", "propcc_250")
varlist4 <- c("shrikes","areasize", "edges", "spruce", "grass", "shrubs", "birch", "raspberry", 
              "branches", "bare", "stones", "vegheight", "distfl10ha",
              "distcc", "propfl_250", "propcc_250")

# numerical variables for colinnearity analysis
varnames <- c("areasize", "grass", "spruce", "shrubs","birch", "raspberry", "branches", "bare", 
              "vegheight", "edges", "distfl10ha", "distcc", "propfl_250", "propcc_250")
varnames.s <- c("areasize", "grass", "spruce", "shrubs","birch", "raspberry", "branches", "bare", 
                "stones", "vegheight", "edges", "distfl10ha", "distcc", "propfl_250", "propcc_250")
# all variables for the model
varnames.f <- c("areasize", "grass", "spruce", "shrubs", "birch", "raspberry", "branches", "bare", 
               "vegheight", "edges", "trees", "stubs", "distfl10ha", "distcc", "propfl_250", "propcc_250",
               "areasize*propfl_250", "areasize*propcc_250", "areasize*distcc", "areasize*distfl10ha", 
               "areasize*vegheight")
varnames.a <- c("areasize", "grass", "spruce", "shrubs", "birch", "raspberry", "branches", "bare", 
               "vegheight", "edges", "trees", "distfl10ha", "distcc", "propfl_250", "propcc_250",
               "areasize*propfl_250", "areasize*propcc_250", "areasize*distcc", "areasize*distfl10ha", 
               "areasize*vegheight")
rescalevars <- c("areasize","vegheight", "edges", "distfl10ha", "distcc")
covervars <- c("grass", "spruce", "shrubs", "birch", "raspberry", "branches", "bare", "stones")


########################
#### Simplenumbers #####
########################
simplenumbers <- NULL
simplenumbers_yfp <- data.frame(colMeans(YH_fp[,varlist2]))
simplenumbers_yfa <- data.frame(colMeans(YH_fa[,varlist2]))
simplenumbers_yap <- data.frame(colMeans(YH_ap[,varlist2]))
simplenumbers_yaa <- data.frame(colMeans(YH_aa[,varlist2]))

simplenumbers_rfp <- data.frame(colMeans(RBS_fp[,varlist2]))
simplenumbers_rfa <- data.frame(colMeans(RBS_fa[,varlist2]))
simplenumbers_rap <- data.frame(colMeans(RBS_ap[,varlist2]))
simplenumbers_raa <- data.frame(colMeans(RBS_aa[,varlist2]))

simplenumbers_yfp$sd <- colSds(as.matrix(YH_fp[,varlist2]))
simplenumbers_yfa$sd <- colSds(as.matrix(YH_fa[,varlist2]))
simplenumbers_yap$sd <- colSds(as.matrix(YH_ap[,varlist2]))
simplenumbers_yaa$sd <- colSds(as.matrix(YH_aa[,varlist2]))

simplenumbers_rfp$sd <- colSds(as.matrix(RBS_fp[,varlist2]))
simplenumbers_rfa$sd <- colSds(as.matrix(RBS_fa[,varlist2]))
simplenumbers_rap$sd <- colSds(as.matrix(RBS_ap[,varlist2]))
simplenumbers_raa$sd <- colSds(as.matrix(RBS_aa[,varlist2]))

count(RBS_ap[which(RBS_ap$trees=="yes"),])/length(RBS_ap$trees)
count(RBS_aa[which(RBS_aa$trees=="yes"),])/length(RBS_aa$trees)
count(RBS_fp[which(RBS_fp$trees=="yes"),])/length(RBS_fp$trees)
count(RBS_fa[which(RBS_fa$trees=="yes"),])/length(RBS_fa$trees)

count(YH_ap[which(YH_ap$trees=="yes"),])/length(YH_ap$trees)
count(YH_aa[which(YH_aa$trees=="yes"),])/length(YH_aa$trees)
count(YH_fp[which(YH_fp$trees=="yes"),])/length(YH_fp$trees)
count(YH_fa[which(YH_fa$trees=="yes"),])/length(YH_fa$trees)

count(RBS_ap[which(RBS_ap$stubs=="yes"),])/length(RBS_ap$stubs)
count(RBS_aa[which(RBS_aa$stubs=="yes"),])/length(RBS_aa$stubs)
count(RBS_fp[which(RBS_fp$stubs=="yes"),])/length(RBS_fp$stubs)
count(RBS_fa[which(RBS_fa$stubs=="yes"),])/length(RBS_fa$stubs)

count(YH_ap[which(YH_ap$stubs=="yes"),])/length(YH_ap$stubs)
count(YH_aa[which(YH_aa$stubs=="yes"),])/length(YH_aa$stubs)
count(YH_fp[which(YH_fp$stubs=="yes"),])/length(YH_fp$stubs)
count(YH_fa[which(YH_fa$stubs=="yes"),])/length(YH_fa$stubs)

for(i in 1:17){
  variable <- varlist[i]
  mean <- mean(yhobs_f[,variable]) 
  sd <- sd()
  simplenumbers <- rbind(simplenumbers, data.frame(variable, mean, sd))
}

rbsf_yr_occ <- as.data.frame(aggregate(as.numeric(rbsobs_f$rbs_occ), list(rbsobs_f$year), mean))
yhf_yr_occ <- aggregate(as.numeric(yhobs_f$yh_occ), list(yhobs_f$year), mean)

##############################
### preparation for dredge ###
##############################
# store variable names
# check structure of the numerical variables of yhobs and rbsobs
str(yhobs[,varnames])
str(rbsobs[,varnames])

# create copies for rescaling
yhobs_rscl <- yhobs
rbsobs_rscl <- rbsobs

# rescale cover variables to 1
yhobs_rscl[,covervars] <- yhobs_rscl[,covervars]/100
rbsobs_rscl[,covervars] <- rbsobs_rscl[,covervars]/100
# rescale other numerical variables to 1
yhobs_rscl[,rescalevars] <- apply(yhobs_rscl[,rescalevars],2,function(col) col/max(col))
rbsobs_rscl[,rescalevars] <- apply(rbsobs_rscl[,rescalevars],2,function(col) col/max(col))

yhobs_rscl <- as.data.frame(yhobs_rscl)
rbsobs_rscl <- as.data.frame(rbsobs_rscl)

## make separate dataframes for clearcuts and farmland
yh_rscl_a <- yhobs_rscl[which(yhobs_rscl$type_lvl1 == "agriculture"),]
yh_rscl_f <- yhobs_rscl[which(yhobs_rscl$type_lvl1 == "forest"),]

rbs_rscl_a <- rbsobs_rscl[which(rbsobs_rscl$type_lvl1 == "agriculture"),]
rbs_rscl_f <- rbsobs_rscl[which(rbsobs_rscl$type_lvl1 == "forest"),]

rbs_rscl_f <- rbs_rscl_f[,-7 ]
rbs_rscl_f <- rbs_rscl_f[,-26]
yh_rscl_f <- yh_rscl_f[,-7]
yh_rscl_f <- yh_rscl_f[,-26]
## checking for zero inflation

zero.test(yh_rscl_f$yellowhammers)
zero.test(rbs_rscl_f$shrikes)
zero.test(yh_rscl_a$yellowhammers)
zero.test(rbs_rscl_a$shrikes)

### SUBSETTING BASED ON CORRELATION ###
### create correlation matrix for weather variables to use in dredge function, cutoff is 0.4, can be changed
is.correlated <- function(i, j, data, conf.level = .95, cutoff = .4, ...) {
  if(j >= i) return(NA)
  ct <- cor.test(data[,i], data[,j], conf.level = conf.level, ...)
  ct$p.value > (1 - conf.level) || abs(ct$estimate) <= cutoff
}

# Need vectorized function to use with 'outer'
vCorrelated <- Vectorize(is.correlated, c("i", "j"))
####
# Create logical matrix for AGRICULTURE
smat_rscl_a <- outer(1:length(varnames), 1:length(varnames), vCorrelated, data = yh_rscl_a[,varnames])
nm <- varnames
dimnames(smat_rscl_a) <- list(nm, nm)
smat_rscl_a

## create subsetting rules FOR AGRICULTURE
subred_a <- smat_rscl_a
i <- as.vector(subred_a == FALSE & !is.na(subred_a))
sexpr_a <-parse(text = paste("!(", paste("(",
                                       varnames[col(subred_a)[i]], " && ",
                                       varnames[row(subred_a)[i]], ")",
                                       sep = "", collapse = " || "), ")"))

####
# Create logical matrix for FOREST
smat_rscl_f <- outer(1:length(varnames), 1:length(varnames), vCorrelated, data = yh_rscl_f[,varnames])
nm <- varnames
dimnames(smat_rscl_f) <- list(nm, nm)
smat_rscl_f

## create subsetting rules FOR FOREST
subred_f <- smat_rscl_f
i <- as.vector(subred_f == FALSE & !is.na(subred_f))
sexpr_f <-parse(text = paste("!(", paste("(",
                                         varnames[col(subred_a)[i]], " && ",
                                         varnames[row(subred_a)[i]], ")",
                                         sep = "", collapse = " || "), ")"))

###########################
### Yellowhammer forest ###
###########################

### dredge Occurrence  ###
### for zero inflation ###
form.full.YHoc.f<-formula(paste0('yh_occ ~ 1 +',paste0(varnames.f,collapse='+'))) # can add random effects in the first part of the expression
# if df.sp is your data frame with response and predictors
options(na.action = na.fail)

m0.YHoc.f<-glm(form.full.YHoc.f, data=yh_rscl_f, family = binomial())

## dredge
ms.YHoc.f<-dredge(m0.YHoc.f,subset=sexpr_f)

mod.YHoc.f.av<-model.avg(subset(ms.YHoc.f, delta<quantile(ms.YHoc.f$delta,0.05)),fit=TRUE)

bm.YHoc.f<-get.models(ms.YHoc.f,delta==0)[[1]]

YHocsummary.f <- summary(bm.YHoc.f)
capture.output(YHocsummary.f,file = "bmYHoc_f_output.csv")

### dredge abundance   ###
### for conditional fm ###

form.full.YHab.f<-formula(paste0('yellowhammers ~ 1 +',paste0(varnames.f,collapse='+'))) # can add random effects in the first part of the expression
# if df.sp is your data frame with response and predictors
options(na.action = na.fail)

m0.YHab.f<-glm(form.full.YHab.f, data=yh_rscl_f, family = poisson())
m0.YHzip.f <- zeroinfl(yellowhammers ~ areasize+grass+spruce+shrubs+birch+raspberry+branches+bare+vegheight+edges+trees+stubs+distfl10ha+distcc+propfl_250+propcc_250+areasize*propfl_250+areasize*propcc_250+areasize*distcc+areasize*distfl10ha+areasize*vegheight | 
                         areasize+grass+spruce+shrubs+birch+raspberry+branches+bare+vegheight+edges+trees+stubs+distfl10ha+distcc+propfl_250+propcc_250+areasize*propfl_250+areasize*propcc_250+areasize*distcc+areasize*distfl10ha+areasize*vegheight, 
                       data = yh_rscl_f, dist = "poisson", link = "logit" )

## dredge
ms.YHab.f<-dredge(m0.YHab.f,subset=sexpr_f)

mod.YHab.f.av<-model.avg(subset(ms.YHab.f, delta<quantile(ms.YHab.f$delta,0.05)),fit=TRUE)

bm.YHab.f<-get.models(ms.YHab.f,delta==0)[[1]]
summary(bm.YHab.f)
capture.output(YHabsummary.f,file = "bmYHab_f_output.txt")

### glmmTMB for full model ###
# with landscape proportions

yh_m_f <- yh_rscl_f[,c("yellowhammers", "areasize", "bare", "propfl_250", "spruce")]

YHf.zinull <- glmmTMB(yellowhammers ~ areasize + bare + propfl_250 + spruce + areasize:propfl_250, 
                      family = poisson(),
                      data = yh_m_f,
                      ziformula = ~ areasize + propfl_250 + spruce + areasize:propfl_250,
                      dispformula = ~ .)

YHf.zip <- zeroinfl(yellowhammers ~ areasize + bare + propfl_250 + spruce + areasize:propfl_250 | areasize + propfl_250 + spruce + areasize:propfl_250, data = yh_m_f, dist = "poisson", link = "logit" )
summary(YHf.zip)
ms.YHf.zip <- dredge(YHf.zip)
bm.YHf.zip <- get.models(ms.YHf.zip,delta==0)[[1]]
summary(bm.YHf.zip)

YHf.simOutput <- simulateResiduals(fittedModel = YHf.zinull, n = 250)
testUniformity(simulationOutput = YHf.simOutput)
testDispersion(simulationOutput = YHf.simOutput)
testZeroInflation(simulationOutput = YHf.simOutput)

## Calculate predicted values and residuals

yh_m_f$residuals <- residuals(YHf.zinull)
yh_m_f$predicted <- predict(YHf.zinull, type = "response")

plot(yh_m_f$yellowhammers, yh_m_f$predicted)
abline(a=0, b=1)
plot(yh_m_f$yellowhammers, yh_m_f$residuals)

#########################
### Yellowhammer agri ###
#########################

### dredge Occurrence  ###
### for zero inflation ###
form.full.YHoc.a <- formula(paste0('yh_occ ~ 1 +',paste0(varnames.a,collapse='+'))) # can add random effects in the first part of the expression
# if df.sp is your data frame with response and predictors
options(na.action = na.fail)

m0.YHoc.a<-glm(form.full.YHoc.a, data=yh_rscl_a, family = binomial())

## dredge
ms.YHoc.a<-dredge(m0.YHoc.a,subset=sexpr_a)

mod.YHoc.a.av<-model.avg(subset(ms.YHoc.a, delta<quantile(ms.YHoc.a$delta,0.05)),fit=TRUE)

bm.YHoc.a<-get.models(ms.YHoc.a,delta==0)[[1]]
YHocsummary.a <- summary(bm.YHoc.a)
capture.output(YHocsummary.a,file = "bmYHoc_a_output.txt")

### dredge abundance   ###
### for conditional fm ###

form.full.YHab.a<-formula(paste0('yellowhammers ~ 1 +',paste0(varnames.a,collapse='+'))) # can add random effects in the first part of the expression
# if df.sp is your data frame with response and predictors
options(na.action = na.fail)

m0.YHab.a<-glm(form.full.YHab.a, data=yh_rscl_a, family = poisson())

## dredge
ms.YHab.a<-dredge(m0.YHab.a,subset=sexpr_a)

mod.YHab.a.av<-model.avg(subset(ms.YHab.a, delta<quantile(ms.YHab.a$delta,0.05)),fit=TRUE)

bm.YHab.a<-get.models(ms.YHab.a,delta==0)[[1]]
YHabsummary.a <- summary(bm.YHab.a)
capture.output(YHabsummary.a,file = "bmYHab_a_output.txt")

## Robust estimates
cov.bm.YHa <- vcovHC(bm.YHab.a, type="HC0")
std.err.YHa <- sqrt(diag(cov.bm.YHa))
r.est.YH <- cbind(Estimate= coef(bm.YHab.a), "Robust SE" = std.err.YHa,
                   "Pr(>|z|)" = 2 * pnorm(abs(coef(bm.YHab.a)/std.err.YHa), lower.tail=FALSE),
                   LL = coef(bm.YHab.a) - 1.96 * std.err.YHa,
                   UL = coef(bm.YHab.a) + 1.96 * std.err.YHa)

r.est.YH

##################
### RBS forest ###
##################

### dredge Occurrence  ###
### for zero inflation ###
form.full.RBSoc.f<-formula(paste0('rbs_occ ~ 1 +',paste0(varnames.f,collapse='+'))) # can add random effects in the first part of the expression
# if df.sp is your data frame with response and predictors
options(na.action = na.fail)

m0.RBSoc.f<-glm(form.full.RBSoc.f, data=rbs_rscl_f, family = binomial())

## dredge
ms.RBSoc.f<-dredge(m0.RBSoc.f,subset=sexpr_f)

mod.RBSoc.f.av<-model.avg(subset(ms.RBSoc.f, delta<quantile(ms.RBSoc.f$delta,0.05)),fit=TRUE)

bm.RBSoc.f<-get.models(ms.RBSoc.f,delta==0)[[1]]
RBSocsummary.f <- summary(bm.RBSoc.f)
capture.output(RBSocsummary.f,file = "bmRBSoc_f_output.txt")

### dredge abundance   ###
### for conditional fm ###

form.full.RBSab.f<-formula(paste0('shrikes ~ 1 +',paste0(varnames.f,collapse='+'))) # can add random effects in the first part of the expression
# if df.sp is your data frame with response and predictors
options(na.action = na.fail)

m0.RBSab.f<-glm(form.full.RBSab.f, data=rbs_rscl_f, family = poisson())

## dredge
ms.RBSab.f<-dredge(m0.RBSab.f,subset=sexpr_f)

mod.RBSab.f.av<-model.avg(subset(ms.RBSab.f, delta<quantile(ms.RBSab.f$delta,0.05)),fit=TRUE)

bm.RBSab.f<-get.models(ms.RBSab.f,delta==0)[[1]]
RBSabsummary.f <- summary(bm.RBSab.f)
capture.output(RBSabsummary.f,file = "bmRBSab_f_output.txt")

### glmmTMB for full model ###
RBSf.zinull <- glmmTMB(shrikes ~ areasize + bare + birch + branches + propfl_250 + raspberry, 
                       family = poisson(),
                       data = rbs_rscl_f,
                       ziformula = ~ areasize + branches + propcc_250 + propfl_250 + shrubs + stubs + areasize:propfl_250,
                       dispformula = ~ spontaneous)

rbs_m_f <- rbs_rscl_f[,c("shrikes", "bare", "birch", "branches", "raspberry", "areasize")]

RBSf.zi <- glmmTMB(shrikes ~ bare + birch + branches + raspberry, 
                   family = poisson(),
                   data = rbs_m_f,
                   ziformula = ~ areasize + branches,
                   dispformula = ~ .)

RBSf.zip <- zeroinfl(shrikes ~ bare + birch + branches + raspberry | areasize + branches, data = rbs_m_f, dist = "poisson", link = "logit")

simo = simulate(RBSf.zi, seed =1)
simdat = rbs_rscl_f
simdat$shrikes = simo[[1]]

RBSf.simOutput <- simulateResiduals(fittedModel = RBSf.zi, n = 250)
testUniformity(simulationOutput = RBSf.simOutput)
testDispersion(simulationOutput = RBSf.simOutput)
testZeroInflation(simulationOutput = RBSf.simOutput)

rbs_m_f$residuals <- residuals(RBSf.zi)
rbs_m_f$predicted <- predict(RBSf.zi, type = "response")

plot(rbs_m_f$shrikes, rbs_m_f$predicted, xlim = c(0,5), ylim = c(0,5))
abline(a = 0, b= 1)

r2(RBSf.zi)
r2(YHf.zinull)

data(Salamanders)
m1 <- glmmTMB(count~ mined, 
              zi=~mined, 
              family=poisson, data=Salamanders)
r2(m1)


################
### RBS agri ###
################

### dredge Occurrence  ###
### for zero inflation ###
form.full.RBSoc.a <- formula(paste0('rbs_occ ~ 1 +',paste0(varnames.a,collapse='+'))) # can add random effects in the first part of the expression
# if df.sp is your data frame with response and predictors
options(na.action = na.fail)

m0.RBSoc.a<-glm(form.full.RBSoc.a, data=rbs_rscl_a, family = binomial())

## dredge
ms.RBSoc.a<-dredge(m0.RBSoc.a,subset=sexpr_a)

mod.RBSoc.a.av<-model.avg(subset(ms.RBSoc.a, delta<quantile(ms.RBSoc.a$delta,0.05)),fit=TRUE)

bm.RBSoc.a<-get.models(ms.RBSoc.a,delta==0)[[1]]
RBSocsummary.a <- summary(bm.RBSoc.a)
capture.output(RBSocsummary.a,file = "bmRBSoc_a_output.txt")

### dredge abundance   ###
### for conditional fm ###

form.full.RBSab.a<-formula(paste0('shrikes ~ 1 +',paste0(varnames.a,collapse='+'))) # can add random effects in the first part of the expression
# if df.sp is your data frame with response and predictors
options(na.action = na.fail)

m0.RBSab.a<-glm(form.full.RBSab.a, data=rbs_rscl_a, family = poisson())

## dredge
ms.RBSab.a<-dredge(m0.RBSab.a,subset=sexpr_a)

mod.RBSab.a.av<-model.avg(subset(ms.RBSab.a, delta<quantile(ms.RBSab.a$delta,0.05)),fit=TRUE)

bm.RBSab.a<-get.models(ms.RBSab.a,delta==0)[[1]]
RBSabsummary.a <- summary(bm.RBSab.a)
capture.output(RBSabsummary.a,file = "bmRBSab_a_output.txt")

## Robust estimates
cov.bm.RBSa <- vcovHC(bm.RBSab.a, type="HC0")
std.err.RBSa <- sqrt(diag(cov.bm.RBSa))
r.est.RBS <- cbind(Estimate= coef(bm.RBSab.a), "Robust SE" = std.err.RBSa,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(bm.RBSab.a)/std.err.RBSa), lower.tail=FALSE),
               LL = coef(bm.RBSab.a) - 1.96 * std.err.RBSa,
               UL = coef(bm.RBSab.a) + 1.96 * std.err.RBSa)

r.est.RBS


##############
### Graphs ###
##############

yellowhammers <- as.data.frame(table(yh_rscl_f$yellowhammers))

ggplot(data = yellowhammers, aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'black') +
  theme_classic() +
  xlab('Number of Yellowhammers') +
  ylab('Number of locations') +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20, coslor = 'black'))

shrikes <- as.data.frame(table(rbs_rscl_f$shrikes))

ggplot(data = shrikes, aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'black') +
  theme_classic() +
  xlab('Number of Red-backed Shrikes') +
  ylab('Number of locations') +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20, color = 'black'))

rbsobs_f$rbs_occ <- as.factor(rbsobs_f$rbs_occ)
yhobs_f$yh_occ <- as.factor(yhobs_f$yh_occ)
yhobs_f$areaha <- yhobs_f$areasize/10000
rbsobs_f$areaha <- rbsobs_f$areasize/10000

rbsobs_a$rbs_occ <- as.factor(rbsobs_a$rbs_occ)
yhobs_a$yh_occ <- as.factor(yhobs_a$yh_occ)
yhobs_a$areaha <- yhobs_a$areasize/10000
rbsobs_a$areaha <- rbsobs_a$areasize/10000

## Lists for ggplot layout of graphs
yhdens <- list(theme_classic(),
  ylab('Number of clear-cuts'),
  scale_x_continuous(expand = c(0,0)),
  scale_y_continuous(expand = c(0,0)),
  scale_fill_manual(values = c("white", "#FFD966")),
  scale_color_manual(values = c("black", "black")),
  theme(legend.position="none",axis.title = element_text(size = 10),
        axis.text = element_text(size = 10, color = 'black'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")))

rbsdens <- list(theme_classic(),
               ylab('Number of clear-cuts'),
               scale_x_continuous(expand = c(0,0)),
               scale_y_continuous(expand = c(0,0)),
               scale_fill_manual(values = c("white", "#C00000")),
               scale_color_manual(values = c("black", "black")),
               theme(legend.position="none",axis.title = element_text(size = 10),
                     axis.text = element_text(size = 10, color = 'black'),
                     plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")))

yhdens.a <- list(theme_classic(),
               ylab('Number of pastures'),
               scale_x_continuous(expand = c(0,0)),
               scale_y_continuous(expand = c(0,0)),
               scale_fill_manual(values = c("white", "#FFD966")),
               scale_color_manual(values = c("black", "black")),
               theme(legend.position="none",axis.title = element_text(size = 10),
                     axis.text = element_text(size = 10, color = 'black'),
                     plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")))

rbsdens.a <- list(theme_classic(),
                ylab('Number of pastures'),
                scale_x_continuous(expand = c(0,0)),
                scale_y_continuous(expand = c(0,0)),
                scale_fill_manual(values = c("white", "#C00000")),
                scale_color_manual(values = c("black", "black")),
                theme(legend.position="none",axis.title = element_text(size = 10),
                      axis.text = element_text(size = 10, color = 'black'),
                      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")))

### Graphs for forest distribution of data
# area size
png("rbs_areasize.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(areaha, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  areax +
  rbsdens 
dev.off()

png("yh_areasize.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(areaha, fill = yh_occ, color = yh_occ)) + 
  areax +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens
dev.off()

#edges 
png("rbs_edges.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(edges, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Edges") +
  rbsdens
dev.off()

png("yh_edges.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(edges, fill = yh_occ, color = yh_occ)) + 
  xlab("Edges") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens
dev.off()

#spruce
png("rbs_spruce.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(spruce, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Spruce cover (%)") +
  rbsdens
dev.off()

png("yh_spruce.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(spruce, fill = yh_occ, color = yh_occ)) + 
  xlab("Spruce cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens
dev.off()

#grass
png("rbs_grass.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(grass, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Grass cover (%)") +
  rbsdens
dev.off()

png("yh_grass.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(grass, fill = yh_occ, color = yh_occ)) + 
  xlab("Grass cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens
dev.off()

#shrubs
png("rbs_shrubs.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(shrubs, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Shrub cover (%)") +
  rbsdens
dev.off()

png("yh_shrubs.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(shrubs, fill = yh_occ, color = yh_occ)) + 
  xlab("Shrub cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens
dev.off()
#birch
png("rbs_birch.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(birch, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Birch cover (%)") +
  rbsdens
dev.off()

png("yh_birch.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(birch, fill = yh_occ, color = yh_occ)) + 
  xlab("Birch cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens
dev.off()
#raspberry 
png("rbs_raspberry.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(raspberry, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Raspberry cover (%)") +
  rbsdens
dev.off()

png("yh_raspberry.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(raspberry, fill = yh_occ, color = yh_occ)) + 
  xlab("Raspberry cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens
dev.off()
#branches
png("rbs_branches.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(branches, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Branches cover (%)") +
  rbsdens
dev.off()

png("yh_branches.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(branches, fill = yh_occ, color = yh_occ)) + 
  xlab("Branches cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens
dev.off()
#bare
png("rbs_bare.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(bare, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Bare cover (%)") +
  rbsdens
dev.off()

png("yh_bare.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(bare, fill = yh_occ, color = yh_occ)) + 
  xlab("Bare cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens
dev.off()
#stones
png("rbs_stones.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(stones, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 3) +
  xlab("Stones cover (%)") +
  rbsdens
dev.off()

png("yh_stones.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(stones, fill = yh_occ, color = yh_occ)) + 
  xlab("Stones cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins =3) +
  yhdens
dev.off()
#vegheight
png("rbs_vegheight.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(vegheight, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Vegetation height (m)") +
  rbsdens
dev.off()

png("yh_vegheight.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(vegheight, fill = yh_occ, color = yh_occ)) + 
  xlab("Vegetation height (m)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens
dev.off()
#distfl10ha
png("rbs_distfl10ha.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(distfl10ha, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Distance to farmland (m)") +
  rbsdens
dev.off()

png("yh_distfl10ha.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(distfl10ha, fill = yh_occ, color = yh_occ)) + 
  xlab("Distance to farmland (m)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens
dev.off()
#distcc
png("rbs_distcc.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(distcc, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Distance to clear-cut (m)") +
  rbsdens
dev.off()

png("yh_distcc.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(distcc, fill = yh_occ, color = yh_occ)) + 
  xlab("Distance to clear-cut (m)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens
dev.off()
#propfl_250
png("rbs_propfl_250.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(propfl_250, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Proportion of farmland in 250 m buffer") +
  rbsdens
dev.off()

png("yh_propfl_250.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(propfl_250, fill = yh_occ, color = yh_occ)) + 
  xlab("Proportion of farmland in 250 m buffer") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens
dev.off()
#propcc_250
png("rbs_propcc_250.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_f, aes(propcc_250, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Proportion of clear-cuts in 250 m buffer") +
  rbsdens
dev.off()

png("yh_propcc_250.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_f, aes(propcc_250, fill = yh_occ, color = yh_occ)) + 
  xlab("Proportion of clear-cuts in 250 m buffer") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens
dev.off()

ggplot(data = rbsobs_f, aes(x = rbs_occ, y = branches)) + 
  geom_boxplot(outlier.color = "black") + 
  theme_classic() + 
  ylim(0,100)

########## Agriculture


# Areasize
png("arbs_areasize.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(areaha, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Field size (ha)") +
  rbsdens.a 
dev.off()

png("ayh_areasize.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(areaha, fill = yh_occ, color = yh_occ)) + 
  xlab("Field size (ha)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens.a
dev.off()

#edges 
png("arbs_edges.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(edges, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Edges") +
  rbsdens.a
dev.off()

png("ayh_edges.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(edges, fill = yh_occ, color = yh_occ)) + 
  xlab("Edges") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens.a
dev.off()

#spruce
png("arbs_spruce.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(spruce, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 2) +
  xlab("Spruce cover (%)") +
  rbsdens.a
dev.off()

png("ayh_spruce.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(spruce, fill = yh_occ, color = yh_occ)) + 
  xlab("Spruce cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 2) +
  yhdens.a
dev.off()

#grass
png("arbs_grass.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(grass, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Grass cover (%)") +
  rbsdens.a
dev.off()

png("ayh_grass.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(grass, fill = yh_occ, color = yh_occ)) + 
  xlab("Grass cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens.a
dev.off()

#shrubs
png("arbs_shrubs.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(shrubs, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Shrub cover (%)") +
  rbsdens.a
dev.off()

png("ayh_shrubs.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(shrubs, fill = yh_occ, color = yh_occ)) + 
  xlab("Shrub cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens.a
dev.off()
#birch
png("arbs_birch.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(birch, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Birch cover (%)") +
  rbsdens.a
dev.off()

png("ayh_birch.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(birch, fill = yh_occ, color = yh_occ)) + 
  xlab("Birch cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens.a
dev.off()
#raspberry 
png("arbs_raspberry.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(raspberry, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 3) +
  xlab("Raspberry cover (%)") +
  rbsdens.a
dev.off()

png("ayh_raspberry.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(raspberry, fill = yh_occ, color = yh_occ)) + 
  xlab("Raspberry cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 3) +
  yhdens.a
dev.off()
#branches
png("arbs_branches.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(branches, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 2) +
  xlab("Branches cover (%)") +
  rbsdens.a
dev.off()

png("ayh_branches.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(branches, fill = yh_occ, color = yh_occ)) + 
  xlab("Branches cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 2) +
  yhdens.a
dev.off()
#bare
png("arbs_bare.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(bare, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 2) +
  xlab("Bare cover (%)") +
  rbsdens.a
dev.off()

png("ayh_bare.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(bare, fill = yh_occ, color = yh_occ)) + 
  xlab("Bare cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 2) +
  yhdens.a
dev.off()
#stones
png("arbs_stones.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(stones, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 3) +
  xlab("Stones cover (%)") +
  rbsdens.a
dev.off()

png("ayh_stones.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(stones, fill = yh_occ, color = yh_occ)) + 
  xlab("Stones cover (%)") +
  geom_histogram(position = "stack", alpha = 0.8, bins =3) +
  yhdens.a
dev.off()
#vegheight
png("arbs_vegheight.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(vegheight, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 4) +
  xlab("Vegetation height (m)") +
  rbsdens.a
dev.off()

png("ayh_vegheight.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(vegheight, fill = yh_occ, color = yh_occ)) + 
  xlab("Vegetation height (m)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 4) +
  yhdens.a
dev.off()
#distfl10ha
png("arbs_distfl10ha.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(distfl10ha, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Distance to farmland (m)") +
  rbsdens.a
dev.off()

png("ayh_distfl10ha.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(distfl10ha, fill = yh_occ, color = yh_occ)) + 
  xlab("Distance to farmland (m)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens.a
dev.off()
#distcc
png("arbs_distcc.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(distcc, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Distance to clear-cut (m)") +
  rbsdens.a
dev.off()

png("ayh_distcc.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(distcc, fill = yh_occ, color = yh_occ)) + 
  xlab("Distance to clear-cut (m)") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens.a
dev.off()
#propfl_250
png("arbs_propfl_250.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(propfl_250, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Proportion of farmland in 250 m buffer") +
  rbsdens.a
dev.off()

png("ayh_propfl_250.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(propfl_250, fill = yh_occ, color = yh_occ)) + 
  xlab("Proportion of farmland in 250 m buffer") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens.a
dev.off()
#propcc_250
png("arbs_propcc_250.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(rbsobs_a, aes(propcc_250, fill = rbs_occ, color = rbs_occ)) + 
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  xlab("Proportion of clear-cuts in 250 m buffer") +
  rbsdens.a
dev.off()

png("ayh_propcc_250.png", units="cm", width=7.75, height=5.64, res=600)
ggplot(yhobs_a, aes(propcc_250, fill = yh_occ, color = yh_occ)) + 
  xlab("Proportion of clear-cuts in 250 m buffer") +
  geom_histogram(position = "stack", alpha = 0.8, bins = 6) +
  yhdens.a
dev.off()