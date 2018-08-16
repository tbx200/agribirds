##############
## Packages ##
##############
install.packages(c("ggplot2", "tidyverse", "tibble", "lubridate", 
                   "lme4", "sparklyr", "reshape2", "Hmisc", "car", 
                   "MuMIn", "glmmTMB", "matrixStats", "hexbin"))
library(lubridate)
library(tibble)
library(ggplot2)
library(readr)
library(lme4)
library(sparklyr)
library(reshape2)
library(Hmisc)
library(car)
library(nlme)
library(MuMIn)
library(dplyr)
library(glmmTMB)
library(matrixStats)
library(RColorBrewer)
library(purrr)
library(tidyr)
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

###################
## Global labels ## 
###################
#rbsy <- ylab(label = "Red-backed shrikes")
#yhy <- ylab(label = "Yellowhammers")
#cnty <- ylab(label = "Count")
#vhx <- xlab(label = "Vegetation height (m)")
#cutdx <- xlab(label = "Cutting date")
#timx <- xlab(label = "Time (HH:MM:SS)")
#areax <- xlab(label = "Area size ("~m^2~")")
#treex <- xlab(label = "Trees on site?")
#cdatex <- xlab(label = "Cutting date (year)")

# Lists of variables
varlist <- c("areasize", "edges", "spruce", "grass", "shrubs", "birch", "raspberry", 
             "branches", "bare", "stones", "trees", "stubs", "vegheight", "distfl10ha",
             "distcc", "farmland_250", "clearcuts250")
varlist2 <- c("areasize", "edges", "spruce", "grass", "shrubs", "birch", "raspberry", 
              "branches", "bare", "stones", "vegheight", "distfl10ha",
              "distcc", "farmland_250", "clearcuts250")
varlist3 <- c("yellowhammers","areasize", "edges", "spruce", "grass", "shrubs", "birch", "raspberry", 
              "branches", "bare", "stones", "vegheight", "distfl10ha",
              "distcc", "farmland_250", "clearcuts250")

varlist4 <- c("shrikes","areasize", "edges", "spruce", "grass", "shrubs", "birch", "raspberry", 
              "branches", "bare", "stones", "vegheight", "distfl10ha",
              "distcc", "farmland_250", "clearcuts250")

# numerical variables for colinnearity analysis
varnames <- c("areasize", "grass", "spruce", "shrubs","birch", "raspberry", "branches", "bare", 
              "vegheight", "edges", "distfl10ha", "distcc", "farmland_250", "clearcuts250")
# all variables for the model
varnames2 <- c("areasize", "grass", "spruce", "shrubs", "birch", "raspberry", "branches", "bare", 
               "vegheight", "edges", "trees", "stubs", "distfl10ha", "distcc", "farmland_250", "clearcuts250")
rescalevars <- c("areasize","vegheight", "edges", "distfl10ha", "distcc", "farmland_250", "clearcuts250")
covervars <- c("grass", "spruce", "shrubs", "birch", "raspberry", "branches", "bare", "stones")

#########################
## Predictor selection ##
## Assumptions         ##
#########################

# check distribution of variables 
boxplot(obs[,3:4])
boxplot(obs[,5], xlab = "Date")
boxplot(obs[,7], xlab = "Areasize")
boxplot(obs[,8], xlab = "cutting date") 
boxplot(obs[,11:18])
boxplot(obs[,21], xlab = "vegheight")
boxplot(obs[,26], xlab = "edges")
boxplot(obs[,27:28])
boxplot(obs[,29:30])

##########

# non normal variables:
# areasize
# shrubs
# birch
# raspberry
# branches
# bare
# vegheight
# distfl10ha
# distcc
# farmland_250



# check structure of obs dataframe
str(obs)
obs$farmland_250 <- as.numeric(obs$farmland_250) 
obs$clearcuts250 <- as.numeric(obs$clearcuts250)
obs$type1_num <- as.numeric(factor(obs$type_lvl1))
obs$year <- year(obs$cuttingdate)

# subset to exclude spontaneous observations
# obs <- obs[which(obs$spontaneous=="0"),]

# select only the variables that will be used in the Yellowhammer model
yhobs <- obs[,c(1,2,3,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,26,27,28,29,30,31,34,35)]
# select only the variables that will be used in the Red-backed shrike model
rbsobs <- obs[,c(1,2,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,26,27,28,29,30,32,34,35)]

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

barplot(rbsf_yr_occ$x~rbsf_yr_occ$Group.1)
###################
# preparation for dredge
# store variable names


# check structure of the numerical variables of yhobs and rbsobs
str(yhobs[,varnames])
str(rbsobs[,varnames])

# variable selection for rescaling

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

##### Check differences between farmland and forest
par(mfrow = c(3,5))
for(i in varnames){
  boxplot(yhobs_rscl[,i]~yhobs_rscl$type_lvl1, ylab = i)
}
par(mfrow = c(3,5))
for(i in varnames){
  boxplot(rbsobs_rscl[,i]~rbsobs_rscl$type_lvl1, ylab = i)
}


## make separate dataframes for clearcuts and farmland
yh_rscl_a <- yhobs_rscl[which(yhobs_rscl$type_lvl1 == "agriculture"),]
yh_rscl_f <- yhobs_rscl[which(yhobs_rscl$type_lvl1 == "forest"),]

rbs_rscl_a <- rbsobs_rscl[which(rbsobs_rscl$type_lvl1 == "agriculture"),]
rbs_rscl_f <- rbsobs_rscl[which(rbsobs_rscl$type_lvl1 == "forest"),]

### SUBSETTING BASED ON CORRELATION ###
#######################################
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

## on with the dredging
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

## on with the dredging
## create subsetting rules FOR FOREST
subred_f <- smat_rscl_f
i <- as.vector(subred_f == FALSE & !is.na(subred_f))
sexpr_f <-parse(text = paste("!(", paste("(",
                                         varnames[col(subred_a)[i]], " && ",
                                         varnames[row(subred_a)[i]], ")",
                                         sep = "", collapse = " || "), ")"))

# replace numerical type1 variable with factorial
# sexpr1 <- parse(text = paste("!((grass && spruce) || (grass && birch) || (grass && vegheight) || (grass && farmland_250) || (spruce && vegheight) || (edges && clearcuts250) || (distfl10ha && farmland_250) || (distcc && clearcuts250))"))
# sexpr2 <-  parse(text = paste("!( (type_lvl1 && grass) || (type_lvl1 && spruce) || (type_lvl1 && birch) || (type_lvl1 && farmland_250) || (grass && spruce) || (grass && birch) || (grass && vegheight) || (grass && farmland_250) || (spruce && vegheight) || (edges && clearcuts250) || (distfl10ha && farmland_250) || (distcc && clearcuts250) )"))

###########################
### Yellowhammer forest ###
###########################

### dredge Occurrence  ###
### for zero inflation ###
form.full.YHoc.f<-formula(paste0('yh_occ ~ 1 +',paste0(varnames2,collapse='+'))) # can add random effects in the first part of the expression
# if df.sp is your data frame with response and predictors
options(na.action = na.fail)

m0.YHoc.f<-glm(form.full.YHoc.f, data=yh_rscl_f, family = binomial())

## dredge
ms.YHoc.f<-dredge(m0.YHoc.f,subset=sexpr_f)

mod.YHoc.f.av<-model.avg(subset(ms.YHoc.f, delta<quantile(ms.YHoc.f$delta,0.05)),fit=TRUE)

bm.YHoc.f<-get.models(ms.YHoc.f,delta==0)[[1]]
YHocsummary.f <- summary(bm.YHoc.f)
capture.output(YHocsummary.f,file = "bmYHoc_f_output.txt")

### dredge abundance   ###
### for conditional fm ###

form.full.YHab.f<-formula(paste0('yellowhammers ~ 1 +',paste0(varnames2,collapse='+'))) # can add random effects in the first part of the expression
# if df.sp is your data frame with response and predictors
options(na.action = na.fail)

m0.YHab.f<-glm(form.full.YHab.f, data=yh_rscl_f, family = poisson())

## dredge
ms.YHab.f<-dredge(m0.YHab.f,subset=sexpr_f)

mod.YHab.f.av<-model.avg(subset(ms.YHab.f, delta<quantile(ms.YHab.f$delta,0.05)),fit=TRUE)

bm.YHab.f<-get.models(ms.YHab.f,delta==0)[[1]]
YHabsummary.f <- summary(bm.YHab.f)
capture.output(YHabsummary.f,file = "bmYHab_f_output.txt")

### glmmTMB for full model ###

YHf.zi <- glmmTMB(yellowhammers ~ farmland_250 + areasize + spruce, 
                  family = poisson(),
                  data = yh_rscl_f,
                  ziformula = ~ areasize + spruce,
                  dispformula = ~ spontaneous)
fixef(YHf.zi)$disp
YHf.zi1 <- glmmTMB(yellowhammers ~ farmland_250 + areasize + spruce, 
                  family = poisson(),
                  data = yh_rscl_f,
                  ziformula = ~ areasize + spruce + farmland_250,
                  dispformula = ~ spontaneous)
YHf.zi2 <- glmmTMB(yellowhammers ~ farmland_250 + areasize + spruce, 
                   family = poisson(),
                   data = yh_rscl_f,
                   ziformula = ~ spruce,
                   dispformula = ~ spontaneous)
YHf.zi3 <- glmmTMB(yellowhammers ~ farmland_250 + areasize, 
                   family = poisson(),
                   data = yh_rscl_f,
                   ziformula = ~ areasize + spruce,
                   dispformula = ~ spontaneous)
YHf.zi4 <- glmmTMB(yellowhammers ~ farmland_250 + spruce, 
                   family = poisson(),
                   data = yh_rscl_f,
                   ziformula = ~ areasize + spruce,
                   dispformula = ~ spontaneous)
# YHf.zi is the best model

#########################
### Yellowhammer agri ###
#########################

### dredge Occurrence  ###
### for zero inflation ###
form.full.YHoc.a <- formula(paste0('yh_occ ~ 1 +',paste0(varnames2,collapse='+'))) # can add random effects in the first part of the expression
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

form.full.YHab.a<-formula(paste0('yellowhammers ~ 1 +',paste0(varnames2,collapse='+'))) # can add random effects in the first part of the expression
# if df.sp is your data frame with response and predictors
options(na.action = na.fail)

m0.YHab.a<-glm(form.full.YHab.a, data=yh_rscl_a, family = poisson())

## dredge
ms.YHab.a<-dredge(m0.YHab.a,subset=sexpr_a)

mod.YHab.a.av<-model.avg(subset(ms.YHab.a, delta<quantile(ms.YHab.a$delta,0.05)),fit=TRUE)

bm.YHab.a<-get.models(ms.YHab.a,delta==0)[[1]]
YHabsummary.a <- summary(bm.YHab.a)
capture.output(YHabsummary.a,file = "bmYHab_a_output.txt")

### glmmTMB for full model ###
YHa.zi <- glmmTMB(yellowhammers ~ areasize, 
                  family = poisson(),
                  data = yh_rscl_a,
                  ziformula = ~ stubs,
                  dispformula = ~1)
# model doesn't fit, need to look at this later..


##################
### RBS forest ###
##################

### dredge Occurrence  ###
### for zero inflation ###
form.full.RBSoc.f<-formula(paste0('rbs_occ ~ 1 +',paste0(varnames2,collapse='+'))) # can add random effects in the first part of the expression
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

form.full.RBSab.f<-formula(paste0('shrikes ~ 1 +',paste0(varnames2,collapse='+'))) # can add random effects in the first part of the expression
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
RBSf.zi <- glmmTMB(shrikes ~ branches + areasize + birch, 
                   family = poisson(),
                   data = rbs_rscl_f,
                   ziformula = ~ branches + farmland_250,
                   dispformula = ~ spontaneous)

################
### RBS agri ###
################

### dredge Occurrence  ###
### for zero inflation ###
form.full.RBSoc.a <- formula(paste0('rbs_occ ~ 1 +',paste0(varnames2,collapse='+'))) # can add random effects in the first part of the expression
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

form.full.RBSab.a<-formula(paste0('shrikes ~ 1 +',paste0(varnames2,collapse='+'))) # can add random effects in the first part of the expression
# if df.sp is your data frame with response and predictors
options(na.action = na.fail)

m0.RBSab.a<-glm(form.full.RBSab.a, data=rbs_rscl_a, family = poisson())

## dredge
ms.RBSab.a<-dredge(m0.RBSab.a,subset=sexpr_a)

mod.RBSab.a.av<-model.avg(subset(ms.RBSab.a, delta<quantile(ms.RBSab.a$delta,0.05)),fit=TRUE)

bm.RBSab.a<-get.models(ms.RBSab.a,delta==0)[[1]]
RBSabsummary.a <- summary(bm.RBSab.a)
capture.output(RBSabsummary.a,file = "bmRBSab_a_output.txt")

### glmmTMB for full model ###
RBSa.zi <- glmmTMB(shrikes ~ farmland_250 + spruce + birch, 
                   family = poisson(),
                   data = rbs_rscl_a,
                   ziformula = ~ spruce,
                   dispformula = ~ spontaneous)




