##############
## Packages ##
##############
install.packages(c("ggplot2", "tidyverse", "tibble", "lubridate", 
                   "lme4", "sparklyr", "reshape2", "Hmisc", "car", 
                   "MuMIn", "glmmTMB"))
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

obs$yh_occ <- ifelse(obs$yellowhammers > 0, 
                                     c("1"), 
                                     c("0"))
obs$rbs_occ <- ifelse(obs$shrikes > 0, 
                                     c("1"), 
                                     c("0"))

obs$heightcat <- cut(obs$vegheight,
                                     breaks = c(0,1,2,3,4,5,Inf),
                                     labels = c("0-1","1-2","2-3","3-4","4-5",">5"),
                                     right = FALSE)

###################
## Global labels ## 
###################
rbsy <- ylab(label = "Red-backed shrikes")
yhy <- ylab(label = "Yellowhammers")
cnty <- ylab(label = "Count")
vhx <- xlab(label = "Vegetation height (m)")
cutdx <- xlab(label = "Cutting date")
timx <- xlab(label = "Time (HH:MM:SS)")
areax <- xlab(label = "Area size ("~m^2~")")
treex <- xlab(label = "Trees on site?")
cdatex <- xlab(label = "Cutting date (year)")
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

# select only the variables that will be used in the Yellowhammer model
yhobs <- obs[,c(2,3,5,6,7,9,11,12,13,14,15,16,17,18,19,20,21,26,27,28,29,30,34)]
# select only the variables that will be used in the Red-backed shrike model
rbsobs <- obs[,c(2,4,5,6,7,9,11,12,13,14,15,16,17,18,19,20,21,26,27,28,29,30,34)]

# preparation for dredge
# store variable names
# numerical variables for colinnearity analysis
varnames <- c("areasize", "grass", "spruce", "shrubs","birch", "raspberry", "branches", "bare", 
              "vegheight", "edges", "distfl10ha", "distcc", "farmland_250", "clearcuts250")
# all variables for the model
varnames2 <- c("areasize", "grass", "spruce", "shrubs", "birch", "raspberry", "branches", "bare", 
               "vegheight", "edges", "trees", "stubs", "distfl10ha", "distcc", "farmland_250", "clearcuts250")

# check structure of the numerical variables of yhobs and rbsobs
str(yhobs[,varnames])
str(rbsobs[,varnames])

# variable selection for rescaling
rescalevars <- c("areasize","vegheight", "edges", "distfl10ha", "distcc", "farmland_250", "clearcuts250")
covervars <- c("grass", "spruce", "shrubs", "birch", "raspberry", "branches", "bare", "stones")

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
varnames3 <- c("areasize", "grass", "spruce", "shrubs", "birch", "raspberry", "branches", "bare", "vegheight", "edges", "distfl10ha", "distcc", "farmland_250", "clearcuts250")
par(mfrow = c(3,5))
for(i in varnames3){
  boxplot(yhobs_rscl[,i]~yhobs_rscl$type_lvl1, ylab = i)
}
par(mfrow = c(3,5))
for(i in varnames3){
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
sexpr1 <- parse(text = paste("!((grass && spruce) || (grass && birch) || (grass && vegheight) || (grass && farmland_250) || (spruce && vegheight) || (edges && clearcuts250) || (distfl10ha && farmland_250) || (distcc && clearcuts250))"))
sexpr2 <-  parse(text = paste("!( (type_lvl1 && grass) || (type_lvl1 && spruce) || (type_lvl1 && birch) || (type_lvl1 && farmland_250) || (grass && spruce) || (grass && birch) || (grass && vegheight) || (grass && farmland_250) || (spruce && vegheight) || (edges && clearcuts250) || (distfl10ha && farmland_250) || (distcc && clearcuts250) )"))

##################################

### SINGLE PREDICTOR glmmTMB ZI ###
###################################
## make lists of specific predictor variables
varlist <- c("areasize", "edges", "spruce", "grass", "shrubs", "birch", "raspberry", 
             "branches", "bare", "stones", "trees", "stubs", "vegheight", "distfl10ha",
             "distcc", "farmland_250", "clearcuts250")


### YELLOWHAMMERS FOREST ###
par(mfrow = c(4,5), mar = c(4,3,3,1))
for (i in varlist) {
  plot(yellowhammers ~ yh_rscl_f[[i]], data = yh_rscl_f, xlab = names(yh_rscl_f[i]), main ="YHf")
}

YHfmodels <- list()
for(i in varlist){
  YHfmodels[[i]] <- glmmTMB(yellowhammers ~ yh_rscl_f[[i]], 
                            family = poisson(), 
                            ziformula = ~spontaneous, 
                            dispformula = ~1,
                            data = yh_rscl_f)
}

lapply(YHfmodels, summary)

## Best model based on single predictor results
YHf.bm2 <- glmmTMB(yellowhammers ~ areasize + farmland_250 + bare, 
                   family = poisson(), 
                   ziformula = ~spontaneous, 
                   dispformula = ~1,
                   data = yh_rscl_f)


### YELLOWHAMMERS AGRICULTURE ###
par(mfrow = c(4,5))
for (i in varlist) {
  plot(yellowhammers ~ yh_rscl_a[[i]], data = yh_rscl_a, xlab = names(yh_rscl_a[i]), main ="YHa")
}

YHamodels <- list()
for(i in varlist){
  YHamodels[[i]] <- glmmTMB(yellowhammers ~ yh_rscl_a[[i]], 
                            family = poisson(), 
                            ziformula = ~spontaneous, 
                            dispformula = ~1,
                            data = yh_rscl_a)
}

# some models don't converge

lapply(YHamodels, summary)

## Best model based on single predictor results
YHa.bm1 <- glmmTMB(yellowhammers ~ areasize + stones + distfl10ha, 
                   family = poisson(), 
                   ziformula = ~spontaneous, 
                   dispformula = ~1,
                   data = yh_rscl_a)
YHa.bm2 <- glmmTMB(yellowhammers ~ areasize + stones, 
                   family = poisson(), 
                   ziformula = ~spontaneous, 
                   dispformula = ~1,
                   data = yh_rscl_a)


### RED-BACKED SHRIKES FOREST ###
par(mfrow = c(4,5))
for (i in varlist) {
  plot(shrikes ~ rbs_rscl_f[[i]], data = rbs_rscl_f, xlab = names(rbs_rscl_f[i]), main ="RBSf")
}

RBSfmodels <- list()
for(i in varlist){
  RBSfmodels[[i]] <- glmmTMB(shrikes ~ rbs_rscl_f[[i]], 
                            family = poisson(), 
                            ziformula = ~spontaneous, 
                            dispformula = ~1,
                            data = rbs_rscl_f)
}

lapply(RBSfmodels, summary)

## Best model based on single predictor results
RBSf.bm1 <- glmmTMB(shrikes ~ areasize + spruce, 
                    family = poisson(), 
                    ziformula = ~spontaneous, 
                    dispformula = ~1,
                    data = rbs_rscl_f)

# best
RBSf.bm2 <- glmmTMB(shrikes ~ areasize + vegheight, 
                    family = poisson(), 
                    ziformula = ~spontaneous, 
                    dispformula = ~1,
                    data = rbs_rscl_f)

RBSf.bm3 <- glmmTMB(shrikes ~ areasize + stubs, 
                    family = poisson(), 
                    ziformula = ~spontaneous, 
                    dispformula = ~1,
                    data = rbs_rscl_f)

RBSf.bm4 <- glmmTMB(shrikes ~ areasize + stubs + vegheight, 
                    family = poisson(), 
                    ziformula = ~spontaneous, 
                    dispformula = ~1,
                    data = rbs_rscl_f)

RBSf.bm5 <- glmmTMB(shrikes ~ areasize + vegheight + farmland_250, 
                    family = poisson(), 
                    ziformula = ~spontaneous, 
                    dispformula = ~1,
                    data = rbs_rscl_f)

RBSf.bm6 <- glmmTMB(shrikes ~ areasize + vegheight + distcc, 
                    family = poisson(), 
                    ziformula = ~spontaneous, 
                    dispformula = ~1,
                    data = rbs_rscl_f)

RBSf.bm7 <- glmmTMB(shrikes ~ areasize + vegheight + birch, 
                    family = poisson(), 
                    ziformula = ~spontaneous, 
                    dispformula = ~1,
                    data = rbs_rscl_f)


### RED-BACKED SHRIKES AGRICULTURE ###
par(mfrow = c(4,5))
for (i in varlist) {
  plot(shrikes ~ rbs_rscl_a[[i]], data = rbs_rscl_a, xlab = names(rbs_rscl_a[i]), main ="RBSa")
}

RBSamodels <- list()
for(i in varlist){
  RBSamodels[[i]] <- glmmTMB(shrikes ~ rbs_rscl_a[[i]], 
                             family = poisson(), 
                             ziformula = ~spontaneous, 
                             dispformula = ~1,
                             data = rbs_rscl_a)
}

# some models have singular convergence, unsure which

lapply(RBSamodels, summary)

## Zero inflation doesn't really work

## Best model based on single predictor results
RBSa.bm1 <- glmmTMB(shrikes ~ birch + stones + farmland_250, 
                    family = poisson(), 
                    ziformula = ~spontaneous, 
                    dispformula = ~1,
                    data = rbs_rscl_a)

RBSa.bm2 <- glmmTMB(shrikes ~ birch + farmland_250, 
                    family = poisson(), 
                    ziformula = ~spontaneous, 
                    dispformula = ~1,
                    data = rbs_rscl_a)

RBSa.bm3 <- glmmTMB(shrikes ~ birch, 
                    family = poisson(), 
                    ziformula = ~spontaneous, 
                    dispformula = ~1,
                    data = rbs_rscl_a)



form.full.YH.a<-formula(paste0('yellowhammers ~ 1 + (1|spontaneous) + (1|date) +',paste0(varnames2,collapse='+'))) # can add random effects in the first part of the expression
# if df.sp is your data frame with response and predictors
options(na.action = na.fail)
m0.YH.a<-glmer(form.full.YH.a, data=yh_rscl_a, family = poisson())

## dredge
ms.YH.a<-dredge(m0.YH.a,subset=sexpr_a)

mod.YH.a.av<-model.avg(subset(ms.YH.a, delta<quantile(ms.YH.a$delta,0.05)),fit=TRUE)

bm.YH.a<-get.models(ms.YH.a,delta==0)[[1]]
YHsummary.a <- summary(bm.YH.a)
YHcapture.output(YHsummary.a,file = "bmYH_a_output.txt")

##############
## Plot model residuals
mdat<-bm.YH@frame
quartz()
par(mfrow=c(2,3))
for(i in 1:6){
  plot(residuals(YH.zi)~mdat[,i],xlab=names(mdat)[i])
  
  #ggplot(data.frame(x1=mdat[,i],pearson=residuals(bm.YH,type="pearson")),
   #      aes(x=x1,y=pearson)) +
    #geom_point() +
    #theme_bw()
}


########################

### RED-BACKED SHRIKES ###
form.full.RBS<-formula(paste0('shrikes ~ 1 + (1|spontaneous) + (1|date) +',paste0(varnames2,collapse='+'))) # can add random effects in the first part of the expression
# if df.sp is your data frame with response and predictors
options(na.action = na.fail)
m0.RBS<-glmer(form.full.RBS, data=rbsobs_rscl, family = poisson())

## dredge
ms.RBS<-dredge(m0.RBS,subset=sexpr2)

mod.RBS.av<-model.avg(subset(ms.RBS, delta<quantile(ms.RBS$delta,0.05)),fit=TRUE)

bm.RBS<-get.models(ms.RBS,delta==0)[[1]]
RBSsummary <- summary(bm.RBS)
capture.output(RBSsummary, file = "bmRBSoutput.txt")

#### RBS with area * distcc interaction
RBS.inter <- lmer(formula = shrikes ~ areasize * farmland_250 + birch + branches + 
       distcc + (1 | spontaneous) + (1 | date), data = rbsobs_rscl)

anova(RBS.inter, bm.RBS)

########################

# plots of bird ~ predictor var
plot(yellowhammers ~ areasize, data=yhobs)
plot(yellowhammers ~ distfl10ha, data=yhobs)
plot(yellowhammers ~ spruce, data=yhobs)

plot(shrikes ~ areasize, data=rbsobs)
plot(shrikes ~ birch, data=rbsobs)
plot(shrikes ~ branches, data=rbsobs)
plot(shrikes ~ distcc, data=rbsobs)
plot(shrikes ~ farmland_250, data=rbsobs)

######################
## glmmTMB

RBS.tmb.nr <- glmmTMB(shrikes ~ areasize + birch + branches + distcc + 
                     farmland_250, family = poisson(), ziformula = ~., data = rbsobs_rscl)
YH.zi <- glmmTMB(yellowhammers ~ areasize + birch + distcc + farmland_250 + spruce, 
                  family = poisson(), 
                  ziformula = ~(1|spontaneous), 
                  dispformula = ~1,
                  data = yhobs_rscl)

