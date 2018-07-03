##############
## Packages ##
##############

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("tibble")
install.packages("lubridate")
install.packages("lme4")
install.packages("sparklyr")
install.packages("reshape2")
install.packages("Hmisc")
install.packages("car")
install.packages("MuMIn")
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

#obs$logarea <- log10(obs$areasize)
#obs$logshrubs <- log10(obs$shrubs + 1)
#obs$logbirch <- log10(obs$birch + 1)
#obs$lograsp <- log10(obs$raspberry + 1)
#obs$logbranch <- log10(obs$branches + 1)
#obs$logbare <- log10(obs$bare + 1)
#obs$logVH <- log10(obs$vegheight + 1)
#obs$logdistfl10 <- log10(obs$distfl10ha)
#obs$logdistcc <- log10(obs$distcc)
#obs$logfl250 <- log10(obs$farmland_250 + 1)

###################
## Global labels ## This is for making figures, you can skip this Paul
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
######################
## Data exploration ## Skip
######################
##########
# Barplots # Skip 
##########

# trees
YHtrees <- ggplot(obs, 
                  aes(trees, 
                      fill = yh_occ))
YHtrees + 
  geom_bar(position = "dodge") +
  theme_classic() +
  treex +
  cnty +
  guides(fill = FALSE)

RBStrees <- ggplot(obs, 
                   aes(trees,
                       fill = rbs_occ))
RBStrees + 
  geom_bar(position = "dodge") +
  theme_classic() +
  treex +
  cnty +
  guides(fill = FALSE)

# vegetation height
vhYH.dcast <- dcast(obs, yh_occ ~ heightcat, fun.aggregate = length)
vhYH.melt = melt(vhYH.dcast, id.vars = "yh_occ", measure.vars = c("0-1", "1-2","2-3","3-4","4-5",">5"))

YHvegHBar <- ggplot(vhYH.melt, 
                    aes(x = variable, 
                        y = value, 
                        fill = yh_occ))

YHvegHBar + 
  geom_bar(position = "dodge", 
           stat = "identity")+
  vhx +
  cnty +
  theme_classic() +
  guides(fill = FALSE)

vhRBS.dcast <- dcast(obs, rbs_occ ~ heightcat, fun.aggregate = length)
vhRBS.melt = melt(vhRBS.dcast, id.vars = "rbs_occ", measure.vars = c("0-1", "1-2","2-3","3-4","4-5",">5"))
  
RBSvegHBar <- ggplot(vhRBS.melt, 
                     aes(x = variable, 
                         y = value, 
                         fill = rbs_occ))
RBSvegHBar + 
  geom_bar(position = "dodge",
           stat = "identity") +
  theme_classic() +
  vhx +
  cnty +
  guides(fill = FALSE)

# observation time
YHtimeBar <- ggplot(obs, 
                    aes(time, 
                        fill = yh_occ))
YHtimeBar + 
  geom_histogram(position = "dodge",
                 binwidth = 2400) +
  theme_classic() +
  timx +
  cnty +
  guides(fill = FALSE)

RBStimeBar <- ggplot(obs, 
                     aes(time,
                         fill = rbs_occ))
RBStimeBar + 
  geom_histogram(position = "dodge", 
           binwidth = 2400) +
  theme_classic() +
  timx +
  cnty +
  guides(fill = FALSE)

# areasize
YHareasizeBar <- ggplot(obs, 
                        aes(areasize, 
                            fill = yh_occ))
YHareasizeBar + 
  geom_bar(position = "dodge", 
                         binwidth = 15000) +
  areax +
  cnty +
  theme_classic() +
  guides(fill = FALSE)

RBSareasizeBar <- ggplot(obs, 
                         aes(areasize,
                             fill = rbs_occ))
RBSareasizeBar + 
  geom_bar(position = "dodge", 
                          binwidth = 15000) +
  areax +
  cnty +
  theme_classic() +
  guides(fill = FALSE)

# cuttingdate
YHccdateBar <- ggplot(obs, 
                      aes(cuttingdate, 
                          fill = yh_occ))
YHccdateBar + 
  geom_bar(position = "dodge", 
           binwidth = 500) +
  cdatex +
  cnty +
  theme_classic() +
  guides(fill = FALSE)

RBSccdateBar <- ggplot(obs,
                       aes(cuttingdate,
                           fill = rbs_occ))
RBSccdateBar + 
  geom_bar(position = "dodge", 
           binwidth = 500) +
  cdatex +
  cnty +
  theme_classic() 

#########################
## Predictor selection ## Here it's relevant for you again Paul!
## Assumptions         ##
#########################

# select all numerical variables
obs_numeric <- obs[,c(3,4,5,6,7,8,11,12,13,14,15,16,17,18,21,26,27,28,29,30)]
str(obs_numeric)
obs_numeric$time <- as.numeric(obs_numeric$time)
obs_numeric$date <- as.numeric(obs_numeric$date)
obs_numeric$cuttingdate <- as.numeric(obs_numeric$cuttingdate)
obs_numeric$farmland_250 <- as.numeric(obs_numeric$farmland_250)
obs_numeric$clearcuts250 <- as.numeric((obs_numeric$clearcuts250))
str(obs_numeric)

#correlation matrix for all numeric variables
cornumeric <- cor(obs_numeric, use = "complete.obs", method = "spearman")
write.csv(cornumeric, "correlation8.csv")
# correlation larger than .6
# spruce x cuttingdate 
# spruce x grass
# vegheight x cuttingdate
# vegheight x grass
# vegheight x spruce

# check p-values
cor.test(obs_numeric$spruce, 
         obs_numeric$cuttingdate, 
         alternative = "less", 
         method = "spearman",
         conf.level = 0.95)

cor.test(obs_numeric$spruce, 
         obs_numeric$grass, 
         alternative = "less", 
         method = "spearman",
         conf.level = 0.95)

cor.test(obs_numeric$vegheight, 
         obs_numeric$cuttingdate, 
         alternative = "less", 
         method = "spearman",
         conf.level = 0.95)

cor.test(obs_numeric$vegheight, 
         obs_numeric$grass, 
         alternative = "less", 
         method = "spearman",
         conf.level = 0.95)

cor.test(obs_numeric$spruce, 
         obs_numeric$vegheight, 
         alternative = "greater", 
         method = "spearman",
         conf.level = 0.95)

cor.test(obs_numeric$farmland_250, 
         obs_numeric$distfl10ha, 
         alternative = "less", 
         method = "spearman",
         conf.level = 0.95)

# remove cuttingdate, grass and spruce to get rid of correlation problems
obs_sel <- obs[,-c(8,11,12)]

# re-check correlations
obs_numeric2 <- obs_sel[,c(3,4,5,6,7,10,11,12,13,14,15,18)]
str(obs_numeric2)
obs_numeric2$time <- as.numeric(obs_numeric2$time)
obs_numeric2$date <- as.numeric(obs_numeric2$date)

cornumeric2 <- cor(obs_numeric2, use = "everything", method = "spearman")
write.csv(cornumeric2, "correlation5.csv")

# all good
boxplot(obs[,c(11,12,13,14,15,16,17,18)])

# check for normality
#######
# obs time Normal
hist(obs_numeric2$time)
# obs date Normal
hist(obs_numeric2$date)
# areasize Not normal
hist(obs_numeric2$areasize)
# log10(areasize) Normal
hist(log10(obs_numeric2$areasize))
# shrubs Not Normal
hist(obs_numeric2$shrubs)
# log10(shrubs) Normal
hist(log10(obs_numeric2$shrubs))
# Birch Not normal
hist(obs_numeric2$birch)
# log10 Birch Normal 
hist(log10(obs_numeric2$birch))
# Raspberry Not normal
hist(obs_numeric2$raspberry)
# log10 Raspberry Normal
hist(log10(obs_numeric2$raspberry))
# branches not normal
hist(obs_numeric2$branches)
# log branches is better
hist(log10(obs_numeric2$branches))
# Bare Not normal
hist(obs_numeric2$bare)
# log10 bare is better
hist(log10(obs_numeric2$bare))
# Stones only has 3 levels, looks fine
hist(obs_numeric2$stones)
# Vegheight Not normal
hist(obs_numeric2$vegheight)
# log10 vegheight Normal
hist(log10(obs_numeric2$vegheight))
# edges looks OK
hist(obs_numeric$edges)
# distfl10ha not normal
hist(obs_numeric$distfl10ha)
# log10 distfl10ha
hist(log10(obs_numeric$distfl10ha))
# distcc not ok
hist(obs_numeric$distcc)
# log10 distcc better
hist(log10(obs_numeric$distcc))
qplot(sample = obs_numeric$distcc)
qplot(sample = log10(obs_numeric$distcc))
# farmland250
hist(obs_numeric$farmland_250)
# log10 farmland250 is better
hist(log10(obs_numeric$farmland_250))
qplot(sample = obs_numeric$farmland_250)
qplot(sample = log10(obs_numeric$farmland_250))
# clearcuts250 is better than log transformed
hist(obs_numeric$clearcuts250)
qplot(sample = obs_numeric$clearcuts250)
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

# select only the variables that will be used in the Yellowhammer model
yhobs <- obs[,c(2,3,5,6,7,9,11,12,13,14,15,16,17,18,19,20,21,26,27,28,29,30)]

# preparation for dredge
# store variable names
# numerical variables for colinnearity analysis
varnames <- c("areasize", "grass", "spruce", "shrubs","birch", "raspberry", "branches", "bare", "stones", "vegheight", "edges", "distfl10ha", "distcc", "farmland_250", "clearcuts250")
# all variables for the model
varnames2 <- c("areasize", "type_lvl1", "grass", "spruce", "shrubs", "birch", "raspberry", "branches", "bare", "stones", "vegheight", "edges", "trees", "stubs", "distfl10ha", "distcc", "farmland_250", "clearcuts250")

# check structure of the numerical variables of yhobs
str(yhobs[,varnames])

# make separate data.frame with only numeric vars
# yhobsnum <- yhobs[,varnames]

# farmland_250 ad clearcuts250 are 'int' and should be 'num'
yhobs$farmland_250 <- as.numeric(yhobs$farmland_250) 
yhobs$clearcuts250 <- as.numeric(yhobs$clearcuts250) 

### create correlation matrix for weather variables to use in dredge function, cutoff is 0.4, can be changed
is.correlated <- function(i, j, data, conf.level = .95, cutoff = .4, ...) {
  if(j >= i) return(NA)
  ct <- cor.test(data[, i], data[, j], conf.level = conf.level, ...)
  ct$p.value > (1 - conf.level) || abs(ct$estimate) <= cutoff
}

# Need vectorized function to use with 'outer'
vCorrelated <- Vectorize(is.correlated, c("i", "j"))
# Create logical matrix
smat <- outer(1:length(varnames), 1:length(varnames), vCorrelated, data = yhobs[,varnames])

nm <- varnames
dimnames(smat) <- list(nm, nm)
smat

# these are the base models I made with all the predictor variables.
# Mixed effects model 
baseYH <- lmer(yellowhammers ~ 1 + (1|spontaneous) + (1|date), data = obs)
baseRBS <- lmer(shrikes ~ 1 + (1|spontaneous) + (1|date), data = obs)

YHall <- update(baseYH, .~. + 
                  areasize  +
                  type_lvl1 +
                  grass +
                  spruce + 
                  shrubs +
                  birch + 
                  raspberry +
                  branches + 
                  bare + 
                  stones + 
                  vegheight + 
                  edges + 
                  trees + 
                  stubs +
                  distfl10ha +
                  distcc +
                  farmland_250 +
                  clearcuts250)
summary(YHall)
print(YHall, correlation = T)
vif(YHall)

RBSall <- update(baseRBS, .~. + 
                  areasize  +
                  type_lvl1 +
                  grass +
                  spruce + 
                  shrubs +
                  birch + 
                  raspberry +
                  branches + 
                  bare + 
                  stones + 
                  vegheight + 
                  edges + 
                  trees + 
                  stubs +
                  distfl10ha +
                  distcc +
                  farmland_250 +
                  clearcuts250)
summary(RBSall)

# clean out vars with >5 VIF value (type_lvl1, grass, spruce)
YHall2 <- update(baseYH,
                 .~. + 
                   logarea + 
                   logshrubs +
                   logbirch + 
                   lograsp +
                   logbranch + 
                   logbare + 
                   stones + 
                   logVH + 
                   edges + 
                   trees + 
                   stubs +
                   logdistfl10 +
                   logdistcc +
                   logfl250 +
                   clearcuts250)

summary(YHall2)
vif(YHall2) # VIF is good now
plot(YHall2)

#dredge to select variables 



