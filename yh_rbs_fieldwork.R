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
                                                   isolation = col_skip(), 
                                                   objectID = col_character(),
                                                   openamount = col_skip(), 
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
######################
## Data exploration ##
######################

##############
# Scatterplots 
##############

# vegetationheight
YHvegH <- ggplot(obs, 
                 aes(vegheight,
                     yellowhammers, shape = obs$spontaneous, colour = obs$spontaneous))
YHvegH + 
  geom_point() + 
  theme_classic() +
  geom_smooth(method = "lm") +
  vhx +
  yhy +
  scale_shape_manual(name = "Observation type",
                     values = c(16,17), 
                     labels = c("planned", "spontaneous")) +
  scale_color_manual(name = "Observation type",
                     values = c("red", "blue"),
                     labels = c("planned", "spontaneous"))

RBSvegH <- ggplot(obs, 
                  aes(vegheight,
                      shrikes))
RBSvegH + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red") +
  theme_classic() +
  vhx +
  rbsy

# cuttingdate
YHccdate <- ggplot(obs, 
                   aes(cuttingdate,
                       yellowhammers))
YHccdate + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red") +
  theme_classic() +
  cutdx +
  yhy

RBSccdate <- ggplot(obs, 
                    aes(cuttingdate,
                        shrikes))
RBSccdate + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red")+
  theme_classic() +
  cutdx +
  rbsy

# observation time
YHtime <- ggplot(obs, 
                 aes(time,
                     yellowhammers))
YHtime + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red") +
  yhy +
  timx
  

RBStime <- ggplot(obs, 
                  aes(time,
                      shrikes))
RBStime + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red") +
  rbsy +
  timx

# area size
YHareas <- ggplot(obs, 
                  aes(areasize,
                      yellowhammers))
YHareas + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red") +
  yhy +
  areax

RBSareas <- ggplot(obs, 
                   aes(areasize,
                       shrikes))
RBSareas + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red") +
  rbsy +
  areax

# spruce
YHspruce <- ggplot(obs, 
                   aes(spruce,
                       yellowhammers))
YHspruce + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red")
RBSspruce <- ggplot(obs, 
                    aes(spruce,
                        shrikes))
RBSspruce + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red")

# birch
YHbirch <- ggplot(obs, 
                  aes(birch,
                      yellowhammers))
YHbirch + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red")
RBSbirch <- ggplot(obs, 
                   aes(birch,
                       shrikes))
RBSbirch +
  geom_point() + 
  geom_smooth(method = "lm",
              colour = "red")

# grass
YHgrass <- ggplot(obs, 
                  aes(grass,
                      yellowhammers))
YHgrass + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red")
RBSgrass <- ggplot(obs, 
                   aes(grass,
                       shrikes))
RBSgrass + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red")

# raspberry
YHraspberry <- ggplot(obs, 
                      aes(raspberry,
                          yellowhammers))
YHraspberry + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red")

RBSraspberry <- ggplot(obs, 
                       aes(raspberry,
                           shrikes))
RBSraspberry + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red")

# stones
YHstones <- ggplot(obs, 
                   aes(stones,
                       yellowhammers))
YHstones + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red")

RBSstones <- ggplot(obs, 
                    aes(stones,
                        shrikes))
RBSstones + 
  geom_point() + 
  geom_smooth(method = "lm", 
              colour = "red")
##########
# Barplots 
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
## Predictor selection ##
## Assumptions         ##
#########################

# select all numerical variables
obs_numeric <- obs[,c(3,4,5,6,7,8,11,12,13,14,15,16,17,18,21,26)]
str(obs_numeric)
obs_numeric$time <- as.numeric(obs_numeric$time)
obs_numeric$date <- as.numeric(obs_numeric$date)
obs_numeric$cuttingdate <- as.numeric(obs_numeric$cuttingdate)
str(obs_numeric)

#correlation matrix for all numeric variables
cornumeric <- cor(obs_numeric, use = "everything", method = "spearman")
write.csv(cornumeric, "correlation6.csv")
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

# check for normality
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

# transform variables:
# areasize
# shrubs
# birch
# raspberry
# branches
# bare
# vegheight

obs_sel$logarea <- log10(obs_sel$areasize)
obs_sel$logshrubs <- log10(obs_sel$shrubs + 1)
obs_sel$logbirch <- log10(obs_sel$birch + 1)
obs_sel$lograsp <- log10(obs_sel$raspberry + 1)
obs_sel$logbranch <- log10(obs_sel$branches + 1)
obs_sel$logbare <- log10(obs_sel$bare + 1)
obs_sel$logVH <- log10(obs_sel$vegheight + 1)

str(obs_sel)
obs_sel$yh_occ <- as.integer(obs_sel$yh_occ)
# Mixed effects model 
baseYH <- lme(yh_occ ~ 1, 
              random = ~1|spontaneous/date/time, 
              data = obs_sel, 
              method = "ML") 

areaYH <- update(baseYH, .~. + logarea)
areaVHYH <- update(areaYH, .~. + logVH)
areaVHYHE <- update(areaVHYH, .~. + edges)
areaALL <- update(baseYH, .~. + 
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
                    stubs)
all2 <- update(baseYH, .~.+ 
                  logarea + 
                  logshrubs +
                  logbirch + 
                  lograsp +
                  logbranch + 
                  stones + 
                  logVH + 
                  edges + 
                  trees + 
                  stubs)

all3 <- update(baseYH, .~.+ 
                  logarea + 
                  logshrubs +
                  logbirch +
                  logbranch + 
                  stones + 
                  logVH + 
                  edges + 
                  trees + 
                  stubs)

all4 <- update(baseYH, .~.+ 
                 logarea + 
                 logshrubs +
                 logbirch +
                 logbranch + 
                 stones + 
                 logVH + 
                 edges + 
                 trees)

all5 <- update(baseYH, .~.+ 
                 logarea +
                 logbirch +
                 logbranch + 
                 stones + 
                 logVH + 
                 edges + 
                 trees)
all6 <- update(baseYH, .~.+ 
                 logarea +
                 logbirch +
                 logbranch + 
                 stones + 
                 logVH + 
                 trees)
all7 <- update(baseYH, .~.+ 
                 logarea +
                 logbirch + 
                 stones + 
                 logVH + 
                 trees)
all8 <- update(baseYH, .~.+ 
                 logarea +
                 stones + 
                 logVH + 
                 trees)


allvarmodel <- lme(yh_occ ~ logarea + type_lvl1 + logshrubs + logbirch + lograsp + logbranch + logbare + stones + trees + stubs + logVH + edgeN + edgeE + edgeS + edgeW,
                   random = ~1|spontaneous/date/time,
                   data = obs_sel,
                   method = "ML")

