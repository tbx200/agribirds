# 1. Packages ####
install.packages(c("tidyverse", "vcdExtra", "Hmisc", "MuMIn", "pscl", "Boruta", "caret"))
     
# tidyverse
library(tidyverse)
library(lattice)
# models
library(vcdExtra)
library(MuMIn)
library(pscl)
library(boot)
library(caret)
library(ROCR)
library(pROC)
library(sandwich)
# misc
library(Hmisc)
library(car)

# 2. Set WD ####
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Agribirds Sweden iCloud/stats/AgriBirds")

# 3. Loading data ####
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

f_obs <- read_csv("Observations by field assistants.csv", 
                  col_types = cols(cuttingdate = col_date(format = "%d/%m/%Y"), 
                                   date = col_date(format = "%d/%m/%Y"), 
                                   stubs = col_factor(levels = c("yes","no")), 
                                   time = col_time(format = "%H:%M"), 
                                   total = col_skip(), 
                                   trees = col_factor(levels = c("yes","no")),
                                   type_lvl1 = col_factor(levels = c("forest","agriculture")), 
                                   type_lvl2 = col_factor(levels = c("clearcut","plantation","agriculture"))))

# 4. Data preparation ####
obs$yh_occ <- as.numeric(ifelse(obs$yellowhammers > 0, 
                                     c("1"), 
                                     c("0")))
obs$rbs_occ <- as.numeric(ifelse(obs$shrikes > 0, 
                                     c("1"), 
                                     c("0")))


str(obs)
obs$farmland_250 <- as.numeric(obs$farmland_250) 
obs$clearcuts250 <- as.numeric(obs$clearcuts250)
obs$type1_num <- as.numeric(factor(obs$type_lvl1))

# select only the variables that will be used in the Yellowhammer model
yhobs <- obs[,c(1,2,3,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,26,27,28,31,32,33,35)]

# select only the variables that will be used in the Red-backed shrike model
rbsobs <- obs[,c(1,2,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,26,27,28,31,32,34,35)]


# 5. Global labels ####
areax <- xlab(label = "Clear-cut size (ha)")

# 6. Lists of variables ####
# numerical variables for colinnearity analysis
varnames <- c("areasize", "grass", "spruce", "shrubs","birch", "raspberry", "branches", "bare", 
              "vegheight", "distfl10ha", "distcc", "propfl_250", "propcc_250")

# all variables for the model
varnames.f <- c("areasize", "grass", "spruce", "shrubs", "birch", "raspberry", "branches", "bare", 
               "vegheight", "distfl10ha", "distcc", "propfl_250", "propcc_250",
               "areasize*propfl_250", "areasize*propcc_250", "areasize*distcc", "areasize*distfl10ha", 
               "areasize*vegheight")
varnames.f2 <- c("areasize", "grass", "spruce", "shrubs", "birch", "raspberry", "branches", "bare", 
                "vegheight", "trees", "stubs", "distfl10ha", "distcc", "propfl_250", "propcc_250",
                "areasize*propfl_250", "areasize*propcc_250", "areasize*distcc", "areasize*distfl10ha", 
                "areasize*vegheight")
rescalevars <- c("areasize","vegheight", "edges", "distfl10ha", "distcc")
covervars <- c("grass", "spruce", "shrubs", "birch", "raspberry", "branches", "bare", "stones")



# 7. Preparation for dredge ####
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
yh_rscl_f <- yhobs_rscl[which(yhobs_rscl$type_lvl1 == "forest"),]

rbs_rscl_f <- rbsobs_rscl[which(rbsobs_rscl$type_lvl1 == "forest"),]

rbs_rscl_f <- rbs_rscl_f[,-7 ]
yh_rscl_f <- yh_rscl_f[,-7]

## checking for zero inflation
zero.test(yh_rscl_f$yellowhammers)
zero.test(rbs_rscl_f$shrikes)

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
# Create logical matrix for Forest
smat <- outer(1:length(varnames), 1:length(varnames), vCorrelated, data = yh_rscl_f[,varnames])
nm <- varnames
dimnames(smat) <- list(nm, nm)
smat

## create subsetting rules FOR Forest
subred <- smat
i <- as.vector(subred == FALSE & !is.na(subred))
sexpr <-parse(text = paste('!(', paste('(count_',
                                       varnames[col(subred)[i]], ' && count_',
                                       varnames[row(subred)[i]], ')',
                                       sep = '', collapse = ' || '), 
                           '||', 
                           paste('(zero_',
                                 varnames[col(subred)[i]], ' && zero_', 
                                 varnames[row(subred)[i]], ')',
                                 sep = '', collapse = ' || '), 
                           ')'))

sexprbin <-parse(text = paste('!(', paste('(',varnames[col(subred)[i]], ' && ',varnames[row(subred)[i]], ')', sep = '', collapse = ' || '),')'))

# 8. DREDGE models ####
## formula for global model
form.full.YHf <- formula(paste0('yellowhammers ~ 1 +', paste0(varnames, collapse='+')))
form.bin.YHf <- formula(paste0('yh_occ ~ 1 +', paste0(varnames.f2, collapse='+')))
form.full.RBSf <- formula(paste0('shrikes ~ 1 +', paste0(varnames, collapse='+')))
form.bin.RBSf <- formula(paste0('rbs_occ ~ 1 +', paste0(varnames.f2, collapse='+')))

form.red.YHf <- formula(paste0('yellowhammers ~ 1 +', paste0(varnames.f[1:3], collapse='+')))
form.red.RBSf <- formula(paste0('shrikes ~ 1 +', paste0(varnames.f[1:3], collapse='+')))
form.glmm.YHf <- formula('yellowhammers ~ areasize + bare + propfl_250 + spruce + areasize:propfl_250 | 
                           areasize + propfl_250 + spruce + areasize:propfl_250')
# set options for dredge
options(na.action = na.fail)

# 8.1 Yellowhammer Forest 
# zero models
m0.YHf <- zeroinfl(form.red.YHf, data=yh_rscl_f, dist = "poisson", link = "logit" )
m0.YHf$formula <-form.full.YHf
m0.YHbin <- glm(form.bin.YHf,data=yh_rscl_f,family="binomial")

## dredge zeroinfl
ms.YHf<-dredge(m0.YHf, trace = 2)
head(ms.YHf)
ms.YHf.sub2 <- subset(ms.YHf, delta<=2)

mod.YHf.av<-model.avg(subset(ms.YHf, delta<quantile(ms.YHf$delta,0.05)),fit=TRUE)
mod.YHf.av2 <- model.avg(ms.YHf.sub2, fit=TRUE)

bm.YHf<-get.models(ms.YHf,delta==0)[[1]]
nullmod.YH <- zeroinfl(yellowhammers ~ 1, data=yh_rscl_f, dist = "poisson", link = "logit" )

YHf.zip <- zeroinfl(yellowhammers ~ areasize + bare + propfl_250 + spruce + areasize:propfl_250 | 
                      areasize + propfl_250 + spruce + areasize:propfl_250, 
                    data = yh_rscl_f, 
                    dist = "poisson", link = "logit" )
summary(YHf.zip)

bm.YHF.delta <- cv.glm(yh_rscl_f, bm.YHf)$delta

L.mod.YH <- logLik(bm.YHf)[1]
lik.mod.YH <-  exp(L.mod.YH)
L.nm.YH <- logLik(nullmod.YH)[1]
lik.nm.YH <- exp(L.nm.YH)
L.ratio.YH <- lik.nm.YH/lik.mod.YH

MfpR2.YHf <- 1-logLik(bm.YHf)/logLik(nullmod.YH)
NpR2.YH <- (1- L.ratio.YH**(2/128))/(1-(abs(lik.nm.YH)**(2/128)))

# dredge binomial Yellowhammer
ms.YHbin <-dredge(m0.YHbin, subset = sexprbin, trace = 2)
ms.YHbin.sub <- subset(ms.YHbin, delta<=2)
bm.YHbin<-get.models(ms.YHbin,delta==0)[[1]]
nm.YHbin <- glm(yh_occ ~ 1, data = yh_rscl_f, family = "binomial")
YHbin.av <- model.avg(ms.YHbin.sub, fit=TRUE)
summary(bm.YHbin, robust = T)

modelChi <- bm.YHbin$null.deviance - bm.YHbin$deviance
chidf <- bm.YHbin$df.null - bm.YHbin$df.residual
chisq.prob.YH <- 1 - pchisq(modelChi, chidf)
R2.mf.YHbin <- 1 - logLik(bm.YHbin)/logLik(nm.YHbin)
R2.cs.YHbin <- 1- exp((bm.YHbin$deviance - bm.YHbin$null.deviance)/128)
R2.n.YHbin <- R2.cs.YHbin  / (1-(exp(-(bm.YHbin$null.deviance/128))))
pR2(bm.YHbin)

1/vif(bm.YHbin)
varImp(bm.YHbin)
sqrt(cv.glm(yh_rscl_f,bm.YHbin)$delta[1]) 

library(popbio)
logi.hist.plot(yh_rscl_f$spruce,yh_rscl_f$yh_occ,boxp=FALSE,type="hist",col="gray")
logi.hist.plot(yh_rscl_f$areasize,yh_rscl_f$yh_occ,boxp=FALSE,type="hist",col="gray")
logi.hist.plot(yh_rscl_f$propfl_250,yh_rscl_f$yh_occ,boxp=FALSE,type="hist",col="gray")

# compare YH zeroinfl with binomial
summary(bm.YHf)
summary(bm.YHbin)
par(mfrow=c(2,2))
plot(bm.YHbin)
par(mfrow = c(1,1))
MfpR2.YHf;NpR2.YH #Yellowhammer zeroinfl McFadden and Nagelkerke R2
R2.mf.YHbin; R2.n.YHbin #Yellowhammer binomial McFadden and Nagelkerke R2

probYH <- predict(bm.YHbin,type="response")
yh_rscl_f$probYH <- probYH
rocYH <- roc(yh_occ ~ probYH, data = yh_rscl_f)
plot(rocYH)

# 8.2 RBS forest
## Zero models
m0.RBSf <- zeroinfl(form.red.RBSf, data=rbs_rscl_f, dist = "poisson", link = "logit" )
m0.RBSf$formula <-form.full.RBSf
m0.RBSbin <- glm(form.bin.RBSf, data = rbs_rscl_f, family = "binomial")

#dredge zeroinfl
ms.RBSf<-dredge(m0.RBSf, subset = sexpr, trace = 2)
ms.RBSf.sub <- subset(ms.RBSf, delta<=2)
mod.RBSf.av2 <- model.avg(ms.RBSf.sub, fit= T)
mod.RBSf.av<-model.avg(subset(ms.RBSf, delta<quantile(ms.RBSf$delta,0.05)),fit=TRUE)

bm.RBSf<-get.models(ms.RBSf,delta==0)[[1]]
nullmod.RBS <- zeroinfl(shrikes ~ 1, data=rbs_rscl_f, dist = "poisson", link = "logit" )

vuong(bm.RBSf,nullmod.RBS)

rbs_m_f <- rbs_rscl_f[,c("shrikes", "bare", "birch", "branches", "raspberry", "areasize")]

RBSf.zip <- zeroinfl(shrikes ~ bare + birch + branches + raspberry | areasize + branches, 
                     data = rbs_m_f, 
                     dist = "poisson", 
                     link = "logit")
summary(RBSf.zip)

cv.glm(rbs_m_f, RBSf.zip)$delta
logLik(mod.RBSf.av2)

L.mod.RBS <- logLik(bm.RBSf)[1]
lik.mod.RBS <-  exp(L.mod.RBS)
L.nm.RBS <- logLik(nullmod.RBS)[1]
lik.nm.RBS <- exp(L.nm.RBS)
L.ratio.RBS <- lik.nm.RBS/lik.mod.RBS

MfpR2.RBS <- 1-L.mod.RBS/L.nm.RBS
NpR2.RBS <- (1- L.ratio.RBS**(2/128))/(1-(abs(lik.nm.RBS)**(2/128)))
MadR2.RBS <- 1-(L.ratio.RBS**(2/128))

# dredge binomial RBS
ms.RBSbin <-dredge(m0.RBSbin, subset = sexprbin, trace = 2)
ms.RBSbin.sub <- subset(ms.RBSbin, delta<=2)
bm.RBSbin<-get.models(ms.RBSbin,delta==0)[[1]]
nm.RBSbin <- glm(rbs_occ ~ 1, data=rbs_rscl_f, family="binomial")
RBSbin.av <- model.avg(ms.RBSbin.sub, fit=TRUE)
summary(bm.RBSbin, robust = F)

modelChi <- bm.RBSbin$null.deviance - bm.RBSbin$deviance
chidf <- bm.RBSbin$df.null - bm.RBSbin$df.residual
chisq.prob.RBS <- 1 - pchisq(modelChi, chidf)
R2.mf.RBSbin <- 1 - logLik(bm.RBSbin)/logLik(nm.RBSbin)
R2.cs.RBSbin <- 1- exp((bm.RBSbin$deviance - bm.RBSbin$null.deviance)/128)
R2.n.RBSbin <- R2.cs.RBSbin  / (1-(exp(-(bm.RBSbin$null.deviance/128))))
pR2(bm.RBSbin)

1/vif(bm.RBSbin)
varImp(bm.RBSbin)
sqrt(cv.glm(rbs_rscl_f,bm.RBSbin)$delta[1])


# compare RBS zeroinfl with binomial
summary(bm.RBSf)
summary(bm.RBSbin)
MfpR2.RBS;NpR2.RBS #RBS zeroinfl McFadden and Nagelkerke R2
R2.mf.RBSbin; R2.n.RBSbin #RBS binomial McFadden and Nagelkerke R2


probRBS <- predict(bm.RBSbin, type = "response")
rbs_rscl_f$probRBS <- probRBS
rocRBS <- roc(rbs_occ ~ probRBS, data = rbs_rscl_f)
plot(rocRBS)

RBS.accuracy <- table(probRBS,rbs_rscl_f$rbs_occ)
sum(diag(RBS.accuracy))/sum(RBS.accuracy)

RBSalternative <- glm(formula = rbs_occ ~ areasize + branches + distcc + distfl10ha + 
      shrubs + stubs + areasize:distfl10ha + 1, family = "binomial", 
    data = rbs_rscl_f)

# 9. Graphs ####
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

# 10. Fit models on Fieldworkers data ####

