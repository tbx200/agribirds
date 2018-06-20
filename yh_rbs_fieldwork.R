##############
## Packages ##
##############

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("tibble")
install.packages("lubridate")
install.packages("lme4")
library(lubridate)
library(tibble)
library(ggplot2)
library(readr)
library(lme4)

##################
## Loading data ##
##################

observations_by_loc <- read_csv("observations_by_loc.csv",
                                col_types = cols(cuttingdate = col_date(format = "%d/%m/%Y"),
                                date = col_date(format = "%d/%m/%Y"),
                                vegheight = col_number()))

######################
## Data preparation ##
######################

observations_by_loc$yh_occ <- ifelse(observations_by_loc$yellowhammers > 0, 
                                     c("occupied"), c("unoccupied"))
observations_by_loc$rbs_occ <- ifelse(observations_by_loc$shrikes > 0, 
                                     c("occupied"), c("unoccupied"))
######################
## Data exploration ##
######################

# Scatterplots 
##############

# vegetationheight
YHvegH <- ggplot(observations_by_loc, aes(vegheight,yellowhammers))
YHvegH + geom_point() + geom_smooth(method = "lm", colour = "red")
RBSvegH <- ggplot(observations_by_loc, aes(vegheight,shrikes))
RBSvegH + geom_point() + geom_smooth(method = "lm", colour = "red")

# cuttingdate
YHccdate <- ggplot(observations_by_loc, aes(cuttingdate,yellowhammers))
YHccdate + geom_point() + geom_smooth(method = "lm", colour = "red")
RBSccdate <- ggplot(observations_by_loc, aes(cuttingdate,shrikes))
RBSccdate + geom_point() + geom_smooth(method = "lm", colour = "red")

# observation time
YHtime <- ggplot(observations_by_loc, aes(time,yellowhammers))
YHtime + geom_point() + geom_smooth(method = "lm", colour = "red")
RBStime <- ggplot(observations_by_loc, aes(time,shrikes))
RBStime + geom_point() + geom_smooth(method = "lm", colour = "red")

# area size
YHareas <- ggplot(observations_by_loc, aes(areasize,yellowhammers))
YHareas + geom_point() + geom_smooth(method = "lm", colour = "red")
RBSareas <- ggplot(observations_by_loc, aes(areasize,shrikes))
RBSareas + geom_point() + geom_smooth(method = "lm", colour = "red")

# spruce
YHspruce <- ggplot(observations_by_loc, aes(spruce,yellowhammers))
YHspruce + geom_point() + geom_smooth(method = "lm", colour = "red")
RBSspruce <- ggplot(observations_by_loc, aes(spruce,shrikes))
RBSspruce + geom_point() + geom_smooth(method = "lm", colour = "red")

# birch
YHbirch <- ggplot(observations_by_loc, aes(birch,yellowhammers))
YHbirch + geom_point() + geom_smooth(method = "lm", colour = "red")
RBSbirch <- ggplot(observations_by_loc, aes(birch,shrikes))
RBSbirch + geom_point() + geom_smooth(method = "lm", colour = "red")

# grass
YHgrass <- ggplot(observations_by_loc, aes(grass,yellowhammers))
YHgrass + geom_point() + geom_smooth(method = "lm", colour = "red")
RBSgrass <- ggplot(observations_by_loc, aes(grass,shrikes))
RBSgrass + geom_point() + geom_smooth(method = "lm", colour = "red")

# raspberry
YHraspberry <- ggplot(observations_by_loc, aes(raspberry,yellowhammers))
YHraspberry + geom_point() + geom_smooth(method = "lm", colour = "red")
RBSraspberry <- ggplot(observations_by_loc, aes(raspberry,shrikes))
RBSraspberry + geom_point() + geom_smooth(method = "lm", colour = "red")

# stones
YHstones <- ggplot(observations_by_loc, aes(stones,yellowhammers))
YHstones + geom_point() + geom_smooth(method = "lm", colour = "red")
RBSstones <- ggplot(observations_by_loc, aes(stones,shrikes))
RBSstones + geom_point() + geom_smooth(method = "lm", colour = "red")

# Barplots 
##########

# trees
YHtrees <- ggplot(observations_by_loc, aes(trees, fill = yh_occ))
YHtrees + geom_bar(position = "dodge")
RBStrees <- ggplot(observations_by_loc, aes(trees,fill = rbs_occ))
RBStrees + geom_bar(position = "dodge")

# vegetation height
YHvegHBar <- ggplot(observations_by_loc, aes(vegheight, fill = yh_occ))
YHvegHBar + 
  geom_histogram(position = "dodge", binwidth = 1) +
  theme_bw()
RBSvegHBar <- ggplot(observations_by_loc, aes(vegheight,fill = rbs_occ))
RBSvegHBar + geom_bar(position = "dodge", binwidth = 0.5)

# observation time
YHtimeBar <- ggplot(observations_by_loc, aes(time, fill = yh_occ))
YHtimeBar + geom_bar(position = "dodge", binwidth = 2400)
RBStimeBar <- ggplot(observations_by_loc, aes(time,fill = rbs_occ))
RBStimeBar + geom_bar(position = "dodge", binwidth = 2400)

# areasize
YHareasizeBar <- ggplot(observations_by_loc, aes(areasize, fill = yh_occ))
YHareasizeBar + geom_bar(position = "dodge", binwidth = 15000)
RBSareasizeBar <- ggplot(observations_by_loc, aes(areasize,fill = rbs_occ))
RBSareasizeBar + geom_bar(position = "dodge", binwidth = 20000)

# cuttingdate
YHccdateBar <- ggplot(observations_by_loc, aes(cuttingdate, fill = yh_occ))
YHccdateBar + geom_bar(position = "dodge", binwidth = 500)
RBSccdateBar <- ggplot(observations_by_loc, aes(cuttingdate,fill = rbs_occ))
RBSccdateBar + geom_bar(position = "dodge", binwidth = 500)

