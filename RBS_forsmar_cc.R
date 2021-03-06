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
forsmar <- read_csv("/Volumes/TRISTANMAC/AgriBirds Sweden/clearcuts_rbs_forsmark.csv", 
                    col_types = cols(Avverktyp = col_skip(), 
                                     Date = col_date(format = "%d/%m/%Y"), 
                                     Extra = col_skip(), `Extra 2` = col_skip(), 
                                     Kommun = col_skip(), Lan = col_skip(), 
                                     Observer = col_skip(), Species = col_skip()))

forsmark_clearcuts <- read_csv("/Volumes/TRISTANMAC/AgriBirds Sweden/QGIS and data/Martin_forsmark/forsmark_clearcuts.csv", 
                               col_types = cols(Avverktyp = col_skip(), 
                                                Efterbild = col_skip(), 
                                                Forebild = col_skip(), 
                                                Kallaareal = col_skip(), 
                                                Kalladatum = col_skip(), 
                                                Kommun = col_skip(), 
                                                Kommunnr = col_skip(), 
                                                Lan = col_skip(), 
                                                Lannr = col_skip()))

######################
## Data preparation ##
######################

forsmar <- as_tibble(forsmar)
forsmark_clearcuts <- as_tibble(forsmark_clearcuts)

## Calculate numerical dates
forsmark_clearcuts$Avvdatum <- as.POSIXct(forsmark_clearcuts$Avvdatum)
forsmar$Date <- as.POSIXct(forsmar$Date)
forsmar$Avvdatum <- as.POSIXct(forsmar$Avvdatum)
forsmar$birdday <- as.numeric(forsmar$Date)
forsmar$clearcutday <- as.numeric(forsmar$Avvdatum)
forsmar$month <- month(forsmar$Date)

## Select only the records for bird observations
forsmarsub <- forsmar[complete.cases(forsmar),]

## Exclude summer surveys
forsmarsub <- forsmarsub[forsmarsub$month<7,]

## Make number of individuals numeric
forsmarsub$Inds <- as.numeric(forsmarsub$Inds)

## Select points where clear cut was cut before date of bird observation
forsmarsub <- forsmarsub[forsmarsub$Date>forsmarsub$Avvdatum,]

## Calculate clearcut age in difference in days
forsmarsub$clearcutage <- year(forsmarsub$Date) - year(forsmarsub$Avvdatum)

## Subset nessecary columns from bird observation data
forsmarsub2 <- forsmarsub[,c(6,9,10,11,12,13,14,15,20)]

## Extract numerical date of clearcutting
# For bird observation clear cut
forsmarsub2$day <- as.numeric(forsmarsub2$Avvdatum)

# For all clear cuts
forsmark_clearcuts$day <- as.numeric(forsmark_clearcuts$Avvdatum)

# Tag datasets with their origin
forsmark_clearcuts$type <- "clearcuts"
forsmarsub2$type <- "birds"

# Combine data from all clearcuts, with the data from the bird 
# observations
all <- rbind(forsmark_clearcuts,forsmarsub2)

## format dates as date
forsmarsub$Date <- as.Date(forsmarsub$Date)
forsmarsub$Avvdatum <- as.Date(forsmarsub$Avvdatum)

###########
## Plots ##
###########

# theme for ggplot
theme_d <- theme(axis.text = element_text(size = 12, color = "black"),
                 axis.title = element_text(size = 14, color = "black"),
                 axis.line = element_line(color = "black"),
                 legend.text = element_text(size = 14, color = "black"))
hist_theme <- theme(axis.text = element_text(size = 12, color = "black"),
                    axis.title = element_text(size = 14, color = "black"),
                    axis.line = element_line(color = "black"),
                    legend.position = "none") 

## Plot of clearcutting day and bird survey date
lims <- c(as.Date("2002-08-01", "%Y-%m-%d"),as.Date("2017-08-01", "%Y-%m-%d"))

ggplot(forsmarsub, aes(Avvdatum, Date)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1) +
  scale_x_date(limits = lims) +
  scale_y_date(limits = lims) +
  theme_classic() +
  labs(x = "Date of clearcutting", y = "Date of RBS sighting") +
  theme_d

## Histogram of age of clear cut at date of observation
ggplot(forsmarsub, aes(x = clearcutage)) +
  geom_histogram(binwidth = 1, color = "gray45", fill = "gray80") +
  labs(x = "Age of clear cut (years)", y = "Number of observations") +
  scale_x_continuous(expand = c(0,0), limits = c(-1,12)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,25)) +
  hist_theme +
  theme_classic()

## Histogram of age of clearcuts in the area and age of clearcuts where 
## RBS were seen
ggplot(all, aes(x=day, fill=type)) +
  geom_density(alpha = 0.2) +
  labs(x = "Date of clearcutting", y = "Density") +
  theme_classic() +
  guides(fill = guide_legend(title=NULL)) +
  scale_fill_discrete(breaks=c("birds", "clearcuts"),
                      labels=c("RBS in clearcuts", "Clearcuts in area")) +
  theme_d


## Density plot of size of clearcuts in the area and size of clearcuts 
## where RBS were seen
ggplot(all, aes(x=Arealha, fill=type)) +
  geom_density(alpha = 0.2) +
  labs(x = "Area size (ha)", y = "Density") +
  theme_classic() +
  guides(fill = guide_legend(title=NULL)) +
  scale_fill_discrete(breaks=c("birds", "clearcuts"),
                      labels=c("RBS in clearcuts", "Clearcuts in area")) +
  theme_d

############
## Models ##
############

nestdatelm <- lm(forsmarsub$birdday ~ forsmarsub$clearcutday)
summary(nestdatelm)

observationmodel <- lme(Inds ~ clearcutage + Arealha + 1, random = ~1|Year, data = forsmarsub, method = 'ML')
summary(observationmodel)

#################
## quick stats ##
#################
## mean clear cut size of the area
mean(forsmark_clearcuts$Arealha)
median(forsmark_clearcuts$Arealha)
sd(forsmark_clearcuts$Arealha)

## mean clear cut size for RBS
mean(forsmarsub2$Arealha)
median(forsmarsub2$Arealha)
sd(forsmarsub2$Arealha)

t.test(forsmark_clearcuts$Arealha,forsmarsub2$Arealha, var.equal=FALSE, paired=FALSE)

## mean clear cut age
mean(forsmark_clearcuts$Avvdatum)
median(forsmark_clearcuts$day)
sd(forsmark_clearcuts$day)

## mean clear cut age for RBS
mean(forsmarsub2$Avvdatum)
median(forsmarsub2$day)
sd(forsmarsub2$day)

t.test(forsmark_clearcuts$day,forsmarsub2$day, var.equal=FALSE, paired=FALSE)




