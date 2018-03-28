
install.packages("ggplot2")
library(ggplot2)
library(readr)

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

## Calculate numerical dates
forsmar$birdday <- as.numeric(forsmar$Date)
forsmar$clearcutday <- as.numeric(forsmar$Avvdatum)

## Select complete cases
forsmarsub <- forsmar[complete.cases(forsmar),]
forsmarsub2 <- forsmarsub[,c(6,9,10,11,12,13,14,15,20)]
forsmarsub2$day <- as.numeric(forsmarsub2$Avvdatum)

forsmark_clearcuts$day <- as.numeric(forsmark_clearcuts$Avvdatum)

forsmark_clearcuts <- data.frame(forsmark_clearcuts)
forsmarsub2 <- data.frame(forsmarsub2)

forsmark_clearcuts$type <- "clearcuts"
forsmarsub2$type <- "birds"

all <- rbind(forsmark_clearcuts,forsmarsub2)

###########
## Plots ##
###########

#theme
theme_d <- theme(axis.text = element_text(size = 12, color = "black"),
                 axis.title = element_text(size = 14, color = "black"),
                 axis.line = element_line(color = "black"),
                 legend.text = element_text(size = 14, color = "black"))

## Plot of cleacutting day and bird survey date
ggplot(forsmarsub, aes(clearcutday, birdday)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  labs(x = "Date of clearcutting", y = "Date of RBS sighting") +
  theme_d


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



