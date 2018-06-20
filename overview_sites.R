##############################
# Selection of clear cuts to #
# visit as exploration       #
##############################
install.packages("tidyverse")
library(tibble)
library(ggplot2)
library(lubridate)

clearcuts_first_visit <- read_csv("/Volumes/TRISTANMAC/AgriBirds Sweden/fieldwork/clearcuts_first_visit.csv")

clearcuts_first_visit <- as_tibble(clearcuts_first_visit)
colnames(clearcuts_first_visit) <- c("id", "date", "area", "distance", "location", "fortyp")

clearcuts_first_visit$date <- as.Date(clearcuts_first_visit$date, format = "%d/%m/%Y")
clearcuts_first_visit$year <- year(clearcuts_first_visit$date)

hist_theme <- theme(axis.text = element_text(size = 12, color = "black"),
                    axis.title = element_text(size = 14, color = "black"),
                    axis.line = element_line(color = "black"),
                    legend.position = "none") 

ggplot(clearcuts_first_visit, aes(x = area)) +
  geom_histogram(binwidth = 2, color = "gray45", fill = "gray75") +
  hist_theme +
  theme_classic() +
  scale_x_continuous(expand = c(0,0), limits = c(-1,12)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,6))

ggplot(clearcuts_first_visit, aes(x = distance)) +
  geom_histogram(binwidth = 200, color = "gray45", fill = "gray75") +
  hist_theme +
  theme_classic() +
  scale_x_continuous(expand = c(0,0), limits = c(-1,1300)) +
  scale_y_continuous(expand = c(0,0))

ggplot(clearcuts_first_visit, aes(x = year)) +
  geom_histogram(binwidth = 1, color = "gray45", fill = "gray80") +
  hist_theme +
  theme_classic() +
  scale_x_continuous(expand = c(0,0), limits = c(2012,2018)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,10))
