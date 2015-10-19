#wikileaks cable Guantanamo analysis for Avery by DK
#10/18/15

#tasks
#(1) a variable showing the number of cables from each embassy in each year 
#containing the word Guantanamo 
#(2) a variable showing the number of times the keyword "Guantanamo" is mentioned 
#by each embassy in each year 
#(3)  a variable showing the number of cables from each embassy in each year 
#containing the word "Guantanamo" excluding cables that have the keyword "resettlement"

#set up work environment
setwd("C:/Users/dan/Desktop/guantanamo")
g <- read.csv("GuantanamoCables.csv")

install.packages("dplyr")
library("dplyr")

install.packages("stringr")
library("stringr")

install.packages("lubridate")
library(lubridate)

#data exploration
names(g)
head(g)
str(g)

levels(g$origin) #looks clean

##########
#cables/embassy/year containing the word "Guantanamo"
##########

#creating a column for whether a cable mentions Guantanamo
g$g <- str_detect(g$text, "Guantanamo")  #might want to check for misspellings or case
g$text[1] #passes a couple of spotchecks 

#parsing dates
g$created_date
g$date <- mdy_hm(g$created_date)
g$year <- year(g$date)

#summarizing cables/embassay/year
cab_emb_yea <- tbl_df(g) %>% filter(g == TRUE) %>% group_by(origin, year) %>% summarise(n_cables = n() )




##########
#number of times the word "Guantanamo" is mentioned per embassy/year 
##########

#creating a column for the number of times a cable contains the word "Guantanamo"
g$g_count <- str_count(g$text, "Guantanamo")  #might want to check for misspellings or case

#summarizing g/embassay/year
g_emb_yea <- tbl_df(g) %>% group_by(origin, year) %>% summarise(n_g = sum(g_count) )



##########
#cables/embassy/year containing the word "Guantanamo" but not the word "resettlement"
##########

#creating a column for whether a cable mentions Guantanamo
g$resettlement <- str_detect(g$text, "resettlement")  #might want to check for misspellings or case

#summarizing cables/embassay/year
cab_emb_yea_res <- tbl_df(g) %>% filter(g == TRUE & resettlement == FALSE) %>% group_by(origin, year) %>% summarise(n_cables = n() )




##########
#a little extra just for fun
##########

install.packages("ggplot2")
library("ggplot2")

#cables over time
ggplot(cab_emb_yea, aes(x = as.factor(year), y = n_cables)) + geom_jitter() + theme_bw()


#guantanamo mentions per embassy
tbl_df(g_emb_yea) %>% group_by(origin) %>% summarise(n_g_total = sum(n_g)) %>% filter(n_g_total > 100)  %>%
ggplot(aes(x = origin, y = n_g_total)) + geom_point() + theme_bw()


#the average number of mentions of "Guantanamo" per cable sent per embassy 
cables_sent <- tbl_df(g) %>% group_by(origin) %>% summarize(n_sent = n(), sum_g = sum(g_count)) 
cables_sent$avg_g_cable <- cables_sent$sum_g / cables_sent$n_sent

ggplot(cables_sent, aes(x = avg_g_cable)) + geom_histogram()
ggplot(subset(cables_sent, n_sent > 10), aes(x = avg_g_cable)) + geom_histogram()
ggplot(cables_sent, aes(x = n_sent, y = sum_g, color = avg_g_cable)) + geom_point() + 
  scale_color_continuous(name = "average mentions per cable", high = "red", low = "slate blue") + theme_bw() + 
  ylab("number of times Guantanamo is mentioned") + xlab("number of cables sent") 
#as expected, the more cables that are sent, the more times g is mentioned.  Need to account for that.  

