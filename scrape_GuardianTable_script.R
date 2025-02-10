#Load Libraries
library(rvest)
library(dplyr)
library(xml2)

rm(list = ls())

######################################################
#           Guardian Table
######################################################

#URL: which one and read the HTML
webpage_url <- "https://www.theguardian.com/education/ng-interactive/2021/sep/11/the-best-uk-universities-2022-rankings"
webpage <- xml2::read_html(webpage_url)

#Transform HTML to Table
GuardianTable <- rvest::html_table(webpage)[[1]] 

#Drop data that is NA (i.e., some weird error)
GuardianTable_clean <-GuardianTable %>%  na.omit()

#Glimpse
GuardianTable_clean %>% dplyr::glimpse(45)
