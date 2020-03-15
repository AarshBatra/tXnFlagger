## tXn flagger app=============================================================


# metadata---------------------------------------------------------------------
# Author: Aarsh Batra
# Date: March 14, 2020
# R version: 3.4.2 (2017-09-28)


# libraries and helper files---------------------------------------------------
library(tidyverse)
library(stringr)
library(dplyr)
library(lubridate)
library(readxl)
library(xlsx)
library(plotly)
library(expss) # for count_if, etc.
library(ggplot2)
library(magrittr)
library(readr)
library(countrycode)
library(wrapr) # learn more about this, useful for finding 
 
# set working directory-------------------------------------------------------
setwd("C:/Users/Aarsh/Dropbox/TxnFlagger")

# get clean dataset-----------------------------------------------------------

# This will come from the "cleaning.R" file. For now, I have the clean 
# dataset ready.

dsCleaned <- read_xlsx("tXnFlagger.xlsx", sheet = "Sheet1")

# cleaning steps: to be transferred to the cleaning file
dsCleaned$Amount <- as.numeric(dsCleaned$Amount)

# Rules-----------------------------------------------------------------------

# Flag transactions, and for each transaction list the rule(s) that caused
# its flagging.

# Rule 1: Flag new counterparties with dates and amounts of transactions

dsCleaned_arrBenDate <- dsCleaned %>% dplyr::arrange(Beneficiary, 
                                                       Date)

dsCleaned_arrBenDate_grpBen <- dsCleaned_arrBenDate %>% dplyr::group_by(
  Beneficiary
)

dsCleaned_arrBenDate_grpBen_summSelFirst <- dsCleaned_arrBenDate_grpBen %>%
  dplyr::summarise(firstTransUniqID = uniqID[[1]])


dsCleaned_filterRule1 <- dplyr::filter(
  dsCleaned, uniqID %in% dsCleaned_arrBenDate_grpBen_summSelFirst$firstTransUniqID)

dsCleaned_filterRule1 <- dsCleaned_filterRule1 %>% dplyr::mutate(
  ruleUniqueID = "r1"
)

# Rule 2: Exceptional Amounts per counterparty 

dsCleaned_grpBen <- dplyr::group_by(dsCleaned, Beneficiary)
dsCleaned_grpBen_summStats <- dsCleaned %>% dplyr::summarise(
  avgAmountSpent = mean(Amount, na.rm = TRUE), 
  stDevAmountSpent = sd(Amount, na.rm = TRUE)
)

