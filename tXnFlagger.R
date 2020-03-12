## tXn flagger app=============================================================


# metadata---------------------------------------------------------------------
# Author: Aarsh Batra
# Date: February 8, 2020
# R version: 3.4.2 (2017-09-28)



x <-  1

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
# unique rows in a dataset. 
                

# helper functions-------------------------------------------------------------

# making paths usable for Windows
correctPath <- function(){
  library(stringr)
  oldPath <- readline()
  newPath <- str_replace_all(oldPath, "\\\\", "/")
  setwd(newPath)
}

# set current working directory------------------------------------------------
setwd("C:/Users/Aarsh/Dropbox/(Transaction Flagger App)")


# Fretch LLC Data--------------------------------------------------------------

fretchData <- read_csv("IDRC Txn Flagger - DataSet 2 Fretch LLC.csv")

# clean fretchData

colnames(fretchData) <- fretchData[1, ]
fretchData <- fretchData[(2:nrow(fretchData)), ]
fretchData <- filter(fretchData, (is.na(FY) == TRUE | str_length(FY) == 3))

fillMonth <- fretchData$FY[1]

for(i in 1 : nrow(fretchData)){
  if(!is.na(fretchData$FY[i])){
    fillMonth <- fretchData$FY[i] 
  } else {
    fretchData$FY[i] <- fillMonth
  }
}

fretchData <- fretchData[, (2:ncol(fretchData))]

for(i in 1 : nrow(fretchData)){
  if(is.na(fretchData$Date[i]) == TRUE){
    # do nothing
  } else if (fretchData$Date[i] == "void"){
    # do nothing
  } else if (fretchData$Date[i] == "X"){
    # do nothing
  } else {
    fretchData$Date[i] <- lubridate::dmy(fretchData$Date[i])
    fretchData$N[i] <- as.numeric(fretchData$N[i])
    fretchData$Amount[i] <- as.numeric(str_extract(fretchData$Amount[i], "(\\d+)(\\.)(\\d+)"))
  }
}

fretchData$Date <-  as_date(as.numeric(fretchData$Date))

# basic checks
unique(fretchData$`On Account`)
unique(fretchData$Beneficiary)
range(fretchData$Amount)



# Online Retail Dataset--------------------------------------------------------

# useful facts about the dataset-----------------------------------------------
# customerID column is not a unique identifier column
# 



# read data--------------------------------------------------------------------
transDataRaw <- read_xlsx("Online Retail.xlsx", sheet = "Online Retail")

# cleaning the raw dataset-----------------------------------------------------
transDataClean <- transDataRaw

# add useful columns to the clean dataset----------------------------------------

# 1. date only column
# 2. country code column: 
#    note that converting to ISO country codes from the names provided in the
#    column named "Country" in the underlying dataset has some exceptions)

#    Warning message given by the 'countrycode' function: Some values 
#    were not matched unambiguously: Channel Islands, EIRE, 
#    European Community, RSA, Unspecified

# 3. log10 unit price column

transDataCleanExpanded <- dplyr::mutate(transDataClean, 
        dateOnly = as.Date(str_extract(InvoiceDate, "....-..-..")),
        ISOCountryCode = countrycode(
          Country, "country.name", "iso3c"
        ), log10UnitPrice = log10(UnitPrice))


# getting some statistics out of the clean expanded dataset--------------------

# unique customers
uniqueCustomersIDVec <- unique(transDataCleanExpanded$CustomerID)

# unique products
uniqueProductsVec <- unique(transDataCleanExpanded$Description)

# summary: number of transactions per customer
transDataCleanExpandedGrpCustomer <- dplyr::group_by(transDataCleanExpanded, 
                                                     CustomerID)
transDataCleanExpandedSummCustomer <- dplyr::summarise(
  transDataCleanExpandedGrpCustomer, numTrans = n() )

plt1 <- ggplot(transDataCleanExpandedSummCustomer) + 
  geom_bar(mapping = aes(x = CustomerID, y = numTrans), stat = "identity")

ggplotly(plt1)


# summary: number of transactions for each product by each customer
transDataCleanExpandedGrpCustomerProd <- dplyr::group_by(
  transDataCleanExpanded, CustomerID, Description)

transDataCleanExpandedSummCustomerProd <- dplyr::summarise(
  transDataCleanExpandedGrpCustomerProd, numTrans = n())


# for a given customer plot a distribution of the log of unit price
randCustomerID <- sample(transDataCleanExpanded$CustomerID, 1)
transDataCleanExpandedFilterByCustomer <- dplyr::filter(
  transDataCleanExpanded, CustomerID == randCustomerID
) 
plt2 <- ggplot(transDataCleanExpandedFilterByCustomer) + geom_histogram(
  mapping = aes(x = log10UnitPrice), binwidth = 0.005
)

plt3 <- ggplot(transDataCleanExpandedFilterByCustomer) + geom_density(
  mapping = aes(x = log10UnitPrice)
)

plt4 <- ggplot(transDataCleanExpandedFilterByCustomer) + geom_point(
  mapping = aes(x = log10UnitPrice), stat = "density" 
)

ggplotly(plt2)
ggplotly(plt3)


pval <- pnorm(transDataCleanExpandedFilterByCustomer$log10UnitPrice,
      mean = mean(transDataCleanExpandedFilterByCustomer$log10UnitPrice),
      sd = sd(transDataCleanExpandedFilterByCustomer$log10UnitPrice),
      lower.tail = FALSE)

# Write a function that generates a histogram for the frequency of 
# different types of products bought by a customer. The input to this
# function should be a customerID, the output should be a histogram 
# of the products bought by that customer

# prodsBought <- function(customerID, dataset)