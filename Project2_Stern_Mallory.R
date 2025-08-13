library(readxl)
SP500 <- read_excel("Desktop/DSCI_302 (Intro to R)/SP500.xls", 
                    sheet = "Data Org")
View(SP500)

SP500[ , c("P/Eratio")] <- NULL

# Columns and rows within the dataframe SP500
dim(SP500)

# Selecting columns SP500, CPI and Rate
SP500[ , c("SP500", "CPI", "Rate")]

# Selecting rows 10, 100, 500 and 1500
SP500[c(10, 100, 500, 1500), ]

# All observations where SP500 is > 2000, or CPI < 100
observ1 <- subset(SP500, (SP500 > 2000) | (CPI < 100))
print(observ1)

# All observations with Earnings > 50, Rate < 3, showing only SP500 and Dividend
observ2 <- subset(SP500, (Earnings > 50) & (Rate < 3), select = c(SP500, Dividend))
print(observ2)

# Removing column Rate
SP500$Rate <- NULL
print(colnames(SP500))

# Adding column RealPrice
SP500$RealPrice <- with(SP500, SP500*CPI/subset(SP500, (Date == 2018.09), select = c(CPI)))

# Adding column RealEarnings
SP500$RealEarnings <- with(SP500, Earnings*CPI/subset(SP500, (Date == 2018.09), select = c(CPI)))

# Adding column P/Eratio
SP500$PEratio <- with(SP500, RealPrice/RealEarnings)
head(SP500, 10)