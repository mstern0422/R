library(readxl)
BlackFriday <- read_excel("Desktop/DSCI_302 (Intro to R)/BlackFriday.xlsx", 
                          sheet = "BlackFriday")
View(BlackFriday)

# Initialize sum variable
sum <- 0
# For loop that iterates through each row and adds purchase column to sum
for (i in c(1:nrow(BlackFriday))){
  sum <- sum + BlackFriday[i, "Purchase" ]
}
# Dividing the sum by the # of rows in BlackFriday dataframe
AvgPurchase <- sum/nrow(BlackFriday)
print(AvgPurchase)

# Initialize both sum and i variables
sum <- 0 
i <- 1
# While loop iterates until i is equal number of rows in given dataframe
while (i <= nrow(BlackFriday)){
  sum <- sum + BlackFriday[i, "Purchase"]
  # For each iteration, 1 is added to i
  i <- i + 1
}
AvgPurchase <- sum/nrow(BlackFriday)
print(AvgPurchase)

# Initialize sum and index variables
i <- 1
sum <- 0
# Repeat loop iterates until the if statement is met, breaking the loop
repeat {
  sum <- sum + BlackFriday[i, "Purchase"]
  i <- i +1
  # If condition that breaks loop when i is greater than # of rows in dataframe
  if (i > nrow(BlackFriday)){
    break
  }
}
AvgPurchase <- sum/nrow(BlackFriday)
print(AvgPurchase)

# Initialize sum variable
sum <- 0
# Create sub dataset from BlackFriday with only rows where the gender is female
FemaleData <-BlackFriday[BlackFriday$Gender == 'F', ]

# For loop that iterates through each row and adds purchase column to sum
for (i in c(1:nrow(FemaleData))){
  sum <- sum + FemaleData[i, "Purchase" ]
}
# Dividing the sum by the # of rows in FemaleData dataframe
AvgPurchase <- sum/nrow(FemaleData)
print(AvgPurchase)

# Initialize both sum and i variables
sum <- 0 
i <- 1
# While loop iterates until i is equal number of rows in dataframe
while (i <= nrow(FemaleData)){
  sum <- sum + FemaleData[i, "Purchase"]
  # For each iteration, 1 is added to i
  i <- i + 1
}
AvgPurchase <- sum/nrow(FemaleData)
print(AvgPurchase)
      
# Initialize sum and index variables
i <- 1
sum <- 0
# Repeat loop iterates until the if statement is met, breaking the loop
repeat {
  sum <- sum + FemaleData[i, "Purchase"]
  i <- i +1
  # If loop that breaks loop when i is greater then # of rows in dataframe
  if (i > nrow(FemaleData)){
    break
  }
}
AvgPurchase <- sum/nrow(FemaleData)
print(AvgPurchase)

# New data subset is created with rows where the gender is male
MaleData <-BlackFriday[BlackFriday$Gender == 'M', ]
# New sum objects specifically for the male and female sums
fsum <- 0
msum <- 0

# Initialize index variable
i <- 1
# Repeat loop iterates until the if statement is met, breaking the loop
# This is done for the FemaleData in this loop
repeat {
  # This time, the loops adds the found purchase data to fsum instead of sum
  fsum <- fsum + FemaleData[i, "Purchase"]
  i <- i +1
  # If loop that breaks loop when i is greater then # of rows in dataframe
  if (i > nrow(FemaleData)){
    break
  }
}

# Initialize sum and index variable
i <- 1
# Repeat loop iterates until the if statement is met, breaking the loop
# This loop is specifically for the MaleData
repeat {
  msum <- msum + MaleData[i, "Purchase"]
  i <- i +1
  # If loop that breaks loop when i is greater then # of rows in dataframe
  if (i > nrow(MaleData)){
    break
  }
} 

# The purchase average of both male and female sums are found
# The difference is then calculate by subtracting the male sum from the female
PurchaseDifference <- (msum/nrow(MaleData))-(fsum/nrow(FemaleData))
print(PurchaseDifference)