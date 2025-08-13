library(readxl)
CarInsurances <- read_excel("Desktop/DSCI_302 (Intro to R)/CarInsurances.xlsx", 
                            sheet = "Data")
View(CarInsurances)

# Using the dim function to view the # of rows and columns, respectively
dim(CarInsurances)

# Assigning the first 8 rows of the dataset to the variable first.eight.rows
# Print function then shows this newly created variable
first.eight.rows <- head(CarInsurances, 8)
print(first.eight.rows)

# Assigning the last 5 rows of the dataset to the variable five.rows
# Print function then shows this newly created variable
five.rows <- tail(CarInsurances, 5)
print(five.rows)

# Using ls() to show all current variables
ls()

# Using objects() to show all current variables
objects()

# Using summary() to print summarized data
summary(CarInsurances)


