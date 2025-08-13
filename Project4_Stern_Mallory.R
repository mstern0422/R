path <- "/Users/mj/Desktop/DSCI_302 (Intro to R)/loan.csv"
loanDB <- read.csv(path)
print(loanDB)

# Verify column class
sapply(loanDB, class)

# Change finite columns to factor class
loanDB$term <- as.factor(loanDB$term)
loanDB$grade <- as.factor(loanDB$grade)
loanDB$emp_length <- as.factor(loanDB$emp_length) 
loanDB$home_ownership <- as.factor(loanDB$home_ownership)
loanDB$verification_status <- as.factor(loanDB$verification_status)
loanDB$loan_status <- as.factor(loanDB$loan_status) 

# Change infinite columns to numerical class (if not already)
loanDB$id <- as.numeric(loanDB$id) 
loanDB$loan_amnt <- as.numeric(loanDB$loan_amnt) 
loanDB$annual_inc <- as.numeric(loanDB$annual_inc) 

# Rechecking columns class types
sapply(loanDB, class)

# Summary shows the min, max, mean, median, and 1st three quartile of loan_amnt
summary(loanDB[, c("loan_amnt")])

# Using sd() function to find standard dev of loan_amnt
sd(loanDB$loan_amnt)

# Summary shows the min, max, mean, median, and 1st three quartile of int_rate
summary(loanDB[, c("int_rate")])

# Quantile shows the 50th percentile within int_rate
quantile(loanDB$int_rate, probs = 0.5)

# Using sd() to find the standard deviation of int_rate
sd(loanDB$int_rate)

# Showing the correlation between int_rate and installment with cor()
cor(loanDB$int_rate, loanDB$installment)

# Showing each individual factor within the term column and their occurance
table(loanDB$term)

# Sorting factors within term by the most used factor
names(sort(-table(loanDB$term)))[1]

# Finding the proportion (percentage) of each occurring factor in loan_status
prop.table(table(loanDB$loan_status))

# Find the mode (highest perc) from loan_status
names(sort(-prop.table(table(loanDB$loan_status))))[1]

# First creating the xtab object using xtabs() for term and loan_status
xtab.term.loan_status <- xtabs(~ term + loan_status, data = loanDB)

# Showing proportion cross table in respect to rows (1)
prop.table(xtab.term.loan_status, margin = 1)

# Showing proportion cross table in respect to columns (2)
prop.table(xtab.term.loan_status, margin = 2)

# Summarizing entire loan.csv dataframe
summary(loanDB)