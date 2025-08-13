path <- "/Users/mj/Desktop/DSCI_302 (Intro to R)/loan.csv"
loan <- read.csv(path)
print(loan)

# Using a histogram to plot loan_amnt including the density
hist(loan$loan_amnt, 
     main = "Histogram of Loan Amount", 
     prob = TRUE,
     xlab = "Loan Amount",
     ylab = "Occurance")
# lines and density function to show density line
lines(density(loan$loan_amnt), lwd = 2, col = "orange")

# Plotting a dotchart with plot function
plot(loan$annual_inc, loan$loan_amnt,
     main = "Dot Chart of Annual Income against Loan Amount",
     xlab = "Annual Income",
     ylab = "Loan Amount")
# Adding a trend line
abline(lm(loan_amnt~annual_inc,
          data = loan),
       lwd = 2,
       col = "pink")

# First checking if term and grade are factors
is.factor(loan$grade)
is.factor(loan$term)

# False is returned for both. Turning both variables into factors 
loan$term <- as.factor(loan$term)
loan$grade <- as.factor(loan$grade)

# Creating cross table of grade and term
tbl.grades <- xtabs(~ grade + term, data = loan)
# Creating a bar plot of the created cross table
barplot(tbl.grades, main="Grade Distribution", 
        col = c("violet","hotpink", "orange","yellow","lightgreen","skyblue","purple"), 
        legend = rownames(tbl.grades),
        beside = TRUE)

# Creating a temp dataset since the loan dataset is so large
temploan <- loan[1:7000,]

# Plot chart for loan amount from created subdata set temploan
dotchart(temploan$loan_amnt, 
         cex = 0.4,  pch = 16, xlab = "Loan Amount")

# Box plot to show loan amount by term
boxplot(loan_amnt ~ term, 
        data = loan, 
        notch = TRUE, 
        col = c("violet", "skyblue"),
        main = "Loan Amount by Term", 
        ylab = "Loan Amount",
        xlab = "Term")
