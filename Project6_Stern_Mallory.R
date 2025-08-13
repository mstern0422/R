path <- "/Users/mj/Desktop/DSCI_302 (Intro to R)/loan.csv"
loan <- read.csv(path)
print(loan)

# First loading the ggplot library into memory
library(ggplot2)

# Creating smaller subset of loan database
temploan <- loan[1:7000,]

# Creating a histogram with density of loan_amnt
# Includes a  vertical line representing the mean
ggplot(data = loan, aes(x = loan_amnt)) + geom_histogram(
  aes(y = after_stat(density)),
  colour = "darkorange",
  fill = "orange")+ geom_density(
    alpha = .3, 
    color = "blue", 
    fill = "lightblue") + geom_vline(
      aes(xintercept = mean(loan_amnt)), 
      color = "red", 
      linewidth = 1) + xlab("Loan Amount")

# Scatter plot of annual income vs loan amount including trend line
ggplot(data = temploan, aes(x = loan_amnt, y = annual_inc)) + geom_point(
  alpha = .2,
  colour = "red") + 
  geom_smooth() + ylab("Annual Income") + xlab("Loan Amount")

# Check that term and grade are factors
is.factor(loan$term)
is.factor(loan$grade)

# Both return false, converting to factors
loan$term <- as.factor(loan$term)
loan$grade <- as.factor(loan$grade)

# Bar plot of term vs grade 
ggplot(data = loan, aes(x = term, y = (..count..))) + geom_bar(
  aes(fill = grade),
  position = "dodge") + ylab("Count") + xlab("Term")

# Dot plot of grade vs loan_amnt
ggplot(data = temploan, aes(x = grade, y = loan_amnt)) + geom_dotplot(
  aes(fill = grade),
  binwidth = 500,
  alpha = .2,
  stackratio = .2,
  binaxis = "y",
  stackdir = "up") + ylab("Loan Amount") + xlab("Grade")

# Box plot of loan_amnt vs term
ggplot(data = loan, aes(x = term, y = loan_amnt)) + geom_boxplot(
  aes(col = term),
  notch = TRUE) + ylab("Loan Amount") + xlab("Term")

# Saving created graph as a .jpg
ggsave("/Users/mj/Desktop/DSCI_302 (Intro to R)/loanterm.jpg", 
       width = 10, 
       height = 25, 
       units = "cm")