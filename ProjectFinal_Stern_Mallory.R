path <- "/Users/mj/Desktop/DSCI_302 (Intro to R)/pmsm_temperature_data.csv"
ptdata <- read.csv(path)
print(ptdata)
library(ggplot2)
library(class) 

# Convert strator_yoke to factor
ptdata$strator_yoke <- as.factor(ptdata$stator_yoke)

# Min of pm
min(ptdata$pm, na.rm = TRUE)
# Max of pm
max(ptdata$pm, na.rm = TRUE)
# Median of pm
median(ptdata$pm, na.rm = TRUE)
# Standard deviation of pm
sd(ptdata$pm, na.rm = TRUE)
# Three quartiles of pm
quantile(ptdata$pm, na.rm = TRUE, probs = c(0.25, 0.5, 0.75))

# Summary of motor_speed
summary(ptdata$motor_speed, na.rm = TRUE)
# Standard deviation of motor speed
sd(ptdata$motor_speed, na.rm = TRUE)

# Calculating correlation coefficient of motor_speed and pm
cor(ptdata$motor_speed, ptdata$pm, use = "complete.obs")

# Frequency table for strator_yoke
table(ptdata$stator_yoke)
# Mode of strator_yoke
names(sort(-table(ptdata$stator_yoke)))[1] 

# Histogram and density plot with mean line for pm
ggplot(data = ptdata, 
       aes(x = pm)) + geom_histogram(aes(y = ..density..),
       colour = "black",
       fill = "white") + geom_density(alpha = .3,
       fill = "orange") + geom_vline(aes(xintercept = mean(pm)),
        color = "blue",
        size = 1)

# Creating smaller subset of pmsm database
temppt <- ptdata[1:50000,]

# Scatterplot of pm against motor_speed with trendline
ggplot(data = temppt, 
       aes(x = motor_speed, y = pm)) + geom_point() + geom_smooth()

# Box plot of pm against strator_yoke
ggplot(data = ptdata, 
       aes(x = stator_yoke, 
       y = pm)) + geom_boxplot(aes(col = stator_yoke),
       notch = TRUE) + ylab("PM") + xlab("Stator Yoke")

# Saving box plot as jpeg
ggsave("/Users/mj/Desktop/DSCI_302 (Intro to R)/pmyoke.jpg", 
       width = 10, 
       height = 25, 
       units = "cm")

# First checking variable types
sapply(ptdata, class)

# Linear regression for pm with Ambient, Coolant, motor_speed, and Torque as predictors
lm.result1 <- lm(pm ~ ambient + coolant + motor_speed + torque, data = ptdata)
summary(lm.result1)$adj.r.squared

# Coefficents for first linear regression model
summary(lm.result1)$coefficients

# Linear regression for pm with Ambient, Coolant, u_d, motor_speed, and Torque as predictors
lm.result2 <- lm(pm ~ ambient + coolant + u_d + motor_speed + torque, data = ptdata)
summary(lm.result2)$adj.r.squared

# Coefficents for first linear regression model
summary(lm.result2)$coefficients

# Linear regression for pm with Ambient, Coolant, u_d, motor_speed, and Torque as predictors
lm.result3 <- lm(pm ~ ambient + coolant + u_d + motor_speed + torque + stator_yoke + stator_winding, 
                 data = ptdata)
summary(lm.result3)$adj.r.squared

# Coefficents for first linear regression model
summary(lm.result3)$coefficients

# --------------------------------
ptdata <- na.omit(ptdata)

# Function to normalize the data
NormalizedData <- function (val){ 
  result <- (val - min(val))/(max(val) - min(val)) 
  return(result)} 

# Splitting the data 80/20
sample.size <- floor(0.8*nrow(ptdata))

# Selecting predictor as pm, ambient, and coolant
predictor1 <- c("pm", "ambient", "coolant")
data.predictor1 <- ptdata[predictor1]

# Select target as diagnosis
data.target <- ptdata$stator_yoke

# Train contains %80 of the data excluding the target column
train1 <- data.predictor1[1:sample.size, , drop = F]

# Test contains %20 of the data excluding the target column
test1 <- data.predictor1[-c(1:sample.size), , drop = F]

# Selecting labels associated with train data
cl <- data.target[1:sample.size]

# Run KNN test
knn.test.predict1 <- knn(train1, test1, cl, k = 1)

# Selecting target of test dataset
test.label <- data.target[-c(1:sample.size)]
table(test.label, knn.test.predict1)

# ------------

# Selecting predictor as Pm, Ambien, Coolant, and  motor_speed 
predictor2 <- c("pm", "ambient", "coolant", "motor_speed")
data.predictor2 <- ptdata[predictor2]

# Train contains %80 of the data excluding the target column
train2 <- data.predictor2[1:sample.size, , drop = F]

# Test contains %20 of the data excluding the target column
test2 <- data.predictor2[-c(1:sample.size), , drop = F]

# Run KNN test for second KNN model
knn.test.predict2 <- knn(train2, test2, cl, k = 1)

# Present table for second KNN test
table(test.label, knn.test.predict2)

# -------------

# Selecting predictor as Pm, Ambien, Coolant, and  motor_speed, u_d, u_d, torque
predictor3 <- c("pm", "ambient", "coolant", "motor_speed", "u_d", "u_q", "torque")
data.predictor3 <- ptdata[predictor3]

# Train contains %80 of the data excluding the target column
train3 <- data.predictor3[1:sample.size, , drop = F]

# Test contains %20 of the data excluding the target column
test3 <- data.predictor3[-c(1:sample.size), , drop = F]

# Run KNN test for third KNN model
knn.test.predict3 <- knn(train3, test3, cl, k = 1)

# Present table for third KNN test
table(test.label, knn.test.predict3)
