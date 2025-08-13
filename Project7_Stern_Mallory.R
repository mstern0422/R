path <- "/Users/mj/Desktop/DSCI_302 (Intro to R)/kc_house_data.csv"
house_data <- read.csv(path)
print(house_data)

# First check the variables types 
sapply(house_data, class)

# Change finite variables to factor
house_data$floors <- as.factor(house_data$floors)
house_data$waterfront <- as.factor(house_data$waterfront)
house_data$view <- as.factor(house_data$view)
house_data$condition <- as.factor(house_data$condition)
house_data$grade <- as.factor(house_data$grade)

#Change infinite variables to numeric
house_data$bedrooms <- as.numeric(house_data$bedrooms)
house_data$bathrooms <- as.numeric(house_data$bathrooms)
house_data$sqft_living <- as.numeric(house_data$sqft_living)
house_data$sqft_lot <- as.numeric(house_data$sqft_lot)
house_data$sqft_above <- as.numeric(house_data$sqft_above)
house_data$sqft_basement <- as.numeric(house_data$sqft_above)
house_data$yr_built <- as.numeric(house_data$yr_built)
house_data$yr_renovated <- as.numeric(house_data$yr_renovated)
house_data$zipcode <- as.numeric(house_data$zipcode)
house_data$sqft_living15 <- as.numeric(house_data$sqft_living15)
house_data$sqft_lot15 <- as.numeric(house_data$sqft_lot15)

sapply(house_data, class)

# Linear model forecasting price using bedrooms, bathrooms, sqft_living
lm.result1 <- lm(price ~ bedrooms + bathrooms + sqft_living, data= house_data)

# Checking variance with r squared
summary(lm.result1)$r.squared

# Checking variance with r squared adjusted
summary(lm.result1)$adj.r.squared

# Linear model with same variables as before, this time with cross effects
lm.result2 <- lm(price ~ bedrooms * bathrooms * sqft_living, data= house_data)

# Looking at coefficients for math formula
coef(lm.result2)

# Checking variance with r squared adjusted
summary(lm.result2)$adj.r.squared

# Linear model with same variables as before including grade and waterfront
lm.result3 <- lm(price ~ bedrooms + bathrooms + sqft_living
                 + waterfront + grade, data= house_data)

# Checking variance
summary(lm.result3)$adj.r.squared

# Linear model forecasting price with all variables except id, date, zipcode
# lat and long and without a y-int
lm.result4 <- lm(price ~ -1+ bedrooms + bathrooms
                 + sqft_living + sqft_lot + floors + waterfront + view
                 + condition + grade + sqft_above + sqft_basement
                 + yr_built + yr_renovated + sqft_living15 + sqft_lot15
                 , data = house_data)

# Checking variance
summary(lm.result4)$adj.r.squared

# Linear model to be used for new created house data
lm.result5 <- lm(price ~ bedrooms + bathrooms
                 + sqft_living + sqft_lot + floors + waterfront + view
                 + condition + grade, data = house_data)

# Creating new house data
new.house <- data.frame(bedrooms = 4, 
                          bathrooms = 2, 
                          sqft_living = 2560,
                          sqft_lot = 7650, 
                          floors = 1.5,  
                          waterfront = 1, 
                          view = 3,
                          condition = 5,
                          grade = 10)

# Changing new house variables to factors as needed
new.house$floors <- as.factor(new.house$floors)
new.house$waterfront <- as.factor(new.house$waterfront)
new.house$view <- as.factor(new.house$view)
new.house$condition <- as.factor(new.house$condition)
new.house$grade <- as.factor(new.house$grade)

# Predicting price of new created house data
predict(lm.result5, newdata = new.house, interval = "predict")
