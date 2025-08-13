path <- "/Users/mj/Desktop/DSCI_302 (Intro to R)/breast_cancer_data.csv"
breast_cancer_data <- read.csv(path)
print(breast_cancer_data)
library(ggplot2)
library(class) 

# Function to create boxplot form given columns
# argx is the x value, argy is the y value
BoxplotPredictorOnTarget <- function(argx, argy){
  genplot <- ggplot(data = breast_cancer_data, 
                    aes(x = argx, y = argy)) + geom_boxplot(aes(col = argx), 
                                                            notch = TRUE,
                    )
  return(genplot)
}

# Using created function for area_mean against diagnosis
BoxplotPredictorOnTarget(breast_cancer_data$diagnosis, breast_cancer_data$area_mean)

# Using created function for area_se against diagnosis
BoxplotPredictorOnTarget(breast_cancer_data$diagnosis, breast_cancer_data$area_se)

# Using created function for texture_mean against diagnosis
BoxplotPredictorOnTarget(breast_cancer_data$diagnosis, breast_cancer_data$texture_mean)

# -----------------------------------

# Convert diagnosis to factor
breast_cancer_data$diagnosis <- as.factor(breast_cancer_data$diagnosis)

# Selecting predictor as area_mean
predictor1 <- c("area_mean")
data.predictor1 <- breast_cancer_data[predictor1]

# Select target as diagnosis
data.target <- breast_cancer_data$diagnosis

# Splitting the data 80/20
sample.size <- floor(0.8*nrow(breast_cancer_data))

# Train contains %80 of the data excluding the target column
train1 <- data.predictor1[1:sample.size, , drop = F]

# Test contains %20 of the data excluding the target column
test1 <- data.predictor1[-c(1:sample.size), , drop = F]

# Selecting labels associated with train data
cl <- data.target[1:sample.size]

# Computing number of neighbors
num.neigbors <- floor(sqrt(nrow(breast_cancer_data)))

# Selecting target of test dataset
test.label <- data.target[-c(1:sample.size)]

# Run KNN test
knn.test.predict1 <- knn(train1, test1, cl, k = num.neigbors)

table(test.label, knn.test.predict1)

# -----------------------------------------------------------

# Selecting predictor as area_mean and area_se
predictor2 <- c("area_mean", "area_se")
data.predictor2 <- breast_cancer_data[predictor2]

# Train contains %80 of the data excluding the target column
train2 <- data.predictor2[1:sample.size, , drop = F]

# Test contains %20 of the data excluding the target column
test2 <- data.predictor2[-c(1:sample.size), , drop = F]

# Run KNN test for second KNN model
knn.test.predict2 <- knn(train2, test2, cl, k = num.neigbors)

# Present table for second KNN test
table(test.label, knn.test.predict2)

# -----------------------------------------------------------

# Selecting predictor as area_mean and area_se
predictor3 <- c("area_mean", "area_se", "texture_mean")
data.predictor3 <- breast_cancer_data[predictor3]

# Train contains %80 of the data excluding the target column
train3 <- data.predictor3[1:sample.size, , drop = F]

# Test contains %20 of the data excluding the target column
test3 <- data.predictor3[-c(1:sample.size), , drop = F]

# Run KNN test for second KNN model
knn.test.predict3 <- knn(train3, test3, cl, k = num.neigbors)

# Present table for second KNN test
table(test.label, knn.test.predict3)
