#1st exercise

# Load libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(GGally)
library(corrplot)
library(randomForest)

# Load datasets
math <- read.csv("D:/1st year 2nd semester/MDV/1st exercise/student-mat.csv",
                 sep = ";")

por <- read.csv("D:/1st year 2nd semester/MDV/1st exercise/student-por.csv",
                sep = ";")

# Combine datasets (optional, if needed)
combined_df <- bind_rows(math, por)


# Check for missing values
sum(is.na(math))  # Math dataset
sum(is.na(por))    # Portuguese dataset


# Remove duplicate rows from the dataset
math_df<- unique(math)
por_df<- unique(por)


# View the cleaned dataset
head(math_df)

combined_df <- combined_df %>% distinct()

# Convert categorical variables to factors
math_df <- math_df %>% mutate_if(is.character, as.factor)
por_df <- por_df %>% mutate_if(is.character, as.factor)


# Summary of numeric features
summary(math_df %>% select(age, Medu, Fedu, G1, G2, G3))
summary(por_df %>% select(age, Medu, Fedu, G1, G2, G3))

# Class distribution for categorical features
table(math_df$school)
table(math_df$sex)
table(math_df$address)


# Boxplot of G3 by school
ggplot(math_df, aes(x = school, y = G3, fill = school)) +
  geom_boxplot() +
  labs(title = "Final Grade (G3) by School", x = "School", y = "G3")

# Histogram of G3
ggplot(math_df, aes(x = G3)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Final Grades (G3)", x = "G3", y = "Count")

# Pair plot for numeric features
ggpairs(math_df %>% select(age, Medu, Fedu, G1, G2, G3))


# Create average grade feature
math_df <- math_df %>% mutate(avg_grade = (G1 + G2) / 2)
por_df <- por_df %>% mutate(avg_grade = (G1 + G2) / 2)

# Convert binary features to numeric (for correlation analysis)
math_df <- math_df %>% mutate(
  schoolsup = ifelse(schoolsup == "yes", 1, 0),
  famsup = ifelse(famsup == "yes", 1, 0),
  paid = ifelse(paid == "yes", 1, 0),
  activities = ifelse(activities == "yes", 1, 0),
  higher = ifelse(higher == "yes", 1, 0),
  internet = ifelse(internet == "yes", 1, 0),
  romantic = ifelse(romantic == "yes", 1, 0)
)



d1=read.table("D:/1st year 2nd semester/MDV/1st exercise/student-mat.csv",
              sep = ";",header=TRUE)

d2=read.table("D:/1st year 2nd semester/MDV/1st exercise/student-por.csv",
              sep = ";",header=TRUE)


d3=merge(d1,d2,by=c("school","sex","age","address","famsize",
                    "Pstatus","Medu","Fedu","Mjob","Fjob","reason",
                    "nursery","internet"))
print(nrow(d3)) # 382 students


# Correlation matrix for numeric features
corr_matrix <- cor(math_df %>% select(age, Medu, Fedu, G1, G2, G3, avg_grade))
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "blue",
         diag = FALSE)


set.seed(123)
train_index <- createDataPartition(math_df$G3, p = 0.8, list = FALSE)
train_data <- math_df[train_index, ]
test_data <- math_df[-train_index, ]


# Train linear regression model
lm_model <- lm(G3 ~ age + Medu + Fedu + G1 + G2 + avg_grade, data = train_data)
summary(lm_model)

# Predict on test data
predictions <- predict(lm_model, test_data)
predictions


# Evaluate model
rmse <- sqrt(mean((test_data$G3 - predictions)^2))
print(paste("RMSE:", rmse))



# Train random forest model
rf_model <- randomForest(G3 ~ age + Medu + Fedu + G1 + G2 + avg_grade,
                         data = train_data, ntree = 100)

# Predict on test data
rf_predictions <- predict(rf_model, test_data)
rf_predictions

# Evaluate model
rf_rmse <- sqrt(mean((test_data$G3 - rf_predictions)^2))
print(paste("Random Forest RMSE:", rf_rmse))



# Load UMAP library
library(umap)

# Select numeric features for UMAP
umap_data <- math_df %>% select(age, Medu, Fedu, G1, G2, G3)
umap_data
# Normalize the data (UMAP works better with normalized data)
umap_data <- scale(umap_data)

# Set seed for reproducibility


# Compute UMAP
set.seed(42)
umap_result <- umap(umap_data, n_components = 2)  # Reduce to 2 dimensions
umap_result


# Create a data frame for plotting
umap_df <- data.frame(
  UMAP1 = umap_result$layout[, 1],
  UMAP2 = umap_result$layout[, 2],
  Class = math_df$G3  # Use G3 as the class for coloring
)
umap_df


# Plot UMAP
ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Class)) +
  geom_point(alpha = 0.7) +
  labs(title = "UMAP of Student Data", x = "UMAP1", y = "UMAP2") +
  scale_color_gradient(low = "blue", high = "red")  # Gradient for numeric G3


# Plot parallel coordinates
ggparcoord(math_df, columns = c(3, 7, 8, 31, 32, 33), groupColumn = "school", 
           alpha = 0.5) +
  labs(title = "Parallel Coordinates Plot")


# Extract feature importance          

library(randomForest)

# Assuming rf_model is your random forest model
importance <- importance(rf_model)

# Create a colorful feature importance plot
varImpPlot(rf_model, 
           main = "Feature Importance (Random Forest)",
           col = rainbow(nrow(importance)), # Use rainbow colors for the bars
           pch = 19, # Use solid circles for the points
           cex = 1.2 # Adjust the size of the points
)

# Save predictions
write.csv(data.frame(Actual = test_data$G3, Predicted = predictions),
 
               "predictions.csv", row.names = FALSE)

# Select numeric features for 3D plotting
plot_data <- math_df %>% select(G1, G2, G3, age, Medu, Fedu)

# Normalize data (optional, for better scaling)
plot_data <- scale(plot_data)


# Load plotly
library(plotly)


# Select features for 3D plot
x <- math_df$G1  # First period grade
y <- math_df$G2  # Second period grade
z <- math_df$G3  # Final grade
color <- math_df$age  # Color by age

# Create 3D scatter plot
plot_ly(math_df, x = ~x, y = ~y, z = ~z, color = ~color, 
        colors = colorRamp(c("blue", "red"))) %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = "G1"),
    yaxis = list(title = "G2"),
    zaxis = list(title = "G3")
  ), title = "3D Scatter Plot of Grades (G1, G2, G3)")



# Predict G3 using a simple linear model

# Create a grid for G1 and G2
g1_range <- seq(min(math_df$G1), max(math_df$G1), length.out = 20)
g2_range <- seq(min(math_df$G2), max(math_df$G2), length.out = 20)
grid <- expand.grid(G1 = g1_range, G2 = g2_range)


#Fit a regression model
model <- lm(G3 ~ G1 + G2, data = math_df)
grid$G3 <- predict(model, newdata = grid)


# Create 3D surface plot
plot_ly(grid, x = ~G1, y = ~G2, z = ~G3, type = "surface",
        colors = colorRamp(c("blue", "red"))) %>%
  layout(scene = list(
    xaxis = list(title = "G1"),
    yaxis = list(title = "G2"),
    zaxis = list(title = "G3")
  ), title = "3D Surface Plot of Grades (G1, G2, G3)")


# Fit a regression model (example: G3 ~ G1 + G2)
model <- lm(G3 ~ G1 + G2, data = math_df)

# Create a grid of G1 and G2 values
grid <- expand.grid(G1 = seq(min(math_df$G1), max(math_df$G1), length.out = 50),
                    G2 = seq(min(math_df$G2), max(math_df$G2), length.out = 50))
grid$G3 <- predict(model, newdata = grid)

# Create a 3D surface plot
plot_ly(grid, x = ~G1, y = ~G2, z = ~G3, type = "surface",
        colors = colorRamp(c("blue", "red"))) %>%
  layout(scene = list(
    xaxis = list(title = "G1 (First Period Grade)"),
    yaxis = list(title = "G2 (Second Period Grade)"),
    zaxis = list(title = "G3 (Final Grade)")
  ), title = "3D Surface Plot of Predicted G3")


library(rgl)


# Load libraries
library(dbscan)



# Select numeric features for clustering
cluster_data <- math_df %>% select(G1, G2, G3)

# Normalize the data (optional, but recommended for DBSCAN)
cluster_data_scaled <- scale(cluster_data)

# Convert scaled data back to a data frame
cluster_data_scaled <- as.data.frame(cluster_data_scaled)


# Perform DBSCAN clustering
set.seed(123)
dbscan_result <- dbscan(cluster_data_scaled, eps = 0.5, minPts = 5)

# Add cluster labels to the original data
cluster_data$cluster <- as.factor(dbscan_result$cluster)


# Create an interactive 3D scatter plot
plot_ly(cluster_data, x = ~G1, y = ~G2, z = ~G3, color = ~cluster,
        colors = c("red", "black", "green")) %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = "G1 (First Period Grade)"),
    yaxis = list(title = "G2 (Second Period Grade)"),
    zaxis = list(title = "G3 (Final Grade)")
  ), title = "3D DBSCAN Clustering of Student Grades")



# Load libraries
library(ggplot2)

# Boxplot for G3 (final grade)
ggplot(math_df, aes(y = G3)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Final Grades (G3)", y = "G3")

# Identify outliers using the IQR method
Q1 <- quantile(math_df$G3, 0.25)
Q3 <- quantile(math_df$G3, 0.75)
IQR <- Q3 - Q1

outliers <- math_df %>% filter(G3 < (Q1 - 1.5 * IQR) | G3 > (Q3 + 1.5 * IQR))
print(paste("Number of outliers in G3:", nrow(outliers)))



# Add random noise to G3
set.seed(123)
noise <- rnorm(nrow(math_df), mean = 0, sd = 1)  # Adjust sd for noise level
math_df$G3_noisy <- math_df$G3 + noise

# Compare original and noisy G3
ggplot(math_df, aes(x = G3, y = G3_noisy)) +
  geom_point(alpha = 0.5) +
  labs(title = "Original vs Noisy G3", x = "Original G3", y = "Noisy G3")


# Load libraries
library(caret)

# Train a linear regression model with cross-validation
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation
model <- train(G3 ~ G1 + G2, data = math_df, method = "lm", trControl = train_control)

# Print cross-validation results
print(model)


# Remove a random subset of data (e.g., 10%)
set.seed(123)
subset_index <- sample(1:nrow(math_df), size = 0.9 * nrow(math_df))
subset_data <- math_df[subset_index, ]

# Train a model on the subset
subset_model <- lm(G3 ~ G1 + G2, data = subset_data)
subset_predictions <- predict(subset_model, math_df)

# Compare predictions
results <- data.frame(
  Actual = math_df$G3,
  Full_Model = predict(model, math_df),  # Model trained on full data
  Subset_Model = subset_predictions
)

# Calculate RMSE for both models
rmse_full <- sqrt(mean((results$Actual - results$Full_Model)^2))
rmse_subset <- sqrt(mean((results$Actual - results$Subset_Model)^2))
print(paste("RMSE (Full Model):", rmse_full))
print(paste("RMSE (Subset Model):", rmse_subset))






