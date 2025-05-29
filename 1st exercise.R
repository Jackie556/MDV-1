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

# Create a grid of G1 and G2 values
grid <- expand.grid(G1 = seq(min(math_df$G1), max(math_df$G1), length.out = 50),
                    G2 = seq(min(math_df$G2), max(math_df$G2), length.out = 50))
grid$G3 <- predict(model, newdata = grid)


# Load libraries
library(rgl)
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

# Scatter plot
library(tidyverse)
library(GGally)

# Select relevant numeric features
features <- math_df %>% select(G1, G2, G3, age, Medu, Fedu)

# Create scatter plot matrix
ggpairs(features,
        columns = 1:6,
        title = "Scatter Plot Matrix of Student Performance",
        aes(color = math_df$school, alpha = 0.5)) +  # Color by school (GP/MS)
  theme_minimal()

# Select features and normalize data (for better visualization)
#Parallel coordinates
parallel_data <- math_df %>%
  select(G1, G2, G3, age, Medu, Fedu, internet) %>%
  mutate(across(c(G1, G2, G3), scale))  # Normalize grades

# Create parallel coordinates plot
ggparcoord(parallel_data,
           columns = 1:6,
           groupColumn = "internet",  # Group by internet access (0/1)
           alphaLines = 0.3,
           title = "Parallel Coordinates Plot") +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal()


#Radviz visualization

library(DataVisualizations)

# Select and normalize features (as matrix)
radviz_data <- math_df %>%
  select(G1, G2, age, Medu, Fedu) %>%
  scale() %>% 
  as.matrix()

# Define a custom RadViz function
radial_viz <- function(data, anchors, color_by = NULL, alpha = 0.7) {
  # Normalize data to [0, 1]
  data_normalized <- apply(data, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  
  # Calculate anchor angles
  n_anchors <- length(anchors)
  angles <- seq(0, 2 * pi, length.out = n_anchors + 1)[1:n_anchors]
  
  # Compute coordinates
  x <- rowSums(data_normalized * cos(angles))
  y <- rowSums(data_normalized * sin(angles))
  coords <- data.frame(x = x / rowSums(data_normalized),
                       y = y / rowSums(data_normalized))
  
  # Plot
  ggplot(coords, aes(x = x, y = y)) +
    geom_point(aes(color = color_by), alpha = alpha, size = 3) +
    geom_text(data = data.frame(x = cos(angles) * 1.1, 
                                y = sin(angles) * 1.1, 
                                label = anchors),
              aes(x = x, y = y, label = label), color = "black", size = 5) +
    coord_fixed() +
    theme_void() +
    labs(title = "Radial Visualization (RadViz)", color = "G3")
}


# Load libraries
library(tidyverse)

# Select features and normalize
radviz_data <- math_df %>%
  select(G1, G2, age, Medu, Fedu) %>%
  mutate(across(everything(), scale))

# Define anchors (features)
anchors <- colnames(radviz_data)

# Generate plot colored by G3 grades
radial_viz(
  data = radviz_data,
  anchors = anchors,
  color_by = math_df$G3  # Color points by final grade
) +
  scale_color_gradient(low = "blue", high = "red")  # Gradient for G3


# Add anchors (feature names)
text(cos(seq(0, 2*pi, length.out = 5)) * 1.1, 
     sin(seq(0, 2*pi, length.out = 5)) * 1.1,
     labels = colnames(radviz_data), 
     col = "black", cex = 1.2)

# Plot with color mapping to G3 grades

library(tidyverse)

# Custom RadViz function
radial_viz <- function(data, anchors, color_var) {
  # Normalize data to [0, 1]
  data_norm <- data %>%
    mutate(across(everything(), ~ (.x - min(.x)) / (max(.x) - min(.x))))
  
  # Calculate anchor angles (equally spaced around a circle)
  n_anchors <- length(anchors)
  angles <- seq(0, 2 * pi, length.out = n_anchors + 1)[1:n_anchors]
  
  # Compute coordinates for each data point
  coords <- data_norm %>%
    mutate(
      x = rowSums(sweep(data_norm, 2, cos(angles), "*")) / rowSums(data_norm),
      y = rowSums(sweep(data_norm, 2, sin(angles), "*")) / rowSums(data_norm)
    )
  
  # Compute anchor positions (for labels)
  anchor_coords <- tibble(
    x = cos(angles) * 1.1,  # Offset anchors slightly outward
    y = sin(angles) * 1.1,
    label = anchors
  )
  
  # Plot
  ggplot(coords, aes(x = x, y = y, color = color_var)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_text(data = anchor_coords, aes(x = x, y = y, label = label), 
              color = "black", size = 5, inherit.aes = FALSE) +
    scale_color_gradient(low = "blue", high = "red") +
    coord_fixed() +
    theme_void() +
    labs(title = "Radial Visualization (RadViz)", color = "G3")
}

#  With student data

radviz_data <- math_df %>% select(G1, G2, age, Medu, Fedu)
anchors <- colnames(radviz_data)

# Generate plot
radial_viz(
  data = radviz_data,
  anchors = anchors,
  color_var = math_df$G3  # Color by final grade
)

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


# Load libraries
library(tidyverse)
library(ggplot2)

# Data preprocessing (remove duplicates and convert factors)
math_df <- math_df %>% distinct()
math_df <- math_df %>% mutate_if(is.character, as.factor)

# Select numeric features for PCA
pca_features <- math_df %>% select(age, Medu, Fedu, G1, G2, G3)

# Scale features
pca_data <- scale(pca_features)

# Perform PCA
pca_res <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Variance explained by each PC
var_explained <- (pca_res$sdev)^2 / sum((pca_res$sdev)^2)
var_explained_pct <- var_explained * 100

# Print variance explained per component
print(round(var_explained_pct, 2))

# Cumulative variance for first two PCs
cum_var <- sum(var_explained_pct[1:2])
print(paste("Cumulative variance explained by first two PCs:", round(cum_var, 2), "%"))



# Prepare data frame for plotting
pca_scores <- as.data.frame(pca_res$x)
pca_scores$school <- math_df$school

# Plot first two PCs colored by school
pca_plot <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = school)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "PCA: First Two Principal Components",
       x = paste0("PC1 (", round(var_explained_pct[1], 1), "% variance)"),
       y = paste0("PC2 (", round(var_explained_pct[2], 1), "% variance)")) +
  theme_minimal()

# Save plot
ggsave("pca_scatter.png", plot = pca_plot, width = 8, height = 6)

# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Data preprocessing: remove duplicates, convert character to factor
math_df <- math_df %>% distinct()
math_df <- math_df %>% mutate_if(is.character, as.factor)

# Select numeric features for PCA
pca_features <- math_df %>% select(age, Medu, Fedu, G1, G2, G3)

# Scale the features
pca_data <- scale(pca_features)

# Perform PCA
pca_res <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Calculate variance explained
var_explained <- (pca_res$sdev)^2 / sum((pca_res$sdev)^2)
var_explained_pct <- var_explained * 100
cum_var <- cumsum(var_explained_pct)

# Print variance explained
print(round(var_explained_pct, 2))
print(round(cum_var, 2))

# Prepare PCA scores dataframe and add school for coloring
pca_scores <- as.data.frame(pca_res$x)
pca_scores$school <- math_df$school

# Function to plot and save PC scatter plots
plot_pc <- function(df, xpc, ypc, xvar, yvar) {
  p <- ggplot(df, aes_string(x = xpc, y = ypc, color = "school")) +
    geom_point(alpha = 0.7, size = 2) +
    labs(title = paste0("PCA: ", xpc, " vs ", ypc),
         x = paste0(xpc, " (", round(var_explained_pct[xvar], 1), "%)"),
         y = paste0(ypc, " (", round(var_explained_pct[yvar], 1), "%)")) +
    theme_minimal()
  
  filename <- paste0(tolower(xpc), "_vs_", tolower(ypc), ".png")
  ggsave(filename, plot = p, width = 8, height = 6)
  print(p)
}

# Plot PC1 vs PC2
plot_pc(pca_scores, "PC1", "PC2", 1, 2)

# Plot PC1 vs PC3
plot_pc(pca_scores, "PC1", "PC3", 1, 3)

# Plot PC2 vs PC3
plot_pc(pca_scores, "PC2", "PC3", 2, 3)

# Plot PC1 vs PC4
plot_pc(pca_scores, "PC1", "PC4", 1, 4)

# Plot PC2 vs PC4
plot_pc(pca_scores, "PC2", "PC4", 2, 4)

# Plot PC3 vs PC4
plot_pc(pca_scores, "PC3", "PC4", 3, 4)

# Plot Variance Explained Bar Chart
var_df <- data.frame(
  PC = paste0("PC", 1:length(var_explained_pct)),
  Variance = var_explained_pct,
  Cumulative = cum_var
)

p_var <- ggplot(var_df, aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = Cumulative, group = 1), color = "red", size = 1) +
  geom_point(aes(y = Cumulative), color = "red", size = 2) +
  labs(title = "Variance Explained by Principal Components",
       y = "Variance Explained (%)",
       caption = "Red line shows cumulative variance") +
  theme_minimal()

ggsave("variance_explained.png", plot = p_var, width = 8, height = 6)
print(p_var)


#Multidimensional Scaling


# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Read and prepare data

math_df <- unique(math_df)
math_df <- math_df %>% mutate_if(is.character, as.factor)
mds_features <- math_df %>% select(age, Medu, Fedu, G1, G2, G3)
mds_matrix <- scale(mds_features)

# Compute distance matrix
dist_matrix <- dist(mds_matrix)

# 1. Classical MDS (cmdscale) - default initialization
mds_cmd <- cmdscale(dist_matrix, k = 2)
mds_df1 <- data.frame(MDS1 = mds_cmd[,1], MDS2 = mds_cmd[,2], school = math_df$school)

# 2. Metric MDS with random start (SMACOF)
library(smacof)
set.seed(123)
mds_smacof <- mds(dist_matrix, ndim = 2, type = "interval", init = "random")
mds_df2 <- data.frame(MDS1 = mds_smacof$conf[,1], MDS2 = mds_smacof$conf[,2], school = math_df$school)

# Plot Classical MDS
ggplot(mds_df1, aes(x = MDS1, y = MDS2, color = school)) +
  geom_point(alpha = 0.7) +
  labs(title = "Classical MDS (cmdscale) on Student Data") +
  theme_minimal()
ggsave("mds_classical.png", width = 8, height = 6)

# Plot Metric MDS (SMACOF, random init)
ggplot(mds_df2, aes(x = MDS1, y = MDS2, color = school)) +
  geom_point(alpha = 0.7) +
  labs(title = "Metric MDS (SMACOF, Random Init) on Student Data") +
  theme_minimal()
ggsave("mds_smacof.png", width = 8, height = 6)


# Assume 'math_df' is my cleaned dataset and contains only numeric columns for depth calculation

# Select relevant numeric features
data_matrix <- math_df %>% select(age, Medu, Fedu, G1, G2, G3) %>% scale() %>% as.matrix()

# Calculate the center (mean) of the data
center <- colMeans(data_matrix)

# Calculate Euclidean depth (negative distance to center)
euclidean_depth <- -apply(data_matrix, 1, function(row) sqrt(sum((row - center)^2)))

# Add the depth to your dataframe
math_df$euclidean_depth <- euclidean_depth

# Visualize: plot depth vs. G3
library(ggplot2)
ggplot(math_df, aes(x = euclidean_depth, y = G3, color = school)) +
  geom_point() +
  labs(title = "Euclidean Depth vs Final Grade (G3)", x = "Euclidean Depth (Centrality)", y = "Final Grade")


# Calculate the multivariate median (componentwise)
center_median <- apply(data_matrix, 2, median)

# Calculate Euclidean distance from each point to the median (depth is negative distance)
euclidean_depth_median <- -apply(data_matrix, 1, function(row) sqrt(sum((row - center_median)^2)))

# Add to data frame
math_df$euclidean_depth_median <- euclidean_depth_median

# Visualization: Depth vs G3
library(ggplot2)
ggplot(math_df, aes(x = euclidean_depth_median, y = G3, color = school)) +
  geom_point(alpha = 0.7) +
  labs(title = "Euclidean Depth to Median vs Final Grade (G3)", 
       x = "Euclidean Depth to Median (Centrality)", y = "Final Grade")
ggsave("euclidean_depth_median_plot.png", width = 8, height = 6)



