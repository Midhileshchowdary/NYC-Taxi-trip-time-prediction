# Install necessary packages if not already installed
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(geosphere)) install.packages("geosphere", dependencies = TRUE)
if (!require(readr)) install.packages("readr", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)

# Load necessary libraries
library(ggplot2)
library(geosphere)
library(readr)
library(dplyr)

# Load the CSV file from the local file system
file_path <-  "C:/Users/MIDHILESH CHOWDARY/Downloads/NYC Taxi Data.csv"
# Check if the file exists
if (!file.exists(file_path)) stop("File does not exist at the specified path")

# Load the CSV file
df <- read_csv(file_path)

# Check the structure of the data
str(df)

# Function to calculate distance between pickup and dropoff points (in kilometers)
calculate_distance <- function(pickup_latitude, pickup_longitude, dropoff_latitude, dropoff_longitude) {
  pickup_coords <- c(pickup_longitude, pickup_latitude)
  dropoff_coords <- c(dropoff_longitude, dropoff_latitude)
  distHaversine(pickup_coords, dropoff_coords) / 1000  # Convert meters to kilometers
}

# Apply distance calculation
df <- df %>%
  mutate(distance_km = mapply(calculate_distance, pickup_latitude, pickup_longitude,
                              dropoff_latitude, dropoff_longitude))

# Check for missing values
if (any(is.na(df$distance_km))) stop("Missing values in distance_km")

# Convert pickup_datetime to datetime format and extract useful time features
df$pickup_datetime <- as.POSIXct(df$pickup_datetime, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
if (any(is.na(df$pickup_datetime))) stop("Invalid datetime format in pickup_datetime")

df$pickup_hour <- as.numeric(format(df$pickup_datetime, "%H"))
df$pickup_day <- as.numeric(format(df$pickup_datetime, "%d"))
df$pickup_month <- as.numeric(format(df$pickup_datetime, "%m"))
df$pickup_dayofweek <- as.numeric(format(df$pickup_datetime, "%u"))

# Features to use for prediction
features <- c('distance_km', 'passenger_count', 'pickup_hour', 'pickup_day', 'pickup_month', 'pickup_dayofweek')

# Define X (independent variables) and y (dependent variable)
X <- df[features]
y <- df$trip_duration

# Check for missing values in the features and target variable
if (any(is.na(X)) | any(is.na(y))) stop("Missing values in features or target variable")

# Split the data into training and testing sets (80% training, 20% testing)
set.seed(42)
train_indices <- sample(seq_len(nrow(df)), size = 0.8 * nrow(df))
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

# Create and train the Linear Regression model
model <- lm(y_train ~ ., data = X_train)

# Make predictions on the test set
y_pred <- predict(model, newdata = X_test)

# Evaluate the model's performance
mse <- mean((y_test - y_pred)^2)
r2 <- summary(model)$r.squared

cat("Mean Squared Error:", mse, "\n")
cat("R-squared Score:", r2, "\n")

# Scatter plot of actual vs predicted trip durations with a line of equality
plot1 <- ggplot(data.frame(Actual = y_test, Predicted = y_pred), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = "dashed") +
  labs(x = "Actual Trip Duration", y = "Predicted Trip Duration", title = "Actual vs Predicted Trip Duration") +
  theme_minimal()

# Scatter plot of trip_duration vs distance_km with the regression line
plot2 <- ggplot(df, aes(x = distance_km, y = trip_duration)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = 'red') +
  labs(x = "Distance (km)", y = "Trip Duration (seconds)", title = "Trip Duration vs Distance") +
  theme_minimal()

# Display plots
print(plot1)
print(plot2)
