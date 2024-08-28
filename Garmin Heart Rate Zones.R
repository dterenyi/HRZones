library(FITfileR)
library(ggplot2)


fit_file_path <- '/Users/dterenyi/Downloads/13010549348_ACTIVITY.fit'
fit_data <- readFitFile(fit_file_path)

# Extract records which contain the heart rate data
activity_records <- records(fit_data)

# Assuming activity_records is a list of tibbles and each has a column `heart_rate`
all_heart_rates <- unlist(lapply(activity_records, function(x) x$heart_rate))

# Clustering to find heart rate zones
set.seed(123)
kmeans_result <- kmeans(all_heart_rates, centers = 5)

# Extract the centers and calculate the zone boundaries
sorted_centers <- sort(kmeans_result$centers)
zone_boundaries <- c(min(all_heart_rates), 
                     (head(sorted_centers, -1) + tail(sorted_centers, -1)) / 2, 
                     max(all_heart_rates))

# Visualize the heart rate distribution
ggplot(data.frame(HeartRate = all_heart_rates), aes(x = HeartRate)) +
  geom_histogram(bins = 50, fill = 'blue', alpha = 0.7) +
  labs(title = "Heart Rate Distribution", x = "Heart Rate (bpm)", y = "Count") +
  theme_minimal()

# Visualize the density with marked zone boundaries
ggplot(data.frame(HeartRate = all_heart_rates), aes(x = HeartRate)) +
  geom_density(fill = 'green', alpha = 0.7) +
  geom_vline(xintercept = zone_boundaries, color = 'red', linetype = "dashed") +
  labs(title = "Heart Rate Density with Zone Boundaries", x = "Heart Rate (bpm)", y = "Density") +
  theme_minimal()

# Plot clusters to visualize the zones
ggplot(data.frame(HeartRate = all_heart_rates, Cluster = kmeans_result$cluster), 
       aes(x = HeartRate, fill = as.factor(Cluster))) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Heart Rate Clusters", x = "Heart Rate (bpm)", y = "Count", fill = "Cluster") +
  theme_minimal()

summary(all_heart_rates)
