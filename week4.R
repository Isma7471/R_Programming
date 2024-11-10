# Load necessary libraries
library(ggplot2)

# Load the dataset
student_data <- read.csv("C:/Users/smrc/Desktop/StudentData.csv")

# View the structure of the dataset
str(student_data)

# Replace empty strings with NA
student_data <- read.csv("C:/Users/smrc/Desktop/StudentData.csv", na.strings = c("", "NA"))

# Check for missing values
colSums(is.na(student_data))
sum(is.na(student_data))
# Impute missing values with median for numerical columns
for (col in colnames(student_data)) {
  if (is.numeric(student_data[[col]])) {
    student_data[[col]][is.na(student_data[[col]])] <- mean(student_data[[col]], na.rm = TRUE)
  }
}

# Correcting inconsistencies in 'Status_of_Admission'
student_data$Status_of_Admission <- ifelse(student_data$Status_of_Admission == "Rejectecd", "Rejected", student_data$Status_of_Admission)

# Convert ordinal categorical variables to factors with ordered levels
student_data$University_Rating <- factor(student_data$University_Rating, levels = c("Very Poor", "Poor", "Good", "Very Good", "Excellent"), ordered = TRUE)
student_data$Stetement_of_Purpose <- factor(student_data$Stetement_of_Purpose, levels = c("Poor", "Good", "Very Good", "Excellent"), ordered = TRUE)
student_data$Letter_of_Recommendation <- factor(student_data$Letter_of_Recommendation, levels = c("Poor", "Good", "Very Good", "Excellent"), ordered = TRUE)

#Descriptive statistics
# Group by admission status
admission_groups <- split(student_data, student_data$Status_of_Admission)

# Calculate descriptive statistics for each group
descriptive_stats <- lapply(admission_groups, function(group) {
  summary_data <- summary(group[, c("TOEFL_Score", "CGPA")])
  sd_data <- apply(group[, c("TOEFL_Score", "CGPA")], 2, sd)
  return(list(Summary = summary_data, Standard_Deviation = sd_data))
})

# Print the descriptive statistics
print(descriptive_stats)



# Data Visualization
# Boxplot for TOEFL Score by Admission Status
ggplot(student_data, aes(x = Status_of_Admission, y = TOEFL_Score, fill = Status_of_Admission)) +
  geom_boxplot() +
  labs(title = "TOEFL Score Distribution by Admission Status", x = "Admission Status", y = "TOEFL Score") +
  theme_minimal()

# Histogram for CGPA by Admission Status
ggplot(student_data, aes(x = CGPA, fill = Status_of_Admission)) +
  geom_histogram(binwidth = 0.1, position = "dodge") +
  labs(title = "CGPA Distribution by Admission Status", x = "CGPA", y = "Count") +
  theme_minimal()

# Bar plot for University Rating by Admission Status
ggplot(student_data, aes(x = University_Rating, fill = Status_of_Admission)) +
  geom_bar(position = "dodge") +
  labs(title = "University Rating by Admission Status", x = "University Rating", y = "Count") +
  theme_minimal()



# Measure of centeral tendency

# Calculate mean, median, and mode for CGPA based on Status_of_Admission
mean_CGPA <- aggregate(CGPA ~ Status_of_Admission, data, mean)
median_CGPA <- aggregate(CGPA ~ Status_of_Admission, data, median)
mode_CGPA <- function(v) { as.numeric(names(sort(table(v), decreasing = TRUE)[1])) }
mode_CGPA_result <- aggregate(CGPA ~ Status_of_Admission, data, mode_CGPA)

# Combine results
central_tendency <- merge(mean_CGPA, median_CGPA, by = "Status_of_Admission", suffixes = c("_Mean", "_Median"))
central_tendency <- merge(central_tendency, mode_CGPA_result, by = "Status_of_Admission")



# Pie chart for Research by Admission Status
ggplot(student_data, aes(x = "", fill = Research)) +
  geom_bar(position = "fill") +
  coord_polar("y", start = 0) +
  facet_wrap(~ Status_of_Admission) +
  labs(title = "Research Experience by Admission Status", x = NULL, y = NULL, fill = "Research Experience") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Stacked bar plot for Statement of Purpose by Admission Status
ggplot(student_data, aes(x = Stetement_of_Purpose, fill = Status_of_Admission)) +
  geom_bar(position = "stack") +
  labs(title = "Statement of Purpose Quality by Admission Status", x = "Statement of Purpose Quality", y = "Count") +
  theme_minimal()

# Stacked bar plot for Letter of Recommendation by Admission Status
ggplot(student_data, aes(x = Letter_of_Recommendation, fill = Status_of_Admission)) +
  geom_bar(position = "stack") +
  labs(title = "Letter of Recommendation Quality by Admission Status", x = "Letter of Recommendation Quality", y = "Count") +
  theme_minimal()



# Calculate variance and standard deviation
variance_CGPA <- aggregate(CGPA ~ Status_of_Admission, student_data, var)
sd_CGPA <- aggregate(CGPA ~ Status_of_Admission, student_data, sd)

# Combine results
dispersion <- merge(variance_CGPA, sd_CGPA, by = "Status_of_Admission", suffixes = c("_Variance", "_SD"))
dispersion


# Calculate mean, median, and mode for CGPA based on Status_of_Admission
mean_CGPA <- aggregate(CGPA ~ Status_of_Admission, student_data, mean)
median_CGPA <- aggregate(CGPA ~ Status_of_Admission, student_data, median)
mode_CGPA <- function(v) { as.numeric(names(sort(table(v), decreasing = TRUE)[1])) }
mode_CGPA_result <- aggregate(CGPA ~ Status_of_Admission, student_data, mode_CGPA)

# Combine results
central_tendency <- merge(mean_CGPA, median_CGPA, by = "Status_of_Admission", suffixes = c("_Mean", "_Median"))
central_tendency <- merge(central_tendency, mode_CGPA_result, by = "Status_of_Admission")

central_tendency


# Check for duplicate rows
sum(duplicated(student_data))

# Remove duplicate rows
student_data <- unique(student_data)



# Load necessary libraries
library(ggplot2)

# ... (previous code for loading and preprocessing data)

# Create a dataset with no missing values for visualizations
student_data_viz <- na.omit(student_data)

# Boxplot for TOEFL Score by Admission Status
ggplot(student_data_viz, aes(x = Status_of_Admission, y = TOEFL_Score, fill = Status_of_Admission)) +
  geom_boxplot() +
  labs(title = "TOEFL Score Distribution by Admission Status", x = "Admission Status", y = "TOEFL Score") +
  theme_minimal()

# Histogram for CGPA by Admission Status
ggplot(student_data_viz, aes(x = CGPA, fill = Status_of_Admission)) +
  geom_histogram(binwidth = 0.1, position = "dodge") +
  labs(title = "CGPA Distribution by Admission Status", x = "CGPA", y = "Count") +
  theme_minimal()

# Bar plot for University Rating by Admission Status
ggplot(student_data_viz, aes(x = University_Rating, fill = Status_of_Admission)) +
  geom_bar(position = "dodge") +
  labs(title = "University Rating by Admission Status", x = "University Rating", y = "Count") +
  theme_classic()

# Pie chart for Research by Admission Status
ggplot(student_data_viz, aes(x = "", fill = Research)) +
  geom_bar(position = "fill") +
  coord_polar("y", start = 0) +
  facet_wrap(~ Status_of_Admission) +
  labs(title = "Research Experience by Admission Status", x = NULL, y = NULL, fill = "Research Experience") +
  theme_void() +
  theme(axis.text.x = element_blank())

# Stacked bar plot for Statement of Purpose by Admission Status
ggplot(student_data_viz, aes(x = Stetement_of_Purpose, fill = Status_of_Admission)) +
  geom_bar(position = "stack") +
  labs(title = "Statement of Purpose Quality by Admission Status", x = "Statement of Purpose Quality", y = "Count") +
  theme_minimal()

# Stacked bar plot for Letter of Recommendation by Admission Status
ggplot(student_data_viz, aes(x = Letter_of_Recommendation, fill = Status_of_Admission)) +
  geom_bar(position = "stack") +
  labs(title = "Letter of Recommendation Quality by Admission Status", x = "Letter of Recommendation Quality", y = "Count") +
  theme_minimal()


# Define custom colors
custom_colors <- c("Yes" = "lightblue", "No" = "salmon")

# Create pie chart for Research by Admission Status
ggplot(student_data_viz, aes(x = "", fill = Research)) +
  geom_bar(position = "fill") +
  coord_polar("y", start = 0) +
  facet_wrap(~ Status_of_Admission) +
  labs(title = "Research Experience by Admission Status", x = NULL, y = NULL, fill = "Research Experience") +
  theme_void() +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = custom_colors)  # Apply custom colors


library(explorer)
create_report(student_data)
