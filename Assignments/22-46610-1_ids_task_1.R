install.packages("ggplot2")
install.packages("dplyr")


library(ggplot2)
library(dplyr)


data <- read.csv("C:/Users/Asus/Documents/student_depression_dataset 1.csv")
data <- na.omit(data)


data$Depression <- as.factor(data$Depression)


ggplot(data, aes(x = Gender)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")

ggplot(data, aes(x = Gender, y = as.numeric(Depression))) +
  geom_violin(fill = "lightblue") +
  labs(title = "Depression by Gender", x = "Gender", y = "Depression")

ggplot(data, aes(x = City)) +
  geom_bar(fill = "darkorchid") +
  labs(title = "City Distribution", x = "City", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = City, y = as.numeric(Depression))) +
  geom_violin(fill = "plum") +
  labs(title = "Depression by City", x = "City", y = "Depression")

ggplot(data, aes(x = Profession)) +
  geom_bar(fill = "mediumseagreen") +
  labs(title = "Profession Distribution", x = "Profession", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = Profession, y = as.numeric(Depression))) +
  geom_violin(fill = "lightgreen") +
  labs(title = "Depression by Profession", x = "Profession", y = "Depression")

ggplot(data, aes(x = Sleep.Duration)) +
  geom_bar(fill = "orange") +
  labs(title = "Sleep Duration Distribution", x = "Sleep Duration", y = "Count")

ggplot(data, aes(x = Sleep.Duration, y = as.numeric(Depression))) +
  geom_violin(fill = "gold") +
  labs(title = "Depression by Sleep Duration", x = "Sleep Duration", y = "Depression")

ggplot(data, aes(x = Dietary.Habits)) +
  geom_bar(fill = "tomato") +
  labs(title = "Dietary Habits Distribution", x = "Diet", y = "Count")

ggplot(data, aes(x = Dietary.Habits, y = as.numeric(Depression))) +
  geom_violin(fill = "lightpink") +
  labs(title = "Depression by Diet", x = "Diet", y = "Depression")

ggplot(data, aes(x = Degree)) +
  geom_bar(fill = "cyan4") +
  labs(title = "Degree Distribution", x = "Degree", y = "Count")

ggplot(data, aes(x = Degree, y = as.numeric(Depression))) +
  geom_violin(fill = "cyan2") +
  labs(title = "Depression by Degree", x = "Degree", y = "Depression")

ggplot(data, aes(x = Financial.Stress)) +
  geom_bar(fill = "goldenrod1") +
  labs(title = "Financial Stress", x = "Stress", y = "Count")

ggplot(data, aes(x = Financial.Stress, y = as.numeric(Depression))) +
  geom_violin(fill = "khaki") +
  labs(title = "Depression by Financial Stress", x = "Stress", y = "Depression")

ggplot(data, aes(x = Family.History.of.Mental.Illness)) +
  geom_bar(fill = "mediumpurple") +
  labs(title = "Family Mental Illness History", x = "Family History", y = "Count")

ggplot(data, aes(x = Family.History.of.Mental.Illness, y = as.numeric(Depression))) +
  geom_violin(fill = "orchid1") +
  labs(title = "Depression by Family History", x = "Family History", y = "Depression")







ggplot(data, aes(x = Depression, y = Age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box Plot: Age by Depression", x = "Depression", y = "Age")

ggplot(data, aes(x = Depression, y = Academic.Pressure)) +
  geom_boxplot(fill = "peachpuff") +
  labs(title = "Box Plot: Academic Pressure by Depression", x = "Depression", y = "Academic Pressure")

ggplot(data, aes(x = Depression, y = Work.Pressure)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Box Plot: Work Pressure by Depression", x = "Depression", y = "Work Pressure")

ggplot(data, aes(x = Depression, y = CGPA)) +
  geom_boxplot(fill = "orchid") +
  labs(title = "Box Plot: CGPA by Depression", x = "Depression", y = "CGPA")

ggplot(data, aes(x = Depression, y = Study.Satisfaction)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Box Plot: Study Satisfaction by Depression", x = "Depression", y = "Study Satisfaction")

ggplot(data, aes(x = Depression, y = Job.Satisfaction)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Box Plot: Job Satisfaction by Depression", x = "Depression", y = "Job Satisfaction")

ggplot(data, aes(x = Depression, y = Work.Study.Hours)) +
  geom_boxplot(fill = "lightyellow") +
  labs(title = "Box Plot: Work/Study Hours by Depression", x = "Depression", y = "Work/Study Hours")







ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Age", x = "Age", y = "Count")

ggplot(data, aes(x = Depression, y = Age)) +
  geom_violin(fill = "lightblue") +
  labs(title = "Age by Depression", x = "Depression", y = "Age")

ggplot(data, aes(x = Academic.Pressure)) +
  geom_histogram(binwidth = 1, fill = "darkorange", color = "black") +
  labs(title = "Histogram of Academic Pressure")

ggplot(data, aes(x = Depression, y = Academic.Pressure)) +
  geom_violin(fill = "peachpuff") +
  labs(title = "Academic Pressure by Depression")

ggplot(data, aes(x = Work.Pressure)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Histogram of Work Pressure")

ggplot(data, aes(x = Depression, y = Work.Pressure)) +
  geom_violin(fill = "lightgreen") +
  labs(title = "Work Pressure by Depression")

ggplot(data, aes(x = CGPA)) +
  geom_histogram(binwidth = 0.2, fill = "purple", color = "black") +
  labs(title = "Histogram of CGPA")

ggplot(data, aes(x = Depression, y = CGPA)) +
  geom_violin(fill = "orchid") +
  labs(title = "CGPA by Depression")

ggplot(data, aes(x = Study.Satisfaction)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue", color = "black") +
  labs(title = "Histogram of Study Satisfaction")

ggplot(data, aes(x = Depression, y = Study.Satisfaction)) +
  geom_violin(fill = "skyblue") +
  labs(title = "Study Satisfaction by Depression")

ggplot(data, aes(x = Job.Satisfaction)) +
  geom_histogram(binwidth = 1, fill = "coral", color = "black") +
  labs(title = "Histogram of Job Satisfaction")

ggplot(data, aes(x = Depression, y = Job.Satisfaction)) +
  geom_violin(fill = "salmon") +
  labs(title = "Job Satisfaction by Depression")

ggplot(data, aes(x = Work.Study.Hours)) +
  geom_histogram(binwidth = 1, fill = "gold", color = "black") +
  labs(title = "Histogram of Work/Study Hours")

ggplot(data, aes(x = Depression, y = Work.Study.Hours)) +
  geom_violin(fill = "lightyellow") +
  labs(title = "Work/Study Hours by Depression")







ggplot(data, aes(x = CGPA, y = Academic.Pressure, color = Depression)) +
  geom_point(alpha = 0.7) +
  labs(title = "CGPA vs Academic Pressure by Depression")

ggplot(data, aes(x = Work.Pressure, y = Job.Satisfaction, color = Depression)) +
  geom_point(alpha = 0.7) +
  labs(title = "Work Pressure vs Job Satisfaction by Depression")

ggplot(data, aes(x = Study.Satisfaction, y = CGPA, color = Depression)) +
  geom_point(alpha = 0.7) +
  labs(title = "Study Satisfaction vs CGPA by Depression")
