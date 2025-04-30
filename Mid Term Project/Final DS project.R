install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr)
dataSet_1 <- read.csv("E:/Downloads/Placement_Data_Full_Class - modified.csv")
dataSet_1 
head(dataSet_1, 20)
dataSet_1[1:nrow(dataSet_1), ]

no_of_col <- ncol(dataSet_1)
no_of_row <- nrow(dataSet_1)
cat("now of row in the dataset: ", no_of_row) 
cat("now of column in the dataset: ", no_of_col) 
str(dataSet_1)


remo_dupli_dataset <- dplyr::distinct(dataSet_1)
remo_dupli_dataset

fresh_dataset <- remo_dupli_dataset 
cat ("No of row and column after removing duplicate instances: ", 
     nrow(remo_dupli_dataset), ncol(remo_dupli_dataset))


unique(dataSet_1$gender)
unique(dataSet_1$ssc_p)
unique(dataSet_1$ssc_b)
unique(dataSet_1$hsc_p)
unique(dataSet_1$hsc_b)
unique(dataSet_1$hsc_s)
unique(dataSet_1$degree_p)
unique(dataSet_1$degree_t)
unique(dataSet_1$workex)
unique(dataSet_1$etest)
unique(dataSet_1$specialisation)
unique(dataSet_1$mba_p)
unique(dataSet_1$status)
unique(dataSet_1$salary)


deal_invalid_dataset <- fresh_dataset


deal_invalid_dataset$gender <- ifelse(
  toupper(substr(deal_invalid_dataset$gender, 1, 1)) == "M", "M",
  ifelse(
    toupper(substr(deal_invalid_dataset$gender, 1, 1)) == "F", "F",
    "OTHER"
  )
)

unique(deal_invalid_dataset$gender)
print(deal_invalid_dataset)

fresh_dataset <- deal_invalid_dataset;


deal_miss_value_dataset <- fresh_dataset;
colSums(is.na(deal_miss_value_dataset));
which(is.na(deal_miss_value_dataset$ salary))

deal_miss_value_dataset <- na.omit(deal_miss_value_dataset)
deal_miss_value_dataset
colSums(is.na(deal_miss_value_dataset))


install.packages('tidyr')
library(tidyr)

bottom_up_dataset <- fresh_dataset %>% fill(salary,workex,degree_t, .direction = 'up')
colSums(is.na(bottom_up_dataset))
top_down_dataset <- fresh_dataset %>% fill(salary,workex,degree_t, .direction = 'down')
colSums(is.na(top_down_dataset))


deal_miss_value_mode <- fresh_dataset

mode_gender <- names(sort(table(deal_miss_value_mode$gender), decreasing = TRUE))[1]
deal_miss_value_mode$gender[is.na(deal_miss_value_mode$gender)] <- mode_gender
mode_ssc_b <- names(sort(table(deal_miss_value_mode$ssc_b), decreasing = TRUE))[1]
deal_miss_value_mode$ssc_b[is.na(deal_miss_value_mode$ssc_b)] <- mode_ssc_b
mode_hsc_b <- names(sort(table(deal_miss_value_mode$hsc_b), decreasing = TRUE))[1]
deal_miss_value_mode$hsc_b[is.na(deal_miss_value_mode$hsc_b)] <- mode_hsc_b
mode_hsc_s <- names(sort(table(deal_miss_value_mode$hsc_s), decreasing = TRUE))[1]
deal_miss_value_mode$hsc_s[is.na(deal_miss_value_mode$hsc_s)] <- mode_hsc_s
mode_degree_t <- names(sort(table(deal_miss_value_mode$degree_t), decreasing = TRUE))[1]
deal_miss_value_mode$degree_t[is.na(deal_miss_value_mode$degree_t)] <- mode_degree_t
mode_specialisation <- names(sort(table(deal_miss_value_mode$specialisation), decreasing = TRUE))[1]
deal_miss_value_mode$specialisation[is.na(deal_miss_value_mode$specialisation)] <- mode_specialisation
mode_status <- names(sort(table(deal_miss_value_mode$status), decreasing = TRUE))[1]
deal_miss_value_mode$status[is.na(deal_miss_value_mode$status)] <- mode_status

deal_miss_value_mode

na_counts <- colSums(is.na(deal_miss_value_mode))
print(na_counts)


deal_miss_value_mean <- fresh_dataset

for(col_name in c("salary", "workex")) {
  if(is.numeric(deal_miss_value_mean[[col_name]])) {
    column_mean <- mean(deal_miss_value_mean[[col_name]], na.rm = TRUE)
    
    deal_miss_value_mean[[col_name]][is.na(deal_miss_value_mean[[col_name]])] <- column_mean
    deal_miss_value_mean[[col_name]] <- round(deal_miss_value_mean[[col_name]], digits = 0)
  }
}

na_counts <- colSums(is.na(deal_miss_value_mean))
print(na_counts)


dataSet_num <- fresh_dataset 
dataSet_num$gender <- factor(dataSet_num$gender, levels = c("M", "F"), labels = c(1,2));
dataSet_num$ssc_b <-  factor(dataSet_num$ssc_b, levels = c("Others", "Central"), labels = c(1,2));
dataSet_num$hsc_s <-  factor(dataSet_num$hsc_s, levels = c("Science", "Commerce","Arts"), labels = c(1,2,3));
dataSet_num$hsc_b <-  factor(dataSet_num$hsc_b, levels = c("Others", "Central"), labels = c(1,2));
dataSet_num$degree_t <-  factor(dataSet_num$degree_t, levels = c("Sci&Tech","Comm&Mgmt","Others"), labels = c(1,2,3));
dataSet_num$specialisation <-  factor(dataSet_num$specialisation, levels = c("Mkt&HR", "Mkt&Fin"), labels = c(1,2));
dataSet_num$status <-  factor(dataSet_num$status, levels = c("Placed", "Not Placed"), labels = c(1,2));
unique(dataSet_num$status)
unique(dataSet_num$specialisation)
unique(dataSet_num$degree_t)
unique(dataSet_num$hsc_s )
unique(dataSet_num$ssc_b)
unique(dataSet_num$gender)
dataSet_num
colSums(is.na(fresh_dataset))


fresh_dataset <- dataSet_num



detect_outlier <- function(dataframe, columns) {
  for (col in columns) {
    if (is.numeric(dataframe[[col]])) {
      
      Quantile1 <- quantile(dataframe[[col]], probs = 0.25)
      Quantile3 <- quantile(dataframe[[col]], probs = 0.75)
      IQR <- Quantile3 - Quantile1
      outlier_flags <- dataframe[[col]] > Quantile3 + (IQR * 1.5) | dataframe[[col]] < Quantile1 - (IQR * 1.5)
      outliers <- dataframe[[col]][outlier_flags]
      if (length(outliers) > 0) {
        cat("Outliers detected in column", col, ":\n")
        print(outliers)
      } else {
        cat("No outliers detected in column", col, "\n")
      }
    } else {
      cat("Column", col, "is not numeric, skipping...\n")
    }
  }
}

detect_outlier <- function(dataframe, columns) {
  for (col in columns) {
    if (is.numeric(dataframe[[col]])) {
      Quantile1 <- quantile(dataframe[[col]], probs = 0.25, na.rm = TRUE)
      Quantile3 <- quantile(dataframe[[col]], probs = 0.75, na.rm = TRUE)
      IQR <- Quantile3 - Quantile1
      
      
      outlier_flags <- dataframe[[col]] > Quantile3 + (IQR * 1.5) |
        dataframe[[col]] < Quantile1 - (IQR * 1.5)
      outliers <- dataframe[[col]][outlier_flags]
      if (length(outliers) > 0) {
        cat("Outliers detected in column", col, ":\n")
        print(outliers)
      } else {
        cat("No outliers detected in column", col, "\n")
      }
    } else {
      cat("Column", col, "is not numeric, skipping...\n")
    }
  }
}

remove_outlier <- function(dataframe, columns) {
  for (col in columns) {
    if (is.numeric(dataframe[[col]])) {
      Quantile1 <- quantile(dataframe[[col]], probs = 0.25, na.rm = TRUE)
      Quantile3 <- quantile(dataframe[[col]], probs = 0.75, na.rm = TRUE)
      IQR <- Quantile3 - Quantile1
      
      dataframe <- dataframe[!(
        dataframe[[col]] > Quantile3 + (IQR * 1.5) |
          dataframe[[col]] < Quantile1 - (IQR * 1.5)
      ), ]
    }
  }
  return(dataframe)
}


detect_outlier(fresh_dataset, names(fresh_dataset))
without_outlier_data <- remove_outlier(fresh_dataset, names(fresh_dataset))
without_outlier_data

detect_outlier(without_outlier_data, names(without_outlier_data))


normalize_dataset <- fresh_dataset;

min_ssc <- min(normalize_dataset$ssc_p, na.rm = TRUE)
max_ssc <- max(normalize_dataset$ssc_p, na.rm = TRUE)
normalize_dataset$ssc_p <- (normalize_dataset$ssc_p - min_ssc) / (max_ssc - min_ssc);

min_hsc <- min(normalize_dataset$hsc_p, na.rm = TRUE)
max_hsc <- max(normalize_dataset$hsc_p, na.rm = TRUE)
normalize_dataset$hsc_p <- (normalize_dataset$hsc_p - min_hsc) / (max_hsc - min_hsc);


min_degree <- min(normalize_dataset$degree_p, na.rm = TRUE)
max_degree <- max(normalize_dataset$degree_p, na.rm = TRUE)
normalize_dataset$degree_p <- (normalize_dataset$degree_p - min_degree) / (max_degree - min_degree);

min_etest <- min(normalize_dataset$etest_p, na.rm = TRUE)
max_etest <- max(normalize_dataset$etest_p, na.rm = TRUE)
normalize_dataset$etest_p <- (normalize_dataset$etest_p - min_etest) / (max_etest - min_etest);

min_mba <- min(normalize_dataset$mba_p , na.rm = TRUE)
max_mba <- max(normalize_dataset$mba_p , na.rm = TRUE)
normalize_dataset$mba_p  <- (normalize_dataset$mba_p  - min_mba) / (max_mba - min_mba);

min_sal <- min(normalize_dataset$salary , na.rm = TRUE)
max_sal <- max(normalize_dataset$salary , na.rm = TRUE)
normalize_dataset$salary  <- (normalize_dataset$salary  - min_sal) / (max_sal - min_sal);

normalize_dataset
summary(fresh_dataset);
columns_to_analyze <- c( 
  "gender",
  "ssc_p", "ssc_b",
  "hsc_p","hsc_b" , "degree_p",  
  "etest_p", "mba_p" 
)
calculate_stats <- function(dataset, columns) {
  for (column_name in columns) {
    if (is.numeric(dataset[[column_name]])) {
      
      column_data <- dataset[[column_name]]
      
      column_mean <- mean(column_data, na.rm = TRUE)
      column_median <- median(column_data, na.rm = TRUE)
      column_mode <- as.numeric(names(sort(table(column_data), decreasing = TRUE))[1])
      cat("Mean of column", column_name, "is", column_mean, "\n")
      cat("Median of column", column_name, "is", column_median, "\n")
      cat("Mode of column", column_name, "is", column_mode, "\n")
      cat("\n")  
    }
  }
}


calculate_stats(fresh_dataset,columns_to_analyze)

calculate_spread <- function(dataset, columns) {
  for (col_name in columns) {
    if (is.numeric(dataset[[col_name]])) {
      column_data <- dataset[[col_name]]
      
      column_range <- range(column_data, na.rm = TRUE)  
      column_iqr <- IQR(column_data, na.rm = TRUE)     
      column_sd <- sd(column_data, na.rm = TRUE)        
      column_variance <- var(column_data, na.rm = TRUE) 
      cat("For column", col_name, ":\n")
      cat("  Range: [", column_range[1], ",", column_range[2], "]\n")
      cat("  IQR:", column_iqr, "\n")
      cat("  Standard Deviation:", column_sd, "\n")
      cat("  Variance:", column_variance, "\n")
      cat("\n")  
    } 
  }
}

calculate_spread(fresh_dataset, columns_to_analyze)


class_distribution <- table(fresh_dataset$status)
print(class_distribution)

if (class_distribution[1] > class_distribution[2]) {
  majority <- filter(fresh_dataset, status == 1) 
  minority <- filter(fresh_dataset, status == 2) 
} else {
  majority <- filter(fresh_dataset, status == 2) 
  minority <- filter(fresh_dataset, status == 1) 
}
set.seed(123)


oversampled_minority <- minority %>% sample_n(nrow(majority), replace = TRUE)
oversampled_data <- bind_rows(majority, oversampled_minority)
table(oversampled_data$status)
oversampled_data


undersampled_majority <- majority %>% sample_n(nrow(minority), replace = FALSE)
undersampled_data <- bind_rows(undersampled_majority, minority)
table(undersampled_data$status)
undersampled_data


fresh_dataset <- oversampled_data 
