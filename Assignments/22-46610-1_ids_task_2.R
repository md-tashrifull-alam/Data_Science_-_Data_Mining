install.packages("ggplot2")
install.packages("dplyr")
install.packages("caret")
install.packages("FSelector")
install.packages("infotheo")


library(ggplot2)
library(dplyr)
library(caret)
library(FSelector)
library(infotheo)


data <- read.csv("C:/Users/Asus/Documents/student_depression_dataset 1.csv")
data <- na.omit(data)


data$Depression <- as.factor(data$Depression)



numeric_cols <- sapply(data, is.numeric)
numeric_data <- data[, numeric_cols]

for (i in 1:(ncol(numeric_data) - 1)) {
  for (j in (i + 1):ncol(numeric_data)) {
    cat("\n---------------------------\n")
    cat("Pearson Correlation:", colnames(numeric_data)[i], "vs", colnames(numeric_data)[j], "\n")
    result <- cor.test(numeric_data[[i]], numeric_data[[j]], method = "pearson")
    print(result)
  }
}






cat_cols <- sapply(data, is.character)
num_cols <- sapply(data, is.numeric)

categorical_vars <- names(data)[cat_cols]
numeric_vars <- names(data)[num_cols]

data[categorical_vars] <- lapply(data[categorical_vars], as.factor)

for (num_col in numeric_vars) {
  for (cat_col in categorical_vars) {
    if (nlevels(data[[cat_col]]) > 1) {
      cat("\n---------------------------\n")
      cat("ANOVA:", num_col, "~", cat_col, "\n")
      formula <- as.formula(paste(num_col, "~", cat_col))
      result <- aov(formula, data = data)
      print(summary(result))
    }
  }
}





valid_cat_cols <- categorical_vars[sapply(data[categorical_vars], function(x) nlevels(x) > 1)]

chi_table <- data.frame(
  Feature_Pair = character(),
  Df = integer(),
  Chi_Sq = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:(length(valid_cat_cols) - 1)) {
  for (j in (i + 1):length(valid_cat_cols)) {
    
    var1 <- valid_cat_cols[i]
    var2 <- valid_cat_cols[j]
    
    tbl <- table(data[[var1]], data[[var2]])
    chi_result <- chisq.test(tbl, simulate.p.value = TRUE, B = 10000)
    
    chi_table <- rbind(chi_table, data.frame(
      Feature_Pair = paste(var1, "vs", var2),
      Df = chi_result$parameter,
      Chi_Sq = round(chi_result$statistic, 2),
      P_Value = round(chi_result$p.value, 5),
      stringsAsFactors = FALSE
    ))
  }
}

print("Chi-squared Test Summary:")
print(chi_table)








library(infotheo)
library(dplyr)

num_cols <- sapply(data, is.numeric)
cat_cols <- sapply(data, function(x) is.character(x) || is.factor(x))

data_discrete <- data

data_discrete[num_cols] <- lapply(data[num_cols], function(col) {
  discretize(col, disc = "equalfreq", nbins = 3)[, 1]
})

data_discrete <- data_discrete %>% mutate(across(everything(), as.factor))

mi_scores <- sapply(names(data_discrete)[names(data_discrete) != "Depression"], function(col) {
  mutinformation(data_discrete[[col]], data_discrete$Depression)
})

mi_df <- data.frame(Feature = names(mi_scores), MI_with_Depression = round(mi_scores, 4))
mi_df <- mi_df[order(-mi_df$MI_with_Depression), ]

print("Mutual Information Scores with Depression:")
print(mi_df)


