install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}
packages <- c("readr", "caret", "rsample", "RWeka", "rpart", "rpart.plot", "MASS", 
              "kernlab", "pROC", "dplyr", "C50", "e1071", "ROSE", "FSelector", 
              "Boruta", "magrittr", "openxlsx", "tidyverse", "data.table", "ada",
              "recipes","FactoMineR","factoextra", "corrplot","DT", "reshape2", 
              "ggplot2", "patchwork", "pROC", "party", "plotrix", "plotmo", "earth")


# Install and load required packages
sapply(packages, install_and_load)

#load project data
setwd('C:\\Users\\dartu\\OneDrive\\Documentos\\Boston University\\Summer 1\\Data Mining\\Project')
df <- read_csv("project_data(1).csv")
df <- as.data.frame(df)

## Data Dimension Reduction
df <- df %>%
  select(-FMONTH, -IDATE, -IMONTH, -IDAY, -IYEAR, -SEQNO, -DISPCODE, 
         -CPDEMO1B, -MARITAL, -EDUCA, -RENTHOM1, -EMPLOY1, -INCOME2, 
         -QSTVER, -QSTLANG, -HTIN4, -WEIGHT2, -HEIGHT3)

### Columns with almost 0 variance
df <- df %>%
  select(-all_of(nearZeroVar(df, names = TRUE)))

### Columns with correlation
numeric_cols <- df %>% select(where(is.numeric))
highly.correlated.variables <- findCorrelation(cor(numeric_cols, use = "complete.obs"), cutoff = 0.7, names = TRUE)
df <- df %>% select(-all_of(highly.correlated.variables))

### Multiple Correspondence Analysis
df_factors <- df %>% mutate_if(is.numeric, as.factor)

# Perform MCA
mca_results <- MCA(df_factors, graph = FALSE)

# Get the contributions of variables to the first two dimensions
contrib_dim1 <- mca_results$var$contrib[,1]
contrib_dim2 <- mca_results$var$contrib[,2]

# Combine contributions and identify the top contributing variables
contrib_total <- contrib_dim1 + contrib_dim2
top_vars <- names(sort(contrib_total, decreasing = TRUE)[1:40])

strip_suffix <- function(var_names) {
  # Remove everything after and including the first underscore
  original_names <- gsub("(_|\\.).*$", "", var_names)
  return(original_names)
}

# Apply the function to top_vars
original_vars <- strip_suffix(top_vars)

# Get unique original variable names
unique_original_vars <- unique(original_vars)

# Filter the dataframe to keep only the top contributing variables
df_principal_vars <- df_factors %>% select(all_of(unique_original_vars))

# Filter the dataframe to keep only the top contributing variables
df <- df_factors %>%
  select(Class, all_of(unique_original_vars))

###### Data Normalization ######
scale <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
df[] <- lapply(names(df), function(column_name) {
  # Access the column by its name
  x <- df[[column_name]]
  
  # Check if the column is not 'Class', is a factor, and needs conversion
  if (column_name != "Class" && is.factor(x)) {
    # Convert factor to numeric by first converting to character
    as.numeric(as.character(x))
  } else {
    # Leave the column as is if it's 'Class', numeric or should not be converted
    x
  }
})

#MENTHLTH
df$MENTHLTH[!(df$MENTHLTH  %in% c(1:30))] <- NA
df$MENTHLTH <- scale(df$MENTHLTH)


#CHOLCHK2 
df$CHOLCHK2[!(df$CHOLCHK2 %in% c(1,2,3,4,5,6))] <- NA
df$CHOLCHK2<- scale(df$CHOLCHK2)

#CVDINFR4 
df$CVDINFR4[!(df$CVDINFR4 %in% c(1,2))] <- NA
df$CVDINFR4<- scale(df$CVDINFR4)

#CVDCRHD4
df$CVDCRHD4[!(df$CVDCRHD4 %in% c(1,2))] <- NA
df$CVDCRHD4<- scale(df$CVDCRHD4)

#ASTHMA3 
df$ASTHMA3[!(df$ASTHMA3 %in% c(1,2))] <- NA
df$ASTHMA3 <- scale(df$ASTHMA3)

#CHCCOPD2
df$CHCCOPD2[!(df$CHCCOPD2 %in% c(1,2))] <- NA
df$CHCCOPD2<- scale(df$CHCCOPD2)

#ADDEPEV3 
df$ADDEPEV3[!(df$ADDEPEV3 %in% c(1,2))] <- NA
df$ADDEPEV3 <- scale(df$ADDEPEV3)

#HAVARTH4 
df$HAVARTH4[!(df$HAVARTH4 %in% c(1,2))] <- NA
df$HAVARTH4<- scale(df$DIABETE4)

#VETERAN3
df$VETERAN3[!(df$VETERAN3 %in% c(1,2))] <- NA
df$VETERAN3<- scale(df$VETERAN3)

#DEAF
df$DEAF[!(df$DEAF %in% c(1,2))] <- NA
df$DEAF<- scale(df$DEAF)

#BLIND
df$BLIND[!(df$BLIND %in% c(1,2))] <- NA
df$BLIND<- scale(df$BLIND)

#DECIDE
df$DECIDE[!(df$DECIDE %in% c(1,2))] <- NA
df$DECIDE <- scale(df$DECIDE)

#DIFFWALK 
df$DIFFWALK[!(df$DIFFWALK %in% c(1,2))] <- NA
df$DIFFWALK <- scale(df$DIFFWALK)

#DIFFALON
df$DIFFALON[!(df$DIFFALON %in% c(1,2))] <- NA
df$DIFFALON<- scale(df$DIFFALON)

#SMOKE100
df$SMOKE100[!(df$SMOKE100 %in% c(1,2))] <- NA
df$SMOKE100<- scale(df$SMOKE100)

#EXERANY2
df$EXERANY2[!(df$EXERANY2 %in% c(1,2))] <- NA
df$EXERANY2<- scale(df$EXERANY2)

#FLUSHOT7
df$FLUSHOT7[!(df$FLUSHOT7 %in% c(1,2))] <- NA
df$FLUSHOT7 <- scale(df$FLUSHOT7)

#PNEUVAC4
df$PNEUVAC4[!(df$PNEUVAC4 %in% c(1,2))] <- NA
df$PNEUVAC4 <- scale(df$PNEUVAC4)

#HIVTST7
df$HIVTST7[!(df$HIVTST7 %in% c(1,2))] <- NA
df$HIVTST7 <- scale(df$HIVTST7)

#DRNKANY5
df$DRNKANY5[!(df$DRNKANY5 %in% c(1,2))] <- NA
df$DRNKANY5 <- scale(df$DRNKANY5)

#TETANUS1
df$TETANUS1[(df$TETANUS1 %in% c(7,9))] <- NA
df$TETANUS1 <- scale(df$TETANUS1)

#DIABETE4
df$DIABETE4 <- ifelse(!(df$DIABETE4 %in% c(1, 2, 3, 4)), NA, df$DIABETE4)
df$DIABETE4 <- scale(df$DIABETE4)

#STRENGTH
df$STRENGTH[df$STRENGTH %in% c(200, 777, 999)] <- NA
df$STRENGTH[df$STRENGTH == 888] <- 0
df$STRENGTH <- ifelse(df$STRENGTH %in% 101:199, df$STRENGTH %% 100, df$STRENGTH)
df$STRENGTH <- ifelse(df$STRENGTH %in% 201:299, df$STRENGTH %% 200, df$STRENGTH)
df$STRENGTH <- round(scale(df$STRENGTH),1)

#FRUIT2
df$FRUIT2[df$FRUIT2 %in% c(777, 999, 555, 300)] <- NA
df$FRUIT2[df$FRUIT2 %in% c(555, 300)] <- c(0, 0.5)[match(df$FRUIT2, c(555, 300))]
df$FRUIT2 <- ifelse(df$FRUIT2 %in% 101:199, df$FRUIT2 %% 100, df$FRUIT2)
df$FRUIT2 <- ifelse(df$FRUIT2 %in% 201:299, df$FRUIT2 %% 200, df$FRUIT2)
df$FRUIT2 <- ifelse(df$FRUIT2 %in% 301:399, df$FRUIT2 %% 300, df$FRUIT2)
df$FRUIT2 <- round(scale(df$FRUIT2),2)

#FRUITJU2
df$FRUITJU2[df$FRUITJU2 %in% c(777, 999)] <- NA
df$FRUITJU2[df$FRUITJU2 %in% c(555, 300)] <- 0
df$FRUITJU2 <- ifelse(df$FRUITJU2 %in% 101:199, df$FRUITJU2 %% 100, df$FRUITJU2)
df$FRUITJU2 <- ifelse(df$FRUITJU2 %in% 201:299, df$FRUITJU2 %% 200, df$FRUITJU2)
df$FRUITJU2 <- ifelse(df$FRUITJU2 %in% 301:399, df$FRUITJU2 %% 300, df$FRUITJU2)
df$FRUITJU2 <- round(scale(df$FRUITJU2),1)

#FVGREEN1
df$FVGREEN1[df$FVGREEN1 %in% c(777, 999)] <- NA
df$FVGREEN1[df$FVGREEN1 %in% c(555, 300)] <- 0
df$FVGREEN1 <- ifelse(df$FVGREEN1 %in% 101:199, df$FVGREEN1 %% 100 , df$FVGREEN1)
df$FVGREEN1 <- ifelse(df$FVGREEN1 %in% 201:299, df$FVGREEN1 %% 200 , df$FVGREEN1)
df$FVGREEN1 <- ifelse(df$FVGREEN1 %in% 301:399, df$FVGREEN1 %% 300, df$FVGREEN1)
df$FVGREEN1 <- round(scale(df$FVGREEN1),1)

#FRENCHF1
df$FRENCHF1[df$FRENCHF1 %in% c(777, 999, 200)] <- NA
df$FRENCHF1[df$FRENCHF1 %in% c(555, 300)] <- 0
df$FRENCHF1 <- ifelse(df$FRENCHF1 %in% 101:199, df$FRENCHF1 %% 100, df$FRENCHF1)
df$FRENCHF1 <- ifelse(df$FRENCHF1 %in% 201:299, df$FRENCHF1 %% 200, df$FRENCHF1)
df$FRENCHF1 <- ifelse(df$FRENCHF1 %in% 301:399, df$FRENCHF1 %% 300, df$FRENCHF1)
df$FRENCHF1 <- round(scale(df$FRENCHF1),1)

#POTATOE1
df$POTATOE1[df$POTATOE1 %in% c(777, 999)] <- NA
df$POTATOE1[df$POTATOE1 %in% c(555, 300)] <- 0
df$POTATOE1 <- ifelse(df$POTATOE1 %in% 101:199, df$POTATOE1 %% 100, df$POTATOE1)
df$POTATOE1 <- ifelse(df$POTATOE1 %in% 201:299, df$POTATOE1 %% 200, df$POTATOE1)
df$POTATOE1 <- ifelse(df$POTATOE1 %in% 301:399, df$POTATOE1 %% 300, df$POTATOE1)
df$POTATOE1 <- round(scale(df$POTATOE1),1)

#VEGETAB2
df$VEGETAB2[df$VEGETAB2 %in% c(777, 999)] <- NA
df$VEGETAB2[df$VEGETAB2 %in% c(555, 300)] <- 0
df$VEGETAB2 <- ifelse(df$VEGETAB2 %in% 101:199, df$VEGETAB2 %% 100, df$VEGETAB2)
df$VEGETAB2 <- ifelse(df$VEGETAB2 %in% 201:299, df$VEGETAB2 %% 200, df$VEGETAB2)
df$VEGETAB2 <- ifelse(df$VEGETAB2 %in% 301:399, df$VEGETAB2 %% 300, df$VEGETAB2)
df$VEGETAB2 <- round(scale(df$VEGETAB2),1)

#####The code builds a **decision tree** to predict Class using HTM4, extracts split points using **entrophy**. If no splits are found, uses the 33rd and 66th percentiles of  bins HTM4 into categories based on these splits
#HTM4  
tree_model <- rpart(Class ~ HTM4, data = df, method = "class", control=rpart.control(minsplit=1, cp=0.001))
splits <- sort(unique(tree_model$frame$split[!is.na(tree_model$frame$split)]))
if (length(splits) == 0) {
  splits <- quantile(df$HTM4, probs = c(0.33, 0.66), na.rm = TRUE)
}
labels <- as.character(1:(length(splits) + 1))
df$HTM4 <- cut(df$HTM4, breaks = c(-Inf, splits, Inf), include.lowest = TRUE, labels = labels)
df$HTM4 <- as.numeric(df$HTM4)
df$HTM4 <- scale(df$HTM4) 

##### Removing duplicates
as.data.frame(df)
sum(duplicated(df))
df <- distinct(df)

#### filling blanks
library(corrplot)
library(dplyr)
library(gridExtra)

# Sample data frame with numeric columns for demonstration purposes
df.graph <- data.frame(matrix(runif(1000), ncol=10))

# Select numeric columns
numeric_cols <- df.graph  %>% select(where(is.numeric))

# Calculate correlation matrix
correlation_matrix <- cor(numeric_cols, use = "complete.obs")

# Calculate variance for each column
variances <- apply(numeric_cols, 2, var)

# Select the top variables with the highest variance
top_vars <- names(sort(variances, decreasing = TRUE))

# Reduce the number of variables if it's too large for visualization
selected_vars <- top_vars[1:min(length(top_vars), 20)]  # Adjust the number 20 based on how many you want to show
selected_vars <- selected_vars[selected_vars %in% colnames(correlation_matrix)]

# Ensure there are enough variables to plot
if (length(selected_vars) > 1) {
  subset_correlation_matrix <- correlation_matrix[selected_vars, selected_vars]
  corrplot(subset_correlation_matrix, 
           type = "upper", 
           order = "hclust", 
           tl.col = "black", 
           tl.srt = 45, 
           tl.cex = 0.7,
           addrect = 2)  # addrect parameter can be adjusted to group highly correlated variables
} else {
  message("Not enough variables to plot a meaningful correlation matrix.")
}
# 1

fill_missing_with_regression <- function(df) { numeric_cols <- df %>% select(where(is.numeric))
correlation_matrix <- cor(numeric_cols, use = "complete.obs") # 1

# 2
correlation_df <- as.data.frame(correlation_matrix) %>%
  rownames_to_column(var = "Row") %>%
  pivot_longer(cols = -Row, names_to = "Column", values_to = "Correlation") %>%
  filter(Row != Column, !is.na(Correlation)) %>%
  arrange(desc(abs(Correlation)))

# 3
cols_with_na <- names(df)[colSums(is.na(df)) > 0]
print(paste("Columns with NA:", toString(cols_with_na)))

for (col in cols_with_na) {
  top_correlation <- correlation_df %>%
    filter(Row == col | Column == col) %>%
    slice_max(abs(Correlation), n = 1) 
  # 4
  if (nrow(top_correlation) > 0) {
    predictor_col <- ifelse(top_correlation$Row[1] == col, top_correlation$Column[1], top_correlation$Row[1])
    print(paste("Processing:", col, "using", predictor_col))
    missing_indices <- which(is.na(df[[col]]) & !is.na(df[[predictor_col]]))
    if (length(missing_indices) == 0) {
      print(paste("No valid data to impute for column:", col))
      next} 
    # 5 
    formula <- as.formula(paste(col, "~", predictor_col))
    model <- lm(formula, data = df, na.action = na.exclude) # 5
    
    # 6
    predictions <- predict(model, newdata = df[missing_indices, , drop = FALSE], type = "response")
    df[[col]][missing_indices] <- predictions
  } else {
    print(paste("No correlations found for column:", col))}} 
return(df)}
df <- fill_missing_with_regression(df)
df[df < 0] <- 0

df <- df[rowSums(is.na(df)) <= 3, ]

get_mode <- function(x) {uniq_x <- unique(x)
uniq_x[which.max(tabulate(match(x, uniq_x)))]}
for (col in names(df)) {mode_value <- get_mode(df[[col]][!is.na(df[[col]])]) 
df[[col]][is.na(df[[col]])] <- mode_value }
numeric_cols <- sapply(df, is.numeric)
df[numeric_cols] <- round(df[numeric_cols], 1)

write.csv(df, "preprocessed_data.csv", row.names = FALSE)
########Split Dataset#########

# Create Balanced Dataset - Method 1
set.seed(17)
split <- initial_split(df, prop = 0.66, strata = Class)
train <- training(split)
train <- ovun.sample(Class ~ ., data = train, method = "both", N = 700, seed = 1)$data
test <- testing(split)
write.csv(train, "train1.csv", row.names = FALSE)
write.csv(test, "test1.csv", row.names = FALSE)
head(train)
head(test)
dim(train)
dim(test)
table(train$Class)
table(test$Class)



set.seed(17)
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.66, 0.34))
train2 <- df[sample, ]

smote_output <- SMOTE(X = train2[, -which(names(train) == "Class")], target = train2$Class)
train_smote <- smote_output$data
train_smote$Class <- as.factor(train_smote$class)
train_smote$class <- NULL  
train2 <- train_smote
train2 <- train_smote[, c("Class", setdiff(names(train_smote), "Class"))]
class_counts <- table(train2$Class)
desired_size <- 700
class_proportions <- class_counts / sum(class_counts)
samples_per_class <- round(desired_size * class_proportions)
set.seed(17)
train2 <- do.call(rbind, 
                  lapply(names(samples_per_class), function(class_name) {
                    subset <- train2[train2$Class == class_name, ]
                    subset[sample(nrow(subset), samples_per_class[class_name]), ]
                  }))
test2 <- df[!sample, ]

head(train2)
head(test2)
dim(train2)
dim(test2)
table(train2$Class)
table(test2$Class)
write.csv(train2, "train2.csv", row.names = FALSE)
write.csv(test2, "test2.csv", row.names = FALSE)
######FEATURE SELECTION #########

## Method 1 - INFO GAIN
## SPLIT 1 
weights <- information.gain(Class ~ ., train)
selected_features <- rownames(subset(weights, attr_importance > 0))
selected_features <- c("Class", selected_features)
ig_train <- train[, selected_features]
ig_test <- test[, selected_features]
## SPLIT 2
weights <- information.gain(Class ~ ., train2)
selected_features <- rownames(subset(weights, attr_importance > 0))
selected_features <- c("Class", selected_features)
ig_train2 <- train2[, selected_features]
ig_test2 <- test2[, selected_features]

##Method 2 - BORUTA
##SPLIT 1
train$Class <- as.factor(train$Class)
boruta_obj <- Boruta(Class ~ ., train)
boruta_result <- attStats(boruta_obj)
relevant_features <- getSelectedAttributes(boruta_obj, withTentative = TRUE)
relevant_features <- c("Class", relevant_features)
bor_train <- train[, relevant_features]
bor_test <- test[, relevant_features]


##SPLIT 2
train2$Class <- as.factor(train2$Class)
boruta_obj <- Boruta(Class ~ ., train2)
boruta_result <- attStats(boruta_obj)
relevant_features <- getSelectedAttributes(boruta_obj, withTentative = TRUE)
relevant_features <- c("Class", relevant_features)
bor_train2 <- train[, relevant_features]
bor_test2 <- test[, relevant_features]


##Method 3 - RFE
if (!require(caret)) {
  install.packages("caret")
  library(caret)
}
if (!require(e1071)) {
  install.packages("e1071")
  library(e1071)
}
control <- rfeControl(
  functions = caretFuncs, 
  method = "cv",           
  number = 10,            
  verbose = FALSE,         
  returnResamp = "all"
)
###split 1 
train$Class <- as.factor(train$Class)
set.seed(123)  
svm_rfe_result <- rfe(
  x = train[, -1],  
  y = train$Class,
  sizes = c(5, 10, 15, 20),  
  rfeControl = control,
  method = "svmLinear"  )
print(svm_rfe_result)
selected_features_svm <- predictors(svm_rfe_result)
print(selected_features_svm)
rfe_train <- train[, c("Class", selected_features_svm)]
rfe_test <- test[, c("Class", selected_features_svm)] 

### split 2 
train2$Class <- as.factor(train2$Class)
set.seed(123)  
svm_rfe_result <- rfe(
  x = train2[, -1],  
  y = train2$Class,
  sizes = c(5,10,15),  
  rfeControl = control,
  method = "svmLinear"  )
print(svm_rfe_result)
selected_features_svm <- predictors(svm_rfe_result)
print(selected_features_svm)
rfe_train2 <- train2[, c("Class", selected_features_svm)]
rfe_test2 <- test2[, c("Class", selected_features_svm)] 

###### Performance Measures Calculation Functions #########

#Calculate Performance Measures
calculate_measures <- function(tp, fp, tn, fn){
  tpr = tp / (tp + fn)
  fpr = fp / (fp + tn)
  tnr = tn / (fp + tn)
  fnr = fn / (fn + tp)
  precision = tp / (tp + fp)
  recall = tpr
  f_measure <- (2 * precision * recall) / (precision + recall)
  mcc <- (tp*tn - fp*fn)/(sqrt(tp+fp)*sqrt(tp+fn)*sqrt(tn+fp)*sqrt(tn+fn))
  total = (tp + fn + fp + tn)
  p_o = (tp + tn) / total
  p_e1 = ((tp + fn) / total) * ((tp + fp) / total)
  p_e2 = ((fp + tn) / total) * ((fn + tn) / total)
  p_e = p_e1 + p_e2
  k = (p_o - p_e) / (1 - p_e)
  
  measures <- c('TPR', 'FPR', 'TNR', 'FNR', 'Precision', 'Recall', 'F-measure', 'MCC', 'Kappa')
  values <- c(tpr, fpr, tnr, fnr, precision, recall, f_measure, mcc, k)
  measure.df <- data.frame(measures, values)
  return (measure.df)
}

#Calculate Performance Measures by class with weighted average 
calculate_measures_per_class_with_averages <- function(cm) {
  results <- data.frame(
    Class = character(),
    TPR = numeric(),
    FPR = numeric(),
    Precision = numeric(),
    Recall = numeric(),
    F_measure = numeric(),
    MCC = numeric(),
    Kappa = numeric(),
    stringsAsFactors = FALSE
  )
  
  total = sum(cm)
  po = (cm[1,1] + cm[2,2]) / total
  pe = ((cm[1,1] + cm[1,2]) * (cm[1,1] + cm[2,1]) + (cm[2,1] + cm[2,2]) * (cm[1,2] + cm[2,2])) / total^2
  
  for (i in 1:2) {
    if (i == 1) {
      tp = cm[1, 1]
      fp = cm[2, 1]
      fn = cm[1, 2]
      tn = cm[2, 2]
      class_label = "Class 0"
    } else {
      tp = cm[2, 2]
      fp = cm[1, 2]
      fn = cm[2, 1]
      tn = cm[1, 1]
      class_label = "Class 1"
    }
    
    tpr = round(tp / (tp + fn), 1)
    fpr = round(fp / (fp + tn), 1)
    precision = round(tp / (tp + fp), 1)
    recall = round(tpr, 1)  # recall is the same as TPR
    f_measure = round(ifelse((precision + recall) == 0, 0, (2 * precision * recall) / (precision + recall)), 1)
    mcc = round(ifelse((sqrt((tp+fp) * (tp+fn) * (tn+fp) * (tn+fn))) == 0, 0,
                       (tp * tn - fp * fn) / sqrt((tp+fp) * (tp+fn) * (tn+fp) * (tn+fn))), 1)
    kappa = round((po - pe) / (1 - pe), 1)
    
    results <- rbind(results, data.frame(
      Class = class_label,
      TPR = tpr,
      FPR = fpr,
      Precision = precision,
      Recall = recall,
      F_measure = f_measure,
      MCC = mcc,
      Kappa = kappa
    ))
  }
  
  supports = rowSums(cm)
  total_support = sum(supports)
  weighted_avgs <- sapply(results[, -1], function(x) sum(x * supports) / total_support, simplify = FALSE)
  
  weighted_row <- c("Wt. Average", sapply(unlist(weighted_avgs), function(x) round(x, 1)))
  results <- rbind(results, setNames(as.list(weighted_row), names(results)))
  
  results <- na.omit(results) # Remove rows with NA values
  results <- arrange(results, desc(Class)) # Sort by Class in descending order
  
  # Use knitr::kable() to create a nicely formatted table
  kable(results, format = "html", caption = "Class Performance Metrics with Weighted Averages")
}

# Calculate and plot the ROC curve
calculate_and_plot_roc <- function(model, test_data, positive_class_index = 2) {
  
  test_probabilities <- predict(model, newdata = test_data, type = "prob")
  actual_labels <- factor(test_data$Class)
  
  if (!is.numeric(test_probabilities[, positive_class_index])) {
    stop("Las probabilidades deben ser numéricas.")
  }
  
  roc_obj <- roc(actual_labels, test_probabilities[, positive_class_index], 
                 levels = rev(levels(actual_labels)))
  
  auc_value <- auc(roc_obj)
  print(paste("El AUC es:", auc_value))
  
  plot(roc_obj, main = "ROC Curve", col = "#1c61b6", print.auc = TRUE)
  
  return(list(roc_object = roc_obj, auc_value = auc_value))
}
calculate_measures_per_class_with_averages <- function(cm) {
  results <- data.frame(
    Class = character(),
    TPR = numeric(),
    FPR = numeric(),
    Precision = numeric(),
    Recall = numeric(),
    F_measure = numeric(),
    MCC = numeric(),
    Kappa = numeric(),
    stringsAsFactors = FALSE
  )
  
  total <- sum(cm)
  po <- (cm[1, 1] + cm[2, 2]) / total
  pe <- ((cm[1, 1] + cm[1, 2]) * (cm[1, 1] + cm[2, 1]) + (cm[2, 1] + cm[2, 2]) * (cm[1, 2] + cm[2, 2])) / total^2
  
  for (i in 1:2) {
    if (i == 1) {
      tp <- cm[1, 1]
      fp <- cm[2, 1]
      fn <- cm[1, 2]
      tn <- cm[2, 2]
      class_label <- "Class N"
    } else {
      tp <- cm[2, 2]
      fp <- cm[1, 2]
      fn <- cm[2, 1]
      tn <- cm[1, 1]
      class_label <- "Class Y"
    }
    
    tpr <- ifelse(tp + fn == 0, NA, round(tp / (tp + fn), 1))
    fpr <- ifelse(fp + tn == 0, NA, round(fp / (fp + tn), 1))
    precision <- ifelse(tp + fp == 0, NA, round(tp / (tp + fp), 1))
    recall <- tpr  # recall is the same as TPR
    f_measure <- ifelse((precision + recall) == 0, NA, round((2 * precision * recall) / (precision + recall), 1))
    mcc_denominator <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
    mcc <- ifelse(mcc_denominator == 0, NA, round((tp * tn - fp * fn) / mcc_denominator, 1))
    kappa <- round((po - pe) / (1 - pe), 1)
    
    results <- rbind(results, data.frame(
      Class = class_label,
      TPR = tpr,
      FPR = fpr,
      Precision = precision,
      Recall = recall,
      F_measure = f_measure,
      MCC = mcc,
      Kappa = kappa
    ))
  }
  
  supports <- rowSums(cm)
  total_support <- sum(supports)
  weighted_avgs <- sapply(results[, -1], function(x) sum(as.numeric(x) * supports, na.rm = TRUE) / total_support, simplify = FALSE)
  
  weighted_row <- c("Wt. Average", sapply(unlist(weighted_avgs), function(x) round(as.numeric(x), 1)))
  results <- rbind(results, setNames(as.list(weighted_row), names(results)))
  
  results <- na.omit(results) # Remove rows with NA values
  
  # Imprime los resultados en la consola
  print(results)
  
  # Melt the data frame for plotting
  results_melted <- melt(results, id.vars = "Class")
  
  # Convert 'value' column to numeric
  results_melted$value <- as.numeric(results_melted$value)
  
  # Check for NA values before plotting
  if (any(is.na(results_melted$value))) {
    stop("Error: NA values present in the data. Check the calculations.")
  }
  
  # Create the plot
  plot <- ggplot(results_melted, aes(x = Class, y = value, fill = Class)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    facet_wrap(~variable, scales = "free_x") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Class Performance Metrics with Weighted Averages", y = "Value", x = "Class") +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2))
  
  # Save the plot as an image
  ggsave("class_performance_metrics.png", plot, width = 12, height = 8)
  
  # Print the plot
  print(plot)
}


calculate_measures_per_class_with_averages_table <- function(cm) {
  results <- data.frame(
    Class = character(),
    TPR = numeric(),
    FPR = numeric(),
    Precision = numeric(),
    Recall = numeric(),
    F_measure = numeric(),
    MCC = numeric(),
    Kappa = numeric(),
    stringsAsFactors = FALSE
  )
  
  total <- sum(cm)
  po <- (cm[1, 1] + cm[2, 2]) / total
  pe <- ((cm[1, 1] + cm[1, 2]) * (cm[1, 1] + cm[2, 1]) + (cm[2, 1] + cm[2, 2]) * (cm[1, 2] + cm[2, 2])) / total^2
  
  for (i in 1:2) {
    if (i == 1) {
      tp = cm[1, 1]
      fp = cm[2, 1]
      fn = cm[1, 2]
      tn = cm[2, 2]
      class_label = "Class N"
    } else {
      tp = cm[2, 2]
      fp = cm[1, 2]
      fn = cm[2, 1]
      tn = cm[1, 1]
      class_label = "Class Y"
    }
    
    tpr = ifelse(tp + fn == 0, NA, round(tp / (tp + fn), 1))
    fpr = ifelse(fp + tn == 0, NA, round(fp / (fp + tn), 1))
    precision = ifelse(tp + fp == 0, NA, round(tp / (tp + fp), 1))
    recall = tpr  # recall is the same as TPR
    f_measure = ifelse((precision + recall) == 0, NA, round((2 * precision * recall) / (precision + recall), 1))
    mcc_denominator = sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
    mcc = ifelse(mcc_denominator == 0, NA, round((tp * tn - fp * fn) / mcc_denominator, 1))
    kappa = round((po - pe) / (1 - pe), 1)
    
    results <- rbind(results, data.frame(
      Class = class_label,
      TPR = tpr,
      FPR = fpr,
      Precision = precision,
      Recall = recall,
      F_measure = f_measure,
      MCC = mcc,
      Kappa = kappa
    ))
  }
  
  # Calculate supports for weighted average
  supports = rowSums(cm)
  total_support = sum(supports)
  weighted_avgs = sapply(results[, -1], function(x) sum(as.numeric(x) * supports, na.rm = TRUE) / total_support, simplify = FALSE)
  
  weighted_row = c("Weighted Average", sapply(unlist(weighted_avgs), function(x) round(as.numeric(x), 1)))
  results <- rbind(results, setNames(as.list(weighted_row), names(results)))
  
  # Print the results as a table
  print(results)
  
  return(results)  # Optionally, return the results as well
}



##### MODELS #####

##ADA
###ig
##split1
##model 1

ada_grid <- expand.grid(iter = 1, maxdepth = 1:2, nu = seq(0.1, 1, by=0.2))
ctrl <- trainControl(
  method = "LOOCV",  # Leave-one-out cross-validation
  savePredictions = "final",
  classProbs = TRUE,      # Needed for classification
  summaryFunction = twoClassSummary  # Use two-class summary function for classification
)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 5, 
                     verboseIter = FALSE,
                     sampling = "up")
ig_ada_model <- train(Class ~ .,
                      data = ig_train,
                      method = "ada",
                      trControl = ctrl,
                      tuneGrid = ada_grid)
ig_ada_model
test_pred_ig_ada <- predict(ig_ada_model, newdata = ig_test)
test_prob_ig_ada <- predict(ig_ada_model, newdata = ig_test, type = "prob")

CM1 <- confusionMatrix(test_pred_ig_ada,as.factor(ig_test$Class))

CM1

cm <- CM1$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
roc_results <- calculate_and_plot_roc(ig_ada_model, ig_train)
calculate_and_plot_roc(ig_ada_model,ig_train)
cm <- as.matrix(CM1$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
##ADA
##Boruta
###split1
##model 2

ada_grid <- expand.grid(iter = 2, maxdepth = 1:3, nu = seq(0.1, 1, by=0.1))
ctrl <- trainControl(
  method = "LOOCV",  
  savePredictions = "final",
  classProbs = TRUE,      
  summaryFunction = twoClassSummary  # Use two-class summary function for classification
)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 5, 
                     verboseIter = FALSE,
                     sampling = "up")
bor_ada_model <- train(Class ~ .,
                       data = bor_train,
                       method = "ada",
                       trControl = ctrl,
                       tuneGrid = ada_grid)
bor_ada_model
test_pred_bor_ada <- predict(bor_ada_model, newdata = bor_test)

CM2 <- confusionMatrix(test_pred_bor_ada,as.factor(bor_test$Class))

cm <- CM2$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(bor_ada_model,bor_train)
cm <- as.matrix(CM2$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)

##ADA
###rfe
##split 1
##model 3

ada_grid <- expand.grid(iter = 1, maxdepth = 1:2, nu = seq(0.1, 1, by=0.2))
ctrl <- trainControl(
  method = "LOOCV",  
  savePredictions = "final",
  classProbs = TRUE,      
  summaryFunction = twoClassSummary  # Use two-class summary function for classification
)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 5, 
                     verboseIter = FALSE,
                     sampling = "up")
rfe_ada_model <- train(Class ~ .,
                       data = rfe_train,
                       method = "ada",
                       trControl = ctrl,
                       tuneGrid = ada_grid)
rfe_ada_model
test_pred_rfe_ada <- predict(rfe_ada_model, newdata = rfe_test)

CM3 <- confusionMatrix(test_pred_rfe_ada,as.factor(rfe_test$Class))

cm <- CM3$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(rfe_ada_model,rfe_train)
cm <- as.matrix(CM3$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)


##ADA
###ig
##split2
##model 4

ada_grid <- expand.grid(iter = 1, maxdepth = 1:2, nu = seq(0.1, 1, by=0.2))
ctrl <- trainControl(
  method = "LOOCV",  # Leave-one-out cross-validation
  savePredictions = "final",
  classProbs = TRUE,      # Needed for classification
  summaryFunction = twoClassSummary  # Use two-class summary function for classification
)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 5, 
                     verboseIter = FALSE,
                     sampling = "up")
ig_ada_model2 <- train(Class ~ .,
                       data = ig_train2,
                       method = "ada",
                       trControl = ctrl,
                       tuneGrid = ada_grid)
ig_ada_model2
test_pred_ig_ada2 <- predict(ig_ada_model2, newdata = ig_test2)

CM4 <- confusionMatrix(test_pred_ig_ada2,as.factor(ig_test2$Class))

cm <- CM4$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(ig_ada_model2,ig_train2)
cm <- as.matrix(CM4$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
##ada
##boruta
###split2
##model 5

ada_grid <- expand.grid(iter = 2, maxdepth = 1:3, nu = seq(0.1, 1, by=0.1))
ctrl <- trainControl(
  method = "LOOCV",  
  savePredictions = "final",
  classProbs = TRUE,      
  summaryFunction = twoClassSummary  # Use two-class summary function for classification
)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 5, 
                     verboseIter = FALSE,
                     sampling = "up")
bor_ada_model2 <- train(Class ~ .,
                        data = bor_train2,
                        method = "ada",
                        trControl = ctrl,
                        tuneGrid = ada_grid)
bor_ada_model2
test_pred_bor_ada2 <- predict(bor_ada_model2, newdata = bor_test2)

CM5 <- confusionMatrix(test_pred_bor_ada2,as.factor(bor_test2$Class))

cm <- CM5$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(bor_ada_model2,bor_train2)
cm <- as.matrix(CM5$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
##ada
###rfe
##split 2
##model 6

ada_grid <- expand.grid(iter = 1, maxdepth = 1:2, nu = seq(0.1, 1, by=0.2))
ctrl <- trainControl(
  method = "LOOCV",  
  savePredictions = "final",
  classProbs = TRUE,      
  summaryFunction = twoClassSummary  # Use two-class summary function for classification
)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 5, 
                     verboseIter = FALSE,
                     sampling = "up")
rfe_ada_model2 <- train(Class ~ .,
                        data = rfe_train2,
                        method = "ada",
                        trControl = ctrl,
                        tuneGrid = ada_grid)
rfe_ada_model2
test_pred_rfe_ada2 <- predict(rfe_ada_model2, newdata = rfe_test2)

CM6 <- confusionMatrix(test_pred_rfe_ada2,as.factor(rfe_test2$Class))
table(test_pred_rfe_ada)
table(rfe_test2$Class)
cm <- CM6$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(rfe_ada_model2,rfe_train2)
cm <- as.matrix(CM6$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
###RF
###SPLIT1
###IG
###MODEL 7

ctrl <- trainControl(method = "CV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)

mtryValues <- seq(2, ncol(ig_train)-1, by = 1)

set.seed(31)
rfFit <- caret::train(x = ig_train[, -1], 
                      y = ig_train$Class,
                      method = "rf",
                      ntree = 500,
                      tuneGrid = data.frame(mtry = mtryValues),
                      importance = TRUE,
                      metric = "ROC",
                      trControl = ctrl)
rfFit
imp <- varImp(rfFit)
imp

pred_ig <- predict(rfFit,ig_test)
table(pred_ig)
CM7 <- caret::confusionMatrix(pred_ig, as.factor(ig_test$Class))

cm <- CM7$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(rfFit,ig_test)
cm <- as.matrix(CM7$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
##boruta 
##RF
##SPLIT 1
###model 8

ctrl <- trainControl(method = "CV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)

mtryValues <- seq(2, ncol(bor_train)-1, by = 1)

set.seed(31)
rfFit <- caret::train(x = bor_train[, -1], 
                      y = bor_train$Class,
                      method = "rf",
                      ntree = 500,
                      tuneGrid = data.frame(mtry = mtryValues),
                      importance = TRUE,
                      metric = "ROC",
                      trControl = ctrl)
rfFit
imp <- varImp(rfFit)
imp

pred_bor <- predict(rfFit,bor_test)
table(pred_bor)
CM8 <- caret::confusionMatrix(pred_bor, as.factor(bor_test$Class))

cm <- CM8$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(rfFit,bor_test)
cm <- as.matrix(CM8$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
##RFE 
##RF
##SPLIT 1
###model 9

ctrl <- trainControl(method = "CV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)

mtryValues <- seq(2, ncol(rfe_train)-1, by = 1)

set.seed(31)
rfFit <- caret::train(x = rfe_train[, -1], 
                      y = rfe_train$Class,
                      method = "rf",
                      ntree = 500,
                      tuneGrid = data.frame(mtry = mtryValues),
                      importance = TRUE,
                      metric = "ROC",
                      trControl = ctrl)
rfFit

imp <- varImp(rfFit)
imp

pred_rfe <- predict(rfFit,rfe_test)
table(pred_rfe)
CM9 <- caret::confusionMatrix(pred_rfe, as.factor(rfe_test$Class))

cm <- CM9$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(rfFit,rfe_test)
cm <- as.matrix(CM9$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
###RF
###SPLIT2
###IG
###MODEL 10

ctrl <- trainControl(method = "CV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)

mtryValues <- seq(2, ncol(ig_train)-1, by = 1)

set.seed(31)
rfFit2 <- caret::train(x = ig_train2[, -1], 
                       y = ig_train2$Class,
                       method = "rf",
                       ntree = 500,
                       tuneGrid = data.frame(mtry = mtryValues),
                       importance = TRUE,
                       metric = "ROC",
                       trControl = ctrl)
rfFit2
imp <- varImp(rfFit2)
imp

pred_ig2 <- predict(rfFit2,ig_test2)
CM10 <- caret::confusionMatrix(pred_ig2, as.factor(ig_test2$Class))

cm <- CM10$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(rfFit2,ig_test2)
cm <- as.matrix(CM10$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
##boruta 
##RF
##SPLIT 1
###model 11

ctrl <- trainControl(method = "CV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)

mtryValues <- seq(2, ncol(bor_train2)-1, by = 1)

set.seed(31)
rfFit2 <- caret::train(x = bor_train2[, -1], 
                       y = bor_train2$Class,
                       method = "rf",
                       ntree = 500,
                       tuneGrid = data.frame(mtry = mtryValues),
                       importance = TRUE,
                       metric = "ROC",
                       trControl = ctrl)
rfFit2
imp <- varImp(rfFit2)
imp

pred_bor2 <- predict(rfFit2,bor_test2)
table(pred_bor2)
CM11 <- caret::confusionMatrix(pred_bor2, as.factor(bor_test2$Class))

cm <- CM11$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(rfFit2,bor_test2)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
##RFE 
##RF
##SPLIT 2
###model 12

ctrl <- trainControl(method = "CV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)

mtryValues <- seq(2, ncol(rfe_train2)-1, by = 1)

set.seed(31)
rfFit2_rfe <- caret::train(x = rfe_train2[, -1], 
                           y = rfe_train2$Class,
                           method = "rf",
                           ntree = 500,
                           tuneGrid = data.frame(mtry = mtryValues),
                           importance = TRUE,
                           metric = "ROC",
                           trControl = ctrl)
rfFit2_rfe

imp <- varImp(rfFit2_rfe)
imp

pred_rfe2 <- predict(rfFit2_rfe,rfe_test2)
table(pred_rfe2)
CM12 <- caret::confusionMatrix(pred_rfe2, as.factor(rfe_test2$Class))

cm <- CM12$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(rfFit2_rfe,rfe_train2)
cm <- as.matrix(CM12$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
##ig
###GLM
##split1
##model 13

glm_grid <- expand.grid(.parameter = seq(1, 10, 1))
ig_glm_model <- train(Class ~ .,
                      data = ig_train, 
                      method = "glm",
                      preProcess = c("scale", "center"),
                      trControl = ctrl,
                      tuneGrid = glm_grid)
ig_glm_model
test_pred_ig_glm <- predict(ig_glm_model, newdata = ig_test)
CM13 <- confusionMatrix(test_pred_ig_glm, as.factor(ig_test$Class), mode = "everything")
cm <- CM13$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(ig_glm_model,ig_test)
cm <- as.matrix(CM13$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
##boruta
###GLM
##split1
##model 14

glm_grid <- expand.grid(.parameter = seq(1, 10, 1))
bor_glm_model <- train(Class ~ .,
                       data = bor_train, 
                       method = "glm",
                       preProcess = c("scale", "center"),
                       trControl = ctrl,
                       tuneGrid = glm_grid)
bor_glm_model
test_pred_bor_glm <- predict(bor_glm_model, newdata = bor_test)
CM14 <- confusionMatrix(test_pred_bor_glm, as.factor(bor_test$Class), mode = "everything")

cm <- CM14$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(bor_glm_model,bor_test)
cm <- as.matrix(CM14$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
##rfe
###GLM
##split1
##model 15

glm_grid <- expand.grid(.parameter = seq(1, 10, 1))
rfe_glm_model <- train(Class ~ .,
                       data = rfe_train, 
                       method = "glm",
                       preProcess = c("scale", "center"),
                       trControl = ctrl,
                       tuneGrid = glm_grid)
rfe_glm_model
test_pred_rfe_glm <- predict(rfe_glm_model, newdata = rfe_test)
CM15 <- confusionMatrix(test_pred_rfe_glm, as.factor(rfe_test$Class), mode = "everything")

cm <- CM15$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(rfe_glm_model,rfe_test)
cm <- as.matrix(CM15$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
##ig
###GLM
##split2
##model 16

glm_grid <- expand.grid(.parameter = seq(1, 10, 1))
ig_glm_model2 <- train(Class ~ .,
                       data = ig_train2, 
                       method = "glm",
                       preProcess = c("scale", "center"),
                       trControl = ctrl,
                       tuneGrid = glm_grid)
ig_glm_model2
test_pred_ig_glm2 <- predict(ig_glm_model2, newdata = ig_test2)
CM16 <- confusionMatrix(test_pred_ig_glm2, as.factor(ig_test2$Class), mode = "everything")

cm <- CM16$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(ig_glm_model2,ig_test2)
cm <- as.matrix(CM16$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
##boruta
###GLM
##split2
##model 17

glm_grid <- expand.grid(.parameter = seq(1, 10, 1))
bor_glm_model2 <- train(Class ~ .,
                        data = bor_train2, 
                        method = "glm",
                        preProcess = c("scale", "center"),
                        trControl = ctrl,
                        tuneGrid = glm_grid)
bor_glm_model2
test_pred_bor_glm2 <- predict(bor_glm_model2, newdata = bor_test2)
CM17 <- confusionMatrix(test_pred_bor_glm2, as.factor(bor_test2$Class), mode = "everything")

cm <- CM17$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(bor_glm_model2,bor_test2)
cm <- as.matrix(CM17$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
##rfe
###GLM
##split2
##model 18

glm_grid <- expand.grid(.parameter = seq(1, 10, 1))
rfe_glm_model2 <- train(Class ~ .,
                        data = rfe_train2, 
                        method = "glm",
                        preProcess = c("scale", "center"),
                        trControl = ctrl,
                        tuneGrid = glm_grid)
rfe_glm_model2
test_pred_rfe_glm2 <- predict(rfe_glm_model2, newdata = rfe_test2)
CM18 <- confusionMatrix(test_pred_rfe_glm2, as.factor(rfe_test2$Class), mode = "everything")

cm <- CM18$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(bor_glm_model2,bor_test2)
cm <- as.matrix(CM18$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
###SVM
##ig
##split 1
##model 19

train_control <- trainControl(method = "CV", number = 10, 
                              summaryFunction = defaultSummary)
svmGrid <-  expand.grid(sigma = seq(0.1, 0.4, by = 0.05), C = seq(1.0, 2.0, by = 0.1))

model <- caret::train(Class ~ ., data = ig_train, method = "svmRadial",
                      preProc = c("center", "scale"),
                      trControl = train_control, tuneGrid = svmGrid)
model
plot(model)

pred_svm_ig <- predict(model, ig_test)
CM19 <- caret::confusionMatrix(pred_svm_ig , as.factor(ig_test$Class), mode = "everything")
CM19

cm <- CM19$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
cm <- as.matrix(CM19$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
###SVM
##boruta
##split 1
##model 20

train_control <- trainControl(method = "CV", number = 10, 
                              summaryFunction = defaultSummary)
svmGrid <-  expand.grid(sigma = seq(0.1, 0.4, by = 0.05), C = seq(1.0, 2.0, by = 0.1))

model <- caret::train(Class ~ ., data = bor_train, method = "svmRadial",
                      preProc = c("center", "scale"),
                      trControl = train_control, tuneGrid = svmGrid)
model
plot(model)

pred_svm_bor <- predict(model, bor_test)
CM20 <- caret::confusionMatrix(pred_svm_bor , as.factor(bor_test$Class))
CM20

cm <- CM20$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
cm <- as.matrix(CM20$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
###SVM
##rfe
##split 1
##model 21

train_control <- trainControl(method = "CV", number = 10, 
                              summaryFunction = defaultSummary)
svmGrid <-  expand.grid(sigma = seq(0.1, 0.4, by = 0.05), C = seq(1.0, 2.0, by = 0.1))

model <- caret::train(Class ~ ., data = rfe_train, method = "svmRadial",
                      preProc = c("center", "scale"),
                      trControl = train_control, tuneGrid = svmGrid)
model
plot(model)

pred_svm_rfe <- predict(model, rfe_test)
CM21 <- caret::confusionMatrix(pred_svm_rfe, as.factor(rfe_test$Class))
CM21

cm <- CM21$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
cm <- as.matrix(CM21$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)

###SVM
##ig
##split 2
##model 22

train_control <- trainControl(method = "CV", number = 10, 
                              summaryFunction = defaultSummary)
svmGrid <-  expand.grid(sigma = seq(0.1, 0.4, by = 0.05), C = seq(1.0, 2.0, by = 0.1))

model_ig_svm2 <- caret::train(Class ~ ., data = ig_train2, method = "svmRadial",
                              preProc = c("center", "scale"),
                              trControl = train_control, tuneGrid = svmGrid)
model_ig_svm2
plot(model_ig_svm2)

pred_svm_ig2 <- predict(model_ig_svm2, ig_test2)
CM22<- caret::confusionMatrix(pred_svm_ig2 , as.factor(ig_test2$Class))
CM22

cm <- CM22$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
cm <- as.matrix(CM22$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)

###SVM
##boruta
##split 1
##model 23

train_control <- trainControl(method = "CV", number = 10, 
                              summaryFunction = defaultSummary)
svmGrid <-  expand.grid(sigma = seq(0.1, 0.4, by = 0.05), C = seq(1.0, 2.0, by = 0.1))

model_svm_bor2 <- caret::train(Class ~ ., data = bor_train2, method = "svmRadial",
                               preProc = c("center", "scale"),
                               trControl = train_control, tuneGrid = svmGrid)
model_svm_bor2 
plot(model_svm_bor2 )

pred_svm_bor2 <- predict(model_svm_bor2 , bor_test2)
CM23 <- caret::confusionMatrix(pred_svm_bor2 , as.factor(bor_test2$Class))
CM23

cm <- CM23$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
cm <- as.matrix(CM23$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)

###SVM
##rfe
##split 2
##model 24

train_control <- trainControl(method = "CV", number = 10, 
                              summaryFunction = defaultSummary)
svmGrid <-  expand.grid(sigma = seq(0.1, 0.4, by = 0.05), C = seq(1.0, 2.0, by = 0.1))

model_rfe_svm2 <- caret::train(Class ~ ., data = rfe_train2, method = "svmRadial",
                               preProc = c("center", "scale"),
                               trControl = train_control, tuneGrid = svmGrid)
model_rfe_svm2
plot(model_rfe_svm2)

pred_svm_rfe2 <- predict(model_rfe_svm2, rfe_test2)
CM24 <- caret::confusionMatrix(pred_svm_rfe2 , as.factor(rfe_test2$Class))
CM24

cm <- CM24$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures

cm <- as.matrix(CM24$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)

###ctree
##ig
##split 1
##model 25

ctrl <- trainControl(method = "CV", number = 10)

ctreeGrid <-  expand.grid(mincriterion = 0.05)

model <- caret::train(Class ~ ., data = ig_train, method = "ctree",
                      trControl = ctrl, tuneGrid = ctreeGrid )
model

pred_ctree_ig <- predict(model, ig_test)
CM25 <- caret::confusionMatrix(pred_ctree_ig , as.factor(ig_test$Class))
CM25

cm <- CM25$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
cm <- as.matrix(CM25$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)

###ctree
##boruta
##split 1
##model 26

ctrl <- trainControl(method = "CV", number = 10)

ctreeGrid <-  expand.grid(mincriterion = 0.05)

model <- caret::train(Class ~ ., data = bor_train, method = "ctree",
                      trControl = ctrl, tuneGrid = ctreeGrid )
model

pred_ctree_bor <- predict(model, bor_test)
CM26 <- caret::confusionMatrix(pred_ctree_bor , as.factor(bor_test$Class))
CM26

cm <- CM26$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
cm <- as.matrix(CM26$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)

###ctree
##rfe
##split 1
##model 27

ctrl <- trainControl(method = "CV", number = 10)

ctreeGrid <-  expand.grid(mincriterion = 0.05)

model <- caret::train(Class ~ ., data = rfe_train, method = "ctree",
                      trControl = ctrl, tuneGrid = ctreeGrid )
model

pred_ctree_rfe <- predict(model, rfe_test)
CM27 <- caret::confusionMatrix(pred_ctree_rfe , as.factor(rfe_test$Class))
CM27

cm <- CM27$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
cm <- as.matrix(CM27$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)

###ctree
##ig
##split 2
##model 28

ctrl <- trainControl(method = "CV", number = 10)

ctreeGrid <-  expand.grid(mincriterion = 0.05)

model <- caret::train(Class ~ ., data = ig_train2, method = "ctree",
                      trControl = ctrl, tuneGrid = ctreeGrid )
model

pred_ctree_ig2 <- predict(model, ig_test2)
CM28 <- caret::confusionMatrix(pred_ctree_ig2 , as.factor(ig_test2$Class))
CM28

cm <- CM28$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
cm <- as.matrix(CM28$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
###ctree
##boruta
##split 2
##model 29

ctrl <- trainControl(method = "CV", number = 10)

ctreeGrid <-  expand.grid(mincriterion = 0.05)

model <- caret::train(Class ~ ., data = bor_train2, method = "ctree",
                      trControl = ctrl, tuneGrid = ctreeGrid )
model

pred_ctree_bor2 <- predict(model, bor_test2)
CM29 <- caret::confusionMatrix(pred_ctree_bor2 , as.factor(bor_test2$Class))
CM29

cm <- CM29$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(model ,bor_test2)
cm <- as.matrix(CM29$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
###ctree
##rfe
##split 2
##model 30

ctrl <- trainControl(method = "CV", number = 10)

ctreeGrid <-  expand.grid(mincriterion = 0.05)

model <- caret::train(Class ~ ., data = rfe_train2, method = "ctree",
                      trControl = ctrl, tuneGrid = ctreeGrid )
model

pred_ctree_rfe2 <- predict(model, rfe_test2)
CM30 <- caret::confusionMatrix(pred_ctree_rfe2 , as.factor(rfe_test2$Class))
CM30

cm <- CM30$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(model ,rfe_test2)
cm <- as.matrix(CM30$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
###gcvEarth
##ig
##split 1
##model 31

ctrl <- trainControl(method = "CV", number = 10)

gcvGrid<- expand.grid(
  degree = c(1, 2, 3)  # Adjust based on your specific needs
)

model <- caret::train(Class ~ ., data = ig_train, method = "gcvEarth",
                      trControl = ctrl, tuneGrid = gcvGrid )
model

pred_gcv_ig <- predict(model, ig_test)
CM31 <- caret::confusionMatrix(pred_gcv_ig , as.factor(ig_test$Class))
CM31

cm <- CM31$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(model ,ig_test)
cm <- as.matrix(CM31$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
###gcvEarth
##boruta
##split 1
##model 32

ctrl <- trainControl(method = "CV", number = 10)

gcvGrid<- expand.grid(
  degree = c(1, 2, 3)  # Adjust based on your specific needs
)

model <- caret::train(Class ~ ., data = bor_train, method = "gcvEarth",
                      trControl = ctrl, tuneGrid = gcvGrid )
model

pred_gcv_bor <- predict(model, bor_test)
CM32 <- confusionMatrix(pred_gcv_bor , as.factor(bor_test$Class))
CM32

cm <- CM32$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(model ,bor_test)
cm <- as.matrix(CM32$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
###gcvEarth
##rfe
##split 1
##model 33

ctrl <- trainControl(method = "CV", number = 10)

gcvGrid<- expand.grid(
  degree = c(1, 2, 3)  # Adjust based on your specific needs
)

model <- caret::train(Class ~ ., data = rfe_train, method = "gcvEarth",
                      trControl = ctrl, tuneGrid = gcvGrid )
model

pred_gcv_rfe <- predict(model, rfe_test)
CM33 <- caret::confusionMatrix(pred_gcv_rfe , as.factor(rfe_test$Class))
CM33

cm <- CM33$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(model ,rfe_test)
cm <- as.matrix(CM33$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
###gcvEarth
##ig
##split 2
##model 34

ctrl <- trainControl(method = "CV", number = 10)

gcvGrid<- expand.grid(
  degree = c(1, 2, 3)  # Adjust based on your specific needs
)

model <- caret::train(Class ~ ., data = ig_train2, method = "gcvEarth",
                      trControl = ctrl, tuneGrid = gcvGrid )
model

pred_gcv_ig2 <- predict(model, ig_test2)
CM34 <- caret::confusionMatrix(pred_gcv_ig2 , as.factor(ig_test2$Class))
CM34

cm <- CM34$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(model ,ig_test2)
cm <- as.matrix(CM34$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
###gcvEarth
##boruta
##split 2
##model 35

ctrl <- trainControl(method = "CV", number = 10)

gcvGrid<- expand.grid(
  degree = c(1, 2, 3)  # Adjust based on your specific needs
)

model <- caret::train(Class ~ ., data = bor_train2, method = "gcvEarth",
                      trControl = ctrl, tuneGrid = gcvGrid )
model

pred_gcv_bor2 <- predict(model, bor_test2)
CM35 <- caret::confusionMatrix(pred_gcv_bor2 , as.factor(bor_test2$Class))
CM35

cm <- CM35$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
calculate_and_plot_roc(model ,bor_test2)
cm <- as.matrix(CM35$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)
###gcvEarth
##rfe
##split 2
##model 36

ctrl <- trainControl(method = "CV", number = 10)

gcvGrid<- expand.grid(
  degree = c(1, 2, 3)  # Adjust based on your specific needs
)

model <- caret::train(Class ~ ., data = rfe_train2, method = "gcvEarth",
                      trControl = ctrl, tuneGrid = gcvGrid )
model

pred_gcv_rfe2 <- predict(model, rfe_test2)
CM36 <- caret::confusionMatrix(pred_gcv_rfe2 , as.factor(rfe_test2$Class))
CM36

cm <- CM36$table

tp = cm[2,2]
fp = cm[2,1]
tn = cm[1,1]
fn = cm[1,2]
performance_measures = calculate_measures(tp, fp, tn, fn)
performance_measures
cm <- as.matrix(CM36$table)
cm <- matrix(as.numeric(cm), nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm)))
calculate_measures_per_class_with_averages_table(cm)

