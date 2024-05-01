#####-------------FUNCTION TO CATEOGRIZE FEATURES-------------#####
# Code resued from my previous assignments
# Now making it a function to use it in future projects and assignments
# I am gonna save the function and will use it in the future from those files

categorize_features <- function(data) {
  # Derive the relevant data types fetaures
  numeric_cols <- colnames(data)[sapply(data, is.numeric)]
  factor_cols <- colnames(data)[sapply(data, is.factor)]
  character_cols <- colnames(data)[sapply(data, is.character)]
  
  # Returning variable
  ret_val <- list("numeric_cols" = numeric_cols, 
                  "factor_cols" = factor_cols, 
                  "character_cols" = character_cols)
  
  return (ret_val)
}

#####-------------FUNCTION TO EVALUATE PREDICTIONS-------------#####
# Just a function defimed by myself
calculate_mae_fn <- function(actual_val, predicted_val) {
  mae_val <- mean(abs(actual_val - predicted_val))
  return(mae_val)
}

calculate_mape_fn <- function(actual_val, predicted_val) {
  mape_val <- mean(abs((actual_val - predicted_val)/actual_val))
  return(mape_val)
}

# Reused functions from the lEcture ntoes
rmse <- function(pred_val, orig_val){
  return((mean((pred_val - orig_val)^2))^0.5)
}


# MOdel Evaltuation Metrics BINary Classification
model_evaluation_fn <- function(act_val, pred_val) {
  # Confusion Matrix
  conf_mat <- table(act_val, pred_val)
  
  # True / False Values
  tp = conf_mat[1, 1]
  tn = conf_mat[2, 2]
  fp = conf_mat[1, 2]
  fn = conf_mat[2, 1]
  
  # Metrics Calculation
  accuracy_rate = (tp + tn) / sum(conf_mat)
  error_rate = 1 - accuracy_rate
  
  
  # Display Results
  print(conf_mat)
  print(CrossTable(act_val, pred_val, prop.chisq= FALSE, prop.t = FALSE, dnn= c('actual', 'predicted')))
  
}


#####-------------FUNCTION TO PERFORM ONE HOT ENCODING-------------#####

one_hot_encoding_fn <- function(data_val, features_to_encode) {
  
  # Saving in anoter variable
  data_val_frame <- data_val
  
  # dataframe to data table conversion
  data_val_table <- as.data.table(data_val_frame)
  
  # encoded data tables
  data_val_table <- one_hot(data_val_table, 
                            cols = features_to_encode,
                            dropCols = TRUE,
                            dropUnusedLevels = TRUE)
  #Encoded Data Frame
  data_val_encoded <- as.data.frame(data_val_table)
  
  # Returingin engcoded dataframe
  return(data_val_encoded)
}


#####-------------FUNCTION TO UNDERSTAND ASSOCIATIONS-------------#####
## Reused My Code from the Assignment 1 - automation
## and made it into a function

# Nuerical vs Numerical - Correlation Test 
association_numerical_numerical <- function(df, 
                                            numer_vars, 
                                            target_var, 
                                            signif_val = 0.05) {
  numer_p_value_dataframe <- data.frame()
  
  for(v in numer_vars[(!numer_vars %in% c(target_var))]){
    
    print(paste("*************",v,"*************",sep= ""))
    plot(x = df[, target_var], 
         y = df[,v], 
         main = paste("Scatter Plot - ",target_var," vs. ",v, sep = ""),
         xlab = target_var, 
         ylab = v)
    
    print(paste("Simple correlation check between the variables : ", cor(df[, target_var], df[, v])))
    
    # Kendall Correlation Test - to test association between 2 numerical variables
    # I am usigng the kendall rank correlation because 
    # I am comparing the G3 (score) varaible which can be ranked.
    # Also, Unlike Pearson correlation, Rank correlation can be
    # applied to both continuous (interval) and ordinal variables.
    hypotheis_test_employed <- "kendall"
    numer_Test_details <- cor.test(df[,v], df[,target_var], method = hypotheis_test_employed)
    
    print(numer_Test_details)
    
    numer_p_value_dataframe <- rbind(
      numer_p_value_dataframe,
      data.frame(
        feature1 = target_var,
        feature2 = v,
        p.value = numer_Test_details["p.value"],
        test_employed = hypotheis_test_employed
      )
    )
  }
  
  numer_p_value_dataframe$signif <- signif_val
  numer_p_value_dataframe$result <- (
    ifelse(
      numer_p_value_dataframe$p.value < numer_p_value_dataframe$signif, "pass", "fail" 
    )
  )
  
  return(numer_p_value_dataframe)
  
}


## Reused My Code From Assignemnet 2
# Categorical vs Categoricl - ChiSQ Test
association_categorical_categorical <- function(df,
                                                factor_vars, 
                                                target_var,
                                                signif_val = 0.05) {
  
  factor_p_value_dataframe <- data.frame()
  for(v in factor_vars[(!factor_vars %in% c(target_var))]){
    print(paste("--------------------",v,"--------------------"))
    ctable <- table(df[, v], df[, target_var])
    mosaicplot(ctable,
               ylab = target_var,
               xlab = v,
               main = paste("Mosaic graph of",target_var,"vs",v,sep = " "))
    print(chisq.test(ctable))
    factor_Test_details = chisq.test(ctable)
    factor_p_value_dataframe <- rbind(
      factor_p_value_dataframe,
      data.frame(feature1 = target_var,
                 feature2 = v,
                 p.value = factor_Test_details["p.value"]))
  }
  
  factor_p_value_dataframe$signif <- signif_val
  factor_p_value_dataframe$result <- (
    ifelse(
      factor_p_value_dataframe$p.value < factor_p_value_dataframe$signif,
      "pass", "fail" ))
  
  return(factor_p_value_dataframe)
}





# Numerical vs Categoricl
association_numerical_categorical <- function(df, 
                                              numer_vars, 
                                              target_var, 
                                              signif_val = 0.05) {
  numer_p_value_dataframe <- data.frame()
  for(v in numer_vars){
    
    print(paste("*************",v,"*************",sep= ""))
    plot(df[,v]~df[, target_var], 
         col="green", 
         main = paste("Box Plot - ",target_var," vs. ",v, sep = ""),
         xlab = target_var,
         ylab = v)
    
    # t.test - to test whether a categorical variable with two levels is
    # independent of a continuous numerical variable
    numer_Test_details = t.test(df[,v] ~ df[,target_var], 
                                alternative = "two.sided")
    print(numer_Test_details)
    numer_p_value_dataframe <- rbind(
      numer_p_value_dataframe,
      data.frame(feature1 = target_var,
                 feature2 = v,
                 p.value = numer_Test_details["p.value"]))
  }
  
  numer_p_value_dataframe$signif <- signif_val
  numer_p_value_dataframe$result <- (
    ifelse(
      numer_p_value_dataframe$p.value < numer_p_value_dataframe$signif,
      "pass", "fail" ))
  
  return(numer_p_value_dataframe)
}