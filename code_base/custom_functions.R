"
This script is created to store all the user defined functions\n
Will be useful to debug codes in future
Main code . script will not be ciongested with a lot of UDFs
Reused the iiudes from Assignemnt 4
"


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
