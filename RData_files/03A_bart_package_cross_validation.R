##performs out-of-sample error estimation for a BART model
my_k_fold_cv = function(X, y, k_folds = 5, folds_vec = NULL, verbose = FALSE, ...){
  if (class(X) != "data.frame"){
    stop("The training data X must be a data frame.")
  }
  if (!(class(y) %in% c("numeric", "integer", "factor"))){
    stop("Your response must be either numeric, an integer or a factor with two levels.\n")
  }	
  if(!is.null(folds_vec) & class(folds_vec)!= "integer") stop("folds_vec must be an a vector of integers specifying the indexes of each folds.")
  
  y_levels = levels(y)
  if (class(y) == "numeric" || class(y) == "integer"){ #if y is numeric, then it's a regression problem
    pred_type = "regression"
  } else if (class(y) == "factor" & length(y_levels) == 2){ #if y is a factor and and binary, then it's a classification problem
    pred_type = "classification"
  }
  
  n = nrow(X)
  Xpreprocess = pre_process_training_data(X)$data
  
  p = ncol(Xpreprocess)
  
  #set up k folds
  if(is.null(folds_vec)){ ##if folds were not pre-set:
    if (k_folds == Inf){ #leave-one-out
      k_folds = n
    }
    
    if (k_folds <= 1 || k_folds > n){
      stop("The number of folds must be at least 2 and less than or equal to n, use \"Inf\" for leave one out")
    }
    temp = rnorm(n)
    folds_vec = cut(temp, breaks = quantile(temp, seq(0, 1, length.out = k_folds + 1)), 
                    include.lowest= T, labels = F)
  }else{
    k_folds = length(unique(folds_vec)) ##otherwise we know the folds, so just get k
  }	
  
  if (pred_type == "regression"){
    L1_err = 0
    L2_err = 0
    yhat_cv = numeric(n) ##store cv
    NullModel_MAE_test = 0 # Added by Vineeth
    NullModel_RMSE_test = 0 # Added by Vineeth
  } else {
    phat_cv = numeric(n)
    yhat_cv = factor(n, levels = y_levels)
    confusion_matrix = matrix(0, nrow = 3, ncol = 3)
    rownames(confusion_matrix) = c(paste("actual", y_levels), "use errors")
    colnames(confusion_matrix) = c(paste("predicted", y_levels), "model errors")
  }
  
  Xy = data.frame(Xpreprocess, y) ##set up data
  
  for (k in 1 : k_folds){
    cat(".")
    train_idx = which(folds_vec != k)
    test_idx = setdiff(1 : n, train_idx)
    test_data_k = Xy[test_idx, ]
    training_data_k = Xy[train_idx, ]
    
    #we cannot afford the time sink of serialization during the grid search, so shut it off manually
    args = list(...)
    args$serialize = FALSE
    
    #build bart object
    bart_machine_cv = do.call(build_bart_machine, c(list(
      X = training_data_k[, 1 : p, drop = FALSE], 
      y = training_data_k[, (p + 1)], 
      run_in_sample = FALSE, 
      verbose = verbose), args))
    predict_obj = bart_predict_for_test_data(bart_machine_cv, test_data_k[, 1 : p, drop = FALSE], test_data_k[, (p + 1)])
    
     
    #tabulate errors
    if (pred_type == "regression"){
      L1_err = L1_err + predict_obj$L1_err
      L2_err = L2_err + predict_obj$L2_err
      yhat_cv[test_idx] = predict_obj$y_hat
      # Added by Vineeth
      NullModel_MAE_test = NullModel_MAE_test + mean(abs(test_data_k[, (p + 1)] - mean(y)))
      NullModel_RMSE_test = NullModel_RMSE_test + sqrt(mean((test_data_k[, (p + 1)] - mean(y))^2))
    } else {
      phat_cv[test_idx] = predict_obj$p_hat
      yhat_cv[test_idx] = predict_obj$y_hat
      tab = table(factor(test_data_k$y, levels = y_levels), factor(predict_obj$y_hat, levels = y_levels))
      confusion_matrix[1 : 2, 1 : 2] = confusion_matrix[1 : 2, 1 : 2] + tab
    }
  }
  cat("\n")
  if (pred_type == "regression"){
    list(y_hat = yhat_cv, L1_err = L1_err, L2_err = L2_err, rmse = sqrt(L2_err / n), PseudoRsq = 1 - L2_err / sum((y - mean(y))^2), folds = folds_vec,
         NullModel_MAE_test= NullModel_MAE_test/n, NullModel_RMSE_test = NullModel_RMSE_test/n # Added by Vineeth
         )
  } else {		
    #calculate the rest of the confusion matrix and return it plus the errors
    confusion_matrix[3, 1] = round(confusion_matrix[2, 1] / (confusion_matrix[1, 1] + confusion_matrix[2, 1]), 3)
    confusion_matrix[3, 2] = round(confusion_matrix[1, 2] / (confusion_matrix[1, 2] + confusion_matrix[2, 2]), 3)
    confusion_matrix[1, 3] = round(confusion_matrix[1, 2] / (confusion_matrix[1, 1] + confusion_matrix[1, 2]), 3)
    confusion_matrix[2, 3] = round(confusion_matrix[2, 1] / (confusion_matrix[2, 1] + confusion_matrix[2, 2]), 3)
    confusion_matrix[3, 3] = round((confusion_matrix[1, 2] + confusion_matrix[2, 1]) / sum(confusion_matrix[1 : 2, 1 : 2]), 3)
    list(y_hat = yhat_cv, phat = phat_cv, confusion_matrix = confusion_matrix, misclassification_error = confusion_matrix[3, 3], folds = folds_vec)
  }
  
}


##### Vineeth's addition  : sep 09 : Source code from github

dummify_data = function(data){
  as.data.frame(pre_process_training_data(data)$data)
}

##private function that handles all pre-processing (dummification, missing data, etc.)
pre_process_training_data = function(data, use_missing_data_dummies_as_covars = FALSE, imputations = NULL){
  
  #first convert characters to factors
  character_vars = names(which(sapply(data, class) == "character"))
  for (character_var in character_vars){
    data[, character_var] = as.factor(data[, character_var])
  }
  
  factors = names(which(sapply(data, class) == "factor"))
  
  factor_lengths = c()
  for (fac in factors){
    #first create the dummies to be appended for this factor
    dummied = do.call(cbind, lapply(levels(data[, fac]), function(lev){as.numeric(data[, fac] == lev)}))
    #ensure they're named appropriately
    colnames(dummied) = paste(fac, levels(data[, fac]), sep = "_")
    #append them to the data
    data = cbind(data, dummied)
    #delete the factor covariate from the design matrix
    data[, fac] = NULL
    #record the length of this factor
    factor_lengths = c(factor_lengths, ncol(dummied))
  }
  
  if (use_missing_data_dummies_as_covars){		
    #now take care of missing data - add each column as a missingness dummy
    predictor_columns_with_missingness = as.numeric(which(colSums(is.na(data)) > 0))
    
    #only do something if there are predictors with missingness
    if (length(predictor_columns_with_missingness) > 0){
      M = matrix(0, nrow = nrow(data), ncol = length(predictor_columns_with_missingness))
      for (i in 1 : nrow(data)){
        for (j in 1 : length(predictor_columns_with_missingness)){
          if (is.missing(data[i, predictor_columns_with_missingness[j]])){
            M[i, j] = 1
          }
        }
      }
      colnames(M) = paste("M_", colnames(data)[predictor_columns_with_missingness], sep = "")
      
      #now we may want to add imputations before the missingness dummies
      if (!is.null(imputations)){
        data = cbind(data, imputations)
      }
      #append the missing dummy columns to data as if they're real attributes themselves
      data = cbind(data, M)			
    }
  } else if (!is.null(imputations)){ #now we may want to add imputations
    data = cbind(data, imputations) 
  }
  #make sure to cast it as a data matrix and return it along with the factor_lengths
  list(data = data.matrix(data), factor_lengths = factor_lengths)
}

is.missing = function(x){
  is.na(x) || is.nan(x)
}

