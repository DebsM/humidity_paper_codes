#From github Source code: https://github.com/kapelner/bartMachine/blob/master/bartMachine/R/bart_package_plots.R
##function for investigating variable inclusion proportions

check_serialization = function(bart_machine){
  if (is.jnull(bart_machine$java_bart_machine)){
    stop("This bartMachine object was loaded from an R image but was not serialized.\n  Please build bartMachine using the option \"serialize = TRUE\" next time.\n")
  }
}

##private function that creates a duplicate of an existing bartMachine object.
bart_machine_duplicate = function(bart_machine, X = NULL, y = NULL, cov_prior_vec = NULL, num_trees = NULL, run_in_sample = NULL, covariates_to_permute = NULL, verbose = NULL, ...){	
  if (is.null(X)){
    X = bart_machine$X
  }
  if (is.null(y)){
    y = bart_machine$y
  }
  if (is.null(cov_prior_vec)){
    cov_prior_vec = bart_machine$cov_prior_vec
  }
  if (is.null(num_trees)){
    num_trees = bart_machine$num_trees
  }	
  if (is.null(run_in_sample)){
    run_in_sample = FALSE
  }
  if (is.null(covariates_to_permute)){
    covariates_to_permute = bart_machine$covariates_to_permute
  }
  if (is.null(verbose)){
    verbose = FALSE
  }	
  build_bart_machine(X, y,
                     num_trees = num_trees, #found many times to not get better after this value... so let it be the default, it's faster too 
                     num_burn_in = bart_machine$num_burn_in, 
                     num_iterations_after_burn_in = bart_machine$num_iterations_after_burn_in, 
                     alpha = bart_machine$alpha,
                     beta = bart_machine$beta,
                     k = bart_machine$k,
                     q = bart_machine$q,
                     nu = bart_machine$nu,
                     prob_rule_class = bart_machine$prob_rule_class,
                     mh_prob_steps = bart_machine$mh_prob_steps, #only the first two matter
                     run_in_sample = run_in_sample,
                     s_sq_y =  bart_machine$s_sq_y, # "mse" or "var"
                     cov_prior_vec = cov_prior_vec,
                     use_missing_data = bart_machine$use_missing_data,
                     covariates_to_permute = covariates_to_permute, #PRIVATE
                     num_rand_samps_in_library = bart_machine$num_rand_samps_in_library, #give the user the option to make a bigger library of random samples of normals and inv-gammas
                     use_missing_data_dummies_as_covars = bart_machine$use_missing_data_dummies_as_covars,
                     replace_missing_data_with_x_j_bar = bart_machine$replace_missing_data_with_x_j_bar,
                     impute_missingness_with_rf_impute = bart_machine$impute_missingness_with_rf_impute,
                     impute_missingness_with_x_j_bar_for_lm = bart_machine$impute_missingness_with_x_j_bar_for_lm,
                     mem_cache_for_speed = bart_machine$mem_cache_for_speed,
                     serialize = FALSE, #we do not want to waste CPU time here since these are created internally by us
                     verbose = verbose)
}


my_investigate_var_importance = function(title="", bart_machine, type = "splits", plot = TRUE, num_replicates_for_avg = 5, num_trees_bottleneck = 20, num_var_plot = Inf, bottom_margin = 10){
  check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
  
  var_props = array(0, c(num_replicates_for_avg, bart_machine$p))
  for (i in 1 : num_replicates_for_avg){
    if (i == 1 & num_trees_bottleneck == bart_machine$num_trees){ ##if original BART is using right number of trees
      var_props[i, ] = get_var_props_over_chain(bart_machine, type)
    } else {
      bart_machine_dup = bart_machine_duplicate(bart_machine, num_trees = num_trees_bottleneck, run_in_sample = FALSE, verbose = FALSE)			
      var_props[i, ] = get_var_props_over_chain(bart_machine_dup, type)				
    }
    cat(".")
  }
  cat("\n")
  
  avg_var_props = colMeans(var_props)
  names(avg_var_props) = bart_machine$training_data_features_with_missing_features
  sd_var_props = apply(var_props, 2, sd)
  names(sd_var_props) = bart_machine$training_data_features_with_missing_features
  
  if (num_var_plot == Inf){
    num_var_plot = bart_machine$p
  }
  
  avg_var_props_sorted_indices = sort(avg_var_props, decreasing = TRUE, index.return = TRUE)$ix
  avg_var_props = avg_var_props[avg_var_props_sorted_indices][1 : num_var_plot]
  sd_var_props = sd_var_props[avg_var_props_sorted_indices][1 : num_var_plot]		
  
  if (plot){
    par(mar = c(bottom_margin, 5, 3, 0))#bottom_margin,6,3,0
    if (is.na(sd_var_props[1])){
      moe = 0
    } else {
      moe = 1.96 * sd_var_props / sqrt(num_replicates_for_avg)
    }
    
    names_vector <- names(avg_var_props)
    names_vector <-sub("DEWP", "Mean dew point temperature", names_vector)
    names_vector <-sub("TMAX", "Monthly mean maximum temperature", names_vector)
    names_vector <-sub("TMIN", "Monthly mean minimum temperature", names_vector)
    names_vector <-sub("TPCP", "Total precipitation in a month", names_vector)
    names_vector <-sub("WDSP", "Monthly mean wind speed", names_vector)
    
    
    assign("names_vector",names_vector,.GlobalEnv)
    assign("avg_var_props",avg_var_props,.GlobalEnv)
    assign("moe",moe,.GlobalEnv)
    assign("title",title,.GlobalEnv)
    
    end_point = 0.5 + length(names_vector) + length(names_vector)-1
    
    bars = barplot(avg_var_props, 
                   names.arg = names_vector,#names(avg_var_props), 
                   las = 3, 
                   #			xlab = "Predictor",
                   ylab = "Inclusion Proportion", 
                   main = title,
                   #			main = paste("Important Variables Averaged over", num_replicates_for_avg, "Replicates by", ifelse(type == "splits", "Number of Variable Splits", "Number of Trees")),
                   col = "gray",#rgb(0.39, 0.39, 0.59),
                   ylim = c(0, max(avg_var_props + moe)),
                   xaxt = "n",
                   space = 1
    )
    #text(cex=1, x=bars-1.25, y=-0.25, names_vector, xpd=TRUE, srt=45)
    text(seq(1.5,end_point,by=2), par("usr")[3]-0.01, 
         srt = 60, adj= 1, xpd = TRUE,
         labels = names_vector, cex=1)
    
    conf_upper = avg_var_props + 1.96 * sd_var_props / sqrt(num_replicates_for_avg)
    conf_lower = avg_var_props - 1.96 * sd_var_props / sqrt(num_replicates_for_avg)
    segments(bars, avg_var_props, bars, conf_upper, col = rgb(0.59, 0.39, 0.39), lwd = 3) # Draw error bars
    segments(bars, avg_var_props, bars, conf_lower, col = rgb(0.59, 0.39, 0.39), lwd = 3)
    par(mar = c(5.1, 4.1, 4.1, 2.1))
  }	
  invisible(list(avg_var_props = avg_var_props, sd_var_props = sd_var_props))	
}