#' This should evaluate the accuracy of our estimates on the known counterfactuals.
evaluate_estimates = function(results, verbose = FALSE) {
  # Loop over the counterfactual files.
  files = results$cf_files
  
  stats = data.frame()
  
  for (file in files) { 
    # ufid = filename without the enclosing directory.
    filename = gsub("^.*/([^./]+?)\\.csv$", "\\1", file, perl = TRUE)
    ufid = gsub("^([^_]+?)_cf$", "\\1", filename, perl = TRUE)
    
    if (verbose) {
      cat("Processing", ufid, "counterfactual file.\n")
    }
    
    # Import cf file
    # Columns: sample_id, y0, y1
    # V1 appears to be an unncessary rownames column
    cf = rio::import(file)
    
    cf_ate = mean(cf$y1) - mean(cf$y0)
    
    our_estimate = results$ate[results$ate$ufid == ufid, , drop = FALSE]
  
    # Evaluate our CI's coverage on the true population parameter.
    ci_covers_true_ate = with(our_estimate, as.numeric(cf_ate <= ri & cf_ate >= li))
    
    # MSE of our estimate.
    est_mse = (our_estimate$effect_size - cf_ate)^2
    
    # TODO: process individual potential outcome estimates.
    
    # Evaluate MSE on the individual potential outcomes.
    # Save result.
    result =
      list(ufid = ufid,
           mse = est_mse,
           ci_covers_true_ate = ci_covers_true_ate)
    
    # Compile into a dataframe.
    stats = rbind.data.frame(stats, result, stringsAsFactors = FALSE)
  }
  
  if (verbose) {
    cat("Coverage:", paste0(round(mean(stats$ci_covers_true_ate) * 100, 1), "%"),
        "Average MSE:", round(mean(stats$mse), 3), "\n")
  }
  
  # Compile and return results.
  # TODO: possibly return more info.
  return(stats)
}