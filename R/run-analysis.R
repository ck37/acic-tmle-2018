#' TODO: document function
run_analysis =
  function(
    input_dir_counterfactuals = "data-raw/practice-censoring",
    specific_files = NULL,
    # Inputs are all *.csvs that don't end in _cf.csv
    # This is a regex with a negative lookbehind.
    input_pattern = "(?<!_cf)\\.csv$",
    input_cf_pattern = "_cf.csv$",
    input_file_covariates = "data-raw/x.csv",
    id_field = "sample_id",
    outcome_field = "y",
    treatment_field = "z",
    tmle_wrapper = NULL,
    prescreen = TRUE,
    verbose = TRUE) {
  
  # Check that input_dir_counterfactuals exists
  if (!dir.exists(input_dir_counterfactuals)) {
    stop(paste("Counterfactual directory not found:", input_dir_counterfactuals))
  }
  
  # Import counterfactuals; this includes the real files and possibly
  # the true counteractual files.
  all_files = list.files(path = input_dir_counterfactuals, full.names = TRUE)
  
  # Now apply perl regex, selecting only files that don't end in _cf.csv.
  files = all_files[grepl(input_pattern, all_files, perl = TRUE)]
  if (verbose) {
    cat("Found", length(files), "files to process.\n")
  }
  
  # The _cf.csv files are for evaluating our estimates at the population & individual levels.
  cf_files = all_files[grepl(input_cf_pattern, all_files, perl = TRUE)]
  if (verbose) {
    cat("Found", length(cf_files), "true counterfactual files for evaluation.\n")
  }
  
  # Check that input_file_covariates exists.
  if (!file.exists(input_file_covariates)) {
    stop(paste("Covariate file not found:", input_file_covariates))
  }
  
  # Import covariate dataset
  covariate_df = rio::import(input_file_covariates)
  
  # Process covariate file. Should return a list with at least $covariates and $id.
  # This function is defined in R/process-covariates.R
  results = process_covariates(covariate_df, verbose = verbose)
  covariate_df = results$data
  id = results$id
  
  # Dataframe to save ATE estimates for each file.
  ate_df = NULL
  # List of individual potential outcome estimates for each file.
  ipo_list = list()
  
  # Looping over all "factual" files.
  cat("\nBegin processing factual datasets.\n")
  for (file in files) {
    # Measure the execution time needed to analyze each file.
    time_start = proc.time()
    
    # ufid = filename without the enclosing directory.
    ufid = gsub("^.*/([^./]+?)\\.csv$", "\\1", file, perl = TRUE)
    
    # Potentially limit to a set of ufids that were specified.
    if (!is.null(specific_files)) {
      if (!ufid %in% specific_files) {
        if (verbose) {
          cat("Skipping", ufid, "- not one of the specific files.\n")
        }
        next
      }
    }
    
    if (verbose) {
      cat("Processing dataset", ufid, "file", which(file == files), "of", length(files), "\n")
    }
    
    # Import one counterfactual file
    # Column names are sample_id, z (treatment), and y (outcome)
    cf = rio::import(file)
    
    # Merge into single dataset
    data = merge(cf, covariate_df, by.x = id_field, by.y = id_field,
                 # Keep all rows in CF, but ok if we drop rows in covariates.
                 all.x = TRUE, all.y = FALSE)
    
    analysis_data = data
    
    # Remove the sample_id from the data that we analyze.
    analysis_data[[id_field]] = NULL
    
    if (verbose) {
      cat("Observations:", prettyNum(nrow(analysis_data), big.mark = ","),
          "Treated pct:",
          paste0(round(mean(analysis_data[[treatment_field]], na.rm = TRUE) * 100, 1), "%"),
          "Censored pct:",
          paste0(round(mean(is.na(analysis_data[[outcome_field]])) * 100, 1), "%"),
          "\n")
    }
    
    # Run TMLE analysis.
    # Should return population ATE with inference, plus df of individual potential outcomes.
    # This function is defined in R/estimate-ate.R
    tmle_result = estimate_ate(analysis_data,
                               outcome_field = outcome_field,
                               treatment_field = treatment_field,
                               id_field = id_field,
                               tmle_wrapper = tmle_wrapper,
                               prescreen = prescreen,
                               verbose = verbose)
    
    # Put sample_id back in the ipo data frame (required in exported file)
    tmle_result$ipo_df = cbind(data[[id_field]], tmle_result$ipo_df)
    names(tmle_result$ipo_df)[1] = "sample_id"
    
    # Put estimates into a list for rbinding into a dataframe.
    ate_result = cbind.data.frame(ufid = ufid, 
                            # Population ATE.
                            effect_size = tmle_result$ate_est,
                            # Left confidence interval.
                            li = tmle_result$ci_left,
                            # Right confidence interval.
                            ri = tmle_result$ci_right
                            )
    
    # Integrate into dataframe for the ATEs.
    ate_df = rbind.data.frame(ate_df, ate_result, stringsAsFactors = FALSE)
    
    # Save individual potential outcome result.
    ipo_list[[ufid]] = tmle_result$ipo_df
    
    time_end = proc.time()
    time_elapsed = (time_end - time_start)
    cat("Time elapsed:", round(time_elapsed["elapsed"] / 60, 1L), "minutes.\n\n")
  }
  
  # Compile results.
  results = list(
    # ATE df includes the point estimate and confidence interval
    ate = ate_df,
    # Each list element is a dataframe of potential outcomes for a given file.
    ipos = ipo_list,
    # Set of counterfactual files that can be used for evaluation.
    cf_files = cf_files
  )
  
  return(results)
}