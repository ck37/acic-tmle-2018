#' TODO: document function
run_analysis = function(
  input_dir_counterfactuals = "data-raw/practice-censoring",
  # Inputs are all *.csvs that don't end in _cf.csv
  # This is a regex with a negative lookbehind.
  input_pattern = "(?<!_cf)\\.csv$",
  input_cf_pattern = "_cf.csv$",
  input_file_covariates = "data-raw/x.csv",
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
  
  # The _cf.csv files are for checking our predictions
  cf_files = all_files[grepl(input_cf_pattern, all_files, perl = TRUE)]
  if (verbose) {
    cat("Found", length(cf_files), "true counterfactual files for evaluation.")
  }
  
  # Check that input_file_covariates exists
  if (!file.exists(input_file_covariates)) {
    stop(paste("Counterfactual directory not found:", input_file_covariates))
  }
  
  # Import covariate dataset
  covariates = rio::import(input_file_covariates)
  
  # Looping over all counterfactual files
  # Data setup
  # Import one counterfactual file
  # Merge counterfactuals with the covariate dataset based on sample_ID
  # Run TMLE analysis
  
  # Return results
  # - ATE, CI
  # - Individual potential outcomes dataframe
  # Counterfactul dataframe
  
}