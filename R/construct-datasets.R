# This pull the file load and merging part out of run_analysis()
# TODO: Need to update run_analysis() to remove the file load/merging part,
# and operate on the output of construct_datasets().
# This will facilitate analysis of our own simulated data.
construct_datasets =
  function(
    input_dir_counterfactuals = "data-raw/practice-censoring",
    # Inputs are all *.csvs that don't end in _cf.csv
    # This is a regex with a negative lookbehind.
    input_pattern = "(?<!_cf)\\.csv$",
    input_cf_pattern = "_cf.csv$",
    input_file_covariates = "data-raw/x.csv",
    id_field = "sample_id",
    outcome_field = "y",
    treatment_field = "z",
    verbose = TRUE)
{
  
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
  
  # Looping over all "factual" files.
  cat("\nBegin importing factual datasets.\n")
  
  # for (file in files) {
  file_results = lapply(files, FUN = function(file) {
    # ufid = filename without the enclosing directory.
    ufid = gsub("^.*/([^./]+?)\\.csv$", "\\1", file, perl = TRUE)
    
    if (verbose) {
      cat("Importing", ufid, "file", which(file == files), "of", length(files), "\n")
    }
    
    # Import one counterfactual file
    # Column names are sample_id, z (treatment), and y (outcome)
    full_file = paste0(here::here(), "/", file)
    if (file.exists(full_file)) {
      cf = rio::import(full_file)
    } else {
      stop(paste("Can't find file:", full_file, "\nPwd:", getwd()))
    }
    
    # Merge into single dataset
    data = merge(cf, covariate_df, by.x = id_field, by.y = id_field,
                 # Keep all rows in CF, but ok if we drop rows in covariates.
                 all.x = TRUE, all.y = FALSE)
    
    # Compile results to return
    result = list(
      "ufid" = ufid,
      "data" = data,
      "id_field" = id_field
    )
    
    result
  })
  
  # Extract $ufid and set as the list element's name.
  names(file_results) = sapply(file_results, `[[`, "ufid")
  
  return(file_results)
}
