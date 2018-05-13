create_output_files =
  function(
    results,
    output_file_ate,
    output_dir_ipo,
    verbose = FALSE) {
  # TODO: add verbose output
    
  # Write separate .csv files for:
  # (1) average treatment effects and confidence intervals for all data files
  write.csv(results$ate, file = output_file_ate)
    
  # (2) individual potential outcomes for each individual data file
  dir.create(output_dir_ipo, showWarnings = FALSE)
  newfiles = NULL
  for (ufid in names(results$ipos)) {
    newfiles = c(newfiles, paste0(ufid, ".csv"))
    write.csv(results$ipos[[ufid]], file = paste0(output_dir_ipo, "/", ufid, ".csv"))
  }
  # Put these in a .zip file
  temp_wd = getwd()
  setwd(output_dir_ipo)
  zip("submission.zip", newfiles)
  setwd(temp_wd)
  rm(temp_wd)
}