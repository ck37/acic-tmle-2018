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
  dir.create(output_dir_ipo)
  dir.create(file.path(output_dir_ipo, "submission"), showWarnings = FALSE)
  newfiles = NULL
  for (ufid in names(results$ipos)) {
    newfiles = c(newfiles, paste0(output_dir_ipo, "/submission/", ufid, ".csv"))
    write.csv(results$ipos[[ufid]], file = paste0(output_dir_ipo, "/submission/", ufid, ".csv"))
  }
  # Put these in a .zip file
  zip(paste0(output_dir_ipo, "/submission.zip"), newfiles)
}