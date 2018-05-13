create_output_files =
  function(
    results,
    output_file_ate,
    output_dir_ipo,
    verbose = FALSE) {
  # TODO: add verbose output
    
  # Write separate .csv files for:
  # (1) average treatment effects and confidence intervals for all data files
  write.csv(results$ate_df, file = output_file_ate)
    
  # (2) individual potential outcomes for each individual data file, 
  # TODO: this should use the output_dir_ipo function argument.
  dir.create(file.path("exports", "submission"), showWarnings = FALSE)
  newfiles = NULL
  # TODO: should not be using $cf_files, doesn't exist for the real data.
  for (file in results$cf_files) {
    ufid = gsub("^.*/([^./]+?)\\_cf.csv$", "\\1", file, perl = TRUE)
    newfiles = c(newfiles, paste0(output_dir_ipo, "/", ufid, ".csv"))
    write.csv(results$ipos[[ufid]], file = paste0(output_dir_ipo, "/", ufid, ".csv"))
  }
  # Put these in a .zip file
  zip("exports/submission.zip", newfiles)
}