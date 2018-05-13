create_output_files =
  function(
    results,
    output_file_ate,
    output_dir_ipo,
    verbose = FALSE) {
  # Write separate .csv files for:
  # (1) average treatment effects and confidence intervals for all data files, and
    write.csv(results$revere$ate_df, file = output_file_ate)
    
  # (2) individual potential outcomes for each individual data file, 
    newfiles = NULL
    for(file in results$cf_files){
      ufid = gsub("^.*/([^./]+?)\\_cf.csv$", "\\1", file, perl = TRUE)
      newfiles = c(newfiles, paste0("exports/submission/", ufid, ".csv"))
      write.csv(results$ipos[[ufid]], file = paste0("exports/submission/", ufid, ".csv"))
    }
  # Put these in a .zip file
    zip("exports/submission.zip", newfiles)
}