#####################################
## @knitr setup

source("R/function_library.R")

conf = list(
  # Maximum amount of memory to allow rJava heap to use for bartMachine.
  # Defaults to 16g but use options(java.mem = "20g") to change.
  java_mem = getOption("java.mem", "16g")
)

# Load/install necessary packages.
startup(auto_install = FALSE, verbose = TRUE, java_mem = conf$java_mem)
# Load all .R files in R/
ck37r::load_all_code("R")

options(sl3.verbose = TRUE)


#####################################
## @knitr setup_savio

(conf = c(conf, list(
  input_dir_counterfactuals = "data-raw/practice-censoring",
  input_file_covariates = "data-raw/x.csv",
  output_file_ate = "exports/ate-test.csv",
  # IPO = Individual potential outcomes
  output_dir_ipo = "exports/ipo-test"
)))

results = list()

# Setup multi-node parallelization
nodes = strsplit(Sys.getenv("SLURM_NODELIST"), ",")[[1]] 
cat("Nodes:", paste(nodes, collapse = ", "), "\n")
#plan(list(tweak(cluster, workers = nodes), multiprocess))
plan(list(tweak(future::cluster, workers = nodes, revtunnel = FALSE)))

#####################################
## @knitr analysis_revere_glm

results$revere_glm =
  run_analysis(
    input_dir_counterfactuals = conf$input_dir_counterfactuals,
    input_file_covariates = conf$input_file_covariates,
    tmle_wrapper = wrapper_revere_glm,
    verbose = TRUE)

#####################################
## @knitr eval_revere_glm

if (exists("results") && "revere_glm" %in% names(results) &&
    "cf_files" %in% names(results$revere_glm) && length(results$revere_glm$cf_files) > 0L) {
  stats = evaluate_estimates(results$revere_glm, verbose = TRUE)
  print(stats)
  save(results, stats,
       file = "data/results-stats-revere-glm.RData")
}

#####################################
## @knitr create_output_files
