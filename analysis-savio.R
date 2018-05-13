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
print("Test")

#####################################
## @knitr analysis_revere_glm


#####################################
## @knitr eval_revere_glm

#####################################
## @knitr create_output_files