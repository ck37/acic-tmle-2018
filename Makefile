
########################################
# General setup

# Directory where sbatch-r.sh, sbatch-rmd.sh, etc. can be found.
SCRIPT_DIR=scripts

# Directory to store command results; set to "." to be current directory.
OUTPUT_DIR=output

# How do we want to run tasks? Can be slurm or bash currently.
# Use SLURM if possible, otherwise use bash.
# Can override if desired: "export JOB_ENGINE=shell"
ifndef JOB_ENGINE
  # Detect if we can use slurm, otherwise use shell.
  HAS_SBATCH := $(shell sbatch --version 2>/dev/null)
  ifndef HAS_SBATCH
		JOB_ENGINE=shell
	else
		JOB_ENGINE=slurm
	endif
	# TODO: check for SGE.
endif

# NOTE: could use env variables in bashrc to avoid much of this logic.
HPC_SYSTEM = unknown
ifeq (brc,$(findstring brc,$(shell uname -n)))
	HPC_SYSTEM = savio
endif
#ifeq ($(findstring bridges,$(uname -n)),bridges)
ifeq ($(findstring bridges,$(shell uname -n)),bridges)
	HPC_SYSTEM = bridges
endif
# TODO: get comet working
# hpc_system="comet"
$(info Detected hpc system: ${HPC_SYSTEM})

########################################
# Savio configuration.

# This allows us to use environmental variables to override this default.
# e.g. we run in BASH: "export SBATCH_ACCOUNT=co_otheraccount"
ifndef SBATCH_ACCOUNT
 	# BRC version
	SBATCH_ACCOUNT=co_biostat
	# Account is not used on Comet.
endif

# This allows us to override the default QOS by setting an environmental variable.
# e.g. we run in BASH: "export SBATCH_QOS=biostat_normal"
ifndef SBATCH_QOS
	# Choose one QOS and comment out the other, or use environmental variables.
	# Not used on Comet.
	SBATCH_QOS=biostat_savio2_normal
	#SBATCH_QOS=savio_lowprio
endif

# Partition is only used on Savio.
# See http://research-it.berkeley.edu/services/high-performance-computing/user-guide/savio-user-guide#Scheduler
ifndef SAVIO_PARTITION
  SAVIO_PARTITION=savio2
endif

########################################
# Execution engines.

SBATCH = sbatch

# BRC uses account and QOS but Comet does not.
ifeq (${HPC_SYSTEM}, savio)
  # Specify account, QOS, and partition.
	SBATCH := ${SBATCH} -A ${SBATCH_ACCOUNT} --qos ${SBATCH_QOS} --partition ${SAVIO_PARTITION}
endif
ifeq (${HPC_SYSTEM},bridges)
	# Comet & Bridges want us to specify tasks per node I think.
	SBATCH := ${SBATCH} --ntasks-per-node=14
endif
ifeq (${HPC_SYSTEM}, comet)
	# Comet & Bridges want us to specify tasks per node I think.
	SBATCH := ${SBATCH} --ntasks-per-node=24
endif

# Setup R to run commands in the background and keep running after logout.
# This is the shell (non-SLURM) execution option.
R=nohup nice -n 19 R CMD BATCH --no-restore --no-save

########################################
# Misc

# Location of the sbatch script for R or Rmd files.
SBATCH_R_RMD=${SCRIPT_DIR}/sbatch-r-rmd.sh

########################################
# Tasks that can be run.

# Example job:
#data-prep: 1-data-prep.Rmd
#	${SBATCH} --nodes 1 --job-name=$< ${SBATCH_R_RMD} --file=$< --dir=${OUTPUT_DIR}


# Install necessary packages; only needs to be run once per machine.
# (OLD - IGNORE)
setup: setup.R
ifeq (${JOB_ENGINE},slurm)
	${SBATCH} --nodes 1 --job-name=$< ${SBATCH_R_RMD} --file=$< --dir=${OUTPUT_DIR}
else
	${R} $< ${OUTPUT_DIR}/$<.out &
endif

analyze: run-analysis.Rmd
ifeq (${JOB_ENGINE},slurm)
	${SBATCH} --nodes 1 --job-name=$< ${SBATCH_R_RMD} --file=$< --dir=${OUTPUT_DIR}
else
	${R} $< ${OUTPUT_DIR}/$<.out &
endif

revere: savio-revere.R
ifeq (${JOB_ENGINE},slurm)
	${SBATCH} --nodes 20 --job-name=$< ${SBATCH_R_RMD} --file=$< --dir=${OUTPUT_DIR}
else
	${R} $< ${OUTPUT_DIR}/$<.out &
endif

drtmle: savio-drtmle.R
ifeq (${JOB_ENGINE},slurm)
  # 1 co_biostat node is being used, temporarily reduce to 19 (from 20 max)
	${SBATCH} --nodes 19 --job-name=$< ${SBATCH_R_RMD} --file=$< --dir=${OUTPUT_DIR}
else
	${R} $< ${OUTPUT_DIR}/$<.out &
endif

final: savio-final.R
ifeq (${JOB_ENGINE},slurm)
  # 1 co_biostat node is being used, temporarily reduce to 19 (from 20 max)
	${SBATCH} --nodes 19 --job-name=$< ${SBATCH_R_RMD} --file=$< --dir=${OUTPUT_DIR}
else
	${R} $< ${OUTPUT_DIR}/$<.out &
endif

final-tmle: final-tmle.R
ifeq (${JOB_ENGINE},slurm)
  # 1 co_biostat node is being used, temporarily reduce to 19 (from 20 max)
	${SBATCH} --nodes 9 --job-name=$< ${SBATCH_R_RMD} --file=$< --dir=${OUTPUT_DIR}
else
	${R} $< ${OUTPUT_DIR}/$<.out &
endif

final-revere: final-revere.R
ifeq (${JOB_ENGINE},slurm)
  # 1 co_biostat node is being used, temporarily reduce to 19 (from 20 max)
	${SBATCH} --nodes 19 --job-name=$< ${SBATCH_R_RMD} --file=$< --dir=${OUTPUT_DIR}
else
	${R} $< ${OUTPUT_DIR}/$<.out &
endif




# Start a bash session with 2 nodes, for up to 12 hours.
# TODO: support non-Savio execution.
bash:
	srun -A ${SBATCH_ACCOUNT} -p ${SAVIO_PARTITION} -N 2 -t 720 --pty bash

# Next line ensures that this rule works even if there's a file named "clean".
.PHONY : clean
clean:
	rm -f *.Rout
	rm -f slurm*.out
	rm -f install*.out
	rm -f cache/*
