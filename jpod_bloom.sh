#!/bin/bash

#SBATCH --job-name=retrieve_bloom_from_jpod
#SBATCH --cpus-per-task=8
#SBATCH --mem=2G

#SBATCH --time=01:30:00
#SBATCH --qos=6hours

#SBATCH --output=retrieve_bloom_from_jpod_logs
#SBATCH --error=retrieve_bloom_from_jpod_errors

module load R
Rscript "/scicore/home/weder/nigmat01/jobs_disruptive_tech/retrieve_bloom.R"