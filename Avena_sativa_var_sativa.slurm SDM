#!/bin/bash
#SBATCH --job-name=Current_Avena_sativa_var_sativa # Name for job
#SBATCH --partition=shared
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --nodes=1
#SBATCH --mem=100G
##SBATCH --core-spec=0 #Uncomment to allow jobs to request all cores on a node
#SBATCH -t 4320
#SBATCH -e /home/suhailak/kantar_koastore/suhaila/cover_crop_project/Scripts/%x-%A_%a.err # Standard err goes to this file
#SBATCH --mail-user suhailak@hawaii.edu # email wish to be notified at
#SBATCH --mail-type ALL #events for email. ALL will alert you of job beginning/completion/failure

module load lang/R/4.2.1-foss-2022a
#Rscript HPC_network_analysis.R
Rscript Avena_sativa_var_sativa.R
