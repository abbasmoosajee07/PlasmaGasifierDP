# =============================================================================
# StartUp.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Loads all required libraries, and runs all the necessary files 
# required for the project
#
# =============================================================================

rm(list=ls()) # Clear the memory

# Install and Load required packages -------------------------------------------

# List of packages to install
packages_used <- c('ggplot2','corrplot','dplyr','tidyr','tidytext',
                   'ggalt','ggrepel','scales','forecast',"deSolve",
                   'stringr','stringdist','scales','tm',"kableExtra",
                   'tidyverse',"GiRaF","ggiraph","reticulate","pracma",
                   "rlang", "Ryacas", "directlabels","tibble","magick",
                   "reshape2")
# Check if each package is already installed before attempting to install it
for (package in packages_used) {
  if (!(package %in% installed.packages()[,"Package"])) {
    # Package is not installed, so install it
    install.packages(package)
  } else {
    # Package is already installed, print a message
    cat(paste("Package", package, "is already installed.\n"))
  }
}

# Load packages
for (package in packages_used) {
  library(package, character.only = TRUE)
}

# Count Lines of Code in Scripts ------------------------------------------

count_lines_in_directory <- function(directory_path) {
  # List all .R files in the directory
  script_files <- list.files(directory_path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  
  # Exclude files within packages
  script_files <- script_files[!grepl("/renv/", script_files)]
  
  # Initialize a variable to store the total lines of code
  total_lines <- 0
  n <- 0
  # Loop through each script file and count its lines of code
  for (file in script_files) {
    lines <- length(readLines(file))
    total_lines <- total_lines + lines
    n <- n + 1
    cat(n , "File:", basename(file), "| Lines of code:", lines, "\n")
  }
  
  # Output the total lines of code
  cat("Total lines of code in directory:", total_lines, "\n")
}

# Call the function with the directory path
directory_path <- "C:/Users/User/Documents/Univeristy Coursework/Local Design Project/PlasmaGasifier-DP"
count_lines_in_directory(directory_path)

# Run Project Scripts ----------------------------------------------------------
pic_folder <- "~/Univeristy Coursework/Local Design Project/PlasmaGasifier-DP/ReportFigs"

# Get the list of R scripts in the project directory
script_files <- c("Functions-DPAK2.R", "PlasmaGasifier.R",
                  "Element_data.R", "PolymerData.R",
                  "StateVariable_Calcs.R","Unit_Funcs.R",
                  "InitialInfo.R", "EB_Polymer.R",
                  "Reactions.R", "EquilibriumK_eqs.R",
                  "conv_calc.R", "ModelPumps_Data.R", 
                  "PostPG_Calc.R","PG_Sizing.R",
                  "CoolingJacket.R","Pipe_Props.R",
                  "Finance.R","Economic_Analysis.R","Fin_Scen.R")

run_scripts <- function(script_files){
  # Run each R script
  for (script_file in script_files) {
    source(script_file, chdir = TRUE)
  }
  
  # Optionally, you can print a message to indicate that all scripts have been run
  cat("All scripts in the project have been executed.\n")
}
run_scripts(script_files)

