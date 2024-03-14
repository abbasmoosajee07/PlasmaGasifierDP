# =============================================================================
# Plasma_Gasifier.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Define all process constants and variables in the Plasma Gasifier 
# Node
#
# =============================================================================

# options(scipen = 0)    # Turn scientific numbering on
# options(scipen = 999)  # Turn scientific numbering off


hrtos <- 3600
Elec_Cost <- 0.270 
Ex_Cap <- 1.1 # Excess Capacity

Reac_temp <- 2750
Gas_out_temp  <- Reac_temp

PG_Press <- 4E+6 
SynG_PI  <- PG_Press
  
WaterStorageTemp <- 288
MaxWaterTemp <- 698
SlagTemp  <- Reac_temp

Room_temp <- 288


FandP_Pa  <- 10E+5

HEX1_Out  <- Gas_out_temp-1014

HEX2_Out  <- HEX1_Out - 1014

