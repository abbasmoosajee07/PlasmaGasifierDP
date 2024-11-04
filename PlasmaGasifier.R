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

# Endothermic Reaction, +dH, absorbs heat
# Exothermic Reaction,  -dH, releases heat

# Production Capacity ---------------------------------------------------------
Opr_days <- 330           # Annual Operation
Daily_hrs <- 24           # 24/7 Daily Operation
Ex_Cap <- 1.1             # Excess capacity (taken to be a multiplier)
Basis_hrs <- Opr_days * Daily_hrs

# Reactor Settings
Reac_temp <- 2750         # Reactor temperature in Kelvin
Gas_out_temp <- Reac_temp # Gas outlet temperature, same as reactor temperature

# Volume and Pressure
Vr <- 50                  # Volume of the reactor in cubic meters
PG_Press <- 4E+6          # Pressure of the gas (assumed to be in Pascals)
SynG_PI <- PG_Press       # Synthesis gas pressure, same as the gas pressure

# Storage and Environmental Conditions
WaterStorageTemp <- 288   # Temperature of water storage in Kelvin
Room_temp <- 288          # Room temperature in Kelvin



