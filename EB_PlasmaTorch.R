# =============================================================================
# EB_PlasmaTorch.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: This script aims to do all the necessary design calculations for 
# the plasma arc torch in the plasma gasifier
#
# =============================================================================

source("EB_Reaction.R") # Calculate Total Energy Requirements
PolyE_Js <- PolyE_Jhr / 3600




# Empirical Constants ------------------------------------------------------
eV <- 6.242e+18       # 1 electronvolt
avogadro <- 6.023E+23 # Avogadro's Number
c = 299792458         # Speed of light

# Energy capacity of Steam Plasma ------------------------------------------
PlasT_eff <- 0.9

mols <- 1                             # Molar Basis
mol_part <- avogadro * mols           # Number of particles (molecules)
GIE_eV   <- 12.6                      # Ionisation Energy per molecule of gas
GeV_Jmol <- (mol_part * GIE_eV) / eV  # Total Energy Required to ionise one mol of Gas

PlasTCp_Jmol <- state_func(main_elements$Water, # Enthalpy of Heating Gas to Ionisation Temp 
                           425, 3500)$Enthalpy
PlasT_Jmol <- PlasTCp_Jmol + GeV_Jmol # Total Energy required to heat and ionise one mol of steam

PlasT_Jkg  <- PlasT_Jmol * # Energy required to heat ad ionise one kg of steam
              element_props[element_props$Component == "Water", "Molar_Mass"]

PlasTW_kgs <- (PolyE_Js/PlasT_Jkg)
PlasTW_kghr <- PlasTW_kgs * 3600

# Plasma Arc Torch ---------------------------------------------------------
V_W <-  10
EI  <- (PolyE_kW / PlasT_eff) * 1000
I_A <- EI / V_W

print(PlasT_Jkg/1E+6)
print(PlasTW_kgs)
print(PlasTW_kghr)

