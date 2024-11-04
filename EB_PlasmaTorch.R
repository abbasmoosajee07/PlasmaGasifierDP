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
k = 1.38E+23          # Boltzman Constant
e = 1.60E-19          # Charge on Electron

# Energy capacity of Steam Plasma ------------------------------------------

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
Q_s <- avogadro * PlasTW_kgs / 18
I = GIE_eV
# Plasma Arc Torch ---------------------------------------------------------
PlasT_eff <- 0.9
V_W <-  15000
lambda <- 1.5
L <- Q_s * PlasT_Jmol/1000 * sqrt(I/(8*pi*k*PolyE_kW*1000*lambda))
EI  <- (PolyE_kW / PlasT_eff) * 1000
I_A <- EI / V_W

print(V_W)
print(I_A)
print(L/100)

# Given data
efficiencies <- seq(0.4, 1, by = 0.2)  # Range of efficiency values from 0.1 to 1
V_W_values <- seq(0, 2, by = 0.1/10) * 1000 # Creating a range of voltage values from 1 to 20

EVI_df <- data.frame()

# Loop through different efficiency values
for (efficiency in efficiencies) {
  # Calculating EI and I_A for each voltage value
  EI_values <- (PolyE_kW / efficiency) * 1000
  I_A_values <- EI_values / V_W_values
  
  # Create a data frame for the current efficiency
  efficiency_df <- data.frame(
    Efficiency = rep(efficiency, length(V_W_values)),
    Voltage = V_W_values,
    Current = I_A_values
  )
  
  # Append the data for the current efficiency to EVI_df
  EVI_df <- bind_rows(EVI_df, efficiency_df)
 }



EVI_plot <-
ggplot(EVI_df, aes(x = (Voltage)/1000, y = (Current)/1000, color = factor(Efficiency))) +
  geom_line() +
  labs(x = "Voltage (kV)", y = "Current (kA)", color = "Efficiency", title = "Identifying Optimum Voltage vS Current") +
  scale_color_discrete(name = "Efficiency") +
  PGDP_theme() + ylim(0,100)

print(EVI_plot)
ggsave(file.path( pic_folder, "EVI_plot.png"), EVI_plot, width = 170, height = 100, units = "mm")
