# =============================================================================
# VesselWeight.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Estimating vessel weight based on the dimensions and vessel 
#   materials
#
# =============================================================================

ceramic_props <- data.frame(
  Material = c("TaB2", "TiB2", "ZrB2", "TiC", "ZrC", "TaC", "TaN", "TiN", "ZrN"),
  Crystal_structure = c("HCP", "HCP", "HCP", "FCC", "FCC", "FCC", "FCC", "FCC", "FCC"),
  Melting_temperature = c(3040, 3225, 3245, 3100, 3530, 3800, 2900, 2950, 2950),
  Density = c(12.5, 4.5, 6.1, 4.9, 6.6, 14.5, 13.4, 5.4, 7.3),
  CTE = c(8.5, 8.1, 6.9, 7.6, 6.82, 7.5, 3.2, 9.35, 7.24),
  Thermal_Cond = c(13, 64.4, 57.9, 19, 20.61, 22.2, 8.3, 29.1, 20.9),
  Electrical_Resist = c(33, 22.2, 9.2, 52.5, 68, 36, 131, 21.7, 13.6),
  Elastic_Modulus = c(551, 575, 489, 437, 387, 537, 490, 400, 384),
  Hardness = c(19.6, 24, 23, 30, 25, 17, 10.8, 18.6, 15)
)

# Weight of Fluid ---------------------------------------------------------
Fluidmass_kghr <- Plastic_kghr + sum(Output_composition$Mass_KGhr)
MassRT_kgs <- Fluidmass_kghr/3600 * Tr_s
CoolingWater_kg <- JktM_khgr
PlasmaTorch_kg <- PolyE_kW * 80/600
CeramicD_m <- PGD_m + (10E-3 * 2)
CeramicVom_m3 <- (0.25 * pi * PGH_m * (CeramicD_m^2 - PGD_m^2)) + (0.25*pi*CeramicD_m^2)
Ceramicrho_kgm3 <- ceramic_props[ceramic_props$Material == "TiB2", "Density"] * 1000
CeramicMass_kg <- CeramicVom_m3 * Ceramicrho_kgm3

GraphiteD_m <- PGD_m + (Wall_thick * 2)

GraphiteVom_m3 <- (0.25 * pi * PGH_m * (GraphiteD_m^2 - CeramicD_m^2)) + (0.25*pi*PGD_m^2)
Graphiterho_kgm3 <- 2.26 * 1000
GraphiteMass_kg <- GraphiteVom_m3 * Graphiterho_kgm3
MatMass_kg <- MassRT_kgs + CoolingWater_kg + GraphiteMass_kg + PlasmaTorch_kg + CeramicMass_kg
TotalMass_kg <- MatMass_kg * 1.15
VesselForce_N <- TotalMass_kg * 9.81
VesselMass <- data.frame(
  Components = MassRT_kgs,
  CoolingWater = CoolingWater_kg,
  PlasmaTorch = PlasmaTorch_kg,
  CeramicMass = CeramicMass_kg,
  GraphiteMass = GraphiteMass_kg,
  TotalMass = TotalMass_kg
)
print(c(TotalMass_kg,VesselForce_N)/1000)
