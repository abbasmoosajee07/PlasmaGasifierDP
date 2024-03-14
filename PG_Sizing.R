# =============================================================================
# PG_Sizing.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Main design calculations for the Plasma Gasifier 
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

Vr <- 50 # Volume of Reactor
PolyE_Jhr <- PolyE_Jhr # Energy required for plasmolysis of plastic
FCR_kghr <- Plastic_kghr
GratD_m   <- 2.5
GratAr_m2 <- 0.25 * pi * GratD_m^2
SGR_kgm2hr<- FCR_kghr / GratAr_m2


PGD_m <- ((1.27*FCR_kghr)/SGR_kgm2hr)^0.5
PGH_m <- calc_ReacHeight(PGD_m, Vr)

PlasT_s <- sqrt((2*PGH_m)/gravity)
Tr_s = Vr /(sum(Output_composition$Vol_m3hr) / 3600)

# Hopper Storage ----------------------------------------------------------
HopperV_hr <- 3
HopperV_m3 <- HopperV_hr * PlasticVol_m3hr
HopperH_m  <- HopperV_m3 / (0.7 * PGD_m^2)

# Gas Injection -----------------------------------------------------------
nozzles <- 4
NoRows  <- 1
NozCols <- nozzles/NoRows

Inlet_gascomp <- data.frame(
  Component = c("Water","Nitrogen","Oxygen"),
  Mass_KGhr  = c(H2O_feed,N2_feed,O2_feed),
  Mol_kmolhr = c(0,0,0),
  Vol_m3hr = c(0,0,0),
  dH_Jhr = c(0,0,0)
)
IGV_m3hr <- Composition_props(Inlet_gascomp[Inlet_gascomp$Component =="Water",],4E+5,698)$Vol_m3hr +
  Composition_props(Inlet_gascomp[Inlet_gascomp$Component =="Nitrogen",],4E+5,298)$Vol_m3hr +
  Composition_props(Inlet_gascomp[Inlet_gascomp$Component =="Oxygen",],4E+5,298)$Vol_m3hr

AFR_kghr <- sum(Inlet_gascomp$Mass_KGhr)
IGD_m <- sqrt((4*AFR_kghr)/(pi*nozzles*IGV_m3hr))
IGV_ms <- (IGV_m3hr/3600)/(0.25*pi*IGD_m^2)
PGCirc_m <- pi * PGD_m
NozSpace <- ((PGCirc_m - (NozCols*IGD_m))/NozCols)

Nozzle_Prop <- data.frame(
  NozzleNo = nozzles,
  NozRows = NoRows,
  NozCols = NozCols,
  MinH_m = 0.7 * PGD_m,
  MaxH_m = 1.4 * PGD_m,
  NozzleD_m = IGD_m,
  NozSpace_m = NozSpace,
  InletV_ms = IGV_ms
)
print(t(Nozzle_Prop))
# Height, Diameter Calculations -----------------------------------------------
# Generate a range of diameters
diameters <- seq(1, 7, by = 0.1)
volumes <- c(50, 100, 150, 200)
VDH_df <- data.frame(Diameter = rep(diameters, each = length(volumes)),
                   Volume = rep(volumes, length(diameters)))

VDH_df$Height <- calc_ReacHeight(VDH_df$Diameter, VDH_df$Volume)

VolDH_Plot <- ggplot() +
  geom_line(VDH_df, mapping = aes(x = Diameter, y = Height, color = factor(Volume))) +
  geom_point() +
  labs(x = "Diameter",
       y = "Height",
       color = "Volume",
       title = "Optimum Diameter to height Ratio") +
  PGDP_theme()
print(VolDH_Plot)

# ggsave(file.path(pic_folder, "VolDH.png"), VolDH_Plot, width = 120, height = 80, units = "mm")

# Wall Thickness ----------------------------------------------------------

Di <- PGD_m
Pi <- SynG_PI
S  <- 575E+9
Wall_thick <- (Pi*Di)/(2*S-1.2*Pi)   # Wall thickness in m

# Output Gasifier Properties ----------------------------------------------

Gasifier_Prop <- data.frame(
  Temp_K = Reac_temp,
  Press_MPa = SynG_PI/1E+6,
  FCR_kghr = sum(Input_composition$Mass_KGhr),
  SGR_kgm2hr = SGR_kgm2hr,
  InputGas = O2_feed, 
  PolyE_MJhr  = PolyE_Jhr/1E+6,
  PlasFallT_s = PlasT_s,
  ResidenceTime_s = Tr_s,
  GasifierVol_m3 = Vr,
  Diameter_m = PGD_m,
  Height_m = PGH_m
)

print(t(Gasifier_Prop))
# view_table(t(Gasifier_Prop),4)
# view_table(t(Nozzle_Prop),4)
# view_table(t(Gasifier_Prop),4,"latex")

