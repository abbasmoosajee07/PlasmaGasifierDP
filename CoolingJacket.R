# =============================================================================
# CoolingJacket.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Calculating required properties for the cooling jacket to control
# gasifier temperature
#
# =============================================================================

# Physical Data for Water from NIST Webbook
RMM <- 18.0315                # Molar Mass of Water
Pressure <- 101.325           # Pressure in kPa, 1atm

H_WaterT <- 50                # Hot Water Temp= 50C[323.15K]
W_Density <- 988.03           # Density of Water at 20C in kg m^-3
W_DViscosity <- 0.00054652    # Dynamic Viscosity of Water at 50C in Pa s
W_IE <- 3.7709                # Internal Energy (kJ/mol)
W_Ethlpy <- 3.7727            # Enthalpy (kJ/mol)    
W_Entrpy <- 12.679            # Entropy (J/mol*K)    
W_k <- 0.64                   # Conductivity of Water
W_Cp <- 4.18E+3               # Heat Capacity in J kg^-1

# Apparatus properties
Ou_TD <- PGD_m + 0.1             # Outer Tank Diameter
In_TD <- PGD_m             # Inner Tank Diameter
Ou_s <- pi * Ou_TD * PGH_m     # Outer Surface Area of tank

T_h <- Reac_temp                    
T_s <- Room_temp
T_base <- T_conv <- T_rad <- Reac_temp


# Heat Source Properties

# Jacket Properties
JktQ_m3hr <- 1
JktM_khgr <- JktQ_m3hr * Gen_fluid[Gen_fluid$Fluid == "Water", "rho_kgm3"]
Jk_thk <- 100E-03                  # Jacket thickness
Jk_d <- PGD_m + (Wall_thick + 100E-3)*2               # Diameter of Jacket
Jk_h <- PGH_m - (1.4 * PGD_m)      # Jacket of height
Jk_V <- 0.25 * pi *(Jk_d^2 - Ou_TD^2) * Jk_h

JktQ_m3s  <- JktQ_m3hr/3600
Jk_u <-  JktQ_m3s/Jk_V
Jk_hd <- Jk_d - Ou_TD

W_Pr <- W_Cp * W_DViscosity / W_k           # Water Prandtl Number
Re_jkt <- (W_Density * Jk_u * Jk_hd)/ W_DViscosity
W_Nu <- 0.87 * Re_jkt^(0.8) * W_Pr^(0.33)  # Water Nusselt
H_h <- (W_Nu * W_k) / Jk_d
Thermal_Res <- 100E-3/64.4 + 2000E-3/1600 #+Jk_thk / 15
U_h <- ((1 / (H_h)) + (Thermal_Res))^-1 # U from Jacket
A_h <- pi * Jk_d * Jk_h    # Area from Jacket
Ch_len <- Jk_thk
Biot <- (U_h * Ch_len) / W_k

# Convectional heat loss
h_s <- 1.41 * ((T_h-T_s) / Jk_h)^0.25 # McAdams formula
U_scv <- (1 / h_s)^-1
U_srd <- (1 / (2 * h_s))^-1
mcp <- JktM_khgr * W_Cp
b_no <- (U_h * A_h) / mcp         # UA/mCp

a_cv <- (U_h * A_h * T_h) + (U_scv * Ou_s * T_s)
b_cv <- (U_h * A_h) + (U_scv * Ou_s)

a_rd <- (U_h * A_h * T_h) + (U_srd * Ou_s * T_s)
b_rd <- (U_h * A_h) + (U_srd * Ou_s)


# Solver  -----------------------------------------------------------------

ts <- 0
dt <- 0.5
times <- seq(ts,Tr_s, by = dt)
for (Tn in 1:length(times)) {
  T_base[Tn + 1] <- T_h + ((T_base[Tn] - T_h) * exp(-b_no * dt))
  T_conv[Tn + 1] <- (a_cv - (a_cv - (b_cv * T_conv[Tn])) * (exp(-b_cv * dt * 1 / mcp))) / b_cv
  T_rad[Tn + 1] <- (a_rd - (a_rd - (b_rd * T_rad[Tn])) * (exp(-b_rd * dt * 1 / mcp))) / b_rd    
  ts[Tn + 1] <- ts[Tn] + dt
}

Jkt_df <- data.frame(ts = ts, T_base = T_base, T_conv = T_conv, T_rad = T_rad)

## Comparison Plot
CoolingJkT_plot <- 
  ggplot(Jkt_df, aes(x = ts)) +
  geom_line(aes(y = T_base, color = "No heat loss")) +
  geom_line(aes(y = T_conv, color = "Only Convectional loss"), linetype = "dashed") +
  geom_line(aes(y = T_rad, color = "Convection and Radiation"), linetype = "dashed") +
  labs(x = "Time(s)", y = "Temperature(K)", title = "Temperature of Reactor with a Cooling Jacket",
       color = "Heat Loss") +
  PGDP_theme() +
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text  = element_text(size = 8),
        plot.title = element_text(size =12)) +
  scale_color_manual(values = c("No heat loss" = "red", "Only Convectional loss" = "green", 
                                "Convection and Radiation" = "blue"))
print(CoolingJkT_plot)

ggsave(file.path(pic_folder, "CoolingJkt.png"), CoolingJkT_plot, width = 120, height = 90, units = "mm")
