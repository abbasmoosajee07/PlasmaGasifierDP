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
W_m <- 1.042                   # Mass of water in kg
Ou_TD <- PGD_m + 0.1             # Outer Tank Diameter
In_TD <- PGD_m             # Inner Tank Diameter
Tk_T <-  0.1              # Tank Thickness
Ou_s <- pi * Ou_TD * PGH_m     # Outer Surface Area of tank

T_h <- 2750                    
T_s <- 288
T_base <- T_conv <- T_rad <- 2750

# Model Calculations
Imp_speed <- ((Stirr_speed / 100) * Imp_maxrpm) / 60 # Impeller speed in m/s
Re_imp <- (W_Density * Imp_speed * (Imp_D^2)) / W_DViscosity            # Reynolds Impeller
W_Pr <- W_Cp * W_DViscosity / W_k           # Water Prandtl Number
W_Nu <- 0.87 * Re_imp^(0.62) * W_Pr^(0.33)  # Water Nusselt

# Heat Source Properties

# Jacket Properties
Jk_thk <- 2.5E-03 # Jacket thickness
Jk_d <- PGD_m    # Diameter of Jacket
Jk_h <- PGH_m - (1.4 * PGD_m)   # Jacket of height

H_h <- (W_Nu * W_k) / Jk_d
U_h <- ((1 / (H_h)) + (Jk_thk / 15))^-1 # U from Jacket
A_h <- pi * Jk_d * Jk_h    # Area from Jacket
Ch_len <- Jk_thk
Biot <- (U_h * Ch_len) / W_k

# Convectional heat loss
h_s <- 1.41 * (13 / Jk_h)^0.25 # McAdams formula
U_scv <- (1 / h_s)^-1
U_srd <- (1 / (2 * h_s))^-1
mcp <- W_m * W_Cp
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
  labs(x = "Time(s)", y = "Temperature(K)", title = "Stirred tank model comparison") +
  PGDP_theme() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("No heat loss" = "red", "Only Convectional loss" = "green", 
                                "Convection and Radiation" = "blue"))
print(CoolingJkT_plot)

ggsave(file.path(pic_folder, "CoolingJkt.png"), CoolingJkT_plot, width = 120, height = 90, units = "mm")
