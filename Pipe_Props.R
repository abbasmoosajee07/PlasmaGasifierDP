# =============================================================================
# Pipe_Props.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Calculating all the pipe properties of the system
#
# =============================================================================

Pipe_df <- data.frame(
  Stream = colnames(Stream_summaryF),
  Mass_kghr = Stream_summary$Totalkghr,
  Flow_m3hr = Stream_summary$Volm3hr
)

Pipe_df$OptDi_m <- 0.33 * ((Pipe_df$Mass_kghr/3600)/(Pipe_df$Mass_kghr/Pipe_df$Flow_m3hr))^0.5

Pipe_df$Di_m <-  round(Pipe_df$OptDi_m,1)
Pipe_df$Di_m[Pipe_df$Di_m == 0.0] <- 0.1
Pipe_df$Di_m[Pipe_df$Di_m == 0.2] <- 0.3


Pipe_df$Area_m2 <- 0.25 * pi * Pipe_df$Di_m^2
Pipe_df$Vel_ms <- (Pipe_df$Flow_m3hr / Pipe_df$Area_m2)/3600
Pipe_df$abs_f <- 0.046E-3/Pipe_df$Di_m
Pipe_df$rel_f <- Pipe_df$abs_f * 1/6
Pipe_df$dP_Pam <- (8*Pipe_df$rel_f)/(Pipe_df$Di_m) * (((Pipe_df$Mass_kghr/Pipe_df$Flow_m3hr) * Pipe_df$Vel_ms^2)/2)
Pipe_df$dP_Pamh <- Pipe_df$dP_Pam * 20

Design_test <- 1.3

K <- 9

Pipe_df$Valve_dP <- K *(Design_test * (Pipe_df$Mass_kghr/Pipe_df$Flow_m3hr) * Pipe_df$Vel_ms^2)
Pipe_df$Valve_Cv <- Design_test * Pipe_df$Flow_m3hr * sqrt(((Pipe_df$Mass_kghr/Pipe_df$Flow_m3hr)
                                                            /Gen_fluid[Gen_fluid$Fluid == "Water","rho_kgm3"])/Pipe_df$Valve_dP)

