
# Quench Tower for slag HEX003,P003 --------------------------------------------
Slag <- sum(Input_composition[Input_composition$Component==c("Other_MSW","Fixed_C"),"Mass_KGhr"]) * 1.1
CoolingT1<- CoolingT_Stream(data.frame(
                          Stream = c("Hot","Cold"),
                          TempIn_K = c(SlagTemp,WaterStorageTemp),
                          TempOut_K = c(Room_temp,MaxWaterTemp),
                          Mf_kghr = c(Slag,0)),"Slag")
Pump_3 <- Pump_func(CoolingT1)


# First Heat Exchanger System HEX001,P001 --------------------------------------
HEX1_Stream <- HEX_Stream(data.frame(
                            Stream = c("Hot","Cold"),
                            TempIn_K = c(Gas_out_temp,WaterStorageTemp),
                            TempOut_K = c(HEX1_Out,MaxWaterTemp)),Output_composition)
HEX_1 <- STHEX_func(HEX1_Stream, 1, 1.5)
HEX1_Stream <- HEX_1$Stream
HEX1_props <- HEX_1$HEX
Pump_1 <- Pump_func(HEX1_Stream)


# Second Heat Exchanger System HEX002,P002 -------------------------------------
HEX2_Stream <- HEX_Stream(data.frame(
                            Stream = c("Hot","Cold"),
                            TempIn_K = c(HEX1_Stream[HEX1_Stream$Stream == "Hot", "TempOut_K" ],WaterStorageTemp),
                            TempOut_K = c(HEX2_Out,MaxWaterTemp)),Output_composition)
HEX_2<- STHEX_func(HEX2_Stream, 1, 1.5)
HEX2_Stream <- HEX_2$Stream
HEX2_props <- HEX_2$HEX
Pump_2 <- Pump_func(HEX2_Stream)




# Cyclone Calculations ---------------------------------------------------------
# Cyclone Input 
Cyclone_Input <- Composition_props(Output_composition,2.23E+07,HEX2_Out)

# Cyclone Dimensions 
Cyc_dim <- {data.frame(
  Body_D = 1.00,
  In_H = 0.50,
  In_W = 0.20,
  Out_L = 0.50,
  Out_D = 0.50,
  Cyl_H = 1.50,
  Ov_H = 4.00,
  Dust_out = 0.38,
  NH = 6.40,
  k = 551.30
)}
rownames(Cyc_dim) <- "Ratio"
Cyc_dim["Length_m",] <- Cyc_dim["Ratio",] * 0.72

# Cyclone Calculations
SngFl_m3hr <- sum(Cyclone_Input$Vol_m3hr)
SngFl_m3s <- SngFl_m3hr/3600
Sngrho_kgm3 <- sum(Cyclone_Input$Mass_KGhr)/SngFl_m3hr
InA_m2 <- Cyc_dim["Length_m","In_H"] * Cyc_dim["Length_m","In_W"]
Inv_ms <- SngFl_m3s/InA_m2
AshD_m <- Solid_waste[Solid_waste$Solid == "Ash", "Size"]
Ashrho_kgm3 <- Solid_waste[Solid_waste$Solid == "Ash", "rho_kgm3"]
dP_Pa <- (Cyc_dim["Length_m","NH"] * Sngrho_kgm3 * Inv_ms^2)/2
Cost_pnd <- ((57800 * InA_m2^0.903)*798.7/100)*2
Cyc_eff <- 1.0

# Mass Balance
Cyclone_Input <- Cyclone_Input["Mass_KGhr"] 
Ash_removed <- Cyclone_Input["Ash",]* Cyc_eff
Carbon_removed <- Cyclone_Input["Carbon",] * Cyc_eff
Cyclone_Output <- Cyclone_Input
Cyclone_Output["Ash",] <- Cyclone_Output["Ash",] - Ash_removed
Cyclone_Output["Carbon",] <- Cyclone_Output["Carbon",] - Carbon_removed
Ash <- Output_composition[Output_composition$Component == "Ash","Mass_KGhr"] + 
        Output_composition[Output_composition$Component == "Carbon","Mass_KGhr"]
Char_composition <- Cyclone_Input - Cyclone_Output

# Output data 
Cyc_calcs <- data.frame(
  Property = c("Syngas Flow", "Syngas Velocity",
               "Overall Height", "Body Diameter",
               "Char Density", "Char Size", "Char Removed",
               "Pressure Drop", "Equipment Cost"),
  Unit = c("m^3/hr", "m/s",
           "m", "m",
           "kg/m^3", "m", "kg/hr",
           "Pa", "£"),
  Value = c(SngFl_m3hr, Inv_ms,
            as.numeric(Cyc_dim["Length_m","Ov_H"]), as.numeric(Cyc_dim["Length_m","Body_D"]),
            Ashrho_kgm3, AshD_m, Ash,
            dP_Pa, Cost_pnd)
)

# Quench Tower for Ash HEX004,P004 ---------------------------------------------
CoolingT2<- CoolingT_Stream(data.frame(
                          Stream = c("Hot","Cold"),
                          TempIn_K = c(HEX2_Stream[HEX2_Stream$Stream == "Hot", "TempOut_K" ],WaterStorageTemp),
                          TempOut_K = c(Room_temp,MaxWaterTemp),
                          Mf_kghr = c(Ash,0)),"Ash")
Pump_4 <- Pump_func(CoolingT2)


# Final Output -----------------------------------------------------------------
source("MBEB_Table.R")
view_table(Stream_summaryF,5)

HEXUnits <- data.frame("HEX1"=t(HEX1_props),"HEX2"=t(HEX2_props))
PumpUnits <- data.frame("Property" = Pump_1$Property, "Units" = Pump_1$Unit,
                        "Pump 1" = Pump_1$Values,"Pump 2" = Pump_2$Values,
                        "Pump 3" = Pump_3$Values,"Pump 4" = Pump_4$Values)
