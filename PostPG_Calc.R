Gas_out_temp <- 2750


piping_properties <- data.frame(
  Material = c("Stainless Steel", "Carbon Steel", "Nickel Alloy", "Plastic"),
  Heat_Resistance = c(900, 600, 1000, NA), # in degrees Celsius
  Corrosion_Resistance = c(8, 6, 9, NA), # Scale of 1-10
  Pressure_Rating = c(200, 150, 250, NA), # in psi
  Thermal_Expansion_Contraction = c(0.000016, 0.000012, 0.000018, NA), # Coefficient of linear expansion (1/°C)
  Insulation_Compatibility = c(1, 1, 1, NA), # 1 for Yes, 0 for No
  Chemical_Compatibility = c(7, 7, 9, NA), # Scale of 1-10
  Mechanical_Strength = c(80, 70, 90, NA), # in MPa
  Ease_of_Installation_Maintenance = c(7, 7, 7, NA), # Scale of 1-10
  Cost_Effectiveness = c(6, 8, 8, NA) # Scale of 1-10
)

view_table(t(piping_properties),2)

# Quench Tower for slag HEX003,P003 --------------------------------------------
Slag <- sum(Input_composition[Input_composition$Component==c("Other_MSW","Fixed_C"),"Mass_KGhr"]) * 1.1
CoolingT1<- CoolingT_Stream(data.frame(
                          Stream = c("Hot","Cold"),
                          TempIn_K = c(SlagTemp,WaterStorageTemp),
                          TempOut_K = c(Room_temp,MaxWaterTemp),
                          Mf_kghr = c(Slag,0)),"Slag")
Pump_003 <- Pump_func(CoolingT1)

initial_pressure <- SynG_PI # Initial pressure in bar
dP_Pa <- 1E+6
Ti <- Reac_temp

# Calculate compressor specifications
Fan_001 <- fan_func(Output_composition, SynG_PI, dP_Pa, Gas_out_temp)


# First Heat Exchanger System HEX001,P001 --------------------------------------
HEX1_Stream <- HEX_Stream(data.frame(
                            Stream = c("Hot","Cold"),
                            TempIn_K = c(Gas_out_temp,WaterStorageTemp),
                            TempOut_K = c(HEX1_Out,MaxWaterTemp)),Output_composition)
HEX_001 <- STHEX_func(HEX1_Stream, 1, 1.5)
HEX1_Stream <- HEX_001$Stream
HEX1_props <- HEX_001$HEX
Pump_001 <- Pump_func(HEX1_Stream)


# Second Heat Exchanger System HEX002,P002 -------------------------------------
Fan_002 <- fan_func(Output_composition, SynG_PI, 3E+6, Gas_out_temp)

HEX2_Stream <- HEX_Stream(data.frame(
                            Stream = c("Hot","Cold"),
                            TempIn_K = c(HEX1_Stream[HEX1_Stream$Stream == "Hot", "TempOut_K" ],WaterStorageTemp),
                            TempOut_K = c(HEX2_Out,MaxWaterTemp)),Output_composition)
HEX_002<- STHEX_func(HEX2_Stream, 1, 1.5)
HEX2_Stream <- HEX_002$Stream
HEX2_props <- HEX_002$HEX
Pump_002 <- Pump_func(HEX2_Stream)


# Cyclone Calculations ---------------------------------------------------------
# Cyclone Input 
Fan_003 <- fan_func(Output_composition, SynG_PI, 2E+6, Gas_out_temp)

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
Pump_004 <- Pump_func(CoolingT2)


# Steam Turbine Calculations ----------------------------------------------

steam_avail <- HEX1_Stream[HEX1_Stream$Stream == "Cold", "Mf_kghr"] + 
                      HEX2_Stream[HEX2_Stream$Stream == "Cold", "Mf_kghr"]
ElecGen_kWh <- Steam_turbine(steam_avail)
print(ElecGen_kWh)
# # Final Output -----------------------------------------------------------------
source("MBEB_Table.R")
view_table(Stream_summaryF,5)

HEXUnits <- data.frame("HEX001"=t(HEX1_props),"HEX002"=t(HEX2_props))
PumpUnits <- data.frame("Property" = Pump_001$Property, "Units" = Pump_001$Unit,
                        "Pump001" = Pump_001$Values,"Pump002" = Pump_002$Values,
                        "Pump003" = Pump_003$Values,"Pump004" = Pump_004$Values)
FanUnits <- data.frame("Property" = Fan_001$Property, "Units" = Fan_001$Unit,
                        "Fan001" = Fan_001$Values,"Fan002" = Fan_002$Values,
                        "Fan003" = Fan_003$Values)
