# =============================================================================
# MBEB_Table.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Create a Mass and Energy Balance Table for all process streams 
#   post-gasifier
#
# =============================================================================

Stream_summary <- data.frame(
  PostUnit = c("Gasifier"),
  TempK = c(Gas_out_temp),
  PresPa = c(SynG_PI),
  HJhr = c(0),
  Volm3hr = c(0),
  Solidkghr = c(0),
  SnGkghr = c(0),
  Waterkghr = c(0),
  HazGaskghr = c(0),
  Totalkghr = c(0)
)
rownames(Stream_summary) <- "SnG001"
PGOut_enthalpy <- Composition_props(Output_composition,Stream_summary["SnG001","PresPa"],Stream_summary["SnG001","TempK"])
Stream_summary["SnG001","HJhr"] <- sum(PGOut_enthalpy$dH_Jhr)
Stream_summary["SnG001","Volm3hr"] <- sum(PGOut_enthalpy$Vol_m3hr)
Solids <- c("Ash", "Carbon")
HazGas <- c("Hydrogen_Chloride","Nitric_Oxide","Nitrogen_Dioxide","Sulfur_Dioxide","Chlorine","Sulfur")
SynGas <- c("Carbon_Monoxide","Hydrogen","Carbon_Dioxide","Methane","Oxygen","Nitrogen")

Stream_summary["SnG001","Waterkghr"] <- Output_composition[Output_composition$Component == "Water","Mass_KGhr"]
Stream_summary["SnG001","HazGaskghr"] <- sum(Output_composition[Output_composition$Component %in% HazGas, "Mass_KGhr"])
Stream_summary["SnG001","SnGkghr"] <- sum(Output_composition[Output_composition$Component %in% SynGas, "Mass_KGhr"])
Stream_summary["SnG001","Solidkghr"] <- sum(Output_composition[Output_composition$Component %in% Solids, "Mass_KGhr"])
Stream_summary["SnG001","Totalkghr"] <- sum(Stream_summary[,c("Waterkghr","HazGaskghr","Solidkghr","SnGkghr")])

# HEX 1 & Pump 1 Stream Calculations -------------------------------------------
Fan_001 <- fan_func(Output_composition, as.numeric(Stream_summary["SnG001","PresPa"]), Fan1dP_Pa, as.numeric(Stream_summary["SnG001","TempK"]))

Stream_summary["SnG002",] <- Stream_summary["SnG001",]
Stream_summary["SnG002","PostUnit"] <- "F001"
Stream_summary["SnG002","PresPa"] <- as.numeric(Stream_summary["SnG001","PresPa"]) + Fan_001[Fan_001$Property == "Pressure Drop", "Values"]
Stream_summary["SnG002","Volm3hr"] <- sum(Composition_props(Output_composition,Stream_summary["SnG002","PresPa"],Stream_summary["SnG002","TempK"])$Vol_m3hr)
Stream_summary["SnG002","TempK"] <-  as.numeric(Stream_summary["SnG001","TempK"]) + Fan_001[Fan_001$Property=="dT","Values"]

HEX1_Stream <- HEX_Stream(data.frame(
  Stream = c("Hot","Cold"),
  TempIn_K = c(as.numeric(Stream_summary["SnG002","TempK"]),WaterStorageTemp),
  TempOut_K = c(HEX1_Out,MaxWaterTemp)),Output_composition)
HEX_001 <- STHEX_func(HEX1_Stream, 1, 1.5)
HEX1_Stream <- HEX_001$Stream
HEX1_props <- HEX_001$HEX
Pump_001 <- Pump_func(HEX1_Stream,1,Pump_15m3_data)


Stream_summary["SnG003",] <- Stream_summary["SnG002",]
Stream_summary["SnG003","PostUnit"] <- "HEX001"
Stream_summary["SnG003","TempK"] <- HEX1_Stream[HEX1_Stream$Stream == "Hot", "TempOut_K"]
Stream_summary["SnG003","PresPa"] <- Stream_summary["SnG002","PresPa"] - HEX1_Stream[HEX1_Stream$Stream == "Hot", "dP_Pa"]
PGOut2 <- Composition_props(Output_composition,Stream_summary["SnG003","PresPa"],Stream_summary["SnG003","TempK"])
Stream_summary["SnG003","HJhr"] <- sum(PGOut2$dH_Jhr)
Stream_summary["SnG003","Volm3hr"] <- sum(PGOut2$Vol_m3hr)

Stream_summary["CW001",] <- c("Storage", 0, 0, 0, 0, 0, 0, 0, 0, 0)
Stream_summary["CW001", "TempK"] <- HEX1_Stream[HEX1_Stream$Stream == "Cold", "TempIn_K"]
Stream_summary["CW001", "Volm3hr"] <- HEX1_Stream[HEX1_Stream$Stream == "Cold", "Vol_m3hr"]
Stream_summary["CW001", "PresPa"] <- Pump_001[Pump_001$Property == "Suction Pressure", "Values"]
Stream_summary["CW001", "Waterkghr"] <- Pump_001[Pump_001$Property == "Fluid Flow", "Values"]
Stream_summary["CW001","Totalkghr"] <- (Stream_summary["CW001","Waterkghr"])
Stream_summary["CW001","HJhr"] <- H_func(as.numeric(Stream_summary["CW001", "TempK"])/1000,main_elements$Water[1,])

Stream_summary["CW002", ] <- Stream_summary["CW001", ]
Stream_summary["CW002","PostUnit"] <- "P001"
Stream_summary["CW002", "PresPa"] <- Pump_001[Pump_001$Property == "Discharge Pressure", "Values"]

Stream_summary["HW001", ] <- Stream_summary["CW002", ]
Stream_summary["HW001","PostUnit"] <- "HEX001"
Stream_summary["HW001", "TempK"]  <- HEX1_Stream[HEX1_Stream$Stream == "Cold", "TempOut_K"]
Stream_summary["HW001", "PresPa"] <- (as.numeric(Stream_summary["CW002", "PresPa"]) - HEX1_Stream[HEX1_Stream$Stream == "Cold", "dP_Pa"])
Stream_summary["HW001","HJhr"] <- H_func(as.numeric(Stream_summary["HW001", "TempK"])/1000,main_elements$Water[1,])
Stream_summary["HW001","Volm3hr"] <- ((as.numeric(Stream_summary["HW001","Totalkghr"])/Gen_fluid[Gen_fluid$Fluid == "Water","RMM"]) * 1000 * R * 
                                        as.numeric(Stream_summary["HW001", "TempK"]) ) / as.numeric(Stream_summary["HW001", "PresPa"])

Steam1 <- data.frame(
  Component = c("Water"), Mass_KGhr = c(HEX1_Stream[HEX1_Stream$Stream == "Cold","Mf_kghr"]),
  Mol_kmolhr = c(0),Vol_m3hr = c(0),dH_Jhr = c(0)
)
Comp_001 <- comp_func(Steam1,as.numeric(Stream_summary["HW001","Volm3hr"]), as.numeric(Stream_summary["HW001", "PresPa"]), 36E+5, HEX1_Stream[HEX1_Stream$Stream == "Cold","TempOut_K"])

Stream_summary["HW002", ] <- Stream_summary["HW001", ]
Stream_summary["HW002","PostUnit"] <- "Comp001"
Stream_summary["HW002", "TempK"]  <- Stream_summary["HW002", "TempK"] #- Comp_001[Comp_001$Property=="dT","Values"]
Stream_summary["HW002", "PresPa"] <- Comp_001[Comp_001$Property=="Final_Pressure","Values"]
Stream_summary["HW002","Volm3hr"] <- Comp_001[Comp_001$Property=="Final_Volume","Values"]

# HEX 2 & Pump 2 Stream Calculations -------------------------------------------

Fan_002 <- fan_func(Output_composition, as.numeric(Stream_summary["SnG003","PresPa"]), Fan2dP_Pa, as.numeric(Stream_summary["SnG003","TempK"]))

Stream_summary["SnG004",] <- Stream_summary["SnG003",]
Stream_summary["SnG004","PostUnit"] <- "F002"
Stream_summary["SnG004","PresPa"] <- as.numeric(Stream_summary["SnG003","PresPa"]) + Fan_002[Fan_002$Property == "Pressure Drop", "Values"]
Stream_summary["SnG004","Volm3hr"] <- sum(Composition_props(Output_composition,as.numeric(Stream_summary["SnG004","PresPa"]),as.numeric(Stream_summary["SnG004","TempK"]))$Vol_m3hr)
Stream_summary["SnG004","TempK"] <-  as.numeric(Stream_summary["SnG003","TempK"]) + Fan_002[Fan_002$Property=="dT","Values"]


HEX2_Stream <- HEX_Stream(data.frame(
  Stream = c("Hot","Cold"),
  TempIn_K = c(as.numeric(Stream_summary["SnG004","TempK"]),WaterStorageTemp),
  TempOut_K = c(HEX2_Out,MaxWaterTemp)),Output_composition)
HEX_002<- STHEX_func(HEX2_Stream, 1, 1.5)
HEX2_Stream <- HEX_002$Stream
HEX2_props <- HEX_002$HEX
Pump_002 <- Pump_func(HEX2_Stream,2,Pump_15m3_data)

Stream_summary["SnG005",] <- Stream_summary["SnG004",]
Stream_summary["SnG005","PostUnit"] <- "HEX002"
Stream_summary["SnG005","TempK"] <- HEX2_Stream[HEX1_Stream$Stream == "Hot", "TempOut_K"]
Stream_summary["SnG005","PresPa"] <- as.numeric(Stream_summary["SnG004","PresPa"]) - HEX2_Stream[HEX2_Stream$Stream == "Hot", "dP_Pa"]
PGOut3 <- Composition_props(Output_composition, as.numeric(Stream_summary["SnG002","PresPa"]),as.numeric(Stream_summary["SnG002","TempK"]))
Stream_summary["SnG005","HJhr"] <- sum(PGOut3$dH_Jhr)
Stream_summary["SnG005","Volm3hr"] <- sum(PGOut3$Vol_m3hr)

Stream_summary["CW003",] <- c("Storage", 0, 0, 0, 0, 0, 0, 0, 0, 0)
Stream_summary["CW003", "TempK"] <- HEX1_Stream[HEX2_Stream$Stream == "Cold", "TempIn_K"]
Stream_summary["CW003", "PresPa"] <- Pump_002[Pump_002$Property == "Suction Pressure", "Values"]
Stream_summary["CW003", "Waterkghr"] <- Pump_002[Pump_002$Property == "Fluid Flow", "Values"]
Stream_summary["CW003","Totalkghr"] <- (Stream_summary["CW003","Waterkghr"])
Stream_summary["CW003", "Volm3hr"] <- HEX2_Stream[HEX2_Stream$Stream == "Cold", "Vol_m3hr"]
Stream_summary["CW003","HJhr"] <- H_func(as.numeric(Stream_summary["CW003", "TempK"])/1000,main_elements$Water[1,])

Stream_summary["CW004", ] <- Stream_summary["CW003", ]
Stream_summary["CW004","PostUnit"] <- "P002"
Stream_summary["CW004", "PresPa"] <- Pump_002[Pump_002$Property == "Discharge Pressure", "Values"]

Stream_summary["HW003", ] <- Stream_summary["CW003", ]
Stream_summary["HW003","PostUnit"] <- "HEX002"
Stream_summary["HW003", "TempK"]  <- HEX2_Stream[HEX2_Stream$Stream == "Cold", "TempOut_K"]
Stream_summary["HW003", "PresPa"] <- (as.numeric(Stream_summary["CW004", "PresPa"]) - HEX2_Stream[HEX2_Stream$Stream == "Cold", "dP_Pa"])
Stream_summary["HW003","HJhr"] <- H_func(as.numeric(Stream_summary["HW002", "TempK"])/1000,main_elements$Water[1,])
Stream_summary["HW003","Volm3hr"] <- ((as.numeric(Stream_summary["HW003","Totalkghr"])/Gen_fluid[Gen_fluid$Fluid == "Water","RMM"]) * 1000 * R * 
                                        as.numeric(Stream_summary["HW003", "TempK"]) ) / as.numeric(Stream_summary["HW003", "PresPa"])

Steam2 <- data.frame(
  Component = c("Water"), Mass_KGhr = c(HEX2_Stream[HEX2_Stream$Stream == "Cold","Mf_kghr"]),
  Mol_kmolhr = c(0),Vol_m3hr = c(0),dH_Jhr = c(0)
)
Comp_002 <- comp_func(Steam2, as.numeric(Stream_summary["HW003","Volm3hr"]), as.numeric(Stream_summary["HW003", "PresPa"]), 36E+5, HEX2_Stream[HEX2_Stream$Stream == "Cold","TempOut_K"])


Stream_summary["HW004", ] <- Stream_summary["HW003", ]
Stream_summary["HW004","PostUnit"] <- "Comp002"
Stream_summary["HW004", "TempK"]  <- Stream_summary["HW004", "TempK"] #- Comp_002[Comp_002$Property=="dT","Values"]
Stream_summary["HW004", "PresPa"] <- Comp_002[Comp_002$Property=="Final_Pressure","Values"]
Stream_summary["HW004","Volm3hr"] <- Comp_002[Comp_002$Property=="Final_Volume","Values"]

# Pre-Cyclone Calculations ------------------------------------------------

Fan_003 <- fan_func(Output_composition, as.numeric(Stream_summary["SnG005","PresPa"]), Fan3dP_Pa, as.numeric(Stream_summary["SnG005","TempK"]))

Stream_summary["SnG006",] <- Stream_summary["SnG005",]
Stream_summary["SnG006","PostUnit"] <- "F003"
Stream_summary["SnG006","PresPa"] <- as.numeric(Stream_summary["SnG005","PresPa"]) + Fan_003[Fan_003$Property == "Pressure Drop", "Values"]
Stream_summary["SnG006","Volm3hr"] <- sum(Composition_props(Output_composition,as.numeric(Stream_summary["SnG006","PresPa"]),as.numeric(Stream_summary["SnG006","TempK"]))$Vol_m3hr)
Stream_summary["SnG006","TempK"] <-  as.numeric(Stream_summary["SnG005","TempK"]) + Fan_003[Fan_003$Property=="dT","Values"]


steam_avail <- HEX1_Stream[HEX1_Stream$Stream == "Cold", "Mf_kghr"] + 
  HEX2_Stream[HEX2_Stream$Stream == "Cold", "Mf_kghr"] -
  H2O_feed
ElecGen_kWh <- Steam_turbine(steam_avail)

# Slag Quench Tower 001 -------------------------------------------------------
Slag <- sum(Input_composition[Input_composition$Component==c("Other_MSW","Fixed_C"),"Mass_KGhr"])

QuenchT1<- Quench_Stream(data.frame(
  Stream = c("Hot","Cold","Mixed"),
  Temp_K = c(SlagTemp,WaterStorageTemp,NA),
  Mf_kghr = c(Slag,QuenchW_1,NA)
),"Slag")
QuenchS1 <- QuenchT1$Stream
Pump_003 <- Pump_func(QuenchT1$Stream,3,Pump_4m3_data)

Stream_summary["Sg001",] <- c("Slag", Gas_out_temp, P_atm, 0, 0, Slag, 0, 0, 0, 0)
Stream_summary["Sg001","Totalkghr"] <- (Stream_summary["Sg001","Solidkghr"])
Stream_summary["Sg001","HJhr"] <-  Slag * as.numeric(Stream_summary["Sg001","TempK"]) * Solid_waste[Solid_waste$Solid == "Slag", "Cp"]
Stream_summary["Sg001","Volm3hr"] <- QuenchS1[QuenchS1$Stream == "Hot", "Vol_m3hr"]

Stream_summary["CW005", ] <- c("Storage", 0, 0, 0, 0, 0, 0, 0, 0, 0)
Stream_summary["CW005", "TempK"] <- QuenchS1[QuenchS1$Stream == "Cold", "Temp_K"]
Stream_summary["CW005", "PresPa"] <- Pump_003[Pump_003$Property == "Suction Pressure", "Values"]
Stream_summary["CW005", "Waterkghr"] <- Pump_003[Pump_003$Property == "Fluid Flow", "Values"]
Stream_summary["CW005","Totalkghr"] <- (Stream_summary["CW005","Waterkghr"])
Stream_summary["CW005", "Volm3hr"] <- QuenchS1[QuenchS1$Stream == "Cold", "Vol_m3hr"]
Stream_summary["CW005","HJhr"] <- H_func(as.numeric(Stream_summary["CW005", "TempK"])/1000,main_elements$Water[1,])

Stream_summary["CW006", ] <- Stream_summary["CW005", ]
Stream_summary["CW006","PostUnit"] <- "P003"
Stream_summary["CW006", "PresPa"] <- Pump_003[Pump_003$Property == "Discharge Pressure", "Values"]


Stream_summary["SW001",] <- Stream_summary["Sg001",]
Stream_summary["SW001","PostUnit"] <- "Quench001"
Stream_summary["SW001","TempK"] <- QuenchS1[QuenchS1$Stream == "Mixed", "Temp_K"]
Stream_summary["SW001","HJhr"] <-  Slag * as.numeric(Stream_summary["SW001","TempK"]) *
                                         Solid_waste[Solid_waste$Solid == "Slag", "Cp"]
Stream_summary["SW001", "Volm3hr"]  <- QuenchS1[QuenchS1$Stream == "Mixed", "Vol_m3hr"]
Stream_summary["SW001","Waterkghr"] <- (Stream_summary["CW006","Waterkghr"])
Stream_summary["SW001", "Totalkghr"]  <- QuenchS1[QuenchS1$Stream == "Mixed", "Mf_kghr"]


# Ash Cyclone Quench Tower 002 ----------------------------------------------
Cyc_calcs <- Cyc_func(Output_composition,as.numeric(Stream_summary["SnG006","PresPa"]),as.numeric(Stream_summary["SnG006","TempK"]))
Stream_summary["SnG007",] <- Stream_summary["SnG006",]
Stream_summary["SnG007","PostUnit"] <- "Cy001"
Stream_summary["SnG007","PresPa"] <- as.numeric(Stream_summary["SnG006","PresPa"]) - Cyc_calcs[Cyc_calcs$Property == "Pressure Drop", "Value"]
PGOut4 <- Composition_props(Output_composition, as.numeric(Stream_summary["SnG007","PresPa"]),as.numeric(Stream_summary["SnG007","TempK"]))
Stream_summary["SnG007","HJhr"] <- sum(PGOut4$dH_Jhr)
Stream_summary["SnG007","Volm3hr"] <- sum(PGOut4$Vol_m3hr)

QuenchT2<- Quench_Stream(data.frame(
  Stream = c("Hot","Cold","Mixed"),
  Temp_K = c(as.numeric(Stream_summary["SnG006","TempK"]),WaterStorageTemp,NA),
  Mf_kghr = c(Ash,QuenchW_2,NA)
),"Ash")
QuenchS2 <- QuenchT2$Stream
Pump_004 <- Pump_func(QuenchT2$Stream,4,Pump_4m3_data)

Stream_summary["A001",] <- c("Char", as.numeric(Stream_summary["SnG006","TempK"]), P_atm, 0, 0, Ash, 0, 0, 0, 0)
Stream_summary["A001","Totalkghr"] <- (Stream_summary["A001","Solidkghr"])
Stream_summary["A001","HJhr"] <-  Ash * as.numeric(Stream_summary["A001","TempK"]) * Solid_waste[Solid_waste$Solid == "Ash", "Cp"]
Stream_summary["A001","Volm3hr"] <- QuenchS2[QuenchS2$Stream == "Hot", "Vol_m3hr"]
Stream_summary["SnG007","Solidkghr"] <- as.numeric(Stream_summary["SnG006","Solidkghr"]) - as.numeric(Stream_summary["A001","Solidkghr"])
Stream_summary["SnG007","Totalkghr"] <- as.numeric(Stream_summary["SnG006","Totalkghr"]) - as.numeric(Stream_summary["A001","Solidkghr"])

Stream_summary["CW007",] <- c("Storage", 0, 0, 0, 0, 0, 0, 0, 0, 0)
Stream_summary["CW007", "TempK"] <- QuenchS2[QuenchS2$Stream == "Cold", "Temp_K"]
Stream_summary["CW007", "PresPa"] <- Pump_004[Pump_004$Property == "Suction Pressure", "Values"]
Stream_summary["CW007", "Waterkghr"] <- Pump_004[Pump_004$Property == "Fluid Flow", "Values"]
Stream_summary["CW007","Totalkghr"] <- (Stream_summary["CW007","Waterkghr"])
Stream_summary["CW007", "Volm3hr"] <- QuenchS2[QuenchS2$Stream == "Cold", "Vol_m3hr"]
Stream_summary["CW007","HJhr"] <- H_func(as.numeric(Stream_summary["CW007", "TempK"])/1000,main_elements$Water[1,])

Stream_summary["CW008", ] <- Stream_summary["CW007", ]
Stream_summary["CW008","PostUnit"] <- "P004"
Stream_summary["CW008", "PresPa"] <- Pump_004[Pump_004$Property == "Discharge Pressure", "Values"]

Stream_summary["SW002",] <- Stream_summary["A001",]
Stream_summary["SW002","PostUnit"] <- "Quench002"
Stream_summary["SW002","TempK"] <- QuenchS2[QuenchS2$Stream == "Mixed", "Temp_K"]
Stream_summary["SW002","HJhr"] <-  Ash * as.numeric(Stream_summary["SW002","TempK"]) *
                                            Solid_waste[Solid_waste$Solid == "Slag", "Cp"]
Stream_summary["SW002", "Volm3hr"]  <- QuenchS2[QuenchS2$Stream == "Mixed", "Vol_m3hr"]
Stream_summary["SW002","Waterkghr"] <- (Stream_summary["CW008","Waterkghr"])
Stream_summary["SW002", "Totalkghr"]  <- QuenchS2[QuenchS2$Stream == "Mixed", "Mf_kghr"]


# Finalise Formatting ---------------------------------------------------------
Stream_summary[, "Volm3hr"] <- round(as.numeric(Stream_summary[, "Volm3hr"]),3)
Stream_summary[, "Totalkghr"] <- round(as.numeric(Stream_summary[, "Totalkghr"]),3)
Stream_summary[, "Solidkghr"] <- round(as.numeric(Stream_summary[, "Solidkghr"]),3)

Stream_summaryF <- Stream_summary[,-1]

Stream_summaryF[] <- lapply(Stream_summaryF[], as.numeric)
Stream_summaryF[,"PresPa"] <- format(signif(Stream_summaryF["PresPa"],3), scientific = TRUE)
Stream_summaryF[,"HJhr"] <- format(signif(Stream_summaryF["HJhr"],3), scientific = TRUE)

Stream_summaryF <- cbind("PostUnit"=Stream_summary[,1],Stream_summaryF)
Stream_summaryF <- t(Stream_summaryF)

view(Stream_summaryF)

