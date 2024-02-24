
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

Stream_summary["SnG002",] <- Stream_summary["SnG001",]
Stream_summary["SnG002","PostUnit"] <- "F001"
Stream_summary["SnG002","PresPa"] <- as.numeric(Stream_summary["SnG001","PresPa"]) + FandP_Pa
Stream_summary["SnG002","Volm3hr"] <- sum(Composition_props(Output_composition,Stream_summary["SnG002","PresPa"],Stream_summary["SnG002","TempK"])$Vol_m3hr)

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
Stream_summary["CW001", "PresPa"] <- Pump_1[Pump_1$Property == "Suction Pressure", "Values"]
Stream_summary["CW001", "Waterkghr"] <- Pump_1[Pump_1$Property == "Fluid Flow", "Values"]
Stream_summary["CW001","Totalkghr"] <- (Stream_summary["CW001","Waterkghr"])
Stream_summary["CW001","HJhr"] <- H_func(as.numeric(Stream_summary["CW001", "TempK"])/1000,main_elements$Water[1,])

Stream_summary["CW002", ] <- Stream_summary["CW001", ]
Stream_summary["CW002","PostUnit"] <- "P001"
Stream_summary["CW002", "PresPa"] <- Pump_1[Pump_1$Property == "Discharge Pressure", "Values"]

Stream_summary["HW001", ] <- Stream_summary["CW002", ]
Stream_summary["HW001","PostUnit"] <- "HEX001"
Stream_summary["HW001", "TempK"]  <- HEX1_Stream[HEX1_Stream$Stream == "Cold", "TempOut_K"]
Stream_summary["HW001", "PresPa"] <- as.numeric(Stream_summary["CW002", "PresPa"]) - HEX1_Stream[HEX1_Stream$Stream == "Cold", "dP_Pa"]
Stream_summary["HW001","HJhr"] <- H_func(as.numeric(Stream_summary["HW001", "TempK"])/1000,main_elements$Water[1,])

Stream_summary["SnG004",] <- Stream_summary["SnG003",]
Stream_summary["SnG004","PostUnit"] <- "F002"
Stream_summary["SnG004","PresPa"] <- as.numeric(Stream_summary["SnG003","PresPa"]) + FandP_Pa
Stream_summary["SnG004","Volm3hr"] <- sum(Composition_props(Output_composition,as.numeric(Stream_summary["SnG004","PresPa"]),as.numeric(Stream_summary["SnG004","TempK"]))$Vol_m3hr)

# HEX 2 & Pump 2 Stream Calculations -------------------------------------------

Stream_summary["SnG005",] <- Stream_summary["SnG004",]
Stream_summary["SnG005","PostUnit"] <- "HEX002"
Stream_summary["SnG005","TempK"] <- HEX2_Stream[HEX1_Stream$Stream == "Hot", "TempOut_K"]
Stream_summary["SnG005","PresPa"] <- as.numeric(Stream_summary["SnG002","PresPa"]) - HEX2_Stream[HEX2_Stream$Stream == "Hot", "dP_Pa"]
PGOut3 <- Composition_props(Output_composition, as.numeric(Stream_summary["SnG002","PresPa"]),as.numeric(Stream_summary["SnG002","TempK"]))
Stream_summary["SnG005","HJhr"] <- sum(PGOut3$dH_Jhr)
Stream_summary["SnG005","Volm3hr"] <- sum(PGOut3$Vol_m3hr)

Stream_summary["CW003",] <- c("Storage", 0, 0, 0, 0, 0, 0, 0, 0, 0)
Stream_summary["CW003", "TempK"] <- HEX1_Stream[HEX2_Stream$Stream == "Cold", "TempIn_K"]
Stream_summary["CW003", "PresPa"] <- Pump_2[Pump_2$Property == "Suction Pressure", "Values"]
Stream_summary["CW003", "Waterkghr"] <- Pump_2[Pump_2$Property == "Fluid Flow", "Values"]
Stream_summary["CW003","Totalkghr"] <- (Stream_summary["CW003","Waterkghr"])
Stream_summary["CW003", "Volm3hr"] <- HEX2_Stream[HEX2_Stream$Stream == "Cold", "Vol_m3hr"]
Stream_summary["CW003","HJhr"] <- H_func(as.numeric(Stream_summary["CW003", "TempK"])/1000,main_elements$Water[1,])

Stream_summary["CW004", ] <- Stream_summary["CW003", ]
Stream_summary["CW004","PostUnit"] <- "P002"
Stream_summary["CW004", "PresPa"] <- Pump_2[Pump_2$Property == "Discharge Pressure", "Values"]

Stream_summary["HW002", ] <- Stream_summary["CW003", ]
Stream_summary["HW002","PostUnit"] <- "HEX002"
Stream_summary["HW002", "TempK"]  <- HEX2_Stream[HEX2_Stream$Stream == "Cold", "TempOut_K"]
Stream_summary["HW002", "PresPa"] <- as.numeric(Stream_summary["CW004", "PresPa"]) - HEX2_Stream[HEX2_Stream$Stream == "Cold", "dP_Pa"]
Stream_summary["HW002","HJhr"] <- H_func(as.numeric(Stream_summary["HW002", "TempK"])/1000,main_elements$Water[1,])

Stream_summary["SnG006",] <- Stream_summary["SnG005",]
Stream_summary["SnG006","PostUnit"] <- "F003"
Stream_summary["SnG006","PresPa"] <- as.numeric(Stream_summary["SnG005","PresPa"]) + FandP_Pa
Stream_summary["SnG006","Volm3hr"] <- sum(Composition_props(Output_composition,as.numeric(Stream_summary["SnG006","PresPa"]),as.numeric(Stream_summary["SnG006","TempK"]))$Vol_m3hr)

# Slag CoolingT Tower
Stream_summary["Sg001",] <- c("Slag", Gas_out_temp, P_atm, 0, 0, Slag, 0, 0, 0, 0)
Stream_summary["Sg001","Totalkghr"] <- (Stream_summary["Sg001","Solidkghr"])
Stream_summary["Sg001","HJhr"] <-  Slag * as.numeric(Stream_summary["Sg001","TempK"]) * Solid_waste[Solid_waste$Solid == "Slag", "Cp"]
Stream_summary["Sg001","Volm3hr"] <- CoolingT1[CoolingT1$Stream == "Hot", "Vol_m3hr"]

Stream_summary["Sg002",] <- Stream_summary["Sg001",]
Stream_summary["Sg002","PostUnit"] <- "HEX003"
Stream_summary["Sg002","TempK"] <- CoolingT1[CoolingT1$Stream == "Hot", "TempOut_K"]
Stream_summary["Sg002","HJhr"] <-  Slag * as.numeric(Stream_summary["Sg002","TempK"]) * Solid_waste[Solid_waste$Solid == "Slag", "Cp"]

Stream_summary["CW005", ] <- c("Storage", 0, 0, 0, 0, 0, 0, 0, 0, 0)
Stream_summary["CW005", "TempK"] <- CoolingT1[CoolingT1$Stream == "Cold", "TempIn_K"]
Stream_summary["CW005", "PresPa"] <- Pump_3[Pump_3$Property == "Suction Pressure", "Values"]
Stream_summary["CW005", "Waterkghr"] <- Pump_3[Pump_3$Property == "Fluid Flow", "Values"]
Stream_summary["CW005","Totalkghr"] <- (Stream_summary["CW005","Waterkghr"])
Stream_summary["CW005", "Volm3hr"] <- CoolingT1[CoolingT1$Stream == "Cold", "Vol_m3hr"]
Stream_summary["CW005","HJhr"] <- H_func(as.numeric(Stream_summary["CW005", "TempK"])/1000,main_elements$Water[1,])

Stream_summary["CW006", ] <- Stream_summary["CW005", ]
Stream_summary["CW006","PostUnit"] <- "P003"
Stream_summary["CW006", "PresPa"] <- Pump_3[Pump_3$Property == "Discharge Pressure", "Values"]

Stream_summary["HW003", ] <- Stream_summary["CW006", ]
Stream_summary["HW003","PostUnit"] <- "HEX003"
Stream_summary["HW003", "TempK"]  <- CoolingT1[CoolingT1$Stream == "Cold", "TempOut_K"]
Stream_summary["HW003", "PresPa"] <- as.numeric(Stream_summary["CW006", "PresPa"]) - CoolingT1[CoolingT1$Stream == "Cold", "dP_Pa"]
Stream_summary["HW003","HJhr"] <- H_func(as.numeric(Stream_summary["HW003", "TempK"])/1000,main_elements$Water[1,])

# Ash Cyclone CoolingT Tower 
Stream_summary["SnG007",] <- Stream_summary["SnG006",]
Stream_summary["SnG007","PostUnit"] <- "Cy001"
Stream_summary["SnG007","TempK"] <- HEX2_Stream[HEX1_Stream$Stream == "Hot", "TempOut_K"]
Stream_summary["SnG007","PresPa"] <- as.numeric(Stream_summary["SnG006","PresPa"]) #- Cyc_calcs[Cyc_calcs$Property == "Pressure Drop", "Value"]
PGOut4 <- Composition_props(Output_composition, as.numeric(Stream_summary["SnG007","PresPa"]),as.numeric(Stream_summary["SnG007","TempK"]))
Stream_summary["SnG007","HJhr"] <- sum(PGOut4$dH_Jhr)
Stream_summary["SnG007","Volm3hr"] <- sum(PGOut4$Vol_m3hr)

Stream_summary["A001",] <- c("Char", Gas_out_temp, P_atm, 0, 0, Ash, 0, 0, 0, 0)
Stream_summary["A001","Totalkghr"] <- (Stream_summary["A001","Solidkghr"])
Stream_summary["A001","HJhr"] <-  Ash * as.numeric(Stream_summary["A001","TempK"]) * Solid_waste[Solid_waste$Solid == "Ash", "Cp"]
Stream_summary["A001","Volm3hr"] <- CoolingT2[CoolingT2$Stream == "Hot", "Vol_m3hr"]
Stream_summary["SnG007","Solidkghr"] <- as.numeric(Stream_summary["SnG006","Solidkghr"]) - as.numeric(Stream_summary["A001","Solidkghr"])
Stream_summary["SnG007","Totalkghr"] <- as.numeric(Stream_summary["SnG006","Totalkghr"]) - as.numeric(Stream_summary["A001","Solidkghr"])

Stream_summary["A002",] <- Stream_summary["A001",]
Stream_summary["A002","PostUnit"] <- "HEX004"
Stream_summary["A002","TempK"] <- CoolingT2[CoolingT2$Stream == "Hot", "TempOut_K"]
Stream_summary["A002","HJhr"] <-  Ash * as.numeric(Stream_summary["A002","TempK"]) * Solid_waste[Solid_waste$Solid == "Ash", "Cp"]

Stream_summary["CW007",] <- c("Storage", 0, 0, 0, 0, 0, 0, 0, 0, 0)
Stream_summary["CW007", "TempK"] <- CoolingT2[CoolingT2$Stream == "Cold", "TempIn_K"]
Stream_summary["CW007", "PresPa"] <- Pump_4[Pump_4$Property == "Suction Pressure", "Values"]
Stream_summary["CW007", "Waterkghr"] <- Pump_4[Pump_4$Property == "Fluid Flow", "Values"]
Stream_summary["CW007","Totalkghr"] <- (Stream_summary["CW007","Waterkghr"])
Stream_summary["CW007", "Volm3hr"] <- CoolingT2[CoolingT2$Stream == "Cold", "Vol_m3hr"]
Stream_summary["CW007","HJhr"] <- H_func(as.numeric(Stream_summary["CW007", "TempK"])/1000,main_elements$Water[1,])

Stream_summary["CW008", ] <- Stream_summary["CW007", ]
Stream_summary["CW008","PostUnit"] <- "P004"
Stream_summary["CW008", "PresPa"] <- Pump_4[Pump_4$Property == "Discharge Pressure", "Values"]

Stream_summary["HW004", ] <- Stream_summary["CW008", ]
Stream_summary["HW004","PostUnit"] <- "HEX004"
Stream_summary["HW004", "TempK"]  <- CoolingT2[CoolingT2$Stream == "Cold", "TempOut_K"]
Stream_summary["HW004", "PresPa"] <- as.numeric(Stream_summary["CW008", "PresPa"]) - CoolingT2[CoolingT2$Stream == "Cold", "dP_Pa"]
Stream_summary["HW004","HJhr"] <- H_func(as.numeric(Stream_summary["HW004", "TempK"])/1000,main_elements$Water[1,])

Stream_summary[, "Volm3hr"] <- round(as.numeric(Stream_summary[, "Volm3hr"]),3)
Stream_summary[, "Totalkghr"] <- round(as.numeric(Stream_summary[, "Totalkghr"]),3)
Stream_summary[, "Solidkghr"] <- round(as.numeric(Stream_summary[, "Solidkghr"]),3)

# Convert numeric characters to numeric
Stream_summaryF <- Stream_summary[,-1]

Stream_summaryF[] <- lapply(Stream_summaryF[], as.numeric)
Stream_summaryF[,"PresPa"] <- format(signif(Stream_summaryF["PresPa"],3), scientific = TRUE)
Stream_summaryF[,"HJhr"] <- format(signif(Stream_summaryF["HJhr"],3), scientific = TRUE)

Stream_summaryF <- cbind("PostUnit"=Stream_summary[,1],Stream_summaryF)
Stream_summaryF <- t(Stream_summaryF)

view(Stream_summaryF)

