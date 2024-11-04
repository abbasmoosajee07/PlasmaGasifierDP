# =============================================================================
# PostPG_Calc.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Calculating the ideal properties of all units in the process node
#
# =============================================================================

Gas_out_temp <- Reac_temp

dP_Pa <- 3E+5
MaxWaterTemp <- 698
SlagTemp  <- Reac_temp

# First Heat Exchanger System HEX001,P001 --------------------------------------
Fan1dP_Pa <- dP_Pa
HEX1_Out  <- 1523

# Second Heat Exchanger System HEX002,P002 -------------------------------------
Fan2dP_Pa <- dP_Pa
HEX2_Out  <- 700

Fan3dP_Pa <- dP_Pa


# Solid Cooling Quench Towers ---------------------------------------------

Slag <- sum(Input_composition[Input_composition$Component==c("Other_MSW","Fixed_C"),"Mass_KGhr"])
Ash <- sum(Output_composition[c("Ash","Carbon"),"Mass_KGhr"])

QuenchFlow_df <- data.frame(WaterFlow = seq(100,5000, by = 100),
                            Ash = 0, Slag = 0)
for (WFn in 1:nrow(QuenchFlow_df)){
  WFlow_n <- QuenchFlow_df[WFn,"WaterFlow"]
  QuenchSlag<- Quench_Stream(data.frame(
    Stream = c("Hot","Cold","Mixed"),
    Temp_K = c(SlagTemp,WaterStorageTemp,NA),
    Mf_kghr = c(Slag,WFlow_n,NA)
  ),"Slag")$Stream
  QuenchFlow_df[WFn,"Temp"] <- QuenchSlag[QuenchSlag$Stream == "Mixed", "Temp_K"]
  
  QuenchAsh<- Quench_Stream(data.frame(
    Stream = c("Hot","Cold","Mixed"),
    Temp_K = c(SlagTemp,WaterStorageTemp,NA),
    Mf_kghr = c(Ash,WFlow_n,NA)
  ),"Ash")$Stream
  QuenchFlow_df[WFn,"Slag"] <- QuenchSlag[QuenchSlag$Stream == "Mixed", "Temp_K"]
  QuenchFlow_df[WFn,"Ash"] <- QuenchAsh[QuenchAsh$Stream == "Mixed", "Temp_K"]
  
}

QuenchFlow_plot <- ggplot(QuenchFlow_df) +
  geom_line(mapping = aes(x = WaterFlow, y = Slag, color = "Slag")) +
  geom_line(mapping = aes(x = WaterFlow, y = Ash, color = "Ash")) +
  labs(x = "Water Flow(kg/hr)", y = "Temperature(K)",
       title = "Outlet Temperature of Mixed Waste vs Input Water Flow",
       colour  = "Solid Waste") +
  scale_color_manual(values = c("red", "blue"), labels = c("Slag", "Ash")) +
  PGDP_theme() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_text(size = 10),
        legend.text  = element_text(size = 10),
        plot.title =  element_text(size = 11))

print(QuenchFlow_plot)
ggsave(file.path(pic_folder, "QuenchFlow_Plot.png"), QuenchFlow_plot, width = 120, height = 80, units = "mm")

# Quench Tower for slag HEX003,P003 --------------------------------------------
QuenchW_1 <- 3000

# Quench Tower for Ash HEX004,P004 --------------------------------------------
QuenchW_2 <- 1000


# Final Output -----------------------------------------------------------------
source("MBEB_Table.R")
view_table(Stream_summaryF,5)

HEXUnits <- data.frame("HEX001"=t(HEX1_props),"HEX002"=t(HEX2_props))

PumpUnits <- data.frame("Property" = Pump_001$Property, "Units" = Pump_001$Unit,
                        "Pump001" = Pump_001$Values,"Pump002" = Pump_002$Values,
                        "Pump003" = Pump_003$Values,"Pump004" = Pump_004$Values)

FanUnits <- data.frame("Property" = Fan_001$Property, "Units" = Fan_001$Unit,
                        "Fan001" = Fan_001$Values,"Fan002" = Fan_002$Values,
                        "Fan003" = Fan_003$Values)

QuenchTowerUnits <- data.frame("Property" = QuenchT1$Tower$Property, 
                                "Units" = QuenchT1$Tower$Unit,
                                "Quench001" = QuenchT1$Tower$Value,
                                "Quench002" = QuenchT2$Tower$Value)


CompUnits <- data.frame("Property" = Comp_001$Property, 
                         "Units" = Comp_001$Unit,
                         "Comp001" = Comp_001$Values,
                         "Comp002" = Comp_002$Values)

cat("\n Syngas Coolers\n")
print(HEXUnits)

cat("\n Quench Towers \n")
print(QuenchTowerUnits)

cat("\n Cyclone \n")
print(Cyc_calcs)

cat("\n Fluid Pumps \n")
print(PumpUnits)

cat("\n Fan Units \n")
print(FanUnits)

cat("\n Compressors Units \n")
print(CompUnits)


