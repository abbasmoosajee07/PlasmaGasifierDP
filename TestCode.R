QuenchT_Stream <- function(St_prop,Solid) {
  HTi <- St_prop[St_prop$Stream=="Hot","Temp_K"]
  CTi <- St_prop[St_prop$Stream=="Cold","Temp_K"]


# Solid property ----------------------------------------------------------
  Cps <- Solid_waste[Solid_waste$Solid == Solid,"Cp"]
  
  Solid_props <- Solid_waste[Solid_waste$Solid == Solid,]
  Solid_kghr <- as.numeric(St_prop[St_prop$Stream == "Hot","Mf_kghr"])
  Solid_m3hr <- Solid_kghr / Solid_props[,"rho_kgm3"]
  SmCp <- Solid_kghr * Cps
  Hsi <- SmCp * HTi
  

# Water Stream Property ---------------------------------------------------
  Cpf <- Gen_fluid[Gen_fluid$Fluid == "Water","Cp"]

  Water_kghr <- as.numeric(St_prop[St_prop$Stream == "Cold","Mf_kghr"])
  Water_m3hr <- Water_kghr / Gen_fluid[Gen_fluid$Fluid == "Water","rho_kgm3"]
  WmCp <- Water_kghr * Cpf
  HWi <- WmCp * CTi

  THi <- Hsi + HWi
  
# Mixed Stream Property ---------------------------------------------------
  THf <- THi
  Mixed_kghr <- Solid_kghr + Water_kghr
  Mixed_m3hr <- Solid_m3hr + Water_m3hr
  
  TmCp <- SmCp + WmCp
  MT_K <-  THf / TmCp

  
  St_prop[,"Mf_kghr"] <- c(Solid_kghr,Water_kghr,Mixed_kghr)
  St_prop[,"Vol_m3hr"] <- c(Solid_m3hr,Water_m3hr,Mixed_m3hr)
  St_prop[,"Temp_K"] <- c(HTi,CTi,MT_K)
  St_prop[,"dH_Jhr"] <- c(Hsi,HWi,THf)
  St_prop[,"mCp"] <- c(SmCp,WmCp,TmCp)
  
  # St_prop[,"dP_Pa"] <- c(0,0)
  
  return(St_prop)
}

Slag <- sum(Input_composition[Input_composition$Component==c("Other_MSW","Fixed_C"),"Mass_KGhr"]) * 1.1
QuenchT1<- QuenchT_Stream(data.frame(
  Stream = c("Hot","Cold","Mixed"),
  Temp_K = c(SlagTemp,WaterStorageTemp,NA),
  Mf_kghr = c(Slag,1000,NA)
  ),"Slag")
print(QuenchT1)
# Pump_003 <- Pump_func(CoolingT1)

QuenchFlow_df <- data.frame(WaterFlow = seq(100,5000, by = 100),
                  Ash = 0, Slag = 0)
for (WFn in 1:nrow(QuenchFlow_df)){
  WFlow_n <- QuenchFlow_df[WFn,"WaterFlow"]
  QuenchSlag<- QuenchT_Stream(data.frame(
    Stream = c("Hot","Cold","Mixed"),
    Temp_K = c(SlagTemp,WaterStorageTemp,NA),
    Mf_kghr = c(Slag,WFlow_n,NA)
  ),"Slag")
  QuenchFlow_df[WFn,"Temp"] <- QuenchSlag[QuenchSlag$Stream == "Mixed", "Temp_K"]
  
  QuenchAsh<- QuenchT_Stream(data.frame(
    Stream = c("Hot","Cold","Mixed"),
    Temp_K = c(SlagTemp,WaterStorageTemp,NA),
    Mf_kghr = c(Ash,WFlow_n,NA)
  ),"Ash")
  QuenchFlow_df[WFn,"Slag"] <- QuenchSlag[QuenchSlag$Stream == "Mixed", "Temp_K"]
  QuenchFlow_df[WFn,"Ash"] <- QuenchAsh[QuenchAsh$Stream == "Mixed", "Temp_K"]
  
}

QuenchFlow_plot <- ggplot() +
  geom_line(QuenchFlow_df, mapping = aes(x = WaterFlow, y = Slag), color = "red") +
  geom_line(QuenchFlow_df, mapping = aes(x = WaterFlow, y = Ash), color = "blue") +
  labs(x = "Water Flow(kg/hr)", y = "Temperature(K)",
       title = "Outlet Temperature of Mixed Waste vs Input Water Flow") +
  scale_color_manual(values = c("red", "blue"), labels = c("Slag", "Ash")) +
  PGDP_theme() 
print(QuenchFlow_plot)
ggsave(file.path(pic_folder, "QuenchFlow_Plot.png"), QuenchFlow_plot, width = 120, height = 80, units = "mm")

