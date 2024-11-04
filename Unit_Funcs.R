# =============================================================================
# Unit_Funcs.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Create functions for all auxiliary units required in the 
# post-gasifier processing
#
# =============================================================================

# Heat Exchanger Stream Calculations -------------------------------------------
HEX_Stream <- function(St_prop,Flow_Composition, HEX_eff = 0.9) {
  HTi <- St_prop[St_prop$Stream=="Hot","TempIn_K"]
  HTf <- St_prop[St_prop$Stream=="Hot","TempOut_K"]
  CTi <- St_prop[St_prop$Stream=="Cold","TempIn_K"]
  CTf <- St_prop[St_prop$Stream=="Cold","TempOut_K"]
  
  Pne <- PG_Press
  Flow_Composition <- Composition_props(Flow_Composition,Pne,HTf,HTi)
  Hot_dH <- -sum(Flow_Composition[,"dH_Jhr"])
  Cold_dH <- Hot_dH / HEX_eff
  
  if (CTf >= element_props[element_props$Component == "Water","Tb_K"]){
    dHv <- Gen_fluid[Gen_fluid$Fluid == "Water","dH_Jmol"]
    result_water   <- state_func(main_elements$Water,CTi,CTf) + dHv
    
  } else {
    result_water   <- state_func(main_elements$Water,CTi,CTf)
  }
  water_molflow <- Cold_dH / result_water$Enthalpy
  water_massflow <- water_molflow * element_props[element_props$Component == "Water","Molar_Mass"]
  water_volflow <- water_massflow / Gen_fluid[Gen_fluid$Fluid == "Water","rho_kgm3"]

  St_prop[,"Mf_kghr"] <- c(sum(Flow_Composition$Mass_KGhr),water_massflow)
  St_prop[,"Vol_m3hr"] <- c(sum(Flow_Composition$Vol_m3hr),water_volflow)
  St_prop[,"dH_Jhr"] <- c(Hot_dH,Cold_dH)
  
  return(St_prop)
}


# Shell & Tube Heat Excanger Function ------------------------------------------
STHEX_func <- function(St_prop, Tube_No = 338, Tube_OD = 50E-3, thk =5E-3, Shell_ID = 3){ 
  # Extracting Stream Properties -----------------------------------------------
  HTi <- St_prop[St_prop$Stream=="Hot","TempIn_K"]
  HTf <- St_prop[St_prop$Stream=="Hot","TempOut_K"]
  CTi <- St_prop[St_prop$Stream=="Cold","TempIn_K"]
  CTf <- St_prop[St_prop$Stream=="Cold","TempOut_K"]
  Cold_dH <- St_prop[St_prop$Stream=="Cold","dH_Jhr"]
  Cold_MFlow <- St_prop[St_prop$Stream=="Cold","Mf_kghr"]
  Hot_MFlow <- St_prop[St_prop$Stream=="Hot","Mf_kghr"]
  Cold_VFlow <- St_prop[St_prop$Stream=="Cold","Vol_m3hr"]
  Hot_VFlow <- St_prop[St_prop$Stream=="Hot","Vol_m3hr"]
  
  # HEX Properties -------------------------------------------------------------
  U_Ov <- 300
  
  # Tube Arrangement - Triangular Pitch
  # 1 Shell pass, 2 Tube Pass
  Tube_Ps  <- 2
  Shell_Ps <- 1
  K1 <- 0.249
  n1 <- 2.207 
  
  # Tube Side Properties
  fD_t <- 3E-2
  Tube_ID <- Tube_OD - thk # Internal Diameter of TUbe
  
  # Baffle Properties
  Bfl_spc <- 0.2    # Baffle Spacing 
  Bfl_no  <- 14     # Baffle Numbers
  Bfl_cut <- 25     # Baffle Cut %
  
  # Shell Properties
  Shell_ID <- 3.00
  fD_s <- 2.5E+01 
  
  # HEX Calculations --------------------------------------------------------
  Delta_T1 <- HTi - CTf 
  Delta_T2 <- HTf - CTi
  LMTD <- (Delta_T1 - Delta_T2) / log(Delta_T1 / Delta_T2)
  Area <- Cold_dH / (U_Ov * LMTD)
  
  # Tube Side Calculations
  Tube_len <- Area/(pi*Tube_No*Tube_ID)
  Tube_Pt <- 1.25 * Tube_OD
  Tube_Ar <- (pi*Tube_ID^2)/4
  ut <- (Hot_VFlow/3600)/Tube_Ar
  rho_ft <- Hot_MFlow/Hot_VFlow
  
  # Assume Viscosity constant across flow profile
  dPt <- Tube_Ps * (8*fD_t*(Tube_len/Tube_ID) + 2.5)*(rho_ft*ut^2)/2
  
  # Shell Side Calculations
  rho_fs <- Gen_fluid[Gen_fluid$Fluid == "Water","rho_kgm3"]
  Shell_Ar <- (Tube_Pt - Tube_OD) * (Shell_ID*Bfl_spc)/Tube_Pt
  Shell_De <- (1.10/Tube_OD) * (Tube_Pt^2 - 0.917*Tube_OD^2)
  us <- ((Cold_MFlow/3600)/Shell_Ar)/rho_fs
  
  dPs <- 8*fD_s*(Shell_ID/Shell_De)*(rho_fs*us^2)/2
  Cost_pnd = (130*(Area/0.093)^0.78)/0.79
  # Outputting as dataframes
  St_prop[,"dP_Pa"] <- c(dPt,dPs)

  HEX_Props <- data.frame(
    Duty_W = Cold_dH, LMTD_K = LMTD, HAr_m2 = Area,
    Tb_No = Tube_No, TOD_m = Tube_OD, TID_m = Tube_ID, Tbln_m = Tube_len, 
    ShAr_m2 = Shell_Ar, ShID_m = Shell_ID, ShLn_m = Tube_len * 1.1, Cost = Cost_pnd
  )
  
  result <- list("Stream" = St_prop,
                 "HEX" = HEX_Props)
  return(result)
}


# Pump Functions ---------------------------------------------------------------
# Pump Functions ---------------------------------------------------------------
Pump_func <- function(PFluid_props,PumpNo,PumpModel) { 
  # Water Storage Properties
  SuctionP_Pa <- P_atm   # Average Pressure in Water Storage Tanks
  PipeD_m <- 15E-3      # Internal Diameter of Pipeline
  PipeL_m <- 20          # Pipe Length 
  fD <- 0.046E-3           # Darcy Friction Coefficient
  pump_eff <- 0.7        # Estimated Pump Efficiency
  Hst <- 2.5             # Difference in Elevation
  hm <- 0.5              # Minor Head Losses from fittings, expansion
  
  # Fluid Properties------------------------------------------------------------
  dHvp <- Gen_fluid[Gen_fluid$Fluid == "Water","dH_Jmol"] /
    Gen_fluid[Gen_fluid$Fluid == "Water","RMM"]
  CpW <- Gen_fluid[Gen_fluid$Fluid == "Water","Cp"]
  rho_f <- Gen_fluid[Gen_fluid$Fluid == "Water","rho_kgm3"]
  InT_K <- 288           # Ref Temp for Vap Pressure
  Pvap_Pa <- 1689.4      # Vapour Pressure of Water
  
  # Pump Calculatons -----------------------------------------------------------
  # Pumping Fluid Properties
  PFluid <- PFluid_props[PFluid_props$Stream == "Cold","Mf_kghr"]
  dP_Pa <- PFluid_props[PFluid_props$Stream == "Cold","dP_Pa"]
  
  Water_flow <- PFluid/rho_f
  
  # Pressue Calculations 
  hf <- (8 * fD * PipeL_m * (Water_flow/3600)^2)/(pi^2 * gravity * PipeD_m^5)
  Total_Head <- Hst + hf + hm
  DischargeP_Pa <- SuctionP_Pa + (Total_Head*rho_f*gravity)
  dP_Pa <- DischargeP_Pa - SuctionP_Pa
  
  # Pump Power Calculations
  pump_power_req<- (Water_flow*dP_Pa)/3.6E+06  # in kWh
  pump_power_out<- pump_power_req/pump_eff
  
  # NPSH available Calculations
  dT <- pump_power_req*(1-pump_eff)/(CpW*Water_flow*rho_f)       # Increase in Temp
  P_vap <-  log((dHvp/R)*(1/InT_K/(InT_K+dT)))*Pvap_Pa # Vapour Pressure of Water
  vel <- (Water_flow/3600)/(pi*0.25*PipeD_m^2)
  NPSH_av <- (SuctionP_Pa - vel^2 - P_vap)/(rho_f*gravity) 
  Cost <- (6900 + 206 * (Water_flow/3600)^0.9)*(798.7/509.7)
  # Output Data ----------------------------------------------------------------
  Pump_props <- data.frame(
    Property = c("Fluid Flow", "Volumetric Flow", "Power Required", "NPSH avail",
                 "Suction Pressure","Discharge Pressure","Diff Pressure", "Equipment Cost"),
    Unit = c("kg/hr", "m^3/hr", "kWh", "m", 
             "Pa", "Pa", "Pa", "£"),
    Values = c(PFluid, Water_flow, pump_power_out, NPSH_av,
               SuctionP_Pa,DischargeP_Pa,dP_Pa, Cost)
    
  )
  
  # Pump Plot ------------------------------------------------------------------
  # System Curve
  Flow_list <- seq(0.0,40, by = 0.10)/3600
  hf_list <- (8 * fD * PipeL_m * (Flow_list)^2)/(pi^2 * gravity * PipeD_m^5)
  TotalH_list <- Hst + hf_list +hm
  
  
  pump_curve_data <- data.frame(FlowRate = Flow_list*3600, 
                                Sys_Head = TotalH_list)
  
  # Plot the pump curve 
  Pump_Plot <-
    ggplot() +
    geom_line(data = pump_curve_data, mapping = aes(x = FlowRate, y = Sys_Head), color ="blue") +
    geom_line(data = PumpModel, mapping = aes(x = Q_m3hr, y = Head_m, color = as.character(RPM))) +
    geom_dl(data = PumpModel, mapping = aes(x = Q_m3hr, y = Head_m, label = as.character(RPM), color = as.character(RPM)), 
            method = list(dl.combine("top.points")), angle = 0, vjust = 0, na.rm = TRUE, size = 0.1) +
    labs(title = paste0("Pump00",PumpNo," Performance Curve"),
         x = "Flow Rate (m^3/h)",
         y = "Head (m)",
         color = "RPM of Pump") +
    PGDP_theme() + 
    theme(axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          legend.position = "none",
          plot.title =  element_text(size = 11)) +
    coord_cartesian(clip = "off")
  print(Pump_Plot)
  
  PlotSave <- paste0("Pump00",PumpNo,"_Plot.png")
  ggsave(file.path(pic_folder, PlotSave), Pump_Plot, width = 120, height = 90, units = "mm")
  
  return(Pump_props)
}
# Cyclone Calculations -------------------------------------------------------
Cyc_func <- function(Composition,Press,Temp){
  Cyclone_Input <- Composition_props(Composition,Press,Temp)
  
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
  return(Cyc_calcs)
}
# Quench Tower Calculations for ------------------------------------------------
Quench_Stream <- function(St_prop,Solid) {
  HTi <- St_prop[St_prop$Stream=="Hot","Temp_K"]
  CTi <- St_prop[St_prop$Stream=="Cold","Temp_K"]
  
  
  # Solid property ----------------------------------------------------------
  Cps <- Solid_waste[Solid_waste$Solid == Solid,"Cp"]
  
  Solid_props <- Solid_waste[Solid_waste$Solid == Solid,]
  Solid_kghr <- as.numeric(St_prop[St_prop$Stream == "Hot","Mf_kghr"])
  Solid_m3hr <- Solid_kghr / Solid_props[,"rho_kgm3"]
  SmCp <- Solid_kghr * Cps
  Hsi  <- SmCp * HTi
  
  
  # Water Stream Property ---------------------------------------------------
  Cpf <- Gen_fluid[Gen_fluid$Fluid == "Water","Cp"]
  
  Water_kghr <- as.numeric(St_prop[St_prop$Stream == "Cold","Mf_kghr"])
  Water_m3hr <- Water_kghr / Gen_fluid[Gen_fluid$Fluid == "Water","rho_kgm3"]
  WmCp <- Water_kghr * Cpf
  HWi  <- WmCp * CTi
  
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
  
  TowerV_m3 <- 2
  Tr_s <- Mixed_m3hr / TowerV_m3
  
  Tower_props <- data.frame(
    Property = c("Solid Flow", "Water Flow", "Total Vol","Outlet Temp",
                 "Tower Vol", "Residence Time"),
    Unit = c("kghr","kghr","m3hr","K","m3","hr"),
    Value = c(Solid_kghr,Water_kghr,Mixed_m3hr,MT_K,TowerV_m3,Tr_s)
  )
  
  result_df <- list(
    "Stream" = St_prop,
    "Tower" = Tower_props
  )
  return(result_df)
}

# Fan Functions ----------------------------------------------------
fan_func <- function(Composition, Pi_Pa, dP_Pa, Ti, efficiency = 0.7) {
  Pf_Pa <- Pi_Pa + dP_Pa
  Pi_Comp <- Composition_props(Composition,Pi_Pa,Ti)
  Pf_Comp <- Composition_props(Composition,Pf_Pa,Ti)
  
  Vi_m3s <- sum(Pi_Comp$Vol_m3hr)/3600
  Vf_m3s <- sum(Pf_Comp$Vol_m3hr)/3600
  
  Cp <- (sum(Pi_Comp$dH_Jhr)/1000)/sum(Pi_Comp$Mol_kmolhr)
  Cv <- Cp - R
  gamma <- Cp / Cv
  n <- gamma
  m <- (gamma - 1) /(gamma * efficiency)
  
  work_done <- R *  Ti * (n/(n-1)) * ((Pf_Pa/Pi_Pa)^((n-1)/n)-1)
  T2 <- Ti*(Pf_Pa/Pi_Pa)^m
  delta_T <- T2 - Ti
  
  compression_ratio <- Pf_Pa / Pi_Pa
  massflow_kghr <- sum(Pi_Comp$Mass_KGhr)
  
  power_required <- (work_done*massflow_kghr) / 0.95
  electricity_usage <- power_required * JhrtokWh
  Cost <- (3800 + 49 * (Vi_m3s*3600)^0.8)*(798.7/509.7)
  
  fan_props <- data.frame(
    Property = c("Fluid_Flow", "Initial_Volume", "Initial_Pressure",
                 "Final_Volume", "Final_Pressure",
                 "Pressure Drop", "Comp Ratio","dT","Power Required", "Electricity Use" ,"Equipment Cost"),
    Unit = c("kg/hr", "m^3/hr", "Pa", "m^3/hr", "Pa","Pa","","K", "W", "kWh", "£"),
    Values = c(massflow_kghr, Vi_m3s, Pi_Pa, Vf_m3s, Pf_Pa,
               dP_Pa, compression_ratio, delta_T,power_required, electricity_usage, Cost)
  )
  return(fan_props)
}
comp_func <- function(Composition, Vi_m3s, Pi_Pa, dP_Pa, Ti, efficiency = 0.7) {
  Pf_Pa <- Pi_Pa + dP_Pa
  Pi_Comp <- Composition_props(Composition,Pi_Pa,Ti)
  Pf_Comp <- Composition_props(Composition,Pf_Pa,Ti)
  
  Vf_m3s <- sum(Pf_Comp$Vol_m3hr)
  
  Cp <- (sum(Pi_Comp$dH_Jhr)/1000)/sum(Pi_Comp$Mol_kmolhr)
  Cv <- Cp - R
  gamma <- Cp / Cv
  n <- gamma
  m <- (gamma - 1) /(gamma * efficiency)
  
  work_done <- R *  Ti * (n/(n-1)) * ((Pf_Pa/Pi_Pa)^((n-1)/n)-1)
  T2 <- Ti*(Pf_Pa/Pi_Pa)^m
  delta_T <- T2 - Ti
  
  compression_ratio <- Pf_Pa / Pi_Pa
  massflow_kghr <- sum(Pi_Comp$Mass_KGhr)
  
  power_required <- (work_done*massflow_kghr) / 0.95
  electricity_usage <- power_required * JhrtokWh
  Cost <- (490000 + 16800 * electricity_usage^0.6)*(798.7/509.7)
  
  comp_props <- data.frame(
    Property = c("Fluid_Flow", "Initial_Volume", "Initial_Pressure",
                 "Final_Volume", "Final_Pressure",
                 "Pressure Drop", "Comp Ratio","dT","Power Required", "Electricity Use" ,"Equipment Cost"),
    Unit = c("kg/hr", "m^3/hr", "Pa", "m^3/hr", "Pa","Pa","","K", "J/s", "kWh", "£"),
    Values = c(massflow_kghr, Vi_m3s, Pi_Pa, Vf_m3s, Pf_Pa,
               dP_Pa, compression_ratio, delta_T, power_required, electricity_usage, Cost)
  )
  return(comp_props)
}

# Steam Turbine -----------------------------------------------------------

Steam_turbine <- function(steam_mass){
  turbine <- 6100 / 25000
  elec_kWh <- turbine * steam_mass
  return(elec_kWh)
}

