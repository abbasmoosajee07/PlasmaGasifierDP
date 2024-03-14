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
  
  if (CTf >= element_props[element_props$Component == "Water","Tb_K"]){
    dHv <- Gen_fluid[Gen_fluid$Fluid == "Water","dH_Jmol"]
    result_water   <- state_func(main_elements$Water,CTi,CTf) + dHv
    
  } else {
    result_water   <- state_func(main_elements$Water,CTi,CTf)
  }
  Hot_dH <- -sum(Flow_Composition[,"dH_Jhr"])
  Cold_dH <- Hot_dH / HEX_eff
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
Pump_func <- function(PFluid_props) { 
  # Water Storage Properties
  SuctionP_Pa <- P_atm   # Average Pressure in Water Storage Tanks
  PipeD_m <- 80E-03      # Internal Diameter of Pipeline
  PipeL_m <- 50          # Pipe Length 
  fD <- 0.0027           # Darcy Friction Coefficient
  pump_eff <- 0.8        # Estimated Pump Efficiency
  Hst <- 2.5             # Difference in Elevation
  hm <- 2.5              # Minor Head Losses from fittings, expansion
  
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
  Flow_list <- seq(0.0, Water_flow*3, by = 0.10)/3600
  hf_list <- (8 * fD * PipeL_m * (Flow_list)^2)/(pi^2 * gravity * PipeD_m^5)
  TotalH_list <- Hst + hf_list +hm
  
  # Pump Curve
  RPM <- 1
  ImpD_m <- 1
  CQ <- Flow_list/(RPM * ImpD_m^3)
  CH <- 8 - (2 * CQ) - (210 * CQ^2)#20 + 300*Flow_list^2#
  PH_list <- (CH*RPM^2 * ImpD_m^2) / gravity
  pump_curve_data <- data.frame(FlowRate = Flow_list*3600, 
                                Sys_Head = TotalH_list, 
                                Pump_Head = PH_list)
  
  # Plot the pump curve 
  Pump_Plot <-
    ggplot() +
    geom_line(data = pump_curve_data, mapping = aes(x = FlowRate, y = Sys_Head), color ="blue") +
    geom_line(data = pump_curve_data, mapping = aes(x = FlowRate, y = Sys_Head), color = "red") +
    labs(title = "Pump Performance Curve",
         x = "Flow Rate (m^3/h)",
         y = "Head (m)") +
    PGDP_theme()
  print(Pump_Plot)
  return(Pump_props)
}


# Quench Tower Calculations for ------------------------------------------------
CoolingT_Stream <- function(St_prop,Solid, HEX_eff = 0.9) {
  HTi <- St_prop[St_prop$Stream=="Hot","TempIn_K"]
  HTf <- St_prop[St_prop$Stream=="Hot","TempOut_K"]
  CTi <- St_prop[St_prop$Stream=="Cold","TempIn_K"]
  CTf <- St_prop[St_prop$Stream=="Cold","TempOut_K"]
  
  Solid_props <- Solid_waste[Solid_waste$Solid == Solid,]
  Solid_MFlow <- as.numeric(St_prop[St_prop$Stream == "Hot","Mf_kghr"])
  Solid_VFlow <- Solid_MFlow / Solid_props[,"rho_kgm3"]
  
  Hot_dH <- Solid_MFlow * Solid_props[,"Cp"] * abs(HTf - HTi)
  result_water   <- state_func(main_elements$Water,CTi,CTf)
  
  Cold_dH <- Hot_dH / HEX_eff
  water_molflow <- Cold_dH / result_water$Enthalpy
  water_massflow <- water_molflow * element_props[element_props$Component == "Water","Molar_Mass"]
  water_volflow <- water_massflow / Gen_fluid[Gen_fluid$Fluid == "Water","rho_kgm3"]
  
  St_prop[,"Mf_kghr"] <- c(Solid_MFlow,water_massflow)
  St_prop[,"Vol_m3hr"] <- c(Solid_VFlow,water_volflow)
  St_prop[,"dH_Jhr"] <- c(Hot_dH,Cold_dH)
  St_prop[,"dP_Pa"] <- c(0,0)
  
  return(St_prop)
}





# Compressor Functions ----------------------------------------------------
fan_func <- function(Composition, Pi_Pa, dP_Pa, Ti, efficiency = 0.8) {
  Pf_Pa <- Pi_Pa + dP_Pa
  Pi_Comp <- Composition_props(Output_composition,Pi_Pa,Ti)
  Pf_Comp <- Composition_props(Output_composition,Pf_Pa,Ti)
  
  Vi_m3hr <- sum(Pi_Comp$Vol_m3hr)
  Vf_m3hr <- sum(Pf_Comp$Vol_m3hr)
  
  Cp <- (sum(Pi_Comp$dH_Jhr)/1000)/sum(Pi_Comp$Mol_kmolhr)
  Cv <- Cp - R
  gamma <- Cp / Cv

    # Calculate work done using the formula: W = (P2 * V2 - P1 * V1) / (gamma - 1)
  work_done <- (Pf_Pa * Vf_m3hr - Pi_Pa * Vi_m3hr) / (gamma - 1)
  
  # Calculate change in enthalpy
  delta_h <- work_done
  
  # Calculate change in temperature using ideal gas law: P1 * V1 / T1 = P2 * V2 / T2
  delta_T <- ((Pf_Pa * Vf_m3hr) / (Pi_Pa * Vi_m3hr)) - 1
  delta_T <- delta_T * (gamma - 1) * ((1 / Vi_m3hr) - (1 / Vf_m3hr))
  
  # Calculate compression ratio
  compression_ratio <- Pf_Pa / Pi_Pa
  
  # Calculate mass flow rate assuming steady flow
  massflow_kghr <- sum(Pi_Comp$Mass_KGhr)
  
  # Calculate power required by the compressor
  power_required <- work_done / efficiency
  
  # Calculate electricity usage
  electricity_usage <- power_required * JhrtokWh# Convert from J/s to kWh
  Cost <- (3800 + 49 * Vi_m3hr^0.8)*(798.7/509.7)
  
  fan_props <- data.frame(
    Property = c("Fluid_Flow", "Initial_Volume", "Initial_Pressure",
                 "Final_Volume", "Final_Pressure",
                 "Pressure Drop", "Power Required", "Electricity Use" ,"Equipment Cost"),
    Unit = c("kg/hr", "m^3/hr", "Pa", "m^3/hr", "Pa","Pa", "kWh", "kWh", "£"),
    Values = c(massflow_kghr, Vi_m3hr, Pi_Pa, Vf_m3hr, Pf_Pa,
               dP_Pa, power_required, electricity_usage, Cost)
  )
  return(fan_props)
}

# Steam Turbine -----------------------------------------------------------

Steam_turbine <- function(steam_mass){
  turbine <- 12500 / 54000
  elec_kWh <- turbine * steam_mass
  return(elec_kWh)
}
