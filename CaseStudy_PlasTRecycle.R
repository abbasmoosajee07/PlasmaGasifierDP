# =============================================================================
# CaseStudy_PlasTRecycle.R.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Case Study on the effect of varying recycled water on oxygen 
#   requiremntes
#
# =============================================================================

# Conversion Function ----------------------------------------------------------
convW_func <- function(Input_composition,O2_feed,O2_purity,Vr,Press_Pa,Water){
  # Initial Conditions -------------------------------------------------------
  Reaction_In <- Input_composition
  Reaction_In[Reaction_In$Component == "Oxygen","Mass_KGhr"] = 
    Reaction_In[Reaction_In$Component == "Oxygen","Mass_KGhr"] + O2_feed
  
  N2_feed <- O2_feed * (1 - O2_purity)
  Reaction_In[Reaction_In$Component == "Nitrogen","Mass_KGhr"] = 
    Reaction_In[Reaction_In$Component == "Nitrogen","Mass_KGhr"] + N2_feed
  
  Reaction_In[Reaction_In$Component == "Water","Mass_KGhr"] = Water +
    Reaction_In[Reaction_In$Component == "Water","Mass_KGhr"]
  Reaction_In <- Composition_props(Reaction_In,SynG_PI,Reac_temp)

  initial_conditions <- c(
    C   = Reaction_In["Carbon", "Mol_kmolhr"],          # Carbon
    O2  = Reaction_In["Oxygen", "Mol_kmolhr"],          # Oxygen
    CO  = 0.0,                                          # Carbon Monoxide
    CO2 = 0.0,                                          # Carbon Dioxide
    H2O = Reaction_In["Water", "Mol_kmolhr"],           # Water
    H2  = Reaction_In["Hydrogen", "Mol_kmolhr"],        # Hydrogen
    CH4 = 0.0,                                          # Methane
    N2  = Reaction_In["Nitrogen", "Mol_kmolhr"],        # Nitrogen
    NO  = 0.0,                                          # Nitric Oxide
    NO2 = 0.0,                                          # Nitrogen Dioxide
    S   = Reaction_In["Sulfur", "Mol_kmolhr"],          # Sulfur
    SO2 = 0.0,                                          # Sulfur Dioxide
    Cl2 = Reaction_In["Chlorine", "Mol_kmolhr"],        # Chlorine
    HCl = 0.0                                           # Hydrogen Chloride
  ) / timeconv  # Convert from mol/hr to mol/min
  
  k_values <- setNames(rate_constants$k_ovt, k_list)
  k_values <- k_values[k_list]
  parameters <- k_values 
  times <- seq(0, 1600/1E+9, length.out = 1000) # Time points
  
  # Solve the differential equations ---------------------------------------------
  # solution <- ode(func = rate_eq, y = initial_conditions, times = times, parms = parameters)
  conc_solution <- ode(func = rate_eq, y = initial_conditions, times = times, parms = parameters, 
                       rtol = 1e-6, atol = 1e-6, method = "lsode")
  conc_df <- as.data.frame(conc_solution)
  conc_melted <- tidyr::gather(conc_df, Element, Concentration, -time)

  # Gasifier Output --------------------------------------------------------------
  Reac_out <- t(conc_df[nrow(conc_df)-0,]) 

  
  Gasifier_MB <- data.frame("Mols_In" = initial_conditions,
                            "Mols_Out" = (Reac_out[-1]))
  Gasifier_MB <- Gasifier_MB * timeconv
  Gasifier_MB["Ash",] <- c(Reaction_In["Ash", "Mol_kmolhr"],Reaction_In["Ash","Mol_kmolhr"] )
  
  for (sym in rownames(Gasifier_MB)){
    sym_elem <- paste0("(",sym,")")
    RMM <- element_props[element_props$Symbol == sym_elem,  "Molar_Mass"]
    Gasifier_MB[sym, "Mass_In"] <- Gasifier_MB[sym, "Mols_In"] * RMM
    Gasifier_MB[sym, "Mass_Out"] <- Gasifier_MB[sym, "Mols_Out"] * RMM
  }
  Gasifier_MB[,"Conv"] <- 1 - (Gasifier_MB[,"Mols_Out"] / Gasifier_MB[,"Mols_In"])

  # Output to dataframe ----------------------------------------------------------
  OutComp_n <- data.frame(
    Component = c("Ash","Water","Carbon","Nitrogen","Oxygen","Carbon_Monoxide","Hydrogen",
                  "Carbon_Dioxide","Methane","Nitric_Oxide","Nitrogen_Dioxide","Sulfur",
                  "Sulfur_Dioxide","Chlorine","Hydrogen_Chloride")
  )
  OutComp_n[,c("Mass_KGhr","Mol_kmolhr","Vol_m3hr","dH_Jhr")] <- 0
  
  for (i in seq_along(sym_df$Component)) {
    components <- sym_df$Component
    mass_columns <- gsub("\\(|\\)", "", sym_df$Symbol)
    OutComp_n$Mass_KGhr[OutComp_n$Component == components[i]] <- 
      Gasifier_MB[mass_columns[i], "Mass_Out"]
  }
  OutComp_n <- Composition_props(OutComp_n,ReacPress_Out,Reac_temp)

  return(OutComp_n)
}

ReacPress_Out <- SynG_PI

# W_plist = seq(0.5,  0.9, by = 0.1)
W_flist = seq(1, 100000, by = 10000)

Case_W <- data.frame()
O2_purity <- 0.77
O2_feed <- 9000    # Minimum O2 req is 6300kg of O2


for (nW in 1:length(W_flist)){
  W_feedn <- W_flist[nW]
  Conv_W <- convW_func(Input_composition,O2_feed,O2_purity,Vr,ReacPress_Out, W_feedn)
  TE <- sum(Composition_props(Conv_W,PG_Press,Reac_temp,723)$dH_Jhr)
  water_properties <- state_func(main_elements$Water, 288, 700 )
  water_molar_flow <- TE / (water_properties$Enthalpy + 40.7E+3)
  water_mass_flow <- water_molar_flow * element_props[element_props$Component == "Water", "Molar_Mass"]
  useful_steam <- water_mass_flow - W_feedn
  elec_kWh <- Steam_turbine(useful_steam)
  
  Case_Wn <- data.frame(W_feed = W_feedn, Elec = elec_kWh,
                        Hydrogen  = as.numeric(Conv_W["Hydrogen","Mass_KGhr"]),
                         # Oxygen  = as.numeric(Conv_W["Oxygen","Mass_KGhr"]),
                         # Water = as.numeric(Conv_W["Water","Mass_KGhr"]),
                         CoolingSteam = useful_steam
  )
  Case_W  <- rbind(Case_W, Case_Wn)
  
}


W_df <- tidyr::gather(Case_W, Element, Concentration, -Elec, -W_feed)
W_df$Eq_ratio <- W_df$W_feed/Plastic_kghr
CSW_plot <-
  ggplot() +
  geom_line(data = W_df, aes(x = W_feed, y = Concentration,
                              color = (Elec)), na.rm = TRUE) +
  labs(x = "Water Flowrate into gasifier(kg/hr)", 
       y = "Hydrogen FlowRate(kg/hr)", 
       color = "Elec(kWh)",
       title = "Effect of Water Feed on Hydrogen yield") +
  PGDP_theme() +
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.position = "right",
        legend.title = element_text(size = 11),
        legend.text  = element_text(size = 11) ) +
  facet_wrap(~ Element, ncol = 2, scales = "free_y") +
  coord_cartesian(clip = "off")

print(CSW_plot)
ggsave(file.path( pic_folder, "CSW.png"), CSW_plot, width = 170, height = 100, units = "mm")
warning()

