
# Conversion Function ----------------------------------------------------------
conv_func <- function(Input_composition,O2_feed,O2_purity,Vr,SynG_PI,Water){
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
    
    k_list <- paste0("k",rate_constants$RNo)
    k_values <- setNames(rate_constants$k_ovt, k_list)
    k_values <- k_values[k_list]
    parameters <- k_values 
    
    times <- seq(0, 1600/1E+9, length.out = 1000) # Time points
    
    # Solve the differential equations ---------------------------------------------
    method = c("lsoda", "lsode", "lsodes", "lsodar", 
               "vode", "daspk", "euler", "rk4", "ode23", "ode45", "radau",
               "bdf", "bdf_d", "adams", "impAdams", "impAdams_d", "iteration")
    
    # solution <- ode(func = rate_eq, y = initial_conditions, times = times, parms = parameters)
    conc_solution <- ode(func = rate_eq, y = initial_conditions, times = times, parms = parameters, 
                    rtol = 1e-6, atol = 1e-6, method = "lsode")
    conc_df <- as.data.frame(conc_solution)
    conc_melted <- tidyr::gather(conc_df, Element, Concentration, -time)

    # Gasifier Output --------------------------------------------------------------
    Reac_out <- t(conc_df[nrow(conc_df)-3,]) 
    
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

Vr <- 15
ReacPress_Out <- SynG_PI

O2_plist = seq(0.2,  1.0, by = 0.1)
O2_flist = seq(Plastic_kghr, Plastic_kghr*5, by = 1000)
Case_O2 <- data.frame()

for (nOp in 1:length(O2_plist)){
  for (nO in 1:length(O2_flist)){
    O2_purityn <- O2_plist[nOp]
    O2_feedn <- O2_flist[nO]
    Conv_O2 <- conv_func(Input_composition,O2_feedn,O2_purityn,Vr,SynG_PI,H2O_feed)
    Case_O2n <- data.frame(O2_feed = O2_feedn, O2_purity = O2_purityn, 
                           Hydrogen  = as.numeric(Conv_O2["Hydrogen","Mass_KGhr"]),
                           Nitric_Oxide  = as.numeric(Conv_O2["Nitric_Oxide","Mass_KGhr"]), 
                           Carbon_Dioxide = as.numeric(Conv_O2["Carbon_Dioxide","Mass_KGhr"]),
                           Oxygen  = as.numeric(Conv_O2["Oxygen","Mass_KGhr"]),
                           Water = as.numeric(Conv_O2["Water","Mass_KGhr"]),
                           Carbon   = as.numeric(Conv_O2["Carbon","Mass_KGhr"])
                           )
    Case_O2  <- rbind(Case_O2, Case_O2n)
    
  }
 }

O2_df <- tidyr::gather(Case_O2, Element, Concentration, -O2_purity, -O2_feed)
O2_df$Eq_ratio <- O2_df$O2_feed/Plastic_kghr
CSO2_plot <-
  ggplot() +
  geom_line(data = O2_df, aes(x = Eq_ratio, y = Concentration, group = O2_purity,
                              color = as.character(O2_purity)), na.rm = TRUE) +
  labs(x = "Equivalence Ratio of Oxygen to Plastic", 
       y = "Component FlowRate(kg/hr)", 
       color = "Purity",
       title = "Effect of Oxygen Feed on Syngas Compoosition") +
  PGDP_theme() +
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.position = "top",
        legend.title = element_text(size = 11),
        legend.text  = element_text(size = 11) ) +
  guides(color = guide_legend(nrow = 1)) +
  facet_wrap(~ Element, ncol = 2, scales = "free_y") +
  coord_cartesian(clip = "off")
print(CSO2_plot)
# ggsave(file.path( pic_folder, "CSO2.png"), CSO2_plot, width = 200, height = 160, units = "mm")
warning()

