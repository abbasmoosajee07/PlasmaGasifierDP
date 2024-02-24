
# Conversion Function ----------------------------------------------------------
conv_func <- function(Input_composition,O2_feed,O2_purity,Vr,SynG_PI){
    # Initial Conditions -------------------------------------------------------
    Reaction_In <- Input_composition
    Reaction_In[Reaction_In$Component == "Oxygen","Mass_KGhr"] = 
      Reaction_In[Reaction_In$Component == "Oxygen","Mass_KGhr"] + O2_feed
    
    N2_feed <- O2_feed * (1 - O2_purity)
    Reaction_In[Reaction_In$Component == "Nitrogen","Mass_KGhr"] = 
      Reaction_In[Reaction_In$Component == "Nitrogen","Mass_KGhr"] + N2_feed
    
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
    
    times <- seq(0, 1600/1E+8, length.out = 1000) # Time points
    
    # Solve the differential equations ---------------------------------------------
    method = c("lsoda", "lsode", "lsodes", "lsodar", 
               "vode", "daspk", "euler", "rk4", "ode23", "ode45", "radau",
               "bdf", "bdf_d", "adams", "impAdams", "impAdams_d", "iteration")
    
    # solution <- ode(func = rate_eq, y = initial_conditions, times = times, parms = parameters)
    solution <- ode(func = rate_eq, y = initial_conditions, times = times, parms = parameters, 
                    rtol = 1e-6, atol = 1e-6, method = "lsoda")
    
    solution_df <- as.data.frame(solution)
    solution_melted <- tidyr::gather(solution_df, Element, Concentration, -time)

    # Gasifier Output --------------------------------------------------------------
    Reac_out <- t(solution_df[nrow(solution_df)-3,]) 
    
    Gasifier_out <- data.frame("Mols_In" = initial_conditions,
                               "Mols_Out" = (Reac_out[-1]))
    Gasifier_out <- Gasifier_out * timeconv
    Gasifier_out["Ash",] <- c(Reaction_In["Ash", "Mol_kmolhr"],Reaction_In["Ash","Mol_kmolhr"] )
    
    for (sym in rownames(Gasifier_out)){
      sym_elem <- paste0("(",sym,")")
      RMM <- element_props[element_props$Symbol == sym_elem,  "Molar_Mass"]
      Gasifier_out[sym, "Mass_In"] <- Gasifier_out[sym, "Mols_In"] * RMM
      Gasifier_out[sym, "Mass_Out"] <- Gasifier_out[sym, "Mols_Out"] * RMM
    }
    Gasifier_out[,"Conv"] <- 1 - (Gasifier_out[,"Mols_Out"] / Gasifier_out[,"Mols_In"])
    # cat("Mass In  =", sum(Gasifier_out[,"Mass_In"]),"\n")
    # cat("Mass Out =", sum(Gasifier_out[,"Mass_Out"]),"\n")
    
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
        Gasifier_out[mass_columns[i], "Mass_Out"]
    }
    OutComp_n <- Composition_props(OutComp_n,ReacPress_Out,Reac_temp)
    return(OutComp_n)
}


# test <- conv_func(Input_composition,O2_feed,O2_purity,Vr,SynG_PI)
O2_purity <- 0.99
O2_feed <- 7500    # Minimum O2 req is 6300kg of O2
Vr <- 15
ReacPress_Out <- SynG_PI

Case_O2 <- data.frame(O2_list = seq(5300, 6300, by = 50))
for (nO in 1:nrow(Case_O2)){
  O2_feed <- Case_O2[nO,"O2_list"]
  Conv_O2 <- conv_func(Input_composition,O2_feed,O2_purity,Vr,SynG_PI)
  Case_O2[nO,"CO2"] <- as.numeric(Conv_O2["Carbon_Dioxide","Mass_KGhr"])
  # Case_O2[nO,"H2"] <- as.numeric(Conv_O2["Hydrogen","Mass_KGhr"])
  # Case_O2[nO,"NO"] <- as.numeric(Conv_O2["Nitric_Oxide","Mass_KGhr"])
}

O2_df <- tidyr::gather(Case_O2, Element, Concentration, -O2_list)

CSO2_plot <-
  ggplot() +
  geom_line(data = O2_df, aes(x = O2_list, y = Concentration, color = Element), na.rm = TRUE) +
  geom_dl(data = O2_df, mapping = aes(x = O2_list, y = Concentration, label = Element, 
                                                color = Element), method = list(dl.combine("last.points")), angle = 0, vjust = 0, size=1e-7, na.rm = TRUE) +
  labs(x = "O2 Feed", y = "Mass FlowRate(kg/hr)", title = "Effect of Oxygen Feed on Carbon Dioxide Production") +
  PGDP_theme() +
  theme(legend.position = "None") +
  coord_cartesian(clip = "off") 

print(CSO2_plot)
ggsave(file.path(pic_folder, "CSO2.png"), CSO2_plot, width = 6, height = 4, units = "in")
warning()

