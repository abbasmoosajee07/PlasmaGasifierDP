source("rate_eq_calc.R")    # Calculating Rate Equations and MB
source("rate_constants.R")  # Calculate Rate Constants
# Initial conditions -----------------------------------------------------------
timeconv <- 3600
# Define initial conditions
O2_purity <- 0.99
O2_feed <- 7500    # Minimum O2 req is 6300kg of O2
N2_feed <- O2_feed * (1 - O2_purity)

Reaction_In <- Input_composition
Reaction_In[Reaction_In$Component == "Oxygen","Mass_KGhr"] = 
  Reaction_In[Reaction_In$Component == "Oxygen","Mass_KGhr"] + O2_feed
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
# k_values <- setNames(reaction_props$K_val, k_list)
k_values <- k_values[k_list]
parameters <- k_values 

times <- seq(0, 1600/1E+8, length.out = 1000) # Time points
Vr = 15

# Solve the differential equations ---------------------------------------------
# Solve the ODE  ---------------------------------------------------------------

method = c("lsoda", "lsode", "lsodes", "lsodar", 
           "vode", "daspk", "euler", "rk4", "ode23", "ode45", "radau",
           "bdf", "bdf_d", "adams", "impAdams", "impAdams_d", "iteration")

# solution <- ode(func = rate_eq, y = initial_conditions, times = times, parms = parameters)
solution <- ode(func = rate_eq, y = initial_conditions, times = times, parms = parameters, 
                rtol = 1e-6, atol = 1e-6, method = "lsoda")

# Convert solution to data frame
solution_df <- as.data.frame(solution)

# Melt the data frame for plotting
solution_melted <- tidyr::gather(solution_df, Element, Concentration, -time)
solution_melted$Concentration <- abs(solution_melted$Concentration)

# Conversion Plot --------------------------------------------------------------
Conv_plot <-
  ggplot() +
  geom_line(  data = solution_melted, aes(x = time, y = Concentration, color = Element), na.rm = TRUE) +
  geom_dl(data = solution_melted, mapping = aes(x = time, y = Concentration, label = Element,
                                                color = Element), method = list(dl.combine("last.points")), angle = 0, vjust = 0, size=1e-7, na.rm = TRUE) +
  labs(x = "Time", y = "Concentration", title = "Concentration vs Time") +
  PGDP_theme() +
  theme(legend.position = "None") +
  coord_cartesian(clip = "off") 

print(Conv_plot + xlim(0,2.5E-6))
# print(Conv_plot + facet_wrap(~ Element) + xlim(0,2.5E-6))

# ggsave(file.path(pic_folder, "ConvPlot.png"), Conv_plot, width = 6, height = 4, units = "in")

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
Gasifier_out <- round(Gasifier_out,3)

# Check if sums are not equal
if (sum(Gasifier_out[,"Mass_In"]) != sum(Gasifier_out[,"Mass_Out"])) {
  warning("Mass in and mass out values do not balance!")
}
cat("Mass In  =", sum(Gasifier_out[,"Mass_In"]),"\n")
cat("Mass Out =", sum(Gasifier_out[,"Mass_Out"]),"\n")

# Output to dataframe ----------------------------------------------------------
Output_composition <- data.frame(
  Component = c("Ash","Water","Carbon","Nitrogen","Oxygen","Carbon_Monoxide","Hydrogen",
                "Carbon_Dioxide","Methane","Nitric_Oxide","Nitrogen_Dioxide","Sulfur",
                "Sulfur_Dioxide","Chlorine","Hydrogen_Chloride")
)
Output_composition[,c("Mass_KGhr","Mol_kmolhr","Vol_m3hr","dH_Jhr")] <- 0

for (i in seq_along(sym_df$Component)) {
  components <- sym_df$Component
  mass_columns <- gsub("\\(|\\)", "", sym_df$Symbol)
  Output_composition$Mass_KGhr[Output_composition$Component == components[i]] <- 
    Gasifier_out[mass_columns[i], "Mass_Out"]
}
Output_composition <- Composition_props(Output_composition,SynG_PI,Reac_temp)

Tr = Vr / sum(Output_composition$Vol_m3hr)

Gasifir_Prop <- data.frame(
  Temp_K = Reac_temp,
  Press_MPa = SynG_PI/1E+6,
  InputFeed_kghr = sum(Input_composition$Mass_KGhr),
  OxygenPurity = O2_purity,
  Oxygen_kghr = O2_feed,
  GasifierVol_m3 = Vr,
  ResidenceTime_s = Tr * 3600
)
view_table(t(Gasifir_Prop),5)
