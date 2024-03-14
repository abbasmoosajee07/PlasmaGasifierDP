# =============================================================================
# conv_calc.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Use an LSODE solver with deSolve package to calculate steady state 
# conversions of the gasifier  
#
# =============================================================================

source("rate_eq_calc.R")    # Calculating Rate Equations and MB
source("rate_constants.R")  # Calculate Rate Constants

# Define Initial conditions ---------------------------------------------------
#  initial conditions
O2_purity <- 0.77
O2_feed <- 9000    # Minimum O2 req is 6300kg of O2
N2_feed <- O2_feed * (1 - O2_purity)
H2O_feed <- 5000

# Gas Injection 
O2N2_purity <- matrix(c(0.99, 0.21,
                        0.01, 0.79), nrow = 2, byrow = TRUE)
Desired_feed <- c(O2_feed, N2_feed)

IndAir_breakdown <- (solve(O2N2_purity,Desired_feed))

Input_gas <- data.frame(
  Oxygen =c(IndAir_breakdown[1]*0.99,IndAir_breakdown[2]*0.21,0,O2_feed),
  Nitrogen = c(IndAir_breakdown[1]*0.01,IndAir_breakdown[2]*0.79,0,N2_feed),
  Steam = c(0,0,H2O_feed,H2O_feed)
)
rownames(Input_gas) <- c("Pure Oxygen", "Air", "Syngas Steam","Total")

# Setting up Initial Conditions for ODE System ---------------------------------
Reaction_In <- Input_composition
Reaction_In[Reaction_In$Component == "Oxygen","Mass_KGhr"] = 
  Reaction_In[Reaction_In$Component == "Oxygen","Mass_KGhr"] + O2_feed
Reaction_In[Reaction_In$Component == "Nitrogen","Mass_KGhr"] = 
  Reaction_In[Reaction_In$Component == "Nitrogen","Mass_KGhr"] + N2_feed
Reaction_In[Reaction_In$Component == "Water","Mass_KGhr"] = H2O_feed +
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
# k_values <- setNames(reaction_props$K_val, k_list)
k_values <- k_values[k_list]
parameters <- k_values 

times <- seq(0, 1.4E-6, by = 1E-9) # Time points
Vr = 15

# Solve the ODE  ---------------------------------------------------------------

method = c("lsoda", "lsode", "lsodes", "lsodar", 
           "vode", "daspk", "euler", "rk4", "ode23", "ode45", "radau",
           "bdf", "bdf_d", "adams", "impAdams", "impAdams_d", "iteration")

# solution <- ode(func = rate_eq, y = initial_conditions, times = times, parms = parameters)
conc_sol <- ode(func = rate_eq, y = initial_conditions, times = times, parms = parameters, 
                rtol = 1e-6, atol = 1e-6, method = "lsode")

# Convert solution to data frame
conc_df <- as.data.frame(conc_sol)

# Melt the data frame for plotting
conc_melted <- tidyr::gather(conc_df[,c("time","C","O2","CO2","H2O","H2","N2","NO")], Element, Concentration, -time)

# Conversion Plot --------------------------------------------------------------
Conv_plot <-
  ggplot() +
  geom_line(  data = conc_melted, aes(x = time, y = Concentration, color = Element), na.rm = TRUE) +
  geom_dl(data = conc_melted, mapping = aes(x = time, y = Concentration, label = Element,
                                                color = Element), method = list(dl.combine("last.points")), angle = 0, vjust = 0, size=1e-7, na.rm = TRUE) +
  labs(x = "Time(s)", y = "Molar Concentration(kmol/s)", title = "Reaction Progression after plasmafication") +
  PGDP_theme() +
  theme(legend.position = "None") +
  coord_cartesian(clip = "off") 

print(Conv_plot)
ggsave(file.path(pic_folder, "ConvPlot.png"), Conv_plot, width = 170, height = 120, units = "mm")

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
# Gasifier_MB <- round(Gasifier_MB,3)

# Check if sums are not equal
if (abs(sum(Gasifier_MB[,"Mass_In"]) - sum(Gasifier_MB[,"Mass_Out"])) >= 1E-2) {
  warning("Mass in and mass out values do not balance!")
}
cat("Mass In  =", sum(Gasifier_MB[,"Mass_In"]),"\n")
cat("Mass Out =", sum(Gasifier_MB[,"Mass_Out"]),"\n")

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
    Gasifier_MB[mass_columns[i], "Mass_Out"]
}
Output_composition <- Composition_props(Output_composition,SynG_PI,Reac_temp)

Gasifier_MB$Component <- rownames(Gasifier_MB)
Gasifier_MB$MolFrac_In <- Gasifier_MB$Mols_In/sum(Gasifier_MB$Mols_In)
Gasifier_MB$MolFrac_Out <- Gasifier_MB$Mols_Out/sum(Gasifier_MB$Mols_Out)

MolFrac_df <- Gasifier_MB[c("C","O2","CO2","H2O","H2","N2","NO"),]
MolFrac_df <- pivot_longer(MolFrac_df, cols = c(MolFrac_In, MolFrac_Out),
                          names_to = "Variable", values_to = "Value")

MB_plot <- ggplot() +
  geom_bar(data = MolFrac_df, mapping = aes(y = Value, fill = Variable, x = Component), 
           stat = "identity", position = "dodge", alpha = 0.7) +
  scale_fill_manual(name = "Mole Fraction", 
                     values = c("red","green"), 
                     labels = c("Inlet","Outlet")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(title = "Comparing Component Mole Fractions",
       x = "Component",
       y = "Mass Flow (kg/hr)") +
  PGDP_theme() +
  theme(legend.position = "top",
        legend.title = element_text(size = 11),
        legend.text  = element_text(size = 11),
        axis.text.x = element_text(angle = 0, hjust = 0.5))+
  guides(fill = guide_legend(nrow = 1))

print(MB_plot)
# ggsave(file.path(pic_folder, "MBPlot.png"), MB_plot, width = 170, height = 70, units = "mm")

