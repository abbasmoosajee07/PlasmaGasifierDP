
sym_df <- element_props[,c("Component","Symbol","Reac_phase")]

rate_func <- function(reaction_df) {
  reaction <- reaction_df[,(comp_start):ncol(reaction_df)-1]
  k_Rno <- paste0("(k",reaction_df[,"RNo"],")")
  rate_Rno <- data.frame()
  for (nc in 1:ncol(reaction)){
    elem <- data.frame(colnames(reaction))[nc,]
    comp <- reaction[,nc]
    if (comp < 0){
      sym <- sym_df$Symbol[sym_df$Component == elem]
      phase <- sym_df$Reac_phase[sym_df$Component == elem]
      
      if (phase == "Gas"){
        sym <- sym
      } else {
        sym <- sym
      }
      
      rate <- paste0("(",sym,"^",abs(comp),")*V")
    } else {
      rate = "(1)"
    }
    rate_Rno <- rbind(rate_Rno,rate)
    colnames(rate_Rno) <- c("comp")
  }
  
  k_eq <- rate_Rno %>% 
    summarise(col = paste(comp, collapse = "*"))
  k_eq <- yac_str(paste0("Simplify",k_Rno,"*",k_eq))
  # print(k_eq)
  return(k_eq)
}

comp_start <- 4

rate_eqs <- reaction_data %>%
  select(-18, -19)
rate_eqs$Rate_eq <- NA
rate_eqs <- rate_eqs[, c(1, 2, 3, ncol(rate_eqs), (comp_start):(ncol(rate_eqs)-1))]
for (n in 1: nrow(reaction_data)) {
  k_eqn <- rate_func(reaction_data[n,])
  rate_eqs[n,"Rate_eq"] <- k_eqn
  for (nc in (comp_start+1):(ncol(rate_eqs))){
    eqn <- (as.numeric(rate_eqs[n,nc]))
    eqn_req <- yac_str(paste("Simplify((",k_eqn,")","*",eqn,")"))
    rate_eqs[n,nc] <- eqn_req
  }
}


Overall_MB <- data.frame()
for (column_index in (comp_start+1):(ncol(rate_eqs))) {
  current_column <- rate_eqs[, column_index]
  equation_to_simplify <- paste(current_column, collapse = "+")
  simplified_equation <- yac_str(paste("Simplify(",equation_to_simplify,")"))
  Elem <- data.frame(colnames(rate_eqs))[column_index,]
  sym <- sym_df$Symbol[sym_df$Component == Elem]
  paste0("(",sym,"_0 -",simplified_equation,"*t)")
  simplified_equation <- paste0("(",simplified_equation,")")
  # print(simplified_equation)
  
  MB_eq <- paste0("(",sym,"_0 -",simplified_equation,"*t)")
  Elem_MB <- data.frame("Elem"=Elem,"Rate_eq"=simplified_equation,"MB_eq"=MB_eq)
  Overall_MB <- rbind(Overall_MB,Elem_MB)
}


# Rate Constants ----------------------------------------------------------

rate_constants <- rate_eqs[,c(1,3,4)]
rate_constants[,c("k_fwd","k_bwd","k_ovt")] <- 1
# Different Rate Constant Eqs ---------------------------------------------

rate_constants[1, c("k_fwd","k_bwd")] <- c("5.96E+02 * Tp * exp(-1800/(T))", "1")
rate_constants[2, c("k_fwd","k_bwd")] <- c("1E+15 * Tp * exp(-16000/(T))", "1")
rate_constants[3, c("k_fwd","k_bwd")] <- c("8.71E+3 * exp(-17967/Tp)", "1")
rate_constants[4, c("k_fwd","k_bwd")] <- c("2.20E+09 * exp(-109000/R*T)", "1")
rate_constants[5, c("k_fwd","k_bwd")] <- c("1.272*T*exp(-22645/T)", "1.044E-4*(T^2)*exp((-6319/T)-17.29)")
rate_constants[6, c("k_fwd","k_bwd")] <- c("2.50E+5 * exp(-16600/T)", "2.38E+3*T*exp(-16600/T)")
rate_constants[7, c("k_fwd","k_bwd")] <- c("3E+5*exp(-15042/T)", "1")
rate_constants[8, c("k_fwd","k_bwd")] <- c("1.368E-3*T*exp((-8078/T)-7.087)", "0.151*(T^0.5)*exp((-13578/T)-0.372)")
rate_constants[9, c("k_fwd","k_bwd")] <- c("3E+5*exp(-15042/T)", "1")
rate_constants[10,c("k_fwd","k_bwd")] <- c("1.272*T*exp(-22645/T)", "1.044E-4*(T^2)*exp((-2363/T)-20.92)")
rate_constants[11,c("k_fwd","k_bwd")] <- c("5.9E+9*T*exp(-6280/(R*T))", "1")
rate_constants[12,c("k_fwd","k_bwd")] <- c(paste(reaction_props[12,"K_val"]), "1")
rate_constants[13,c("k_fwd","k_bwd")] <- c(paste(reaction_props[13,"K_val"]), "1")
rate_constants[14,c("k_fwd","k_bwd")] <- c(paste(reaction_props[14,"K_val"]), "1")


# Evaluating Rate Constant at various Temps -------------------------------

k_func <- function(equation_text, R, Tp, T) {
  eval(parse(text = equation_text))
}

k_temp <- function(n,Tpn,Tn){
  kn_f <- k_func(rate_constants[n,"k_fwd"], R, Tpn, Tn)
  kn_b <- k_func(rate_constants[n,"k_bwd"], R, Tpn, Tn)
  kn_t <- kn_f / kn_b
  # print(c(kn_f,kn_b,kn_t))
  return(kn_t)
}

temp_list <- (seq(1500,6000, by = 100))
k_df <- data.frame()
for (n in 1:nrow(rate_constants)){
  for (nT in 1:length(temp_list)){
    Tn <- temp_list[nT]
    k_df[n,nT] <- k_temp(n,Tn,Tn)
  }
}

k_df["Temp",] <- temp_list
k_df <- data.frame(t(k_df))
k_df <- pivot_longer(k_df, cols = -Temp, names_to = "Variable", values_to = "Value")
k_df <- k_df %>% 
  mutate(Variable = gsub("X", "K", Variable))
k_df[,"lnk"] <- log(k_df$Value)

k_plot <-   
  ggplot() +
  geom_line(  data = k_df, aes(x = Temp, y = lnk, color = Variable), na.rm = TRUE) +
  geom_dl(data = k_df, mapping = aes(x = Temp, y = lnk, color = Variable, label = Variable), 
          method = list(dl.combine("last.points")), angle = 0, vjust = 0, size=1e-7, na.rm = TRUE) +
  labs(x = "Temp", y = "lnK", title = "lnK vs Temp(K)") +
  PGDP_theme() +
  theme(legend.position = "None") +
  coord_cartesian(clip = "off") #+ xlim(1500, 3000) + ylim(0,1E+16)
print(k_plot)

# ggsave(file.path(pic_folder, "lnKvT.png"), k_plot, width = 8, height = 6, units = "in")

# Reaction Temp Rate Constants --------------------------------------------

for (n in 1:nrow(rate_constants)){
  rate_constants[n,"k_ovt"] <- k_temp(n,Reac_temp,Reac_temp)
}
view_table(rate_constants)

# Initial conditions -----------------------------------------------------------
timeconv <- 3600
# Define initial conditions
O2_purity <- 0.95
O2_feed <- 7000
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

times <- seq(0, 1600/1E+9, length.out = 100) # Time points



# Solve the differential equations ---------------------------------------------
# Solve the ODE  ---------------------------------------------------------------
diff_func_maker <- function(equation_text, 
                            C, O2, CO, CO2, H2O, H2, CH4, N2, NO, NO2, S, SO2, Cl2, HCl,V, 
                            k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14) {
  eval(parse(text = equation_text))
}
Vr = 10
rate_eq <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    equations <- Overall_MB[, "Rate_eq"]
    diffs <- lapply(seq_along(equations), function(i) {
      diff_func_maker(equations[i],
                      C, O2, CO, CO2, H2O, H2, CH4, N2, NO, NO2, S, SO2, Cl2, HCl, Vr, 
                      k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14)
    })
    return(list(diffs))
  })
}

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

print(Conv_plot)
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
Gasifier_out[,"Conv"] <- 1 - (Gasifier_out[,"Mass_Out"] / Gasifier_out[,"Mass_In"])
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

