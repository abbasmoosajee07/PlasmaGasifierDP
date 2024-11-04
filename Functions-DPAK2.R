# =============================================================================
# Functions-DPAK2.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: All the main functions being used within the calculations of the 
# whole node
#
# =============================================================================

# Process Constants -----------------------------------------------------------
P_atm <- 101325
Ref_T <- 298
gravity <- 9.81
R <- 8.314

# Unit Conversions ------------------------------------------------------------
JhrtokWh <- 1 / (3600 * 1000)
timeconv <- 3600

# Visual Functions -----------------------------------------------------------
pounds_format <- dollar_format(prefix = "£", big.mark = ",")

PGDP_theme <- function(){
  theme_minimal() +
    theme(
      text = element_text(color = "black"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 12,face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "gray", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray", fill = NA, linewidth = 0.5)
    )
}

view_table <- function(table, round = 2, type = "html") {
  rounded_table <- table 
  for (nr in 1:nrow(table)){
    for (nc in 1:ncol(table)){
      cell <- table[nr,nc]
      if (class(table[nr,nc]) == "numeric"){
        rounded_table[nr,nc] <- signif(table[nr,nc],round)
      }
    }
  }
  formatted_table <- rounded_table %>%
    kable(format = type) %>%
    kable_styling(full_width = FALSE)
  print(formatted_table)
}

# General Functions -------------------------------------------------------

total_func <- function(df){
  nr <- nrow(df)
  df[nr+1,] <- 0
  df[nr+1,1] <- "Total"
  for (nc in 2:ncol(df)){
    df[nr+1,nc] <- sum(as.numeric(df[,nc]))
  }
  return(df)
}

Cp_int <- function(T, TP_data) {
  if (T == 0) {
    Cp_int = 0
  } else {
    t = T 
    Cp_int = (TP_data$A * t + (TP_data$B * t^2) / 2 + 
                (TP_data$C * t^3) / 3 + (TP_data$D * t^4) / 4 - TP_data$E / t)
  }
  return(Cp_int)
}

S_func <- function(T, TP_data) {
  if (T == 0) {
    S_eq = 0
  } else {
    t=T
    S_eq <- TP_data$A*log(t) + TP_data$B*t + (TP_data$C*t^2)/2 + 
      (TP_data$D*t^3)/3 - TP_data$E/(2*t^2) + TP_data$G
    return(S_eq)
  }
}

H_func <- function(T, TP_data) {
  if (T == 0) {
    H_eq = 0
  } else {
    t=T
    H_eq <- (TP_data$A * t + (TP_data$B * t^2) / 2 + 
               (TP_data$C * t^3) / 3 + (TP_data$D * t^4) / 4 - TP_data$E / t +
                TP_data$F - TP_data$H )
    return(H_eq*1000)
  }
}


Composition_props <- function(Composition,Pa_e,Tfa_e,Ti_e = 0){
  for (n in 1:nrow(Composition)) {
    Element <- Composition[n,"Component"]
    elem_thermo <- thermo_properties(all_elements,Ti_e,Tfa_e)
    Composition[n, "Mol_kmolhr"] <- Composition[n, "Mass_KGhr"] / 
      element_props[element_props$Component == Element,  "Molar_Mass"]
    Composition[n, "dH_Jhr"] =  Composition[n,"Mol_kmolhr"] *
      elem_thermo[elem_thermo$Element == Element,"Enthalpy"]
  }
  TotalMols <- sum(Composition$Mol_kmolhr)
  for (n in 1:nrow(Composition)) {
    Element <- Composition[n,"Component"]
    Composition[n, "Vol_m3hr"] <- Vol_func(Composition, Element, TotalMols, Tfa_e, Pa_e)
  }
  rownames(Composition) <- Composition[,"Component"]
  return(Composition)
}

calc_ReacHeight <- function(diameter, volume) {
  radius <- diameter / 2
  height <- volume / (pi * radius^2)
  return(height)
}
# Newton-Raphson method function -------------------------------------------

# Create RK Functions ----------------------------------------------------------
# Redlich-Kwong equation
EOS_RK <- function(Z, Pr, Tr) {
  sig1 = (0.42747 / Tr^2.5)
  sig2 = (0.08664 / Tr)
  eq = Z^3 - Z^2 + (sig1 * Pr - sig2 * Pr  - sig2^2 * Pr^2) * Z - sig1 * sig2 * Pr^2
  return(eq)
}

# Derivative of Redlich-Kwong equation
EOS_RK_derivative <- function(Z, Pr, Tr) {
  sig1 = (0.42747 / Tr^2.5)
  sig2 = (0.08664 / Tr)
  derivative = 3 * Z^2 - 2 * Z + (sig1 * Pr - sig2 * Pr  - sig2^2 * Pr^2)
  return(derivative)
}

# Newton-Raphson method function -------------------------------------------
newton_raphson <- function(f, df, x0, tol = 1e-6, max_iter = 1e+6) {

    Zi <- x0
    iter <- 0
    
    while (abs(f(Zi)) > tol && iter < max_iter) {
      Zi <- Zi - f(Zi) / df(Zi)
      iter <- iter + 1
    }
    
    if (iter == max_iter) {
      Zi = Z_n
      warning("Maximum number of iterations reached.")
    }
    return(Zi)
}

Vol_func <- function(Composition,element,TotalMols,Ta_e,Pa_e){
  Elem_mols <- Composition[Composition$Component == element,"Mol_kmolhr"] * 1000
  TotalMols <- TotalMols * 1000
  elementaldata <- element_props[element_props$Component == element,]
  RMM <- elementaldata$Molar_Mass
  
  Molar_ratio <- Elem_mols/TotalMols
  Tc_e <- elementaldata$Tc_K
  Pc_e <- elementaldata$Pc_bar * 1E+5
  Pi_e <- Molar_ratio * Pa_e 
  Tr_e <- Ta_e/Tc_e
  Pr_e <- Pi_e/Pc_e
  
  Zn_e <- newton_raphson(function(Z) EOS_RK(Z, Pr_e, Tr_e),
                         function(Z) EOS_RK_derivative(Z, Pr_e, Tr_e),
                         1)
  V_e <- (Elem_mols * R * Zn_e * Ta_e) / Pa_e
  # V_e <- (Elem_mols * R * Ta_e)/ Pa_e
  if (any(is.infinite(V_e))){
    V_e = 0
  } else {
    V_e = V_e
  }
}

# Conversion ODE Function -------------------------------------------------
diff_func_maker <- function(equation_text, 
                            C, O2, CO, CO2, H2O, H2, CH4, N2, NO, NO2, S, SO2, Cl2, HCl, 
                            k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14,
                            V) {
  eval(parse(text = equation_text))
}

rate_eq <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    equations <- Overall_MB[, "Rate_eq"]
    diffs <- lapply(seq_along(equations), function(i) {
      diff_func_maker(equations[i],
                      C, O2, CO, CO2, H2O, H2, CH4, N2, NO, NO2, S, SO2, Cl2, HCl, 
                      k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14,
                      Vr)
    })
    return(list(diffs))
  })
}


