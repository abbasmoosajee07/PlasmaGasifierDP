x <- sym("x")
P <- sym("P")

equilibrium_const_eq <- function(selected_reaction) {
  
  # Extracting columns excluding unnecessary ones
  reactant_products <- selected_reaction[, !names(selected_reaction) %in%  
                                           c("RNo", "No", "Equation", "xOG", "DeltaH", "K_eq")]
  
  # Calculating total moles of reactants and products
  total_reactant_moles <- -sum(t(reactant_products)[t(reactant_products) < 0])
  total_product_moles <- sum(reactant_products)
  
  # print(c(n,total_product_moles))
  
  total_moles_expression <- paste("(", total_reactant_moles, "+", total_product_moles, "*x", ")")
  
  # Function to calculate partial pressure expression for a component
  partial_pressure <- function(component_moles) {
    if (component_moles > 0) {
      initial_moles = component_moles
    } else {
      initial_moles = 0
    }
    equilibrium_moles <- paste("(", initial_moles, "-", component_moles, "*x", ")")
    pi_eq <- paste("(", equilibrium_moles, "/", total_moles_expression, ")")
    pi_eq_power <- paste("(", pi_eq, "*P)^", -component_moles)
    return(pi_eq_power)
  }
  
  # Initialising a string for the cumulative product of partial pressures
  cumulative_product <- paste("(1)")
  
  # Looping through each column of reactants and products
  for (column_index in 1:ncol(reactant_products)) {
    component_moles <- reactant_products[, column_index]
    
    # Checking if the component is not present in the reaction
    if (all(component_moles == 0)) {
      partial_pressure_expression = "(1)"
    } else {
      partial_pressure_expression <- paste("(", partial_pressure(-component_moles), ")")
    }
    
    # Updating the cumulative product
    cumulative_product <- paste(cumulative_product, "*", partial_pressure_expression)
  }
  cumulative_product <- yac_str(paste("Simplify",cumulative_product))
  return(cumulative_product)
}



thermo_conv <- function(equation_text,k_eq,Pn){
  
  function_maker <- function(equation_text){
    expression_function <- function(x, P) { 
      out <- eval(parse(text = equation_text)) 
      return (out)
    }
    return(expression_function)
  }
  
  text_eq <- paste("(",equation_text,"-",k_eq,")")
  expression_function <- function_maker(text_eq)
  conv_T <- uniroot(function(x) expression_function(x, Pn) ,interval = c(0.00, 1))$root
  return(conv_T)
}

eq_const_func <- function(reaction_props,reaction_data,elem_props) {
  
  reac_no <- reaction_props$RNo
  reac_elem <- reaction_data[reac_no,]
  reac_comp <- reac_elem[, !names(reac_elem) %in%  
                           c("RNo", "No", "Equation", "xOG", "DeltaH", "K_eq")]
  
  reac_thermo <- data.frame()
  for (element in names(reac_comp)){
    element_mols <- reac_comp[element]
    
    elem_prop <- elem_props[elem_props$Element == element,]
    Elem_Enthalpy <- elem_prop["Enthalpy"] * element_mols
    Elem_Entropy  <- elem_prop["Entropy"] * element_mols
    
    reac_energy <- data.frame("dH" = Elem_Enthalpy,"dS" = Elem_Entropy, "Elem" = element)
    reac_thermo <- rbind(reac_thermo, reac_energy)
  }
  
  dHT = sum(reac_thermo$Enthalpy) + reac_elem$DeltaH*1000
  dST = sum(reac_thermo$Entropy)
  dG  = dHT - dST * Reac_temp
  
  lnK = dG / (-R * Reac_temp)
  K = exp(lnK)
  result <- data.frame("Gibbs"=dG, "K"=K)
  return(result)
}


all_elem <- thermo_properties(main_elements,298,Reac_temp)

Pres <- 1
for (n in 1:nrow(reaction_data)) {
  Rthermo_prop <- eq_const_func(reaction_props[n,],reaction_data,all_elem)
  reaction_props[n,"No"] <- as.character(reaction_data[n,"No"])
  reaction_props[n,"dG"] <- Rthermo_prop["Gibbs"]
  reaction_props[n,"K_val"] <- Rthermo_prop["K"]
  k_eq <- reaction_props[n,8]
  equilibrium_equation <- equilibrium_const_eq(reaction_data[n,])
  conv_T2 <- thermo_conv(equilibrium_equation,k_eq,Pres)
  reaction_props[n,"K_eq"] <- equilibrium_equation
  reaction_props[n,"x_thermo"] <- conv_T2
}

Reacx_plot <- ggplot() +
  geom_point(reaction_props, mapping = aes(x = RNo, y = x_thermo, color = No), size = 5, shape = 16) +
  geom_text(data = reaction_props[reaction_props$x_thermo >  0.5,], mapping = aes(x = RNo-0.2, y=x_thermo, label = paste("dH =",DeltaH)), color = "black", angle = 90, hjust = +1.2, na.rm = TRUE) +
  geom_text(data = reaction_props[reaction_props$x_thermo <= 0.5,], mapping = aes(x = RNo-0.2, y=x_thermo, label = paste("dH =",DeltaH)), color = "black", angle = 90, hjust = -.2, na.rm = TRUE) +
  
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(title = "Thermodynamic Equilibrium Conversion for each Reaction",
       y = "Equilibrium Conversion (x)",
       x = "Reaction No",
       color = "Reaction No") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = scales::number_format()) +
  scale_x_continuous(breaks = seq(1, 14, by = 1), labels = scales::number_format()) +
  PGDP_theme() +
  theme(legend.position = "None") +
  coord_cartesian(clip = "off") 

print(Reacx_plot)
ggsave(file.path(pic_folder, "GibbsXPlot.png"), Reacx_plot, width = 170, height = 80, units = "mm")
