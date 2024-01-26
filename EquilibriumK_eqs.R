
equilibrium_const <- function(selected_reaction) {
  
  # Extracting columns excluding unnecessary ones
  reactant_products <- selected_reaction[, !names(selected_reaction) %in%  
                                           c("RNo", "No", "Equation", "xOG", "DeltaH", "K_eq")]
  
  # Calculating total moles of reactants and products
  total_reactant_moles <- sum(t(reactant_products)[t(reactant_products) > 0])
  total_product_moles <- -sum(reactant_products)
  
  # print(c(n,total_product_moles))
  
  total_moles_expression <- paste("(", total_reactant_moles, "+", total_product_moles, "*x", ")")
  
  # Function to calculate partial pressure expression for a component
  partial_pressure <- function(component_moles) {
    equilibrium_moles <- paste("(", component_moles, "-", component_moles, "*x", ")")
    pi_eq <- paste("(", equilibrium_moles, "/", total_moles_expression, ")")
    pi_eq_power <- paste("(", pi_eq, "*P)^", -component_moles)
    return(pi_eq_power)
  }
  
  # Initializing a string for the cumulative product of partial pressures
  cumulative_product <- paste("(1)")
  
  # Looping through each column of reactants and products
  for (column_index in 1:ncol(reactant_products)) {
    component_moles <- reactant_products[, column_index]
    
    # Checking if the component is not present in the reaction
    if (all(component_moles == 0)) {
      partial_pressure_expression = "(1)"
    } else {
      partial_pressure_expression <- paste("(", partial_pressure(component_moles), ")")
    }
    
    # Updating the cumulative product
    cumulative_product <- paste(cumulative_product, "*", partial_pressure_expression)
  }
  cumulative_product <- as_r(yac_str(paste("Simplify",cumulative_product)))
  return(cumulative_product)
}
reaction_data$K_eq <-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)

for (n in 1:nrow(reaction_data)){
  reaction_data[n,20] <- equilibrium_const(reaction_data[n,])
}

P=1
test_eq <- reaction_data[1,20]
test <- as.function(alist(x =, eval(parse(text = test_eq))))
curve(test, 0, 1, ylab = "y(x)")


# Define the expression as a function
# redefine D function
D <- function(eq, order = 1) {
  yac_str(paste("D(x,", order, ")", eq))
}

Dtest_eq <- D(test_eq)
print(Dtest_eq)

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

Z_n <- newton_raphson(function(Z) EOS_RK(Z, P_n, T_n),
                      function(Z) EOS_RK_derivative(Z, P_n, T_n),
                      1)
