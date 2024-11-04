# =============================================================================
# rate_eq_calc.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Defining reaction rates for each reaction and conducting overall 
#   mass balance for system
#
# =============================================================================

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

      rate <- paste0("(",sym,"^",abs(comp),")")
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
  simplified_equation <- paste0("(",simplified_equation,")*V")
  Elem <- data.frame(colnames(rate_eqs))[column_index,]
  sym <- sym_df$Symbol[sym_df$Component == Elem]
  MB_eq <- paste0("(",sym,"_0 -",simplified_equation,"*t)")
  Elem_MB <- data.frame("Elem"=Elem,"Rate_eq"=simplified_equation,"MB_eq"=MB_eq)
  Overall_MB <- rbind(Overall_MB,Elem_MB)
}


