# =============================================================================
# EB_Reaction.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Energy Balance of the reaction system
#
# =============================================================================

source("EB_Polymer.R")
# Plasma Gasifier Energy Requirements -------------------------------------

ReacC_dh <- Reaction_In
ReacC_dh[c("Other_MSW","Fixed_C"),c("Mass_KGhr")] <- 0
Reac_RTtoRefT <- Composition_props(ReacC_dh,PG_Press,Reac_temp,Ref_T)
Prod_RefTtoRT <- Composition_props(Output_composition,PG_Press,Ref_T,Reac_temp)

# Reaction Enthalpies 
Gasifier_MB$dHf_Jhr <- 0
Gasifier_MB$Mols_dif <- Gasifier_MB[,"Mols_Out"] - Gasifier_MB[,"Mols_In"]
for (comp in rownames(Gasifier_MB)){
  comp_el <- paste0("(",comp,")")
  dHf <- element_props[element_props$Symbol == comp_el, "dHf_Jmol"]
  Gasifier_MB[comp,"dHf_Jhr"] <- Gasifier_MB[comp,"Mols_dif"] * dHf
}

dHR_Jhr <- sum(Gasifier_MB$dHf_Jhr)
TReach_dH <- sum(Reac_RTtoRefT$dH_Jhr) + sum(Prod_RefTtoRT$dH_Jhr) + dHR_Jhr

HessCycle <- data.frame(
  Reac_MJhr = sum(Reac_RTtoRefT$dH_Jhr)/1E+6,
  dH_MJhr = dHR_Jhr/1E+6,
  Prod_MJhr = sum(Prod_RefTtoRT$dH_Jhr)/1E+6,
  Total_MJhr = TReach_dH/1E+6
)

PGE_Jhr <- PolyE_Jhr + TReach_dH
PGE_kWh <- PGE_Jhr * JhrtokWh

# Plasma Arc Torch --------------------------------------------------------

cat("Total Energy Required (MJ/hr)", PGE_Jhr/1E+6,"\n")
cat("Total Power Required (kWh)", PGE_kWh,"\n")
