# Endothermic Reaction, +dH, absorbs heat
# Exothermic Reaction,  -dH, releases heat

Inlet_temp <- Dryer_out_temp   # Temp Leaving Dryer in K
Degrade_temp <- 600 # Degradation Temp in K
Decomp_Energy <- 1E+6

poly_results <- data.frame()


for (n in 1:nrow(polymer_data)) {
  poly_name <- polymer_data$Polymer[n]

  polymer <- polymer_data[polymer_data$Polymer == poly_name,]
  monomer <- monomer_data[monomer_data$OGPolymer == polymer$Polymer,]

  # Energy to reach Melting Point in J/kg
  Step1 <- polymer$Cp_Values*(polymer$Melting_Points-Inlet_temp)

  # Melting Energy in J/kg
  Step2 <- polymer$Latent_Heat_Melting
  
  # Energy to degrade Monomers in J/kg
  Step3 <- polymer$Cp_Values*(Degrade_temp-polymer$Melting_Points)
  
  # Breaking Monomer Bonds
  Step4 <- -polymer$BondEnergy*1000/(polymer$Molecular_Weight)
  
  # Heating Monomers
  Step5 <- Cp_int(monomer$Max_temp,monomer) - Cp_int(Degrade_temp,monomer)
  
  # Decomposition Energy
  Step6a <- -((monomer$dHf_J_KG))
  Step6b <- Cp_int(monomer$Max_temp,monomer) - (monomer$Max_temp*(22+2*30)*1000/monomer$MR)
  Step6c <- Cp_int(Inlet_temp,monomer) - (Inlet_temp*(22*2+2*30)*1000/monomer$MR)
  Step6  <- Step6a #+ Step6b + Step6c

  
  # Sensible Heats of Gases
  Step7Cn <- state_func(main_elements$Carbon,  monomer$Max_temp,Reac_temp) * monomer$Cn
  Step7Hn <- state_func(main_elements$Hydrogen,monomer$Max_temp,Reac_temp) * monomer$Hn
  Step7On <- state_func(main_elements$Oxygen,  monomer$Max_temp,Reac_temp) * monomer$On/2
  Step7Cln<- state_func(main_elements$Chlorine,monomer$Max_temp,Reac_temp) * monomer$Cln/2
  Step7mol<- Step7Cn["Enthalpy"] + Step7Hn["Enthalpy"] + Step7On["Enthalpy"] + Step7Cln["Enthalpy"]
  Step7   <- as.numeric(Step7mol/monomer$MR * Decomp_Energy) / 1000
  
  # Total Energy per polymer
  Total_Jkg <- Step1 + Step2 + Step3 + Step4 + Step5 + Step6 +  Step7
  Polymer_kg <- gasifier_feed[,poly_name]
  Total_Jhr  <- Total_Jkg * Polymer_kg
  
  Step_Energy <- data.frame("Polymer"=poly_name,"S1"=Step1,"S2"=Step2,"S3"=Step3,
                            "S4"=Step4, "S5"=Step5,"S6"=Step6,"S7"=Step7,
                            "Total_Jkg"=Total_Jkg, "Total_Jhr" = Total_Jhr)
  poly_results <- rbind(poly_results, Step_Energy)
}

PolyE_Jhr <- sum(poly_results$Total_Jhr)


PolyE_kWh <- PolyE_Jhr * JhrtokWh
PolyE_kW  <- PolyE_kWh / 3600
Elec_Pnd = PolyE_kWh * Elec_Cost


# cat("Total Energy(J/kg)", sum(poly_results$Total_Jkg),"\n")
# cat("Total Energy(MJ/hr)", PolyE_Jhr/1E+6,"\n")
# cat("Total Power(kWh)", PolyE_kWh,"\n")
