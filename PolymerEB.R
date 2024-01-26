# Endothermic Reaction, +dH, absorbs heat
# Exothermic Reaction,  -dH, releases heat

Inlet_temp <- 373   # Temp Leaving Dryer in K
Degrade_temp <- 600 # Degradation Temp in K
Reac_temp <- 2750
Decomp_Energy <- 1e6

poly_properties <- function(polymer_data,monomer_data){
  all_results <- data.frame()
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
    Step6a <- -((monomer$dHf_J_KG)/monomer$MR)*1000
    Step6b <- 0#Cp_int(monomer$Max_temp,monomer) - (monomer$Max_temp*(22+2*30)*1000/monomer$MR)
    Step6c <- 0#Cp_int(Inlet_temp,monomer) - (Inlet_temp*(22*2+2*30)*1000/monomer$MR)
    Step6  <- Step6a + Step6b + Step6c
    
    # Sensible Heats of Gases
    Step7Cn <- state_func(main_elements$Carbon,Reac_temp,monomer$Max_temp) * monomer$Cn
    Step7Hn <- state_func(main_elements$Hydrogen,Reac_temp,monomer$Max_temp) * monomer$Hn
    Step7On <- state_func(main_elements$Oxygen,Reac_temp,monomer$Max_temp) * monomer$On/2
    Step7Cln<- state_func(main_elements$Chlorine,Reac_temp,monomer$Max_temp) * monomer$Cln/2
    Step7mol<- Step7Cn[1] + Step7Hn[1] + Step7On[1] + Step7Cln[1]
    Step7   <- Step7mol/monomer$MR*Decomp_Energy
    
    # Total Energy per polymer
    Total_Jkg <- Step1 + Step2 + Step3 + Step4 + Step5 + Step6 +  Step7
    Total_Jhr <- Total_Jkg
    
    Step_Energy <- data.frame("Polymer"=poly_name,"S1"=Step1,"S2"=Step2,"S3"=Step3,"S4"=Step4,
                              "S5"=Step5,"S6"=Step6,"S7"=Step7,"Total"=Total_Jkg)
    all_results <- rbind(all_results, Step_Energy)
    
  }
  
  return(all_results)
}

poly_results <- poly_properties(polymer_data,monomer_data)
