Dryer_out_temp <- 330 # Temp in K
Dryer_out_press <- 1 # pressure in bar

gasifier_feed <- data.frame(
  LDPE = 751.58, 
  HDPE = 620.87,
  PP = 457.48,
  PS = 294.10,
  PVC = 196.06 * 0.1,
  PET = 326.77,
  Others = 601.02,
  Microplastics = 0, 
  Paper_and_card = 0,
  Glass = 0,
  Other_materials = 31.97,
  WEEE_IBA_Metals = 0, 
  Textiles = 0,
  Food_Organic = 10.14,
  Water = 175.00
)

Plastic_kghr = sum(gasifier_feed[c("LDPE","HDPE","PP","PS","PVC","PET","Others")])
PlasticVol_m3hr <- sum(gasifier_feed[,1:7]/polymer_data$Density)
Plasticrho_kgm3  <- Plastic_kghr/PlasticVol_m3hr
feed_comp <-(ultimate_analysis[,2:5])* as.numeric(t(gasifier_feed[,1:7]))
feed_comp <- cbind("Type"= (ultimate_analysis[,1]),
                   feed_comp,ultimate_analysis[,6:11]* feed_comp$Volatiles)
feed_comp <- rbind(feed_comp,(cbind(Type = "Total",
                                    t(colSums(feed_comp[, 2:11])))))
feed_comp <- feed_comp %>%
  mutate_at(vars(-1), as.numeric)

# view_table(feed_comp,2)
Input_composition <- data.frame(
  Component = c("Other_MSW","Fixed_C","Ash","Water","Carbon","Hydrogen",
                "Nitrogen","Sulfur","Oxygen","Chlorine"),
  Mass_KGhr  = c(sum(gasifier_feed[,8:14]),feed_comp[8,"Fixed_C"],
                 feed_comp[8,"Ash"],feed_comp[8,"Water"]+gasifier_feed$Water,
                 feed_comp[8,"Carbon"],feed_comp[8,"Hydrogen"],feed_comp[8,"Nitrogen"],
                 feed_comp[8,"Sulfur"],feed_comp[8,"Oxygen"],feed_comp[8,"Chlorine"]),
  Mol_kmolhr = c(0,0,0,0,0,0,0,0,0,0),
  Vol_m3hr = c(0,0,0,0,0,0,0,0,0,0),
  dH_Jhr = c(0,0,0,0,0,0,0,0,0,0)
)

