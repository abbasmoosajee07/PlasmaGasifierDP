
Elec_Pnd = PolyE_kWh * Elec_Cost


cat("Total Energy(MJ/hr)", PolyE_Jhr/1E+6,"\n")
cat("Total Power(kW)", PolyE_kW,"\n")
cat("Total Power(kWh)", PolyE_kWh,"\n")
cat("Cost of Power(£)", Elec_Pnd,"\n")

Hourly_Elec <- data.frame(
  Unit = c("Gasifier", "Pumps"),
  ElectricityCost = c(PolyE_kWh, sum(PumpUnits[3,3:6])) * Elec_Cost
)

# Create the pie chart
Elec_pie <- ggplot() +
  geom_bar(data=Hourly_Elec, mapping = aes(x = "", y = ElectricityCost, fill = Unit),stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  PGDP_theme() + 
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Breakdown of electricity cost")

# Print the pie chart
print(Elec_pie)

ggsave(file.path(pic_folder, "Elec_pie.png"), Elec_pie, width = 90, height = 90, units = "mm")
