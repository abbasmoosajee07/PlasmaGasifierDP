# =============================================================================
# Finance.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Financial Calculations of the process node
#
# =============================================================================

# Capital Cost ------------------------------------------------------------
GasifierCost <- 78E+6 * ((Plastic_kghr/3600)/39.4)^0.67 * 0.79
PG_CapCost <- data.frame(
  Unit = c("Gasifier","SyngasHEX","Cyclone", "Pumps", "Fans", "Comps","Turbine"),
  EquipmentCost = c(GasifierCost,sum(HEXUnits["Cost",]), Cyc_calcs[9,"Value"],sum(PumpUnits[8,3:6]),sum(FanUnits[11,3:5]),sum(CompUnits[11,3:4]),1E+6)
)
CapCost_pie <- ggplot() +
  geom_bar(data=PG_CapCost, mapping = aes(x = "", y = EquipmentCost, fill = Unit),stat = "identity", width = 1) +
  PGDP_theme() + 
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Breakdown of Capital Cost of Equipment") +
  theme(axis.text.y = element_blank()) +
  coord_polar("y", start = 0) 

print(CapCost_pie)
# ggsave(file.path(pic_folder, "CapitalCost_pie.png"), CapCost_pie, width = 120, height = 120, units = "mm")

# Inside Site Battery Limit Cost Calculations ---------------------------------
ISBL_df <- data.frame(
  Node = c("Pre Treat", "Wash Dry", "WasteWater", "Gasifier", "Syngas Treat", "Haz Gas"),
  Equip_Cost = c(2612918, 274135, 2229540, sum(PG_CapCost$EquipmentCost), 2569356, 1595104)
)

ISBL_df$Install_Cost = ISBL_df$Equip_Cost * ((1 + 0.53) * 1 + (0.47 + 0.27 + 0.18 + 0.27 + 0.17 + 0.08))
ISBL_df <- total_func(ISBL_df)

# Total Capital Cost Calculations ---------------------------------------------
CapCost_df <- data.frame(
  Cost = c("ISBL", "OSBL", "Eng", "Cont", "Working"),
  Ratio = c(1.0, 0.3, 0.3, 0.1, 0.15)
)

CapCost_df$Value <- CapCost_df$Ratio * ISBL_df[ISBL_df$Node=="Total","Install_Cost"]
CapCost_df <- total_func(CapCost_df)

# Fixed Cost of Production ----------------------------------------------------

Labour_df <- data.frame(
  Role = c("Operating Labor", "Supervision Labor", "Direct Overhead"),
  Salary = c(33538, 37159, 30159),
  No_of_employees = c(120, 38, 62)
)
Labour_df$Ann_Cost <- Labour_df$Salary * Labour_df$No_of_employees
Labour_df <- total_func(Labour_df)


FCOP_df <- data.frame(
  Type = c("Labour Costs", "Maintenance", "Overhead Exp", "Tax and Ins", "Transport"),
  Value = c(Labour_df[Labour_df$Role=="Total","Ann_Cost"],
            c(5,3,3,1)/100 * CapCost_df[CapCost_df$Cost=="Total","Value"])
)
FCOP_df <- total_func(FCOP_df)

# Cash Cost of Production -----------------------------------------------------
Elec_Cost <- 0.270        # Cost of electricity (assumed to be per kWh)

Elec_reqd <- data.frame(
  Unit = c("Gasifier", "Pumps", "Fans", "Comps"),
  Elec_Cost = c(PolyE_kWh, sum(PumpUnits[3,3:6]),sum(FanUnits[10,3:5]),sum(CompUnits[10,3:4]))
)

Elec_diff <- sum(Elec_reqd$Elec_Cost) - ElecGen_kWh

Elec_pie <- ggplot() +
  geom_bar(data=Elec_reqd, mapping = aes(x = "", y = Elec_Cost, fill = Unit),stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  PGDP_theme() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Breakdown of electricity cost")

print(Elec_pie)
# ggsave(file.path(pic_folder, "Elec_pie.png"), Elec_pie, width = 120, height = 120, units = "mm")

vals_func <- function(df){
  df$Cost_Pnd = df$Unit_Cost * df$Quant_Req
  df$Unit_yr = df$Quant_Req * Basis_hrs
  df$Rev_yr = df$Unit_Cost * df$Unit_yr

  return(df)
}




# Raw Materials and Consumables -----------------------------------------------
RawMat_df <- data.frame(
  Type = c("Oxygen", "NaOH"),
  Unit_Cost = c(0.07,3.07),
  Quant_Req = c(O2_feed,250.3)
)
RawMat_df <- vals_func(RawMat_df)

Utilities_df <- data.frame(
  Type = c("Electricity", "Water"),
  Unit_Cost = c(Elec_Cost,0.26/1000),
  Quant_Req = c(-Elec_diff,sum(PumpUnits[1,3:6]))
)

Utilities_df <- vals_func(Utilities_df)

# Product Revenues ------------------------------------------------------------
Prods_df <- data.frame(
  Type = c("Hydrogen", "Carbon Dioxide","Ash","Slag","Plastic Waste", "Sorted Waste"),
  Unit_Cost = c(12,0.05,0.13,0.13,0.10,0.05),
  Quant_Req = c(Output_composition[Output_composition$Component=="Hydrogen","Mass_KGhr"]*0.82,
                Output_composition[Output_composition$Component=="Carbon_Dioxide","Mass_KGhr"]*1.01
                ,Ash,Slag,Plastic_kghr,Plastic_kghr/0.138)
)


Prods_df <- vals_func(Prods_df)


# print(ISBL_df)
# print(CapCost_df)
# print(Labour_df)
# print(FCOP_df)
# print(Utilities_df)
# print(RawMat_df)
# print(Prods_df)

BalanceSheet_df <- data.frame(
  CapCost = CapCost_df[CapCost_df$Cost=="Total","Value"] - CapCost_df[CapCost_df$Cost=="Working","Value"],
  WorkCap = CapCost_df[CapCost_df$Cost=="Working","Value"],
  FCOP = FCOP_df[FCOP_df$Type=="Total","Value"],
  VCOP = sum(Utilities_df$Rev_yr),
  CCOP = sum(RawMat_df$Rev_yr) + sum(Utilities_df$Rev_yr) + FCOP_df[FCOP_df$Type=="Total","Value"],
  H2Rev = Prods_df[Prods_df$Type=="Hydrogen","Rev_yr"],
  TotalRev = sum(Prods_df$Rev_yr)
)
BalanceSheet_df$Profit <- BalanceSheet_df$TotalRev - BalanceSheet_df$CCOP

print(BalanceSheet_df)
