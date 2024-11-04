# Cash Flow Analysis ----------------------------------------------------------
H2Min_BS <- BalanceSheet_df
H2Min_BS$H2Rev <- H2Min_BS$H2Rev * 0.75
H2Min_BS$TotalRev <- (BalanceSheet_df$TotalRev - BalanceSheet_df$H2Rev) + H2Min_BS$H2Rev

H2Max_BS <- BalanceSheet_df
H2Max_BS$H2Rev <- H2Max_BS$H2Rev * 1.25
H2Max_BS$TotalRev <- (BalanceSheet_df$TotalRev - BalanceSheet_df$H2Rev) + H2Max_BS$H2Rev

Cashflow_df <- CashFlow_func(BalanceSheet_df)
H2Min_CFdf <- CashFlow_func(H2Min_BS)
H2Max_CFdf <- CashFlow_func(H2Max_BS)

print(Cashflow_df)
CFPlot_df <- data.frame(
  Year = Cashflow_df$Year,
  Main = Cashflow_df$LifeTime/1e+6,
  H2Min = H2Min_CFdf$LifeTime/1e+6,
  H2Max = H2Max_CFdf$LifeTime/1e+6
)

# Cash Flow Plot --------------------------------------------------------------
CashFlow_plot <- ggplot(data = CFPlot_df, aes(x = Year)) +
  geom_ribbon(aes(ymin = H2Min, ymax = H2Max), fill = "gray", alpha = 0.7) + 
  geom_line(aes(y = H2Min), color = "gray",size = 0.4) +
  geom_line(aes(y = H2Max), color = "gray",size = 0.4) +
  geom_line(aes(y = Main),  color = "green", size = 0.7) +
  geom_point(aes(y = Main), color = "green", size = 1.2) +
  labs(x = "Time(Years)", y = "Cumulative Cash Flow (£ millions)", 
       title = "Cash Flow Analysis of Project") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  scale_x_continuous( breaks = seq(0, max(Years), by = 2)) + 
  scale_y_continuous(breaks = seq(round(min(CFPlot_df),-2), 
                                  round(max(CFPlot_df),-2), by = 50)) +
  PGDP_theme() + 
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray", linetype = "solid")
  )

print(CashFlow_plot)
ggsave(file.path(pic_folder, "CashFlow IndRep.png"), CashFlow_plot, width = 190, height = 80, units = "mm")

# Financial Scenarios ---------------------------------------------------------

# Reduced Feed Scenario -------------------------------------------
RedFeed_BS <- BalanceSheet_df 
RedCap <- 0.80
RedFeed_BS[c("VCOP","H2Rev","TotalRev")] <- RedFeed_BS[c("VCOP","H2Rev","TotalRev")] * RedCap
RedFeed_BS$CCOP <- RedFeed_BS$FCOP + RedFeed_BS$VCOP
RedFeed_CFdf <- CashFlow_func(RedFeed_BS)

# Fuel Cell Scenario ----------------------------------------------
FuelCell_BS <- BalanceSheet_df * 1.5
FuelCell_CFdf <- CashFlow_func(FuelCell_BS)

# Carbon Credits Scenario -----------------------------------------
Credits_BS <- BalanceSheet_df
Credits_BS$CO2_Saved <- (Plastic_kghr/12 * 48 + 
                           (Output_composition["Hydrogen","Mass_KGhr"] * 33.6 + ElecGen_kWh) * 0.66)/1000
Credits_BS$CO2_Rev <- Credits_BS$CO2_Saved * Basis_hrs * 10.27
Credits_BS$TotalRev <- Credits_BS$TotalRev + Credits_BS$CO2_Rev


Credits_CFdf <- CashFlow_func(Credits_BS)

# 
FinScen_df <- data.frame(
  Year = Cashflow_df$Year,
  Main = Cashflow_df$LifeTime/1e+6,
  RedFeed = RedFeed_CFdf$LifeTime/1e+6,
  Credits = Credits_CFdf$LifeTime/1e+6 ,
  FuelCell = FuelCell_CFdf$LifeTime/1e+6 
)

FinScen_plot <- ggplot(data = FinScen_df, aes(x = Year)) +
  geom_line(aes(y = RedFeed,  color = "Reduced Feed"), size = 0.7) +
  geom_line(aes(y = Credits,  color = "Carbon Credits"), size = 0.7) +
  geom_line(aes(y = FuelCell,  color = "Fuel Cell"), size = 0.7) +
  geom_line(aes(y = Main,  color = "Base Case"), size = 0.7) +
  geom_point(aes(y = Main), color = "green", size = 1.2) +
  labs(x = "Time(Years)", y = "Cumulative Cash Flow (£ millions)", 
       title = "Financial Feasibility of Project in Different Scenarios",color ="Scenario") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  scale_x_continuous( breaks = seq(0, max(Years), by = 2)) + 
  scale_y_continuous(breaks = seq(round(min(FinScen_df),-2), 
                                  round(max(FinScen_df),-2), by = 50)) +
  PGDP_theme() + 
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray", linetype = "solid")
  ) + 
  scale_color_manual(values = c("Base Case" = "green", "Carbon Credits" = "blue", 
                                "Reduced Feed" = "red", "Fuel Cell" = "purple"))

print(FinScen_plot)
ggsave(file.path(pic_folder, "FinScenario IndRep.png"), FinScen_plot, width = 190, height = 80, units = "mm")
