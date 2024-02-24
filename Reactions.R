# Data
prod_def <- -1
reaction_data <- data.frame(
  RNo = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
  No = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5, 6, 7, 8),
  Equation = c(
    "C + 0.5O2 -> CO",       # 1
    "CO + 0.5O2 -> CO2",     # 2
    "C + O2 -> CO2",         # 3
    "H2 + 0.5O2 -> H2O",     # 4
    "C + H2O -> CO + H2",    # 5
    "CO + H2O -> CO2 + H2",  # 6
    "CH4 + H2O -> CO + 3H2",
    "C + 2H2 -> CH4",
    "CO + 3H2 -> CH4 + H2O",
    "C + CO2 -> 2CO",
    "N2 + O2 -> 2NO",
    "2NO + O2 -> 2NO2",
    "S + 2CO2 -> 2CO + SO2",
    "H2 + Cl2 -> 2HCl"
  ),
  Carbon            = c(+01,+00,+01,+00,+01,+00,+00,+01,+00,+01,+00,+00,+00,+00)*prod_def,
  Oxygen            = c(+.5,+.5,+01,+.5,+00,+00,+00,+00,+00,+00,+01,+01,+00,+00)*prod_def,
  Carbon_Monoxide   = c(-01,+01,+00,+00,-01,+01,-01,+00,+01,-02,+00,+00,-02,+00)*prod_def,
  Carbon_Dioxide    = c(+00,-01,-01,+00,+00,-01,+00,+00,+00,+01,+00,+00,+02,+00)*prod_def,
  Water             = c(+00,+00,+00,-01,+01,+01,+01,+00,-01,+00,+00,+00,+00,+00)*prod_def,
  Hydrogen          = c(+00,+00,+00,+01,-01,-01,-03,+02,+03,+00,+00,+00,+00,+01)*prod_def,  
  Methane           = c(+00,+00,+00,+00,+00,+00,+01,-01,-01,+00,+00,+00,+00,+00)*prod_def,
  Nitrogen          = c(+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,+01,+00,+00,+00)*prod_def,
  Nitric_Oxide      = c(+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,-02,+02,+00,+00)*prod_def,
  Nitrogen_Dioxide  = c(+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,-02,+00,+00)*prod_def,
  Sulfur            = c(+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,+01,+00)*prod_def,
  Sulfur_Dioxide    = c(+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,-01,+00)*prod_def,
  Chlorine          = c(+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,+01)*prod_def,
  Hydrogen_Chloride = c(+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,+00,-02)*prod_def,
  
  xOG = c(15, 15, 15, 15, 95, 95, 95, 15, 0, 90, 100, 100, 100, 100),
  DeltaH = c(-111, -283, -394, -242, 131, -41, 206, -75, -227, 172, 90.29, 33.1, 269.12, -92.31)
)


reaction_props <- (reaction_data[c("RNo", "Equation", "xOG", "DeltaH")])
reaction_props <- reaction_props %>%
  mutate(
    K_eq = 0,
    x_thermo = 0,
    dG = 0,
    K_val = 0
  )


# Create table
# view_table(reaction_data)
# view_table(reaction_props,"latex")

# Save table to a file if needed
# save_kable(table, "reaction_table.html")

