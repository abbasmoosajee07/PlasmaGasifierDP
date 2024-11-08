# =============================================================================
# Element_Data.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Defining thermo-physical properties of constituent elements in 
#   the system
#
# =============================================================================

main_elements <- {list(
    Oxygen = data.frame(
      Eq_type = c(1, 1, 1),
      Min_temp = c(100, 700, 2000),
      Max_temp = c(700, 2000, 6000),
      A = c(31.32234, 30.03235, 20.91111),
      B = c(-20.23531, 8.772972, 10.72071),
      C = c(57.86644, -3.988133, -2.020498),
      D = c(-36.50624, 0.788313, 0.146449),
      E = c(-0.007374, -0.741599, 9.245722),
      F = c(-8.903471, -11.32468, 5.337651),
      G = c(246.7945, 236.1663, 237.6185),
      H = c(0.0, 0.0, 0.0)
    ),
    Carbon = data.frame(
      Eq_type = c(0, 0, 1),
      Min_temp = c(0, 0, 298.0),
      Max_temp = c(0, 0, 6000.0),
      A = c(0, 0, 21.17510),
      B = c(0, 0, -0.812428),
      C = c(0, 0, 0.448537),
      D = c(0, 0, -0.043256),
      E = c(0, 0, -0.013103),
      F = c(0, 0, 710.3470),
      G = c(0, 0, 183.8734),
      H = c(0, 0, 716.6690)
    ),
    Hydrogen = data.frame(
      Eq_type = c(1, 1, 1),
      Min_temp = c(298, 1000, 2500),
      Max_temp = c(1000, 2500, 6000),
      A = c(33.066178, 18.563083, 43.413560),
      B = c(-11.363417, 12.257357, -4.293079),
      C = c(11.432816, -2.859786, 1.272428),
      D = c(-2.772874, 0.268238, -0.096876),
      E = c(-0.158558, 1.977990, -20.533862),
      F = c(-9.980797, -1.147438, -38.515158),
      G = c(172.707974, 156.288133, 162.081354),
      H = c(0.0, 0.0, 0.0)
    ),
    Nitrogen = data.frame(
      Eq_type = c(1, 1, 1),
      Min_temp = c(100, 500, 2000),
      Max_temp = c(500, 2000, 6000),
      A = c(28.98641, 19.50583, 35.51872),
      B = c(1.853978, 19.88705, 1.128728),
      C = c(-9.647459, -8.598535, -0.196103),
      D = c(16.63537, 1.369784, 0.014662),
      E = c(0.000117, 0.527601, -4.553760),
      F = c(-8.671914, -4.935202, -18.97091),
      G = c(226.4168, 212.3900, 224.9810),
      H = c(0.0, 0.0, 0.0)
    ),
    Water = data.frame(
      Eq_type = c(1, 1, 1),
      Min_temp =  c(298, 500, 1700),
      Max_temp = c(500, 1700, 6000),
      A = c(-203.6060, 30.09200, 41.96426),
      B = c(1523.290, 6.832514, 8.622053),
      C = c(-3196.413, 6.793435, -1.499780),
      D = c(2474.455, -2.534480, 0.098119),
      E = c(3.855326, 0.082139, -11.15764),
      F = c(-256.5478, -250.8810, -272.1797),
      G = c(-488.7163, 223.3967, 219.7809),
      H = c(-285.8304, -241.8264, -241.8264)
    ),
    Carbon_Dioxide = data.frame(
      Eq_type = c(0, 1, 1),
      Min_temp =  c(0, 298, 1200),
      Max_temp = c(0, 1200, 6000),
      A = c(0, 24.99735, 58.16639),
      B = c(0, 55.18696, 2.720074),
      C = c(0, -33.69137, -0.492289),
      D = c(0, 7.948387, 0.038844),
      E = c(0, -0.136638, -6.447293),
      F = c(0, -403.6075, -425.9186),
      G = c(0, 228.2431, 263.6125),
      H = c(0, -393.5224, -393.5224)
    ),
    Carbon_Monoxide = data.frame(
      Eq_type = c(0, 1, 1),
      Min_temp =  c(0, 298, 1300),
      Max_temp = c(0, 1300, 6000),
      A = c(0, 25.56759, 35.15070),
      B = c(0, 6.096130, 1.300095),
      C = c(0, 4.054656, -0.205921),
      D = c(0, -2.671301, 0.013550),
      E = c(0, 0.131021, -3.282780),
      F = c(0, -118.0089, -127.8375),
      G = c(0, 227.3665, 231.7120),
      H = c(0, -110.5271, -110.5271)
    ),
    Methane = data.frame(
      Eq_type = c(0, 1, 1),
      Min_temp =  c(0, 298, 1300),
      Max_temp = c(0, 1300, 6000),
      A = c(0, -0.703029, 85.81217),
      B = c(0, 108.4773, 11.26467),
      C = c(0, -42.52157, -2.114146),
      D = c(0, 5.862788, 0.138190),
      E = c(0, 0.678565, -26.42221),
      F = c(0, -76.84376, -153.5327),
      G = c(0, 158.7163, 224.4143),
      H = c(0, -74.87310, -74.87310)
    ),
    Nitric_Oxide = data.frame(
      Eq_type = c(0, 1, 1),
      Min_temp =  c(0, 298, 1200),
      Max_temp = c(0, 1200, 6000),
      A = c(0, 23.83491, 35.99169),
      B = c(0, 12.58878, 0.957170),
      C = c(0, -1.139011, -0.148032),
      D = c(0, -1.497459, 0.009974),
      E = c(0, 0.214194, -3.004088),
      F = c(0, 83.35783, 73.10787),
      G = c(0, 237.1219, 246.1619),
      H = c(0, 90.29114, 90.29114)
    ),
    Nitrogen_Dioxide = data.frame(
      Eq_type = c(0, 1, 1),
      Min_temp =  c(0, 298, 1200),
      Max_temp = c(0, 1200, 6000),
      A = c(0, 16.10857, 56.82541),
      B = c(0, 75.89525, 0.738053),
      C = c(0, -54.38740, -0.144721),
      D = c(0, 14.30777, 0.009777),
      E = c(0, 0.239423, -5.459911),
      F = c(0, 26.17464, 2.846456),
      G = c(0, 240.5386, 290.5056),
      H = c(0, 33.09502, 33.09502)
    ),
    Sulfur = data.frame(
      Eq_type = c(1, 1, 1, 1, 1),
      Min_temp =  c( 298, 388.86, 432, 882.117, 1400),
      Max_temp =  c(388.86, 432, 882.117, 1400, 6000),
      A = c(24.23749, -4540.970, -37.93350, 27.45968, 16.55345),
      B = c(-4.983060, 26065.60, 133.2420, -13.32784, 2.400266),
      C = c(33.00900, -55520.70, -95.32450, 10.06574, -0.255760),
      D = c(-15.29616, 42012.20, 24.00940, -2.662381, 0.005821),
      E = c(-0.182237, 54.58860, 7.654530, -0.055851, 3.564793),
      F = c(-7.516263, 787.8070, 29.78810, 269.1149, 278.4356),
      G = c(61.49141, -10826.30, -13.15340,204.2955, 194.5447),
      H = c(0.360001, 1.853781, 1.853781, 276.9804, 276.9804)
    ),
    Chlorine = data.frame(
      Eq_type = c(1, 1, 1),
      Min_temp =  c(298, 1000, 3000),
      Max_temp = c(1000, 3000,6000),
      A = c(33.05060, 42.67730, -42.55350),
      B = c(12.22940, -5.009570, 41.68570),
      C = c(-12.06510, 1.904621, -7.126830),
      D = c(4.385330, -0.165641, 0.387839),
      E = c(-0.159494, -2.098480, 101.1440),
      F = c(-10.83480, -17.28980, 132.7640),
      G = c(259.0290, 269.8400, 264.7860),
      H = c(0.000000, 0.000000, 0.000000)
    ),
    Hydrogen_Chloride = data.frame(
      Eq_type = c(0, 1, 1),
      Min_temp =  c(0, 298, 1200),
      Max_temp = c(0, 1200, 6000),
      A = c(0, 32.12392, 31.91923),
      B = c(0, -13.45805, 3.203184),
      C = c(0, 19.86852, -0.541539),
      D = c(0, -6.853936, 0.035925),
      E = c(0, -0.049672, -3.438525),
      F = c(0, -101.6206, -108.0150),
      G = c(0, 228.6866, 218.2768),
      H = c(0, -92.31201, -92.31201)
    ), 
    Sulfur_Dioxide = data.frame(
      Eq_type = c(0, 1, 1),
      Min_temp =  c(0, 298, 1200),
      Max_temp = c(0, 1200, 6000),
      A = c(0, 21.43049, 57.48188),
      B = c(0, 74.35094, 1.009328),
      C = c(0, -57.75217, -0.076290),
      D = c(0, 16.35534, 0.005174),
      E = c(0, 0.086731, -4.045401),
      F = c(0, -305.7688, -324.4140),
      G = c(0, 254.8872, 302.7798),
      H = c(0, -296.8422, -296.8422)
    )
  )
}

element_props <- data.frame(
  Component = c("Other_MSW", "Fixed_C", "Ash", "Water", "Carbon", "Hydrogen", "Nitrogen", "Sulfur", "Oxygen", "Chlorine", 
                "Carbon_Monoxide", "Hydrogen_Chloride", "Carbon_Dioxide", "Methane", "Nitric_Oxide", "Nitrogen_Dioxide", "Sulfur_Dioxide"),
  Symbol = c("(MSW)", "(FC)","(Ash)", "(H2O)", "(C)",  "(H2)", "(N2)",  "(S)", "(O2)", 
             "(Cl2)", "(CO)", "(HCl)", "(CO2)", "(CH4)", "(NO)", "(NO2)","(SO2)" ),
  Molar_Mass = c(100, 12, 15, 18, 12, 2, 28, 32, 32, 70.9, 28, 36.5, 44, 16, 30, 46, 64),
  Tb_K = c(4100, 4100, 4100, 373.17,  4100, 20.25, 77.34, 0, 90.2, 239.5, 81.63, 188, 258, 111, 121.4, 295.08, 263),
  Tc_K = c(7020.5, 7020.50, 7020.50, 647, 7020.5, 33.18, 126.19, 1313, 154.58, 416.95, 134.45, 324.68, 304.12, 190.6, 180, 431.4, 430.34),
  Pc_bar = c(7967.19, 7967.19, 7967.19, 220.64, 7967.19, 13.00, 33.97, 182.08, 50.43, 79.91, 34.98, 82.56, 73.77, 46.1, 64.8, 10.13, 78.84),
  rho_kgm3 = c(1550.00, 1550.00, 1550.00, 6.12, 2267.00, 0.67, 9.24, 2.16, 10.59, 2.99, 13.37, 1.63,  14.64, 6.15, 1.24, 3.40, 30.70),
  visc_Pas = c(0.00E+00, 0.00E+00, 0.00E+00, 2.65E-05, 0.00E+00, 3.36E-05,  3.98E-05, 0.00E+00, 2.61E-05, 2.50E-05, 1.65E-05, 0.00E+00,  3.25E-05, 2.01E-05, 3.70E-05, 0.00E+00, 2.30E-05),
  dHf_Jmol = c(0,0,0,241.826,0,0,0,0,0,0,110.53,0,393.12,0,90.29,33.1,296.81),
  Reac_phase = c("Solid", "Solid", "Solid", "Gas", "Solid", "Gas", "Gas", "Solid", "Gas", 
                 "Gas", "Gas", "Gas", "Gas", "Gas", "Gas", "Gas", "Gas")
)


all_elements <- {list(
  Other_MSW = main_elements$Carbon,
  Fixed_C = main_elements$Carbon,
  Ash = main_elements$Carbon,
  Water = main_elements$Water,
  Carbon = main_elements$Carbon,
  Hydrogen = main_elements$Hydrogen,
  Nitrogen = main_elements$Nitrogen,
  Sulfur = main_elements$Sulfur,
  Oxygen = main_elements$Oxygen,
  Chlorine = main_elements$Chlorine,
  Carbon_Monoxide = main_elements$Carbon_Monoxide,
  Hydrogen_Chloride = main_elements$Hydrogen_Chloride,
  Carbon_Dioxide = main_elements$Carbon_Dioxide,
  Methane = main_elements$Methane,
  Nitric_Oxide = main_elements$Nitric_Oxide,
  Nitrogen_Dioxide = main_elements$Nitrogen_Dioxide,
  Sulfur_Dioxide = main_elements$Sulfur_Dioxide
  )
}


Gen_fluid <- data.frame(
  Fluid = c("Air","Water"),
  rho_kgm3 = c(1.225, 997.5),
  RMM = c(28.97,18),
  visc_Pas = c(18.37E-06, 0.8891E-03),
  dH_Jmol = c(0, 40.7E+3),
  Cp = c(0.7, 4180)
)

Solid_waste <- data.frame(
  Solid = c("Slag","Ash"),
  rho_kgm3 = c(2522, 1700),
  Size = c(0.1, 1E-04),
  Cp = c(1450, 1450),
  dHf = c(100, 100)
)
