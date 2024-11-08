# =============================================================================
# ModelPumps_Data.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: GrundFos Pumps used for pumping of fluids(water) from storage
#
# =============================================================================

Pump_15m3_data <- structure(list(
  Q_m3hr = c(0, 8.84625, 17.6925, 25.002821250201, 31.8938857810493, 
             38.6490071078948, 45.5107605485063, 52.7341495453213, 60.6494149020603, 
             0, 8.11125, 16.2225, 24.33375, 31.8945407702458, 38.6498373341653, 
             45.5117982445315, 52.7354492066434, 60.6510702204779, 
             0, 7.37625, 14.7525, 22.12875, 29.505, 36.88125, 44.2575, 
             51.63375, 59.01, 0, 6.64125, 13.2825, 19.92375, 26.565, 
             33.20625, 39.8475, 46.48875, 53.13, 0, 5.905, 11.81, 
             17.715, 23.62, 29.525, 35.43, 41.335, 47.24, 0, 5.17, 
             10.34, 15.51, 20.68, 25.85, 31.02, 36.19, 41.36, 0, 
             4.435, 8.87, 13.305, 17.74, 22.175, 26.61, 31.045, 35.48, 
             0, 3.7, 7.4, 11.1, 14.8, 18.5, 22.2, 25.9, 29.6, 0, 
             2.965, 5.93, 8.895, 11.86, 14.825, 17.79, 20.755, 23.72, 
             0, 2.23, 4.46, 6.69, 8.92, 11.15, 13.38, 15.61, 17.84), 
  Head_m = c(14.78, 14.78,14.78, 13.08, 11, 8.962, 6.799, 4.329, 1.326, 
             13.22, 13.23, 12.98, 12.39, 11, 8.962, 6.798, 4.328, 1.325, 
             10.93, 10.94, 10.73, 10.25, 9.41, 8.16, 6.428, 4.148, 1.254, 
             8.863, 8.868, 8.7, 8.304, 7.627, 6.614, 5.21, 3.362, 1.015, 
             7.009, 7.013, 6.88, 6.567, 6.032, 5.231, 4.121, 2.661, 0.806, 
             5.372, 5.375, 5.274, 5.034, 4.623, 4.009, 3.159, 2.039, 0.617, 
             3.953, 3.955, 3.88, 3.704, 3.402, 2.95, 2.324, 1.5, 0.453, 
             2.751, 2.752, 2.7, 2.577, 2.367, 2.053, 1.617, 1.043, 0.315, 
             1.766, 1.767, 1.734, 1.655, 1.52, 1.318, 1.038, 0.669, 0.201, 
             0.999, 0.999, 0.98, 0.936, 0.859, 0.745, 0.587, 0.378, 0.113), 
  RPM = c(4440, 4440, 4440, 4440, 4440, 4440,  
          4440, 4440, 4440, 4071, 4071, 4071, 4071, 4071, 4071, 4071, 4071, 
          4071, 3702, 3702, 3702, 3702, 3702, 3702, 3702, 3702, 3702, 3333, 
          3333, 3333, 3333, 3333, 3333, 3333, 3333, 3333, 2964, 2964, 2964, 
          2964, 2964, 2964, 2964, 2964, 2964, 2595, 2595, 2595, 2595, 2595, 
          2595, 2595, 2595, 2595, 2226, 2226, 2226, 2226, 2226, 2226, 2226, 
          2226, 2226, 1857, 1857, 1857, 1857, 1857, 1857, 1857, 1857, 1857, 
          1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1119, 1119, 
          1119, 1119, 1119, 1119, 1119, 1119, 1119)
), 
class = c("tbl_df","tbl", "data.frame"), row.names = c(NA, -90L)
)


Pump_4m3_data <- data.frame(
  Q_m3hr = c(
    0, 8.84625, 17.6925, 25.00282125, 31.89388578, 38.64900711, 45.51076055, 52.73414955, 60.6494149,
    0, 2.124875, 4.24975, 6.374625, 8.4995, 10.624375, 12.74925, 14.874125, 16.999,
    0, 2.124875, 4.24975, 6.374625, 8.4995, 10.624375, 12.74925, 14.874125, 16.999,
    0, 2.124875, 4.24975, 6.374625, 8.4995, 10.624375, 12.74925, 14.874125, 16.999,
    0, 1.80125, 3.6025, 5.40375, 7.205, 9.00625, 10.8075, 12.60875, 14.41,
    0, 2.124875, 4.24975, 6.374625, 8.4995, 10.624375, 12.74925, 14.874125, 16.999,
    0, 2.124875, 4.24975, 6.374625, 8.4995, 10.624375, 12.74925, 14.874125, 16.999,
    0, 2.124875, 4.24975, 6.374625, 8.4995, 10.624375, 12.74925, 14.874125, 16.999,
    0, 2.090748798, 4.24975, 6.374625, 8.4995, 10.624375, 12.74925, 14.874125, 16.999
  ),
  Head_m = c(
    0.975, 1.03, 1.057, 1.053, 1.016, 0.942, 0.828, 0.671, 0.467,
    1.969, 2.241, 2.513, 2.785, 3.058, 3.33, 3.602, 3.875, 3.104,
    3.425, 4.104, 4.783, 5.461, 6.14, 6.819, 5.745, 4.429, 3.104,
    3.78, 3.78, 3.78, 3.78, 3.78, 3.78, 3.78, 3.78, 3.104,
    4.586, 4.87, 5.011, 4.997, 4.817, 4.462, 3.921, 3.183, 2.238,
    4.882, 6.681, 8.48, 9.478, 8.234, 7.005, 5.745, 4.429, 3.104,
    6.535, 6.535, 6.535, 6.535, 6.535, 6.535, 5.745, 4.429, 3.104,
    9.213, 9.213, 9.213, 9.213, 8.234, 7.005, 5.745, 4.429, 3.104,
    11.78, 11.82, 10.79, 9.478, 8.234, 7.005, 5.745, 4.429, 3.104
  ),
  RPM = c(
    1367, 1367, 1367, 1367, 1367, 1367, 1367, 1367, 1367,
    01, 01, 01, 01, 01, 01, 01, 01, 01,
    02, 02, 02, 02, 02, 02, 02, 02, 02,
    03, 03, 03, 03, 03, 03, 03, 03, 03,
    2965, 2965, 2965, 2965, 2965, 2965, 2965, 2965, 2965,
    04, 04, 04, 04, 04, 04, 04, 04, 04,
    05, 05, 05, 05, 05, 05, 05, 05, 05,
    06, 06, 06, 06, 06, 06, 06, 06, 06,
    4754, 4754, 4754, 4754, 4754, 4754, 4754, 4754, 4754
  )
)


