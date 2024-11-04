# =============================================================================
# ProcessControl.R
# Author: Abbas Moosajee
# Date: 28/05/2024
# Project: Plasma Gasifier DP
#
# Description: Process Control requirements across the node
#
# =============================================================================

# PID controller parameters
Kp <- 1.20  # Proportional gain
Ki <- 0.01  # Integral gain
Kd <- 0.01  # Derivative gain

# Desired setpoint and initial temperature
setpoint <- Reac_temp # K
initial_temp <- 1300 # K

# Time vector
time <- seq(0, Tr_s, by=1)

# Temperature control function using PID
pid_control <- function(time, temp) {
  error <- setpoint - temp
  P <- Kp * error
  I <- Ki * cumsum(error)
  D <- Kd * c(0, diff(error))
  control_signal <- P + I + D
  return(control_signal)
}

# Simulating temperature control
temp <- numeric(length(time))
temp[1] <- initial_temp
for (i in 2:length(time)) {
  control_signal <- pid_control(time[1:i], temp[1:i])
  temp[i] <- temp[i-1] + control_signal[i]
}

# Convert to data frame for ggplot
temp_data <- data.frame(time = time, temperature = temp)

PC_Temp <-
ggplot(temp_data, aes(x = time, y = temperature)) +
  geom_line(color = 'blue') +
  geom_hline(yintercept = setpoint, linetype = 'dashed', color = 'red') +
  labs(title = 'Temperature Control Using PID', x = 'Time (s)', y = 'Temperature (K)') +
  PGDP_theme()

print(PC_Temp)

