# =============================================================================
# rate_constants.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Using existing models for rate constants based on Arhenius eq
#  calculate rate constant value for reaction temp
#
# =============================================================================

rate_constants <- rate_eqs[,c("RNo","Equation","Rate_eq")]
rate_constants[,"DeltaH"] <- reaction_data$DeltaH

rate_constants[,c("k_fwd","k_bwd","k_ovt")] <- 1
k_list <- paste0("k",rate_constants$RNo)
# Different Rate Constant Eqs ---------------------------------------------

rate_constants[1, c("k_fwd","k_bwd")] <- c("5.96E+02 * Tp * exp(-1800/(T))", "1")
rate_constants[2, c("k_fwd","k_bwd")] <- c("1E+15 * Tp * exp(-16000/(T))", "1")
rate_constants[3, c("k_fwd","k_bwd")] <- c("8.71E+3 * exp(-17967/Tp)", "1")
rate_constants[4, c("k_fwd","k_bwd")] <- c("2.20E+09 * exp(-109000/R*T)", "1")
rate_constants[5, c("k_fwd","k_bwd")] <- c("1.272*T*exp(-22645/T)", "1.044E-4*(T^2)*exp((-6319/T)-17.29)")
rate_constants[6, c("k_fwd","k_bwd")] <- c("2.50E+5 * exp(-16600/T)", "2.38E+3*T*exp(-16600/T)")
rate_constants[7, c("k_fwd","k_bwd")] <- c("3E+5*exp(-15042/T)", "1")
rate_constants[8, c("k_fwd","k_bwd")] <- c("1.368E-3*T*exp((-8078/T)-7.087)", "0.151*(T^0.5)*exp((-13578/T)-0.372)")
rate_constants[9, c("k_fwd","k_bwd")] <- c("3E+5*exp(-15042/T)", "1")
rate_constants[10,c("k_fwd","k_bwd")] <- c("1.272*T*exp(-22645/T)", "1.044E-4*(T^2)*exp((-2363/T)-20.92)")
rate_constants[11,c("k_fwd","k_bwd")] <- c("5.9E+9*T*exp(-6280/(R*T))", "1")
rate_constants[12,c("k_fwd","k_bwd")] <- c(paste(reaction_props[12,"K_val"],"*T"), "1")
rate_constants[13,c("k_fwd","k_bwd")] <- c(paste(reaction_props[13,"K_val"],"*T"), "1")
rate_constants[14,c("k_fwd","k_bwd")] <- c(paste(reaction_props[14,"K_val"],"*T"), "1")


# Evaluating Rate Constant at various Temps -------------------------------

k_func <- function(equation_text, R, Tp, T) {
  eval(parse(text = equation_text))
}

k_temp <- function(n,Tpn,Tn){
    kn_f <- k_func(rate_constants[n,"k_fwd"], R, Tpn, Tn)
    kn_b <- k_func(rate_constants[n,"k_bwd"], R, Tpn, Tn)
    kn_t <- kn_f / kn_b
    # print(c(kn_f,kn_b,kn_t))
    return(kn_t)
}

temp_list <- (seq(1500,6000, by = 100))
k_df <- data.frame()
for (n in 1:nrow(rate_constants)){
  for (nT in 1:length(temp_list)){
  Tn <- temp_list[nT]
  k_df[n,nT] <- k_temp(n,Tn,Tn)
  }
}

k_df["Temp",] <- temp_list
k_df <- data.frame(t(k_df))
K_df1 <- k_df
k_df <- pivot_longer(k_df, cols = -Temp, names_to = "KVar", values_to = "KVal")
k_df <- k_df %>% 
  mutate(KVar = gsub("X", "K", KVar))
k_df[,"lnk"] <- log(k_df$KVal)

k_valuesdH <- setNames(rate_constants$DeltaH, k_list)
k_df$dH <- rep(k_valuesdH[k_list], length.out = nrow(k_df))

k_plot <-   
  ggplot() +
  geom_line(data = k_df, aes(x = Temp, y = lnk, group = KVar, color = as.character(sign(dH))), na.rm = TRUE) +
  geom_dl(data = k_df, mapping = aes(x = Temp, y = lnk, group = KVar, color = as.character(sign(dH)), label = KVar),
          method = list(dl.combine("last.points")), angle = 0, vjust = 0, size = 1e-7, na.rm = TRUE) +
  geom_vline(xintercept = Reac_temp, linetype = "dashed", color = "black") +
  geom_text(data = data.frame(x = Reac_temp, y = 40), aes(x, y, label = paste("Reaction Temp (K) =", Reac_temp)), 
            angle = 0, vjust = 0, hjust = -0.015, color = "black", size = 4.5, na.rm = TRUE) +
  labs(x = "Temp", y = "lnK", title = "lnK vs Temp(K)") +
  scale_color_manual(name = "Reaction Enthalpy", 
                     values = c("blue","red"), 
                     labels = c("-dH: Exothermic","+dH: Endothermic")) +
  PGDP_theme() +
  theme(legend.position = "top",
        legend.title = element_text(size = 11),
        legend.text  = element_text(size = 11) ) +
  coord_cartesian(clip = "off") # + xlim(1500,3000)
print(k_plot)


ggsave(file.path(pic_folder, "lnKvT.png"), k_plot,  width = 200, height = 160, units = "mm")

# Reaction Temp Rate Constants --------------------------------------------

# view_table(rate_constants)
for (n in 1:nrow(rate_constants)){
  rate_constants[n,"k_ovt"] <- k_temp(n,Reac_temp,Reac_temp)
}
