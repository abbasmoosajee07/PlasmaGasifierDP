
rate_constants <- rate_eqs[,c(1,3,4)]
rate_constants[,c("k_fwd","k_bwd","k_ovt")] <- 1
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
rate_constants[12,c("k_fwd","k_bwd")] <- c(paste(reaction_props[12,"K_val"]), "1")
rate_constants[13,c("k_fwd","k_bwd")] <- c(paste(reaction_props[13,"K_val"]), "1")
rate_constants[14,c("k_fwd","k_bwd")] <- c(paste(reaction_props[14,"K_val"]), "1")


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
k_df <- pivot_longer(k_df, cols = -Temp, names_to = "Variable", values_to = "Value")
k_df <- k_df %>% 
  mutate(Variable = gsub("X", "K", Variable))
k_df[,"lnk"] <- log(k_df$Value)

k_plot <-   
  ggplot() +
  geom_line(  data = k_df, aes(x = Temp, y = lnk, color = Variable), na.rm = TRUE) +
  geom_dl(data = k_df, mapping = aes(x = Temp, y = lnk, color = Variable, label = Variable), 
          method = list(dl.combine("last.points")), angle = 0, vjust = 0, size=1e-7, na.rm = TRUE) +
  labs(x = "Temp", y = "lnK", title = "lnK vs Temp(K)") +
  PGDP_theme() +
  theme(legend.position = "None") +
  coord_cartesian(clip = "off")
print(k_plot)

ggsave(file.path(pic_folder, "lnKvT.png"), k_plot, width = 8, height = 6, units = "in")

# Reaction Temp Rate Constants --------------------------------------------

# view_table(rate_constants)
for (n in 1:nrow(rate_constants)){
  rate_constants[n,"k_ovt"] <- k_temp(n,Reac_temp,Reac_temp)
}
