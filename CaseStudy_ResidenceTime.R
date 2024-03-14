Vol_list   <- seq(0,200,  by = 25)
Press_list <- seq(1E+5,5E+6, by = 1E+6)

Case_RT <- data.frame()

for (nV in 1:length(Vol_list)){
  for (nP in 1:length(Press_list)){
    Voln <- Vol_list[nV]
    Pressn <- Press_list[nP]
    
    Comp_n <- Composition_props(Output_composition,Pressn,Reac_temp)
    VFlow_n <- sum(Comp_n$Vol_m3hr)/ 3600

    RTn <- (Voln / VFlow_n) 
    Case_RTn <- data.frame(Vol = Voln, Press = Pressn,
                           Vol_Flow = VFlow_n, RT = RTn
    )
    Case_RT  <- rbind(Case_RT, Case_RTn)
  }
}

CSRT_plot <-
  ggplot() +
  geom_line(data = Case_RT, aes(x = Press, y = RT, group = Vol, color = as.character(Vol)), na.rm = TRUE) +
  geom_dl(data = Case_RT, mapping = aes(x = Press, y = RT, group = Vol, color = as.character(Vol), label =  as.character(Vol)),
          method = list(dl.combine("last.points")), angle = 0, vjust = 0, size=1e-7, na.rm = TRUE) +
  labs(x = "Press(Pa)", 
       y = "Residence Time(s)", 
       color = "Volume(m^3)",
       title = "Effect of Pressure and Volume on Residence Time") +
  PGDP_theme() +
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.position = "top",
        legend.title = element_text(size = 11),
        legend.text  = element_text(size = 11) ) +
  guides(color = guide_legend(nrow = 1)) +
  coord_cartesian(clip = "off")

print(CSRT_plot)
# ggsave(file.path( pic_folder, "CSRT.png"), CSRT_plot, width = 170, height = 120, units = "mm")

