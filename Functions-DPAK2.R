# List of packages to install
packages_used <- c('ggplot2','corrplot','dplyr','tidyr','tidytext',
                   'ggalt','ggrepel','scales','forecast',"deSolve",
                   'stringr','stringdist','scales','tm',"kableExtra",
                   'tidyverse',"GiRaF","ggiraph","reticulate","Ryacas")

# Check if each package is already installed before attempting to install it
for (package in packages_used) {
  if (!(package %in% installed.packages()[,"Package"])) {
    # Package is not installed, so install it
    install.packages(package)
  } else {
    # Package is already installed, print a message
    cat(paste("Package", package, "is already installed.\n"))
  }
}

# Load packages
for (package in packages_used) {
  library(package, character.only = TRUE)
}

PGDP_theme <- function(){
  theme_minimal() +
    theme(
      text = element_text(color = "black"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 12,face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "gray", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray", fill = NA, linewidth = 0.5)
    )
}

