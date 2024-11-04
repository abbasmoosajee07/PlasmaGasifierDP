# =============================================================================
# StateVariable_Calcs.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: A function that uses a series of if-fucntions to evaluate which 
#   shomate equation should be used to calculate the enthalpy, entropy and Cp
#
# =============================================================================

state_func <- function(TP_data,Start_Temp,Final_Temp){
  nrow = nrow(TP_data)

  if (Start_Temp < Final_Temp){
    def = +1
    Lower_Temp = Start_Temp
    Upper_Temp = Final_Temp
  } else {
    def = -1
    Lower_Temp = Final_Temp
    Upper_Temp = Start_Temp
  }

  
  TP_data3 = TP_data[nrow,]
  if (any(is.na(TP_data3))) {
    Max_t3 = 0
    Min_t3 = 0
  } else {
    if (Upper_Temp < TP_data3$Min_temp){
      Max_t3 = 0
    } else{
      if (Upper_Temp<=TP_data3$Max_temp ){
        if(Upper_Temp >= TP_data3$Min_temp){
          Max_t3 = Upper_Temp
        } else {
          Max_t3 = TP_data3$Max_temp
        }
      } else{
        Max_t3 = 0
      }
    }
    
    if (Max_t3 == 0){
      Min_t3 = 0
    } else {
      if (Lower_Temp <= TP_data3$Min_temp){
        Min_t3 = TP_data3$Min_temp
      } else {
        Min_t3 = Lower_Temp
      }
    }
  }
  
  TP_data2 = TP_data[nrow-1,]
  if (any(is.na(TP_data2))) {
    Max_t2 = 0
    Min_t2 = 0
  } else {
    if (Min_t3 == 0){
      if (Upper_Temp <= TP_data2$Min_temp){
        Max_t2 = 0
      } else {
        Max_t2 = Upper_Temp
      }
    } else {
      if (Min_t3 <= TP_data2$Max_temp){
        if (Min_t3 == TP_data2$Max_temp) {
          Max_t2 = TP_data2$Max_temp
        } else {
          Max_t2 = Upper_Temp
        }
      } else {
        Max_t2 = 0
      }
    }
    
    if (Max_t2 == 0){
      Min_t2 = 0
    } else {
      if (Lower_Temp <= TP_data2$Min_temp){
        Min_t2 = TP_data2$Min_temp
      } else {
        Min_t2 = Lower_Temp
      }
    }
  }
  
  TP_data1 = TP_data[nrow-2,]
  if (any(is.na(TP_data1))) {
    Max_t1 = 0
    Min_t1 = 0
  } else {
    if (Min_t2 == 0) {
      if (Upper_Temp <= TP_data1$Min_temp){
        Max_t1 = 0
      } else {
        
        if (Min_t3 == 0) {
          Max_t1 = Upper_Temp
        } else {
          Max_t1 = 0
        }
        
      }
    } else {
      if (Min_t2 == TP_data1$Max_temp){
        Max_t1 = TP_data1$Max_temp
      } else {
        Max_t1 = 0
      }
    }
    
    if (Max_t1 == 0){
      Min_t1 = 0
    } else {
      Min_t1 = Lower_Temp
    }
  }

  
  T1000 = 1000
  Cp3  <- Cp_int(Max_t3/T1000,TP_data3) - Cp_int(Min_t3/T1000,TP_data3)
  Cp2  <- Cp_int(Max_t2/T1000,TP_data2) - Cp_int(Min_t2/T1000,TP_data2)
  Cp1  <- Cp_int(Max_t1/T1000,TP_data1) - Cp_int(Min_t1/T1000,TP_data1)
  CpdT  <- (Cp3 + Cp2 + Cp1) * def
  
  dH3  <- H_func(Max_t3/T1000,TP_data3) - H_func(Min_t3/T1000,TP_data3)
  dH2  <- H_func(Max_t2/T1000,TP_data2) - H_func(Min_t2/T1000,TP_data2)
  dH1  <- H_func(Max_t1/T1000,TP_data1) - H_func(Min_t1/T1000,TP_data1)
  dHT  <- (dH3 + dH2 + dH1) * def
  
  dS3  <- S_func(Max_t3/T1000,TP_data3) - S_func(Min_t3/T1000,TP_data3)
  dS2  <- S_func(Max_t2/T1000,TP_data2) - S_func(Min_t2/T1000,TP_data2)
  dS1  <- S_func(Max_t1/T1000,TP_data1) - S_func(Min_t1/T1000,TP_data1)
  dST  <- (dS3 + dS2 + dS1) * def
  

  # print(c(Max_t3,Min_t3,Max_t2,Min_t2,Max_t1,Min_t1))
  result <- data.frame("Enthalpy"=dHT, "Entropy" = dST, "CpdT" =CpdT)
  return(result)
  
}

# Calculate properties for each element
thermo_properties <- function(elements,Start_Temp,Final_Temp){
  all_results <- data.frame()
  for (element_name in names(elements)) {
    element_data <- elements[[element_name]]
    # print(element_name)
    index <- element_props$Component == element_name
    result_df <- element_props[index, ]
    
    result <- state_func(element_data,Start_Temp,Final_Temp)
    result <- data.frame(Element=element_name,Enthalpy=result["Enthalpy"],
                         Entropy=result["Entropy"],CpT=result["CpdT"])
    all_results <- rbind(all_results, result)
  }
  
  return(all_results)
}


