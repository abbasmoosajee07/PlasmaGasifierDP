Cp_int <- function(T, TP_data) {
  if (T == 0) {
    Cp_int = 0
  } else {
    t = T 
    Cp_int = (TP_data$A * t + (TP_data$B * t^2) / 2 + 
                (TP_data$C * t^3) / 3 + (TP_data$D * t^4) / 4 - TP_data$E / t)
  }
  return(Cp_int)
}

S_func <- function(T, TP_data) {
  if (T == 0) {
    S_eq = 0
  } else {
    t=T
    S_eq <- TP_data$A*log(t) + TP_data$B*t + (TP_data$C*t^2)/2 + 
      (TP_data$D*t^3)/3 - TP_data$E/(2*t^2) + TP_data$G
    return(S_eq)
  }
}


state_func <- function(TP_data,Upper_limit,Lower_limit){
  nrow = nrow(TP_data)
  
  TP_data3 = TP_data[nrow,]
  if (any(is.na(TP_data3))) {
    Max_t3 = 0
    Min_t3 = 0
  } else {
    if (Upper_limit < TP_data3$Min_temp){
      Max_t3 = 0
    } else{
      if (Upper_limit<=TP_data3$Max_temp ){
        if(Upper_limit >= TP_data3$Min_temp){
          Max_t3 = Upper_limit
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
      if (Lower_limit <= TP_data3$Min_temp){
        Min_t3 = TP_data3$Min_temp
      } else {
        Min_t3 = Lower_limit
      }
    }
  }
  
  TP_data2 = TP_data[nrow-1,]
  if (any(is.na(TP_data2))) {
    Max_t2 = 0
    Min_t2 = 0
  } else {
    if (Min_t3 == 0){
      if (Upper_limit <= TP_data2$Min_temp){
        Max_t2 = 0
      } else {
        Max_t2 = Upper_limit
      }
    } else {
      if (Min_t3 <= TP_data2$Max_temp){
        if (Min_t3 == TP_data2$Max_temp) {
          Max_t2 = TP_data2$Max_temp
        } else {
          Max_t2 = Upper_limit
        }
      } else {
        Max_t2 = 0
      }
    }
    
    if (Max_t2 == 0){
      Min_t2 = 0
    } else {
      if (Lower_limit <= TP_data2$Min_temp){
        Min_t2 = TP_data2$Min_temp
      } else {
        Min_t2 = Lower_limit
      }
    }
  }
  
  TP_data1 = TP_data[nrow-2,]
  if (any(is.na(TP_data1))) {
    Max_t1 = 0
    Min_t1 = 0
  } else {
    if (Min_t2 == 0) {
      if (Upper_limit <= TP_data1$Min_temp){
        Max_t1 = 0
      } else {
        
        if (Min_t3 == 0) {
          Max_t1 = Upper_limit
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
      Min_t1 = Lower_limit
    }
  }
  
  
    T1000 = 1000

  dH3  <- Cp_int(Max_t3/T1000,TP_data3) - Cp_int(Min_t3/T1000,TP_data3)
  dH2  <- Cp_int(Max_t2/T1000,TP_data2) - Cp_int(Min_t2/T1000,TP_data2)
  dH1  <- Cp_int(Max_t1/T1000,TP_data1) - Cp_int(Min_t1/T1000,TP_data1)
  dHT  <- (dH3) + (dH2) + (dH1)
  
  dS3  <- S_func(Max_t3/T1000,TP_data3) - S_func(Min_t3/T1000,TP_data3)
  dS2  <- S_func(Max_t2/T1000,TP_data2) - S_func(Min_t2/T1000,TP_data2)
  dS1  <- S_func(Max_t1/T1000,TP_data1) - S_func(Min_t1/T1000,TP_data1)
  dST  <- (dS3) + (dS2) + (dS1)
  
  dG3  <- (Cp_int(Max_t3/T1000,TP_data3) - S_func(Max_t3/T1000,TP_data3)*Max_t3/T1000) -
    (Cp_int(Min_t3/T1000,TP_data3) - S_func(Min_t3/T1000,TP_data3)*Min_t3/T1000)
  dG2  <- (Cp_int(Max_t2/T1000,TP_data2) - S_func(Max_t2/T1000,TP_data2)*Max_t2/T1000) -
    (Cp_int(Min_t2/T1000,TP_data2) - S_func(Min_t2/T1000,TP_data2)*Min_t2/T1000)
  dG1  <- (Cp_int(Max_t1/T1000,TP_data1) - S_func(Max_t1/T1000,TP_data1)*Max_t1/T1000) -
    (Cp_int(Min_t1/T1000,TP_data1) - S_func(Min_t1/T1000,TP_data1)*Min_t1/T1000)
  dGT  <- (dG3) + (dG2) + (dG1)
  # print(c(Max_t3,Min_t3,Max_t2,Min_t2,Max_t1,Min_t1))

  return(c(dHT,dST, dGT))
  
}
# Calculate properties for each element
thermo_properties <- function(elements,Upper_limit,Lower_limit){
  all_results <- data.frame()
  for (element_name in names(elements)) {
    element_data <- elements[[element_name]]
    
    index <- element_props$Component == element_name
    result_df <- element_props[index, ]
    
    result = state_func(element_data,Upper_limit,Lower_limit)
    result <- data.frame(Enthalpy=result[1],Entropy=result[2],Gibbs=result[3],Element=element_name)
    all_results <- rbind(all_results, result)
  }
  
  return(all_results)
}

U_T = 1500
L_T = 330
# U_T = 2750
# L_T = 1500

# main_results <- thermo_properties(main_elements,U_T,L_T)
Input_results <- thermo_properties(Input_elements,U_T,L_T)
Output_results <- thermo_properties(Output_elements,U_T,L_T)
dHiT = sum(Input_results$Enthalpy) + sum(Output_results$Enthalpy)

