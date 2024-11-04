# =============================================================================
# Economic_Analysis.R
# Author: Abbas Moosajee
# Date: 07/03/2024
# Project: Plasma Gasifier DP
#
# Description: Perform Cashflow Analysis on the Economic Viability of Project
#
# =============================================================================

Cons_Sched <- data.frame(
  Year = c(1, 2, 3, 4, 5, 6, 7),
  FC = c(40, 40, 20, 0, 0, 0, 0)/100,
  WC = c(0, 0, 100, 0, 0, 0, 0)/100,
  FCOP = c(0, 0, 100, 100, 100, 100, 100)/100,
  VCOP = c(0, 0, 30, 90, 100, 100, 100)/100,
  Rev = c(0, 0, 30, 90, 100, 100, 100)/100
)

Years = seq(1,20,by = 1)


# Economic Assumptions --------------------------------------------------------
Equity <- 0.10
Equity_Stake <- 0.15
Interest_Rate <- 0.0425
Corporate_Tax_Rate <- 0.25
Loan_Period <- 10
Cost_of_Capital <- 0.0533

CashFlow_func <- function(BalanceSheet_df){
  
  Debt_Amount <- (BalanceSheet_df$CapCost + BalanceSheet_df$WorkCap) * (1-Equity)
  Equity_Amount <- (BalanceSheet_df$CapCost + BalanceSheet_df$WorkCap) * Equity
  Equity_Stake_Amount <- BalanceSheet_df$Profit * Equity_Stake
  Annual_Depreciation <- BalanceSheet_df$CapCost / Loan_Period
  
  Ann_LoanPayment <- (Debt_Amount * Interest_Rate * (1 + Interest_Rate)^Loan_Period) /
                      ((1 + Interest_Rate)^Loan_Period - 1)
  Total_Loan_Repayment <- Ann_LoanPayment * Loan_Period
  
  Cashflow_df <- {data.frame(
    Year = c(),
    Cap_Exp = c(),
    Tot_Rev = c(),
    CCOP = c(),
    Gross_Profit = c(),
    Depreciation = c(),
    Taxable_Income = c(),
    Tax_Paid = c(),
    Cash_Flow = c(),
    Loan_Payment = c(),
    PV_CF = c(),
    Net_Profit = c(),
    Lifetime = c()
  )}
  
  # Cash Flow Analysis ----------------------------------------------------------
  for (Yr in 1:length(Years)){
    
    if (Yr >= nrow(Cons_Sched)){
      Cons_Yr <- Cons_Sched[nrow(Cons_Sched),]
    } else {
      Cons_Yr <- Cons_Sched[Yr,]
    }
    
    Cashflow_df[Yr,"Year"]    = Yr
    Cashflow_df[Yr,"Cap_Exp"] = Cons_Yr$FC  * BalanceSheet_df$CapCost + 
                                  Cons_Yr$WC * BalanceSheet_df$WorkCap
    Cashflow_df[Yr,"Tot_Rev"] = Cons_Yr$Rev * BalanceSheet_df$TotalRev
    Cashflow_df[Yr,"CCOP"]    = Cons_Yr$FCOP * BalanceSheet_df$FCOP +
                                  Cons_Yr$VCOP * BalanceSheet_df$VCOP
    Cashflow_df[Yr,"Gross_Profit"] = Cashflow_df[Yr,"Tot_Rev"] - Cashflow_df[Yr,"CCOP"]
    
    Cashflow_df[Yr,"Depreciation"] = 0
    
    if(Cashflow_df[Yr,"Cap_Exp"] <= 0){
  
      if (sum(Cashflow_df[,"Depreciation"]) >= BalanceSheet_df$CapCost){
        Cashflow_df[Yr,"Depreciation"] = 0
        } else {
        if (Cashflow_df[Yr,"Gross_Profit"] >= 0){
          Cashflow_df[Yr,"Depreciation"] = Annual_Depreciation
        }
      }
    }
    
    
    
    Cashflow_df[Yr,"Taxable_Income"] = Cashflow_df[Yr,"Gross_Profit"] - 
                                        Cashflow_df[Yr,"Depreciation"]
    if (Cashflow_df[Yr,"Taxable_Income"] >= 0){
      Cashflow_df[Yr,"Tax_Paid"] = Cashflow_df[Yr,"Taxable_Income"] * Corporate_Tax_Rate
    } else {
      Cashflow_df[Yr,"Tax_Paid"] = 0
    }
    
    Cashflow_df[Yr,"Cash_Flow"] = Cashflow_df[Yr,"Gross_Profit"] - 
                    Cashflow_df[Yr,"Tax_Paid"] - Cashflow_df[Yr,"Cap_Exp"]
    Cashflow_df[Yr,"Loan_Payment"] = 0
    
    if(Cashflow_df[Yr,"Cap_Exp"] <= 0){
  
      if ((Total_Loan_Repayment-sum(Cashflow_df[,"Loan_Payment"])) >= Ann_LoanPayment){
        if (Cashflow_df[Yr,"Cash_Flow"] > 0){
          Cashflow_df[Yr,"Loan_Payment"] = Ann_LoanPayment
        }
      }
    }
    
    Cashflow_df[Yr,"PV_CF"] = Cashflow_df[Yr,"Cash_Flow"] * (1 + Interest_Rate)^-Yr
    Cashflow_df[Yr,"Net_Profit"] = Cashflow_df[Yr,"Cash_Flow"] - Cashflow_df[Yr,"Loan_Payment"]
    Cashflow_df[Yr,"LifeTime"] = sum(Cashflow_df$Net_Profit)

  }
  return(Cashflow_df)
}
