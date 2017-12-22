
#******************************************************************************
# Auxilary functions
#******************************************************************************

# 1. pmt = function(pv,r,N)

# 2. pv = function(pmt,r,N)

# 3. index.table = function(Table,index.vec)

# 4. pv.CF(CF,r)
#
# 5. AddDF(df1,df2)
#------------------------------------------------------------------------------

#******************************************************************************
# Loan type functions
#******************************************************************************

# 1. ShpitzerTable = function (Principal,Interest,Horizon)

# 2. VarShpitzerTable = function (Principal,Interest,Horizon)

# 3. index.table = function(Table,index.vec)

# 4. RealShpitzerTable = function(Table,CPI.index)
#  
# 5. MortgageLoanTable = function(Principal, AnnualHorizon, Schedule = "Shpitzer"
#                            ,LoanType = NULL,NominalInterest = NULL
#                            ,RealInterest = NULL, InterestVector = NULL
#                            ,IndexVector = NULL)
#
# 6. SetMortgageLoan = function(Principal, AnnualHorizon, Schedule = "Shpitzer"
#                           ,LoanType = NULL,NominalInterest = NULL
#                           ,RealInterest = NULL, InterestVector = NULL
#                           ,IndexVector = NULL)
#
# 7. CalculateMortgageLoan = function(MortgageLoan)
#
# 8. CalculateMortgageMix = function(MortgageMix, IndexVector,PrimeVector)
#
# 9. CalculateRefinanceCashFlow = function(CurrentMortgageLoan, refinance_period, refinance_rate)
#
# 10. CalculateMortgageLoanRefinance = function(CurrentMortgageLoan, term_structure)
#------------------------------------------------------------------------------

pmt = function(pv,r,N){
  
    pmt = pv / ((1 - (1 / (1+r) ^ N)) / r)
    
    return(pmt)
}

#******************************************************************************
pv = function(pmt,r,N){
  
    pv = pmt * (1 - (1 / (1+r) ^ N)) / r
}

#******************************************************************************

index.table = function(Table,index.vec){
  
  
  colnames = c("PrincipalPmt","InterestPmt","TotalPmt","EndBalance")
  
  Table[,colnames(Table) %in% colnames] = 
    Table[,colnames(Table) %in% colnames] * index.vec
  
  
  Table[,colnames(Table) == "StartBalance"] =
    Table[,colnames(Table) == "StartBalance"] * c(1,index.vec[-length(index.vec)])
  
  return(Table)
  
}

#*******************************************************************************

pv.CF = function(CF,r){
  
  
return(sum(CF / (1+r) ^ (1: length(CF))))

  
}

#*******************************************************************************
ShpitzerTable = function (Principal,Interest,Horizon){
  
    ShpitzerTable = data.frame(Period = numeric(Horizon),
                               StartBalance = numeric(Horizon),
                               PrincipalPmt = numeric(Horizon),
                               InterestPmt = numeric(Horizon),
                               TotalPmt = numeric(Horizon),
                               EndBalance = numeric(Horizon))
    
      ShpitzerTable$Period = seq(1,Horizon)
                               
      ShpitzerTable$TotalPmt = pmt(pv = Principal,r = Interest,N = Horizon)
      
      ShpitzerTable$StartBalance = pv(ShpitzerTable$TotalPmt,r = Interest,
                                      N = Horizon - ShpitzerTable$Period + 1)
    
      ShpitzerTable$InterestPmt = Interest * ShpitzerTable$StartBalance
      
      ShpitzerTable$PrincipalPmt = 
      ShpitzerTable$TotalPmt - ShpitzerTable$InterestPmt
      
      ShpitzerTable$EndBalance = 
      ShpitzerTable$StartBalance - ShpitzerTable$PrincipalPmt
    
return(ShpitzerTable)
  
}

AddDF = function(df1,df2){
  
  # Check number of rows in each data frame then
  # Assign large df to df.large and small df to df.small
  
  if (nrow(df1) >= nrow(df2)){
    
      df.large = df1
      
      df.small = df2

  } else {
    
      df.large = df2
      
      df.small = df1

  }
  
  # Check number of rows in df.small if df.small has rows add data
  
  rownum = nrow(df.small)
  
  if (rownum > 0) {
    
      df = df.small[!names(df.small) == "Period"]
      + df.large[1:rownum,!names(df.large) == "Period"]
      
      df = cbind("Period" = df.small$Period,df)
      
      df = rbind(df,df.large[(rownum+1):nrow(df.large),])

      return(df)
  
  } else {
    
      return(df.large)
  }
  
}

#*******************************************************************************

VarShpitzerTable = function (Principal,Interest,Horizon){
  
  
  if (!length(Interest) == Horizon) {
    
    stop("Interest vector in not the same length as Horizon")
    
    }
  
  ShpitzerTable = data.frame(Period = numeric(Horizon),
                             StartBalance = numeric(Horizon),
                             PrincipalPmt = numeric(Horizon),
                             InterestPmt = numeric(Horizon),
                             TotalPmt = numeric(Horizon),
                             EndBalance = numeric(Horizon))
  
  # Set First row in PMT table
  
  ShpitzerTable$Period = seq(1,Horizon)
  
  ShpitzerTable$TotalPmt = pmt(pv = Principal,N = Horizon,r = Interest[1])
  
  ShpitzerTable$StartBalance[1] = Principal
  
  ShpitzerTable$InterestPmt = Interest[1] * ShpitzerTable$StartBalance[1]
  
  ShpitzerTable$PrincipalPmt[1] = 
    ShpitzerTable$TotalPmt[1] - ShpitzerTable$InterestPmt[1]
  
  ShpitzerTable$EndBalance = 
    ShpitzerTable$StartBalance[1] - ShpitzerTable$PrincipalPmt[1]
  
  for (i in 2:Horizon){
    
    ShpitzerTable$StartBalance[i] = ShpitzerTable$EndBalance[i - 1]
    
    ShpitzerTable$TotalPmt = pmt(pv = ShpitzerTable$StartBalance[i]
                                 ,N = (Horizon - i + 1),r = Interest[i])
    
    ShpitzerTable$InterestPmt[i] = Interest[i] * ShpitzerTable$StartBalance[i]
    
    ShpitzerTable$PrincipalPmt[i] = 
      ShpitzerTable$TotalPmt[i] - ShpitzerTable$InterestPmt[i]
    
    ShpitzerTable$EndBalance[i] = 
      ShpitzerTable$StartBalance[i] - ShpitzerTable$PrincipalPmt[i]
    
  }
  
  
  return(ShpitzerTable)
  
}

#*******************************************************************************

RealShpitzerTable = function(Table,CPI.index){
  
  resTable = index.table(Table = Table, index.vec = CPI.index)
  
  return(resTable)
}

#*******************************************************************************

MortgageLoanTable = function(Principal, AnnualHorizon, Schedule = "Shpitzer"
                        ,LoanType = "FixedRate",NominalInterest = NULL
                        ,RealInterest = NULL, InterestVector = NULL
                        ,IndexVector = NULL){
  
# Shpitzer Schedule
#------------------------------------------------------------------------------  
  if(Schedule == "Shpitzer"){
    
    if(LoanType == "FixedRate"){
    
    MortgageLoan = ShpitzerTable(Principal = Principal
                                 ,Interest = NominalInterest
                                 ,Horizon = AnnualHorizon)
    }
    
    if(LoanType %in% c("Prime","VarRate")){
      
      MortgageLoan = VarShpitzerTable(Principal = Principal
                                   ,Interest = InterestVector
                                   ,Horizon = AnnualHorizon)
    }
    
    if(LoanType == "RealFixedRate"){
      
      MortgageLoan = RealShpitzerTable(ShpitzerTable(Principal = Principal
                                                     ,Interest = RealInterest
                                                     ,Horizon = AnnualHorizon)
                                       ,IndexVector)
    }
    
    
  }

#------------------------------------------------------------------------------
  
return(MortgageLoan)
  
}

#*******************************************************************************

SetMortgageLoan = function(Principal, AnnualHorizon, Schedule = "Shpitzer"
                           ,LoanType = NULL,NominalInterest = NULL
                           ,RealInterest = NULL, InterestVector = NULL
                           ,IndexVector = NULL){
  
    MortgageLoan = list()
    
    MortgageLoan[["Principal"]] = Principal
      
    MortgageLoan[["AnnualHorizon"]] = AnnualHorizon
    
    MortgageLoan[["Schedule"]] = Schedule
    
    MortgageLoan[["LoanType"]] = LoanType
    
    MortgageLoan[["NominalInterest"]] = NominalInterest
    
    MortgageLoan[["RealInterest"]] = RealInterest
    
    MortgageLoan[["InterestVector"]] = InterestVector
    
    MortgageLoan[["IndexVector"]] = IndexVector
    
  
return(MortgageLoan)  
  
}

CalculateMortgageLoan = function(MortgageLoan){
  
  return(MortgageLoanTable(Principal = MortgageLoan[["Principal"]]
                           ,AnnualHorizon = MortgageLoan[["AnnualHorizon"]]
                           ,Schedule = MortgageLoan[["Schedule"]]
                           ,LoanType = MortgageLoan[["LoanType"]]
                           ,NominalInterest = MortgageLoan[["NominalInterest"]]
                           ,RealInterest = MortgageLoan[["RealInterest"]]
                           ,InterestVector = MortgageLoan[["InterestVector"]]
                           ,IndexVector = MortgageLoan[["IndexVector"]]))
  
}


CalculateMortgageMix = function(MortgageMix, IndexVector,PrimeVector){
  
  df = data.frame(Period = numeric(0),
                  StartBalance = numeric(0),
                  PrincipalPmt = numeric(0),
                  InterestPmt = numeric(0),
                  TotalPmt = numeric(0),
                  EndBalance = numeric(0))
  
  for (Loan in MortgageMix){
    
    if (Loan[["LoanType"]] == "Prime") {
      
        Loan[["InterestVector"]] = PrimeVector[1:length(Loan[["Period"]])]}
    
    if (Loan[["LoanType"]] == "RealFixedRate") {
      
        Loan[["IndexVector"]] = IndexVector[1:length(Loan[["Period"]])]}
    
    df = AddDF(df1 = df, df2 = CalculateMortgageLoan(MortgageLoan = Loan))
      
  }
  
  return(df)
  
}

CalculateRefinanceCashFlow = function(CurrentMortgageLoan,
                                      refinance_period, refinance_rate){
  
  current_mortgage_table = CalculateMortgageLoan(CurrentMortgageLoan)
  
  NewMortgageLoan = CurrentMortgageLoan
  
  NewMortgageLoan[["Principal"]] = 
  current_mortgage_table[refinance_period,"EndBalance"]
  
  NewMortgageLoan[["AnnualHorizon"]] = 
  NewMortgageLoan[["AnnualHorizon"]] - refinance_period
  
  NewMortgageLoan[["NominalInterest"]] = refinance_rate
  
  new_mortgage_table = CalculateMortgageLoan(NewMortgageLoan)
  
  row_ind = (refinance_period + 1) : nrow(current_mortgage_table)
  
  return(current_mortgage_table[row_ind,] - new_mortgage_table)
  
}

CalculateMortgageLoanRefinance = function(CurrentMortgageLoan, term_structure){
  
  refinance_cf = list()
  
  for (i in 1:nrow(term_structure)){
    
    period = CurrentMortgageLoan$AnnualHorizon - term_structure$Maturity[i]
    
    refinance_cf[[i]] = 
      CalculateRefinanceCashFlow(CurrentMortgageLoan,
                                 refinance_period = period, 
                                 refinance_rate = term_structure$Rate[i])
    
  }
  
  return(refinance_cf)
  
}

