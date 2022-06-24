#####################################################################
#                    Petrological Indices                           #
#                                                                   #
#####################################################################

Indices <- function(data, Calcite)
{
######################################################################
#                ##Environment for dataframes##                      #
######################################################################

Weight <- new.env()
data(OxiWeight, envir = Weight)

######################################################################
#                    Generate a dataframe                            #
######################################################################


#Generate a dataframe
mole.data <- data.frame(matrix(ncol = 8, nrow = length(data[,"Sample"]),
                      dimnames = list(NULL, c("SiO2", "Al2O3", "FeO", "Fe2O3","MgO", "Na2O", "P2O5", "CO2"))))
					  
mole.data$SiO2  = as.numeric(data$`SiO2 adj`)/Weight$OxiWeight["SiO2","Rounded"]
mole.data$Al2O3  = as.numeric(data$`Al2O3 adj`)/Weight$OxiWeight["Al2O3","Rounded"]
mole.data$FeO = as.numeric(data$`FeO adj`)/Weight$OxiWeight["FeO","Rounded"]
mole.data$Fe2O3 = as.numeric(data$`Fe2O3 adj`)/Weight$OxiWeight["Fe2O3","Rounded"]
mole.data$MgO = as.numeric(data$`MgO adj`)/Weight$OxiWeight["MgO","Rounded"]
mole.data$CaO = as.numeric(data$`CaO adj`)/Weight$OxiWeight["CaO","Rounded"]
mole.data$Na2O = as.numeric(data$`Na2O adj`)/Weight$OxiWeight["Na2O","Rounded"]
mole.data$K2O = as.numeric(data$`K2O adj`)/Weight$OxiWeight["K2O","Rounded"]
mole.data$P2O5 = as.numeric(data$`P2O5 adj`)/Weight$OxiWeight["P2O5","Rounded"]
mole.data$CO2 = as.numeric(data$`CO2 adj`)/Weight$OxiWeight["CO2","Rounded"]
          
#####################################################################
#                     Change values NA->0                           #
#####################################################################        
         
          mole.data[is.na(mole.data)] <- 0 ##Change values NA->0  ##Change values NA->0 
                    
####################################################################################################
#Generate a dataframe
pre_CIA <- data.frame(matrix(ncol = 4, nrow = length(data[,"Sample"]),
                      dimnames = list(NULL, c("CaO_alt", "Calcite_logical", "CO2_alt", "CaO" ))))
                      
                                       
pre_CIA$CaO_alt <- mole.data$CaO_alt <- mole.data$CaO - ((10/3) * mole.data$P2O5)
pre_CIA$Calcite_logical <- ifelse (Calcite == TRUE , TRUE, FALSE)
pre_CIA$CO2_alt <- ifelse (pre_CIA$Calcite_logical== TRUE, mole.data$CO2, 0)
pre_CIA$CaO <- pre_CIA$CaO_alt - pre_CIA$CO2_alt
####################################################################################################
##Define output##
#Generate a dataframe
outputI <- data.frame(matrix(ncol = 15, nrow = length(data[,"Sample"]),
                      dimnames = list(NULL, c("Sample", "S.I.", "A.R.", "CIA", "CIW", "Mg.number", "Fe_star", "MALI", "ACNK", "ANK", "AI", "tau", "sigma", "Temp_SiO2", "Temp_MgO"))))
outputI$Sample <- data$Sample
outputI$S.I. <- (100*(as.numeric(data$`MgO adj`)))/((as.numeric(data$`MgO adj`)) + (as.numeric(data$`Fe2O3 adj`)) + (as.numeric(data$`FeO adj`)) + (as.numeric(data$`Na2O adj`)) + (as.numeric(data$`K2O adj`)))                    
outputI$CIA <- round ((mole.data$Al2O3/(mole.data$Al2O3 + pre_CIA$CaO  + mole.data$Na2O + mole.data$K2O)) * 100, 2)
outputI$CIW <- round ((mole.data$Al2O3/(mole.data$Al2O3 + pre_CIA$CaO  + mole.data$Na2O)) * 100, 2)
outputI$Mg.number <- round (100 * (mole.data$MgO/(mole.data$MgO + mole.data$FeO)), 2)# (Mg#=molar MgO/[molar MgO+molar FeOT
outputI$Fe_star <- round ((as.numeric(data$`FeO adj`) + (0.9*as.numeric(data$`Fe2O3 adj`))) / (as.numeric(data$`FeO adj`) + (0.9*as.numeric(data$`Fe2O3 adj`)) + as.numeric(data$`MgO adj`)), 2)
outputI$MALI <-  round (as.numeric(data$`Na2O adj`) + as.numeric(data$`K2O adj`) - as.numeric(data$`CaO adj`),  2)      
outputI$ACNK <- round(((mole.data$Al2O3) / (mole.data$CaO -(1.67*mole.data$P2O5) + mole.data$Na2O + mole.data$K2O)), 2)
outputI$ANK <- round(((mole.data$Al2O3) / (mole.data$Na2O + mole.data$K2O)), 2)
outputI$AI <- round((mole.data$Al2O3) - (mole.data$K2O + mole.data$Na2O), 2)
outputI$tau <- round(log((as.numeric(data$`Al2O3 adj`)-as.numeric(data$`Na2O adj`))/as.numeric(data$`TiO2 adj`)), 2) 
outputI$sigma <- round(log(((as.numeric(data$`K2O adj`) + as.numeric(data$`Na2O adj`))**2)/(as.numeric(data$`SiO2 adj`)-43)), 2)#(Gottini, 1968);(Rittmann, 1957)
outputI$A.R. <- ifelse ((as.numeric(data$`SiO2 adj`)) > 0 & (as.numeric(data$`K2O adj`))/(as.numeric(data$`Na2O adj`)) > 1.0 & (as.numeric(data$`K2O adj`))/(as.numeric(data$`Na2O adj`)) < 2.5, ((as.numeric(data$`Al2O3 adj`)) + (as.numeric(data$`CaO adj`)) + (2*(as.numeric(data$`Na2O adj`))))/((as.numeric(data$`Al2O3 adj`)) + (as.numeric(data$`CaO adj`)) - (2*(as.numeric(data$`Na2O adj`)))), ((as.numeric(data$`Al2O3 adj`)) + (as.numeric(data$`CaO adj`)) + (as.numeric(data$`Na2O adj`)) + (as.numeric(data$`K2O adj`)))/((as.numeric(data$`Al203 adj`)) + (as.numeric(data$`CaO adj`)) - (as.numeric(data$`Na2O adj`)) - (as.numeric(data$`K2O adj`))))
outputI$Temp_SiO2 <- round(((-14.16*as.numeric(data$`SiO2 adj`)) + 1723), 2)
outputI$Temp_MgO <- round((887.6*as.numeric(data$`MgO adj`)^(0.0989)), 2)

col.outputI<-c("Sample", "S.I.", "A.R.", "CIA", "CIW", "Mg#", "Fe*", "MALI", "ACNK", "ANK", "AI", "tau", "sigma", "Temp_SiO2", "Temp_MgO")
colnames(outputI) <- col.outputI
return(outputI)

######################################################################
#                         Clean Environment                          #
######################################################################
rm(Weight)
}
## End(Not run)
