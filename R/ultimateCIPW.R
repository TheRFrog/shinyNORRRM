#####################################################################
#                           CIPW norm                               #
#                  (Based on Verma, 2002, 2003)                     #
#####################################################################

ultimateCIPW<-
function (data, Type, Fe.adjustment, Cancrinite, Calcite){
#####################################################################
#                                                                   #
#       Calculate major elements data on an anhydrous basis         #
#####################################################################
#                         Create Matrix                             #
#####################################################################

major.adj <- adjRock (data, Type, Fe.adjustment, Cancrinite, Calcite)

##Changes to numeric inputs

######################################################################
#                ##Environment for dataframes##                      #
######################################################################

Weight <- new.env()
data(AtomWeight, envir = Weight)
data(MinWeight, envir = Weight)
data(OxiWeight, envir = Weight)
##

input <- data.frame(#Sample = as.character(data$Sample),
                SiO2.major = as.numeric(major.adj$`SiO2 adj`),
                TiO2.major = as.numeric(major.adj$`TiO2 adj`),
           	    Al2O3.major = as.numeric(major.adj$`Al2O3 adj`),
          	    Fe2O3.major = as.numeric(major.adj$`Fe2O3 adj`),
           	    FeO.major = as.numeric(major.adj$`FeO adj`),
           	    MnO.major = as.numeric(major.adj$`MnO adj`),
           	    MgO.major = as.numeric(major.adj$`MgO adj`),
           	    CaO.major = as.numeric(major.adj$`CaO adj`),
          	    Na2O.major = as.numeric(major.adj$`Na2O adj`),
          	    K2O.major = as.numeric(major.adj$`K2O adj`),
          	    P2O5.major = as.numeric(major.adj$`P2O5 adj`),
           	    CO2.major = as.numeric(major.adj$`CO2 adj`),
           	    SO3.min = as.numeric(major.adj$`SO3 adj`),
           	    S = as.numeric(major.adj$`S adj`),
          	    F.trace = as.numeric(major.adj$`F adj`),
           	    Cl = as.numeric(major.adj$`Cl adj`),
           	    BaO.min = as.numeric(data$Ba*((Weight$OxiWeight ['BaO','OWeight'])/(Weight$AtomWeight ['Ba','AWeight']))*0.0001),
           	    CoO.min = as.numeric(data$Co*((Weight$OxiWeight ['CoO','OWeight'])/(Weight$AtomWeight ['Co','AWeight']))*0.0001),
           	    Cr2O3.min = as.numeric(data$Cr*((Weight$OxiWeight ['Cr2O3','OWeight'])/(Weight$AtomWeight ['Cr','AWeight']*2))*0.0001),
            	Cs2O.min = as.numeric(data$Cs*((Weight$OxiWeight ['Cs2O','OWeight'])/(Weight$AtomWeight ['Cs','AWeight']*2))*0.0001),
            	Li2O.min = as.numeric(data$Li*((Weight$OxiWeight ['Li2O','OWeight'])/(Weight$AtomWeight ['Li','AWeight']*2))*0.0001),
            	NiO.min = as.numeric(data$Ni*((Weight$OxiWeight ['NiO','OWeight'])/(Weight$AtomWeight ['Ni','AWeight']))*0.0001),
           	    Rb2O.min = as.numeric(data$Rb*((Weight$OxiWeight ['Rb2O','OWeight'])/(Weight$AtomWeight ['Rb','AWeight']*2))*0.0001),
           	    SrO.min = as.numeric(data$Sr*((Weight$OxiWeight ['SrO','OWeight'])/(Weight$AtomWeight ['Sr','AWeight']))*0.0001),
           	    V2O3.min = as.numeric(data$V*((Weight$OxiWeight ['V2O3','OWeight'])/(Weight$AtomWeight ['V','AWeight']*2))*0.0001),
           	    ZrO2.min = as.numeric(data$Zr*((Weight$OxiWeight ['ZrO2','OWeight'])/(Weight$AtomWeight ['Zr','AWeight']))*0.0001))

#####################################################################
#                     Change values NA->0                           #
#####################################################################

    input[is.na(input)] <- 0 ##Change values NA->0

######################################################################
#                         Adjust to 100%                             #
######################################################################

    Sums_input <- rowSums(input)

    adjustCIPW <- function(m)
    {##function to an anhydrous adjust
    adjust <- (100*m)/(Sums_input)
    }

    adj_input <- adjustCIPW (input)#test out <- rowSums(adj)== 100
    

######################################################################
#                     ##Mole computation##                           #
######################################################################

mole.data <- list(SiO2 = adj_input$SiO2.major/Weight$OxiWeight["SiO2","OWeight"],
                  TiO2 = adj_input$TiO2.major/Weight$OxiWeight["TiO2","OWeight"],
                  Al2O3  = adj_input$Al2O3.major/Weight$OxiWeight["Al2O3","OWeight"],
                  FeO = adj_input$FeO.major/Weight$OxiWeight["FeO","OWeight"],
                  Fe2O3 = adj_input$Fe2O3.major/Weight$OxiWeight["Fe2O3","OWeight"],
                  MnO = adj_input$MnO.major/Weight$OxiWeight["MnO","OWeight"],
                  MgO = adj_input$MgO.major/Weight$OxiWeight["MgO","OWeight"],
                  CaO = adj_input$CaO.major/Weight$OxiWeight["CaO","OWeight"],
                  Na2O = adj_input$Na2O.major/Weight$OxiWeight["Na2O","OWeight"],
                  K2O = adj_input$K2O.major/Weight$OxiWeight["K2O","OWeight"],
                  P2O5 = adj_input$P2O5.major/Weight$OxiWeight["P2O5","OWeight"],
                  CO2 = adj_input$CO2.major/Weight$OxiWeight["CO2","OWeight"],
                  BaO = adj_input$BaO.min/Weight$OxiWeight["BaO","OWeight"],
                  CoO = adj_input$CoO.min/Weight$OxiWeight["CoO","OWeight"],
                  Cr2O3 = adj_input$Cr2O3.min/Weight$OxiWeight["Cr2O3","OWeight"],
                  Cs2O = adj_input$Cs2O.min/Weight$OxiWeight["Cs2O","OWeight"],
                  Li2O = adj_input$Li2O.min/Weight$OxiWeight["Li2O","OWeight"],
                  NiO = adj_input$NiO.min/Weight$OxiWeight["NiO","OWeight"],
                  Rb2O = adj_input$Rb2O.min/Weight$OxiWeight["Rb2O","OWeight"],
                  SO3 = adj_input$SO3.min/Weight$OxiWeight["SO3","OWeight"],
                  SrO = adj_input$SrO.min/Weight$OxiWeight["SrO","OWeight"],
                  V2O3 = adj_input$V2O3.min/Weight$OxiWeight["V2O3","OWeight"],
                  ZrO2 = adj_input$ZrO2.min/Weight$OxiWeight["ZrO2","OWeight"],
                  S = adj_input$S/Weight$AtomWeight["S","AWeight"],
                  F.trace = adj_input$F.trace/Weight$AtomWeight["F","AWeight"],
                  Cl = adj_input$Cl/Weight$AtomWeight["Cl","AWeight"])

######################################################################
#              ##Corrected molecular weight FeO##                    #
######################################################################
#Generate a dataframe
ratio.Fe <- data.frame(matrix(ncol = 10, nrow = length(data[,"Sample"]),
                      dimnames = list(NULL, c("xfeo", "xmno", "xnio", "xcoo", "femg", "xfer", "xmgr", "FeO.corr", "Fe.corr", "diff"))))

mole.data$FeO <- mole.data$FeO + mole.data$MnO + mole.data$NiO + mole.data$CoO #test out (ratio.Fe$xmno + ratio.Fe$xfeo + ratio.Fe$xnio + ratio.Fe$xcoo)== 1
ratio.Fe$xfeo <- ((adj_input$FeO.major/Weight$OxiWeight["FeO","OWeight"])/mole.data$FeO)
ratio.Fe$xmno <- (mole.data$MnO/mole.data$FeO)
ratio.Fe$xnio <- (mole.data$NiO/mole.data$FeO)
ratio.Fe$xcoo <- (mole.data$CoO/mole.data$FeO)
ratio.Fe$FeO.corr <- ((Weight$OxiWeight["FeO","OWeight"])*ratio.Fe$xfeo) + ((Weight$OxiWeight["MnO","OWeight"])*ratio.Fe$xmno)+ ((Weight$OxiWeight["NiO","OWeight"])*ratio.Fe$xnio)+ ((Weight$OxiWeight["CoO","OWeight"])*ratio.Fe$xcoo)
ratio.Fe$Fe.corr <- ratio.Fe$FeO.corr  - (mole.data$FeO/Weight$AtomWeight["O","AWeight"])
ratio.Fe[is.na(ratio.Fe)] <- 0 ##Change values NA->0
ratio.Fe$diff <- ratio.Fe$FeO.corr - ratio.Fe$Fe.corr

######################################################################
#              ##Corrected molecular weight CaO##                    #
######################################################################
#Generate a dataframe
ratio.Ca <- data.frame(matrix(ncol = 6, nrow = length(data[,"Sample"]),
                      dimnames = list(NULL, c("xcao", "xbao", "xsro", "CaO.corr", "Ca.corr", "diff"))))

mole.data$CaO <- mole.data$CaO + mole.data$BaO + mole.data$SrO #test out (ratio.Ca$xcao + ratio.Ca$xbao + ratio.Ca$xsro)== 1
ratio.Ca$xcao <- ((adj_input$CaO.major/Weight$OxiWeight["CaO","OWeight"])/mole.data$CaO)
ratio.Ca$xbao <- (mole.data$BaO/mole.data$CaO)
ratio.Ca$xsro <- (mole.data$SrO/mole.data$CaO)
ratio.Ca$CaO.corr <- ((Weight$OxiWeight["CaO","OWeight"])*ratio.Ca$xcao) + ((Weight$OxiWeight["BaO","OWeight"])*ratio.Ca$xbao) + ((Weight$OxiWeight["SrO","OWeight"])*ratio.Ca$xsro)
ratio.Ca$Ca.corr <- ratio.Ca$CaO.corr - (mole.data$CaO/Weight$AtomWeight["O","AWeight"])
ratio.Ca[is.na(ratio.Ca)] <- 0 ##Change values NA->0
ratio.Ca$diff <- ratio.Ca$CaO.corr - ratio.Ca$Ca.corr

######################################################################
#              ##Corrected molecular weight K2O##                    #
######################################################################
#Generate a dataframe
ratio.K <- data.frame(matrix(ncol = 5, nrow = length(data[,"Sample"]),
                      dimnames = list(NULL, c("xk2o", "xrb2o", "xcs2o", "K2O.corr", "K.corr"))))

mole.data$K2O <- mole.data$K2O + mole.data$Rb2O + mole.data$Cs2O #test out (ratio.K$xk2o + ratio.K$xrb2o + ratio.K$xcs2o)== 1
ratio.K$xk2o <- ((adj_input$K2O.major/Weight$OxiWeight["K2O","OWeight"])/mole.data$K2O)
ratio.K$xrb2o <- (mole.data$Rb2O/mole.data$K2O)
ratio.K$xcs2o <- (mole.data$Cs2O/mole.data$K2O)
ratio.K$K2O.corr <- ((Weight$OxiWeight["K2O","OWeight"])*ratio.K$xk2o) + ((Weight$OxiWeight["Rb2O","OWeight"])*ratio.K$xrb2o) + ((Weight$OxiWeight["Cs2O","OWeight"])*ratio.K$xcs2o)
ratio.K$K.corr <- ratio.K$K2O.corr - (mole.data$K2O/(Weight$AtomWeight["O","AWeight"]/2))
ratio.K[is.na(ratio.K)] <- 0 ##Change values NA->0

######################################################################
#              ##Corrected molecular weight Na2O##                   #
######################################################################
#Generate a dataframe
ratio.Na <- data.frame(matrix(ncol = 5, nrow = length(data[,"Sample"]),
                      dimnames = list(NULL, c("xna2o", "xli2o", "Na2O.corr", "Na.corr", "diff"))))

mole.data$Na2O <- mole.data$Na2O + mole.data$Li2O #test out (ratio.Na$xna2o + ratio.Na$xli2o)== 1
ratio.Na$xna2o <- ((adj_input$Na2O.major/Weight$OxiWeight["Na2O","OWeight"])/mole.data$Na2O)
ratio.Na$xli2o <- (mole.data$Li2O/mole.data$Na2O)
ratio.Na$Na2O.corr <- ((Weight$OxiWeight["Na2O","OWeight"])*ratio.Na$xna2o) + ((Weight$OxiWeight["Li2O","OWeight"])*ratio.Na$xli2o)
ratio.Na$Na.corr <- ratio.Na$Na2O.corr - (mole.data$Na2O/(Weight$AtomWeight["O","AWeight"]/2))
ratio.Na[is.na(ratio.Na)] <- 0 ##Change values NA->0
ratio.Na$diff <- ratio.Na$Na2O.corr - ratio.Na$Na.corr

######################################################################
#             ##Corrected molecular weight Cr2O3##                   #
######################################################################
#Generate a dataframe
ratio.Cr <- data.frame(matrix(ncol = 4, nrow = length(data[,"Sample"]),
                      dimnames = list(NULL, c("xcr2o3", "xv2o3", "Cr2O3.corr", "Cr.corr"))))

mole.data$Cr2O3 <- mole.data$Cr2O3 + mole.data$V2O3 #test out (ratio.Cr$xcr2o3 + ratio.Cr$xv2o3)== 1
ratio.Cr$xcr2o3 <- ((adj_input$Cr2O3.min/Weight$OxiWeight["Cr2O3","OWeight"])/mole.data$Cr2O3)
ratio.Cr$xv2o3 <- (mole.data$V2O3/mole.data$Cr2O3)
ratio.Cr$Cr2O3.corr <- ((Weight$OxiWeight["Cr2O3","Rounded"])*ratio.Cr$xcr2o3) + ((Weight$OxiWeight["V2O3","OWeight"])*ratio.Cr$xv2o3)
ratio.Cr$Cr.corr <- ratio.Cr$Cr2O3.corr - (mole.data$Cr2O3/(Weight$AtomWeight["O","AWeight"]/(2/3)))
ratio.Cr[is.na(ratio.Cr)] <- 0 ##Change values NA->0

######################################################################
#                         Main functions                             #
######################################################################
NORRRM.minerals.mole<-c("Q","C","Or","Ab","An","Ne","Lc","Kp","Nc","FREE_Cl","FREE_SO3","FREE_Cr2O3","F.trace.free", "FREE_CO2","Ac","Z","Ns","Ks","Di","Wo","Hy","Ol","Ds","Th","Cm","Cc","Mt","Il","Hm","Tn","Pf","Ru","Y","Ap","DEFSIO2","FREE_P2O5","Fr","Pr","FREEO_12b","FREEO_12c","FREEO_13","FREEO_14","FREEO_16","Orp","Abp","Lcp","Dip","Olp","Wop","D","D1","D2","D3","D4","D5","D6","Tnp","Hl","ApCaO","FREE_F","ApCaF")
m<-rep(0,length(NORRRM.minerals.mole))
names(m)<-NORRRM.minerals.mole
m<-as.list(m)

###############################################################
#                          Zircon                             #
###############################################################
m$Z <- mole.data$ZrO2
m$Y<- m$Z

###############################################################
#                    Apatite/FREE_P2O5                        #
###############################################################
m$Ap <- ifelse (mole.data$CaO >= (10 / 3) * mole.data$P2O5, mole.data$P2O5, mole.data$CaO/(3+(1/3)))
m$FREE_P2O5 <- ifelse (mole.data$CaO >= (10 / 3) * mole.data$P2O5, 0, mole.data$P2O5-m$Ap)
m$FREE_P2O5 <- ifelse (mole.data$P2O5 > 0, m$FREE_P2O5, 0)
mole.data$CaO <- ifelse (mole.data$CaO >= ((10 / 3)*mole.data$P2O5), (mole.data$CaO)-(m$Ap * 10 / 3), 0)
m$F.trace.free <- ifelse (mole.data$F.trace >= ((2 / 3)*m$Ap), mole.data$F.trace-((2 / 3)*m$Ap), 0)
m$ApCaF <- ifelse (mole.data$F.trace >= ((2 / 3)*m$Ap), (2 / 3)*m$Ap, mole.data$F.trace)
m$ApCaO <- m$Ap-m$ApCaF#test out (m$ApCaF + m$ApCaO)== m$Ap

##############################################################
m$FREEO_12b <- ifelse (mole.data$F.trace >= (2 / 3)*m$Ap, (m$Ap * 1/3),0)
m$FREEO_12c <- ifelse (mole.data$F.trace < (2 / 3)*m$Ap, mole.data$F.trace*0.5, 0)

###############################################################
#                        Fluorite                             #
###############################################################
m$Fr <- ifelse (mole.data$CaO >= 0.5*m$F.trace.free, 0.5*m$F.trace.free, mole.data$CaO)
m$Fr <- ifelse (m$F.trace.free > 0, m$Fr, 0)
mole.data$CaO <- ifelse (mole.data$CaO >= 0.5*m$F.trace.free, mole.data$CaO - (0.5*m$F.trace.free), 0)
m$FREE_F <- ifelse (mole.data$CaO >= 0.5*m$F.trace.free, 0,m$F.trace.free - (m$Fr*2))
m$FREEO_13 <- m$Fr

###############################################################
#                         Halite                              #
###############################################################
m$Hl <- ifelse (mole.data$Na2O >= mole.data$Cl, mole.data$Cl, mole.data$Na2O)
m$Hl <- ifelse (mole.data$Cl > 0, m$Hl, 0)
m$FREEO_14 <- (m$Hl*0.5)
m$FREE_Cl <- ifelse (mole.data$Na2O >= mole.data$Cl, 0, mole.data$Cl-(m$Hl))
m$FREE_Cl <- ifelse (mole.data$Cl>0, m$FREE_Cl,0)
mole.data$Na2O <- mole.data$Na2O-(m$Hl)

###############################################################
#                       Thenardite                            #
###############################################################
m$Th <- ifelse (mole.data$Na2O >= mole.data$SO3, mole.data$SO3, mole.data$Na2O)
m$Th <- ifelse (mole.data$SO3 > 0, m$Th, 0)
mole.data$Na2O <- ifelse (mole.data$Na2O >= mole.data$SO3, mole.data$Na2O - mole.data$SO3, mole.data$Na2O-m$Th)
m$FREE_SO3 <- ifelse (mole.data$Na2O >= mole.data$SO3, 0, mole.data$SO3-m$Th)

###############################################################
#                         Pyrite                              #
###############################################################
m$Pr <- ifelse (mole.data$FeO >= mole.data$S*0.5, mole.data$S*0.5, mole.data$FeO)
m$Pr <- ifelse (m$Pr > 0, m$Pr, 0)
m$FREE_S <- ifelse (mole.data$FeO >= mole.data$S*0.5, 0, mole.data$S - m$Pr*2)
m$FREE_S <- ifelse (m$FREE_S > 0, m$FREE_S, 0)
mole.data$FeO <- ifelse (mole.data$FeO >= mole.data$S*0.5, mole.data$FeO-(mole.data$S*0.5), 0)
m$FREEO_16 <- m$Pr


###############################################################
#               Sodium carbonate/calcite                      #
###############################################################
#Generate a dataframe
CO2_user <- data.frame(matrix(ncol = 2, nrow = length(data[,"Sample"]),
                      dimnames = list(NULL, c("Cancrinite_logical", "Calcite_logical"))))

CO2_user$Cancrinite_logical <- ifelse (Cancrinite == TRUE , TRUE, FALSE)
m$Nc <- ifelse (mole.data$Na2O >= mole.data$CO2, mole.data$CO2, mole.data$Na2O)
m$Nc <- ifelse (CO2_user$Cancrinite_logical == TRUE, m$Nc, 0)
mole.data$Na2O <- ifelse (m$Nc > 0, mole.data$Na2O-m$Nc, mole.data$Na2O)
m$FREE_CO2 <- ifelse (m$Nc > 0, mole.data$CO2-m$Nc, mole.data$CO2)
m$FREE_CO2 <- ifelse (m$FREE_CO2 < 0, 0, m$FREE_CO2)

###############################################################
CO2_user$Calcite_logical <- ifelse (Calcite == TRUE , TRUE, FALSE)

mole.data$CO2 <- ifelse (CO2_user$Cancrinite_logical == TRUE, m$FREE_CO2, mole.data$CO2)
m$Cc <- ifelse (mole.data$CaO >= mole.data$CO2, mole.data$CO2, mole.data$CaO)
m$Cc <- ifelse (CO2_user$Calcite_logical == TRUE, m$Cc, 0)
mole.data$CaO <- ifelse (m$Cc > 0, mole.data$CaO-m$Cc, mole.data$CaO)
m$FREE_CO2 <- ifelse (m$Cc > 0, mole.data$CO2-m$Cc, mole.data$CO2)


###############################################################
#                        Chromite                             #
###############################################################
m$Cm <- ifelse (mole.data$FeO >= mole.data$Cr2O3, mole.data$Cr2O3, mole.data$FeO)
m$Cm <- ifelse (m$Cm > 0, m$Cm, 0)
m$FREE_Cr2O3 <- ifelse (mole.data$FeO >= mole.data$Cr2O3, 0, mole.data$Cr2O3 - m$Cm)
m$FREE_Cr2O3 <- ifelse (m$FREE_Cr2O3 > 0, m$FREE_Cr2O3, 0)
mole.data$FeO <- ifelse (mole.data$FeO >= mole.data$Cr2O3, mole.data$FeO-mole.data$Cr2O3, 0)


###############################################################
#                        Ilmenite                             #
###############################################################
m$Il <- ifelse (mole.data$FeO >= mole.data$TiO2, mole.data$TiO2, mole.data$FeO)
mole.data$FeO <- ifelse (mole.data$FeO >= mole.data$TiO2, mole.data$FeO-m$Il, 0)
mole.data$TiO2 <- ifelse (m$Il > 0, mole.data$TiO2-m$Il, 0)
mole.data$TiO2 <- ifelse (mole.data$TiO2 < 0, 0, mole.data$TiO2)

###############################################################
#             Orthoclase/Potassium metasilicate               #
###############################################################
m$Orp <- ifelse (mole.data$Al2O3 >= mole.data$K2O, mole.data$K2O, mole.data$Al2O3)
mole.data$Al2O3 <- ifelse (mole.data$Al2O3 >= mole.data$K2O, mole.data$Al2O3-m$Orp, 0)
mole.data$K2O <- ifelse (mole.data$Al2O3 >= mole.data$K2O, 0, mole.data$K2O-m$Orp)
m$Ks <- ifelse (mole.data$K2O > 0, mole.data$K2O, 0)
m$Y <- ifelse (m$Ks > 0, m$Y+(6*m$Orp)+m$Ks, m$Y+(6*m$Orp))

###############################################################
#                       Albite                                #
###############################################################
m$Abp <- ifelse (mole.data$Al2O3 >= mole.data$Na2O, mole.data$Na2O, mole.data$Al2O3)
mole.data$Na2O <- ifelse (mole.data$Al2O3 >= mole.data$Na2O, 0, mole.data$Na2O-m$Abp)
mole.data$Al2O3 <- ifelse (mole.data$Al2O3 > m$Abp, mole.data$Al2O3-m$Abp, 0)

m$Y<- m$Y+(6*m$Abp)

###############################################################
#                Acmite/Sodium metasilicate                   #
###############################################################
m$Ac <- ifelse (mole.data$Na2O >= mole.data$Fe2O3, mole.data$Fe2O3, mole.data$Na2O)
mole.data$Na2O <- ifelse (mole.data$Na2O >= mole.data$Fe2O3, mole.data$Na2O-m$Ac, mole.data$Na2O*0)
mole.data$Fe2O3 <- mole.data$Fe2O3-m$Ac
mole.data$Fe2O3 <- ifelse (mole.data$Fe2O3 >0, mole.data$Fe2O3, 0)
m$Ns <- ifelse (mole.data$Na2O > 0, mole.data$Na2O, 0)
m$Y <- ifelse (m$Ns > 0, (m$Ac*4)+m$Ns+m$Y, (m$Ac*4)+m$Y)

###############################################################
#                    Anorthite/Corundum                       #
###############################################################
m$An <- ifelse (mole.data$Al2O3 >= mole.data$CaO, mole.data$CaO, mole.data$Al2O3)
mole.data$Al2O3 <- ifelse (mole.data$Al2O3 >= mole.data$CaO, mole.data$Al2O3-m$An, 0)
mole.data$CaO <- ifelse (mole.data$Al2O3 == 0, mole.data$CaO-m$An, 0)
m$C <- ifelse (mole.data$Al2O3 > 0, mole.data$Al2O3, 0)
m$Y<- (m$An*2)+m$Y

###############################################################
#                      Sphene/Rutile                          #
###############################################################
m$Tnp <- ifelse(mole.data$CaO >= mole.data$TiO2, mole.data$TiO2, mole.data$CaO)
mole.data$CaO <- ifelse(mole.data$CaO >= mole.data$TiO2, mole.data$CaO-m$Tnp, 0)
mole.data$TiO2 <- ifelse(mole.data$CaO == 0, mole.data$TiO2-m$Tnp, 0)
m$Ru <- ifelse(mole.data$TiO2 > 0, mole.data$TiO2, 0)
m$Y<- (m$Tnp)+m$Y

###############################################################
#                  Magnetite/Hematite                         #
###############################################################
m$Mt <- ifelse(mole.data$Fe2O3 >= mole.data$FeO, mole.data$FeO, mole.data$Fe2O3)
mole.data$FeO <- ifelse(mole.data$Fe2O3 >= mole.data$FeO, 0, mole.data$FeO-m$Mt)
mole.data$Fe2O3 <- ifelse(mole.data$FeO == 0, mole.data$Fe2O3-m$Mt, 0)
m$Hm <- ifelse(mole.data$Fe2O3 > 0, mole.data$Fe2O3, 0)

######################################################################
#       Subdivision of some normative minerals (end-members)         #
######################################################################
ratio.Fe$femg <- mole.data$FeO + mole.data$MgO
ratio.Fe$xfer <- mole.data$FeO / (mole.data$FeO + mole.data$MgO)
ratio.Fe$xmgr <- mole.data$MgO / (mole.data$MgO + mole.data$FeO)

###############################################################
#             Diopside/Wollastonite/Hypersthene               #
###############################################################
m$Dip <- ifelse(mole.data$CaO >= ratio.Fe$femg, ratio.Fe$femg, mole.data$CaO)
mole.data$CaO <- ifelse(mole.data$CaO >= ratio.Fe$femg, mole.data$CaO-m$Dip, 0)
ratio.Fe$femg <- ifelse(mole.data$CaO == 0, ratio.Fe$femg-m$Dip, 0)
m$Wop <- ifelse(mole.data$CaO > 0, mole.data$CaO, 0)
m$Hyp <- ifelse(ratio.Fe$femg > 0, ratio.Fe$femg, 0)
m$Y <- ifelse(m$Wop > 0 ,(m$Dip*2)+m$Wop+m$Y, (m$Dip*2)+m$Hyp+m$Y)

###############################################################
#                Quartz/Undersatured Quartz                   #
###############################################################
m$Q <- ifelse(mole.data$SiO2 >= m$Y, mole.data$SiO2-m$Y, 0)
m$D <- ifelse(mole.data$SiO2 < m$Y, m$Y-mole.data$SiO2, 0)
saturated <- ifelse(mole.data$SiO2 >= m$Y,TRUE,FALSE)#
unsaturated <- ifelse(mole.data$SiO2 < m$Y,TRUE,FALSE)

###############################################################
#                   Olivine/Hypersthene                       #
###############################################################
m$Olp <- ifelse(m$D < (m$Hyp/2), m$D, m$Hyp/2)
m$Olp <- ifelse(unsaturated, m$Olp, 0)
m$Hy <- ifelse(m$D < (m$Hyp/2), m$Hyp-(2*m$D), m$Hy<-0)
m$Hy <- ifelse(unsaturated, m$Hy, m$Hyp)
m$D1 <- ifelse(m$D >= (m$Hyp/2), m$D-(m$Hyp/2), 0)
m$D1 <- ifelse(unsaturated, m$D1, 0)
unsaturated <- ifelse(m$D1 > 0,TRUE,FALSE)#

###############################################################
#                    Sphene/Perovskite                        #
###############################################################
m$Pf <- ifelse(m$D1 < m$Tnp, m$D1, m$Tnp)
m$Pf <- ifelse(unsaturated, m$Pf, 0)
m$Tn <- ifelse(m$D1 < m$Tnp, m$Tnp-m$D1, 0)
m$D2 <- ifelse(m$D1 >= m$Tnp, m$D1-m$Tnp, 0)
unsaturated <- ifelse(m$D2 > 0,TRUE,FALSE)#

###############################################################
#                    Nepheline/Albite                         #
###############################################################
m$Ne <- ifelse(m$D2 < m$Abp*4, m$D2/4, m$Abp)
m$Ne <- ifelse(unsaturated, m$Ne, 0)
m$Abpp <- ifelse(m$D2 < m$Abp*4, m$Abp-(m$D2/4), 0)
m$Ab <- ifelse(unsaturated, m$Abpp, m$Abp)
m$D3 <- ifelse(m$D2 >= m$Abp*4, m$D2-(m$Abp*4), 0)
unsaturated <- ifelse(m$D3 > 0,TRUE,FALSE)#

###############################################################
#                    Leucite/Orthoclase                       #
###############################################################
m$Lcp <- ifelse(m$D3 < m$Orp*2, m$D3/2, m$Orp)
m$Lcp <- ifelse(unsaturated, m$Lcp, 0)
m$Orpp <- ifelse(m$D3 < m$Orp*2, m$Orp-(m$D3/2), 0)
m$Or <- ifelse(unsaturated, m$Orpp, m$Orp)
m$D4 <- ifelse(m$D3 >= m$Orp*2, m$D3-(m$Orp*2), 0)
unsaturated <- ifelse(m$D4 > 0,TRUE,FALSE)#

###############################################################
#            Dicalcium silicate/Wollastonite                  #
###############################################################
m$Ds <- ifelse(m$D4 < m$Wop/2, m$D4, m$Wop/2)
m$Ds <- ifelse(unsaturated, m$Ds, 0)
m$Wopp <- ifelse(m$D4 < m$Wop/2, m$Wop-(m$D4*2), 0)
m$Wo <- ifelse(unsaturated, m$Wopp, m$Wop)
m$D5 <- ifelse(m$D4 >= m$Wop/2, m$D4-(m$Wop/2), 0)
unsaturated <- ifelse(m$D5 > 0,TRUE,FALSE)#

###############################################################
#         Adjust Diopside/Dicalcium silicate/Olivine          #
###############################################################
m$Ds <- ifelse(m$D5 < m$Dip, m$Ds+(m$D5/2), m$Ds+m$Dip/2)
m$Olpp <- ifelse(m$D5 < m$Dip, m$Olp+(m$D5/2), m$Olp+m$Dip/2)
m$Ol <- ifelse(unsaturated, m$Olpp, m$Olp)
m$Dipp <- ifelse(m$D5 < m$Dip, m$Dip-(m$D5), 0)
m$Di <- ifelse(unsaturated, m$Dipp, m$Dip)
m$D6 <- ifelse(m$D5 >= m$Dip, m$D5-(m$Dip), 0)
unsaturated <- ifelse(m$D6 > 0,TRUE,FALSE)#

###############################################################
#                Adjust Kaliophilite/Leucite                  #
###############################################################
m$Kp <- ifelse(m$Lcp >= m$D6/2, m$D6/2, m$Lcp)
m$Lcpp <- ifelse(m$Lcp >= m$D6/2, m$Lcp-(m$D6/2), 0)
m$Lc <- ifelse(unsaturated, m$Lcpp, m$Lcp)
m$DEFSIO2  <- m$D6-(2*m$Kp)

######################################################################
#                             Free O                                 #
######################################################################
#Generate a dataframe
NORRRM.Free <- data.frame(matrix(ncol = 6, nrow = length(data[,"Sample"]),
                      dimnames = list(NULL, c("MWApCF2","FreeO_a","FreeO_b","FreeO_c","FreeO_d","FreeO_e"))))
#NORRRM.Free$MWApCF2<-round(((3*ratio.Ca$CaO.corr) + ((1/3)*(ratio.Ca$Ca.corr)) + Weight$MinWeight["Apatite-F",4]), digits = 3)
NORRRM.Free$FreeO_a <- (round((Weight$AtomWeight["O","AWeight"]) * m$FREEO_12b, digits = 3))*(Weight$OxiWeight["CaF2","OWeight"]/100)
NORRRM.Free$FreeO_a[is.na(NORRRM.Free$FreeO_a)] <- 0 ##Change values NA->0
NORRRM.Free$FreeO_b <- (round((Weight$AtomWeight["O","AWeight"]) *  m$FREEO_12c, digits = 3))
NORRRM.Free$FreeO_b[is.na(NORRRM.Free$FreeO_b)] <- 0 ##Change values NA->0
NORRRM.Free$FreeO_c <- round((ratio.Ca$diff + Weight$AtomWeight["O","AWeight"]) *  m$FREEO_13, digits = 3)
NORRRM.Free$FreeO_d <- round((ratio.Na$diff + Weight$AtomWeight["O","AWeight"]) *  m$FREEO_14, digits = 3)
NORRRM.Free$FreeO_e <- round((ratio.Fe$diff + Weight$AtomWeight["O","AWeight"]) *  m$FREEO_16, digits = 3)

######################################################################
#                         Print Minerals                             #
######################################################################
#Generate a dataframe
Normative.minerals.t <- data.frame(matrix(ncol = 42, nrow = length(data[,"Sample"]),
                      dimnames = list(NULL, c("Q","C","Or","Ab","An","Ap","Ne","Lc","Kp","Zrn","Ac","Ns","Ks","Di","Wo","En","Fs","Fo","Fa","Dcs","Mt","Il","Hm","Tn","Pf","Ru","Fr","NaCl","Th","Pr","Nc","Cc","Cm","DEFSiO2","Free_P2O5","Free_F","Free_Cl","Free_SO3","Free_S","Free_CO2","Free_Cr2O3","Free_O"))))

Normative.minerals.t[,"Zrn"] <- round(m$Z*Weight$MinWeight["Zircon",3], digits = 3)
Normative.minerals.t[,"C"] <- round(m$C*Weight$MinWeight["Corundum",3], digits = 3)
Normative.minerals.t[,"Q"] <- round(m$Q*Weight$MinWeight["Quartz",3], digits = 3)
Normative.minerals.t[,"En"] <- round((m$Hy*ratio.Fe$xmgr) * Weight$MinWeight["Hypersthene-Mg",3], digits = 3)
Normative.minerals.t[,"Hm"] <- round(m$Hm*Weight$MinWeight["Hematite",3], digits = 3)
Normative.minerals.t[,"Ru"] <- round(m$Ru*Weight$MinWeight["Rutile",3], digits = 3)
Normative.minerals.t[,"An"] <- round(m$An*(ratio.Ca$CaO.corr + Weight$MinWeight["Anorthite",4]), digits = 3)
#Normative.minerals.t[,"Di.Mg"] <- (round((m$Di*ratio.Fe$xmgr) * (ratio.Ca$CaO.corr + Weight$MinWeight["Diopside-Mg",4]), digits = 3))
#Normative.minerals.t[,"Di.Fe"] <- (round((m$Di*ratio.Fe$xfer) * (ratio.Ca$CaO.corr + ratio.Fe$FeO.corr + Weight$MinWeight["Diopside-Ca",4]), digits = 3))
Normative.minerals.t[,"Di"] <- (round((m$Di*ratio.Fe$xfer) * (ratio.Ca$CaO.corr + ratio.Fe$FeO.corr + Weight$MinWeight["Diopside-Ca",4]), digits = 3)) + (round((m$Di*ratio.Fe$xmgr) * (ratio.Ca$CaO.corr + Weight$MinWeight["Diopside-Mg",4]), digits = 3))
Normative.minerals.t[,"Wo"] <- round(m$Wo*(ratio.Ca$CaO.corr + Weight$MinWeight["Wollastonite",4]), digits = 3)
Normative.minerals.t[,"Dcs"] <- round(m$Ds*((ratio.Ca$CaO.corr*2) + Weight$MinWeight["Dicalcium silicate",4]), digits = 3)
Normative.minerals.t[,"Tn"] <- round(m$Tn*(ratio.Ca$CaO.corr + Weight$MinWeight["Sphene",4]), digits = 3)
Normative.minerals.t[,"Pf"] <- round(m$Pf*(ratio.Ca$CaO.corr + Weight$MinWeight["Perovskite",4]), digits = 3)
#Normative.minerals.t[,"Ap.F"] <- round(m$ApCaF*((3*ratio.Ca$CaO.corr) + ((1/3)*(ratio.Ca$Ca.corr)) + Weight$MinWeight["Apatite-F",4]), digits = 3)
#Normative.minerals.t[,"Ap.Ca"] <- round(m$ApCaO*((10/3)*(ratio.Ca$CaO.corr) + (Weight$MinWeight["Apatite-Ca",4])), digits = 3)
Normative.minerals.t[,"Ap"] <- round ((m$ApCaF*((3*ratio.Ca$CaO.corr) + ((1/3)*(ratio.Ca$Ca.corr)) + Weight$MinWeight["Apatite-F",4])) + (m$ApCaO*((10/3)*(ratio.Ca$CaO.corr) + (Weight$MinWeight["Apatite-Ca",4]))), digits = 3)
Normative.minerals.t[,"Cc"] <- round(m$Cc*(ratio.Ca$CaO.corr + Weight$MinWeight["Calcite",4]), digits = 3)
Normative.minerals.t[,"Fs"] <- round((m$Hy*ratio.Fe$xfer) * (ratio.Fe$FeO.corr + Weight$MinWeight["Hypersthene-Fe",4]), digits = 3)
Normative.minerals.t[,"Fa"] <- round(((m$Ol*ratio.Fe$xfer) * ((ratio.Fe$FeO.corr*2) + Weight$MinWeight["Olivine-Fe",4])), digits = 3)
Normative.minerals.t[,"Fo"] <- round(((m$Ol*ratio.Fe$xmgr) * Weight$MinWeight["Olivine-Mg",3]), digits = 3)
Normative.minerals.t[,"Mt"] <- round(m$Mt*(ratio.Fe$FeO.corr + Weight$MinWeight["Magnetite",4]), digits = 3)
Normative.minerals.t[,"Il"] <- round(m$Il*(ratio.Fe$FeO.corr + Weight$MinWeight["Ilmenite",4]), digits = 3)
Normative.minerals.t[,"Ab"] <- round(m$Ab*(ratio.Na$Na2O.corr + Weight$MinWeight["Albite",4]), digits = 3)
Normative.minerals.t[,"Ne"] <- round(m$Ne*(ratio.Na$Na2O.corr + Weight$MinWeight["Nepheline",4]), digits = 3)
Normative.minerals.t[,"Th"] <- round(m$Th*(ratio.Na$Na2O.corr + Weight$MinWeight["Thenardite",4]), digits = 3)
Normative.minerals.t[,"Nc"] <- round(m$Nc*(ratio.Na$Na2O.corr + Weight$MinWeight["Sodium Carbonate",4]), digits = 3)
Normative.minerals.t[,"Ac"] <- round(m$Ac*(ratio.Na$Na2O.corr + Weight$MinWeight["Acmite",4]), digits = 3)
Normative.minerals.t[,"Ns"] <- round(m$Ns*(ratio.Na$Na2O.corr + Weight$MinWeight["Sodium metasilicate",4]), digits = 3)
Normative.minerals.t[,"Or"] <- round(m$Or*(ratio.K$K2O.corr + Weight$MinWeight["Orthoclase",4]), digits = 3)
Normative.minerals.t[,"Lc"] <- round(m$Lc*(ratio.K$K2O.corr + Weight$MinWeight["Leucite",4]), digits = 3)
Normative.minerals.t[,"Kp"] <- round(m$Kp*(ratio.K$K2O.corr + Weight$MinWeight["Kaliophilite",4]), digits = 3)
Normative.minerals.t[,"Ks"] <- round(m$Ks*(ratio.K$K2O.corr + Weight$MinWeight["Potassium metasilicate",4]), digits = 3)
Normative.minerals.t[,"Cm"] <- round(m$Cm*(ratio.Fe$FeO.corr + ratio.Cr$Cr2O3.corr), digits = 3)
Normative.minerals.t[,"NaCl"] <- (round(m$Hl*(ratio.Na$Na2O.corr + Weight$AtomWeight["Cl","AWeight"]), digits = 3)) - NORRRM.Free$FreeO_d
Normative.minerals.t[,"Fr"] <- round(m$Fr*(ratio.Ca$CaO.corr + Weight$MinWeight["Fluorite",4]), digits = 3) - NORRRM.Free$FreeO_c
Normative.minerals.t[,"Pr"] <- round(m$Pr*(ratio.Fe$FeO.corr + Weight$MinWeight["Pyrite",4]), digits = 3) - NORRRM.Free$FreeO_e
Normative.minerals.t[,"Free_O"] <- round(NORRRM.Free$FreeO_a + NORRRM.Free$FreeO_b + NORRRM.Free$FreeO_c + NORRRM.Free$FreeO_d + NORRRM.Free$FreeO_e, digits = 3)
Normative.minerals.t[,"DEFSiO2"] <- round(m$DEFSIO2*Weight$OxiWeight["SiO2",2] * (-1), digits = 3)

######################################################################
#                             Excess                                 #
######################################################################
Normative.minerals.t[,"Free_F"] <- round(m$FREE_F * Weight$AtomWeight["F","AWeight"], digits = 3)
Normative.minerals.t[,"Free_Cl"] <- round(m$FREE_Cl  * Weight$AtomWeight["Cl","AWeight"], digits = 3)
Normative.minerals.t[,"Free_SO3"] <- round(m$FREE_SO3 * Weight$OxiWeight["SO3","OWeight"], digits = 3)
Normative.minerals.t[,"Free_S"] <- round(m$FREE_S * Weight$AtomWeight["S","AWeight"], digits = 3)
Normative.minerals.t[,"Free_CO2"] <- round(m$FREE_CO2 * Weight$OxiWeight["CO2","OWeight"], digits = 3)
Normative.minerals.t[,"Free_Cr2O3"] <- round(m$FREE_Cr2O3 * ratio.Cr$Cr2O3.corr, digits = 3)
Normative.minerals.t[,"Free_P2O5"] <- round(m$FREE_P2O5*Weight$OxiWeight["P2O5","OWeight"], digits = 3)

#####################################################################
#                   Create Matrix for density                       #
#####################################################################
#Generate a dataframe
inputV <- data.frame(matrix(ncol = 34, nrow = length(data$Sample),
                      dimnames = list(NULL, c("VZ", "VQ", "VC", "VOr","VAb", "VAn", "VNe", "VLc","VKp",
					  "VAc","VNs","VKs","VApF","VAp.Ca","VDi.Mg","VDi.Fe","VWo","VEn","VFs","VFo","VFa",
					  "VCs","VMt","VIl","VHm","VTn","VPf","VRu","VNc","VCc","VCm","VHl","VFr","VPr"))))

inputV$VZ  = Normative.minerals.t[,"Zrn"]/ Weight$MinWeight ["Zircon",5]
inputV$VQ  = Normative.minerals.t[,"Q"] / Weight$MinWeight ["Quartz",5]    
inputV$VC = Normative.minerals.t[,"C"] / Weight$MinWeight["Corundum",5]
inputV$VOr = Normative.minerals.t[,"Or"] / Weight$MinWeight["Orthoclase",5]
inputV$VAb <- Normative.minerals.t[,"Ab"] / Weight$MinWeight["Albite",5]
inputV$VAn <- Normative.minerals.t[,"An"] / Weight$MinWeight["Anorthite",5]
inputV$VNe <- Normative.minerals.t[,"Ne"] / Weight$MinWeight["Nepheline",5]
inputV$VLc <- Normative.minerals.t[,"Lc"] / Weight$MinWeight["Leucite",5]
inputV$VKp <- Normative.minerals.t[,"Kp"] / Weight$MinWeight["Kaliophilite",5]
inputV$VAc <- Normative.minerals.t[,"Ac"] / Weight$MinWeight["Acmite",5]
inputV$VNs <- Normative.minerals.t[,"Ns"] / Weight$MinWeight["Sodium metasilicate",5]
inputV$VKs <- Normative.minerals.t[,"Ks"] / Weight$MinWeight["Potassium metasilicate",5]
inputV$VAp.F <- round(m$ApCaF*((3*ratio.Ca$CaO.corr) + ((1/3)*(ratio.Ca$Ca.corr)) + Weight$MinWeight["Apatite-F",4]), digits = 3) / Weight$MinWeight["Apatite-F",5]
inputV$VAp.Ca <- round(m$ApCaO*((10/3)*(ratio.Ca$CaO.corr) + (Weight$MinWeight["Apatite-Ca",4])), digits = 3) / Weight$MinWeight["Apatite-Ca",5]
inputV$VDi.Mg <- (round((m$Di*ratio.Fe$xmgr) * (ratio.Ca$CaO.corr + Weight$MinWeight["Diopside-Mg",4]), digits = 3)) / Weight$MinWeight["Diopside-Mg",5]
inputV$VDi.Fe <- (round((m$Di*ratio.Fe$xfer) * (ratio.Ca$CaO.corr + ratio.Fe$FeO.corr + Weight$MinWeight["Diopside-Ca",4]), digits = 3)) / Weight$MinWeight["Diopside-Fe",5]
inputV$VWo <- Normative.minerals.t[,"Wo"] / Weight$MinWeight["Wollastonite",5]
inputV$VEn <- Normative.minerals.t[,"En"] / Weight$MinWeight["Hypersthene-Mg",5]
inputV$VFs  <- Normative.minerals.t[,"Fs"] /  Weight$MinWeight["Hypersthene-Fe",5]
inputV$VFo  <- Normative.minerals.t[,"Fo"] /  Weight$MinWeight["Olivine-Mg",5]
inputV$VFa  <- Normative.minerals.t[,"Fa"]  / Weight$MinWeight["Olivine-Fe",5]
inputV$VCs  <- Normative.minerals.t[,"Dcs"] / Weight$MinWeight["Dicalcium silicate",5]
inputV$VMt <- Normative.minerals.t[,"Mt"] / Weight$MinWeight["Magnetite",5]
inputV$VIl <- Normative.minerals.t[,"Il"] / Weight$MinWeight["Ilmenite",5]
inputV$VHm <- Normative.minerals.t[,"Hm"] / Weight$MinWeight["Hematite",5]
inputV$VTn <- Normative.minerals.t[,"Tn"] / Weight$MinWeight["Sphene",5]
inputV$VPf <- Normative.minerals.t[,"Pf"] / Weight$MinWeight["Perovskite",5]
inputV$VRu <- Normative.minerals.t[,"Ru"] / Weight$MinWeight["Rutile",5]
inputV$VNc <- Normative.minerals.t[,"Nc"] / Weight$MinWeight["Sodium Carbonate",5]
inputV$VCc <- Normative.minerals.t[,"Cc"] / Weight$MinWeight["Calcite",5]
inputV$VCm <- Normative.minerals.t[,"Cm"] / Weight$MinWeight["Chromite",5]
inputV$VHl <- Normative.minerals.t[,"NaCl"] / Weight$MinWeight["Halite",5]
inputV$VFr <- Normative.minerals.t[,"Fr"] / Weight$MinWeight["Fluorite",5]
inputV$VPr <- Normative.minerals.t[,"Pr"] / Weight$MinWeight["Pyrite",5]

#####################################################################
#                     Change values NA->0                           #
#####################################################################

    inputV[is.na(inputV)] <- 0 ##Change values NA->0

######################################################################
#                      Sums.Density                                  #
######################################################################

Sums.Density<- rowSums(inputV, na.rm = any(!is.na(inputV)))

######################################################################
#                      Density of minerals                           #
######################################################################
NORRRM.Density<- c("Z","Q","C","Or","Ab","An","Ne","Lc","Kp","Ac","Ns","Ks","Di.Mg","Di.Fe","Wo","En","Fs","Fo","Fa","Cs","Mt","Il","Hm","Tn","Pf","Ru","Ap-F","Ap-Ca","Nc","Cc","Cm","Hl","Fr","Pr")
Dens <- matrix(data = NA, ncol = length(NORRRM.Density), nrow = length(data$Sample), byrow = FALSE, dimnames = NULL)
colnames(Dens) <- NORRRM.Density
Dens[,"Z"] <- ((inputV$VZ * (100/Sums.Density)) * Weight$MinWeight ["Zircon",5]) / 100  
Dens[,"Q"] <- ((inputV$VQ * (100/Sums.Density)) * Weight$MinWeight ["Quartz",5]) / 100  
Dens[,"C"] <- ((inputV$VC * (100/Sums.Density)) * Weight$MinWeight ["Corundum",5]) / 100  
Dens[,"Or"] <- ((inputV$VOr * (100/Sums.Density)) * Weight$MinWeight["Orthoclase",5]) / 100
Dens[,"Ab"] <- ((inputV$VAb * (100/Sums.Density)) * Weight$MinWeight["Albite",5]) / 100
Dens[,"An"] <- ((inputV$VAn * (100/Sums.Density)) * Weight$MinWeight["Anorthite",5]) / 100
Dens[,"Ne"] <- ((inputV$VNe * (100/Sums.Density)) * Weight$MinWeight["Nepheline",5]) / 100
Dens[,"Lc"] <- ((inputV$VLc * (100/Sums.Density)) * Weight$MinWeight["Leucite",5]) / 100
Dens[,"Kp"] <- ((inputV$VKp * (100/Sums.Density)) * Weight$MinWeight["Kaliophilite",5]) / 100
Dens[,"Ac"] <- ((inputV$VAc * (100/Sums.Density)) * Weight$MinWeight["Acmite",5]) / 100
Dens[,"Ns"] <- ((inputV$VNs * (100/Sums.Density)) * Weight$MinWeight["Sodium metasilicate",5]) / 100
Dens[,"Ks"] <- ((inputV$VKs * (100/Sums.Density)) * Weight$MinWeight["Potassium metasilicate",5]) / 100
Dens[,"Ap-F"] <- ((inputV$VAp.F * (100/Sums.Density)) * Weight$MinWeight["Apatite-F",5]) / 100
Dens[,"Ap-Ca"] <- ((inputV$VAp.Ca * (100/Sums.Density)) * Weight$MinWeight["Apatite-Ca",5]) / 100
Dens[,"Di.Mg"] <- ((inputV$VDi.Mg * (100/Sums.Density)) * Weight$MinWeight["Diopside-Mg",5]) / 100
Dens[,"Di.Fe"] <- ((inputV$VDi.Fe * (100/Sums.Density)) * Weight$MinWeight["Diopside-Fe",5]) / 100
Dens[,"Wo"] <- ((inputV$VWo * (100/Sums.Density)) * Weight$MinWeight["Wollastonite",5]) / 100
Dens[,"En"] <- ((inputV$VEn * (100/Sums.Density)) * Weight$MinWeight["Hypersthene-Mg",5]) / 100
Dens[,"Fs"] <- ((inputV$VFs * (100/Sums.Density)) * Weight$MinWeight["Hypersthene-Fe",5]) / 100
Dens[,"Fo"] <- ((inputV$VFo * (100/Sums.Density)) * Weight$MinWeight["Olivine-Mg",5]) / 100
Dens[,"Fa"] <- ((inputV$VFa * (100/Sums.Density)) * Weight$MinWeight["Olivine-Fe",5]) / 100
Dens[,"Cs"] <- ((inputV$VCs * (100/Sums.Density)) * Weight$MinWeight["Dicalcium silicate",5]) / 100
Dens[,"Mt"] <- ((inputV$VMt * (100/Sums.Density)) * Weight$MinWeight["Magnetite",5]) / 100
Dens[,"Il"] <- ((inputV$VIl * (100/Sums.Density)) * Weight$MinWeight["Ilmenite",5]) / 100
Dens[,"Hm"] <- ((inputV$VHm * (100/Sums.Density)) * Weight$MinWeight["Hematite",5]) / 100
Dens[,"Tn"]<- ((inputV$VTn * (100/Sums.Density)) * Weight$MinWeight["Sphene",5]) / 100
Dens[,"Pf"] <- ((inputV$VPf * (100/Sums.Density)) * Weight$MinWeight["Perovskite",5]) / 100
Dens[,"Ru"] <- ((inputV$VRu * (100/Sums.Density)) * Weight$MinWeight["Rutile",5]) / 100
Dens[,"Nc"] <- ((inputV$VNc * (100/Sums.Density)) * Weight$MinWeight["Sodium Carbonate",5]) / 100
Dens[,"Cc"] <- ((inputV$VCc * (100/Sums.Density)) * Weight$MinWeight["Calcite",5]) / 100
Dens[,"Cm"] <- ((inputV$VCm * (100/Sums.Density)) * Weight$MinWeight["Chromite",5]) / 100
Dens[,"Hl"] <- ((inputV$VHl * (100/Sums.Density)) * Weight$MinWeight["Halite",5]) / 100
Dens[,"Fr"] <- ((inputV$VFr * (100/Sums.Density)) * Weight$MinWeight["Fluorite",5]) / 100
Dens[,"Pr"] <- ((inputV$VPr * (100/Sums.Density)) * Weight$MinWeight["Pyrite",5]) / 100

##Define output##
preoutput<- data.frame(Normative.minerals.t,  stringsAsFactors = FALSE)
i  <- (colSums(preoutput, na.rm=T) != 0) # T if colSum is not 0, F otherwise
output.minerals <- preoutput[, i] # all the non-zero columns
output.minerals$Wo <-Normative.minerals.t[,"Wo"]
output.minerals[,"Sum (%)"] <- round(rowSums(output.minerals, na.rm = any(!is.na(output.minerals))),3)
output.minerals [output.minerals == 0] <- NA  ##Change values 0->NA
output <- data.frame(data$Sample, format(output.minerals, nsmall = 3))
output$Salic <- format(Normative.minerals.t[,"Q"] + Normative.minerals.t[,"Or"] + Normative.minerals.t[,"Ab"] + Normative.minerals.t[,"An"], nsmall = 3)
output$Femic <- format((round((m$Di*ratio.Fe$xmgr) * (ratio.Ca$CaO.corr + Weight$MinWeight["Diopside-Mg",4]), digits = 3)) + (round((m$Di*ratio.Fe$xfer) * (ratio.Ca$CaO.corr + ratio.Fe$FeO.corr + Weight$MinWeight["Diopside-Ca",4]), digits = 3)) + Normative.minerals.t[,"En"] + Normative.minerals.t[,"Fs"] + Normative.minerals.t[,"Fo"] + Normative.minerals.t[,"Fa"] + Normative.minerals.t[,"Mt"] + Normative.minerals.t[,"Il"] + Normative.minerals.t[,"Hm"], nsmall = 3)
output$C.I. <- Normative.minerals.t[,"An"] + (2.1570577*(round((m$Di*ratio.Fe$xmgr) * (ratio.Ca$CaO.corr + Weight$MinWeight["Diopside-Mg",4]), digits = 3))) + Normative.minerals.t[,"Fo"] + (0.7007616*Normative.minerals.t[,"Fs"])
output$D.I. <-  Normative.minerals.t[,"Q"] + Normative.minerals.t[,"Or"] + Normative.minerals.t[,"Ab"] + Normative.minerals.t[,"Ne"] + Normative.minerals.t[,"Lc"] 
#output$S.I. <- (100*x$MgO.adj)/(x$MgO.adj + x$Fe2O3.adj + x$FeO.adj + x$Na2O.adj + x$K2O.adj)
#output$A.R. <- ifelse (x$SiO2.adj > 0 & x$K2O.adj/x$Na2O.adj > 1.0 & x$K2O.adj/x$Na2O.adj < 2.5, (x$Al2O3.adj + x$CaO.adj + (2*x$Na2O.adj))/(x$Al2O3.adj + x$CaO.adj - (2*x$Na2O.adj)), (x$Al2O3.adj + x$CaO.adj + x$Na2O.adj + x$K2O.adj)/(x$Al2O3.adj + x$CaO.adj - x$Na2O.adj - x$K2O.adj) )
output$Density <- round (rowSums(Dens, na.rm = any(!is.na(Dens))), digits = 2)###Total Density
output$FSSI <- round (Normative.minerals.t[,"Q"] - (Normative.minerals.t[,"Lc"]  + (2*(Normative.minerals.t[,"Ne"]  + Normative.minerals.t[,"Kp"] ))) /100, 2)
col.output<-c("Sample", colnames(output.minerals), "Salic", "Femic", "C.I.", "D.I.", "Density", "FSSI")

colnames(output) <- col.output

return(output)

}
## End(Not run)