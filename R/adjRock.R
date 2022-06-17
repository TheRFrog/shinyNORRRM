#####################################################################
#                        Main function                              #
#       Calculate major elements data on an anhydrous basis         #
#####################################################################
# must recast to 100 %.....more or less
adjRock <- function(data, Type, Fe.adjustment, Cancrinite, Calcite)
{

#####################################################################
#                         Create Matrix                             #
#                                                                   #
#####################################################################

##find whether the column CO2 exists in the data.frame
#data$CO2 <- ifelse("CO2" %in% colnames(data), data$CO2, rep(0, length(data$sample)))


# Empty variables
# n = length(data[,"Sample"])

major <- data.frame(#Sample = as.character(data$Sample),
                    SiO2 = data$SiO2,
                    TiO2 = data$TiO2,
                    Al2O3 = data$Al2O3,
                    Fe2O3 = data$Fe2O3,
                    FeO = data$FeO,
                    MnO = data$MnO,
                    MgO = data$MgO,
                    CaO = data$CaO,
                    Na2O = data$Na2O,
                    K2O = data$K2O,
                    P2O5 = data$P2O5,
                    CO2 = data$CO2,
                    SO3 = data$SO3,
                    S = data$S,
                    F.trace = data$F,
                    Cl = data$Cl,
                    LOI = data$LOI)

major[is.na(major)] <- 0 ##Change values NA->0

######################################################################
#                         Adjustment of CO2                          #
######################################################################

#Generate a dataframe
rock.type <- data.frame(matrix(ncol = 12, nrow = length(data[,"Sample"]),
                      dimnames = list(NULL, c("CO2_logical", "R.Fe", "Rock", "Rock.Intrusive", "Type", "Volc", "Ox.Fe", "Magma" , "Rock.TAS" , "Adj", "Fe.ratio", "T.Fe"))))

    rock.type$CO2_logical <- ifelse (Cancrinite | Calcite == TRUE , TRUE, FALSE)
    major$CO2 <- ifelse (rock.type$CO2_logical == TRUE , major$CO2, 0)
    major$LOI <- major$LOI-major$CO2
    Sums.maj <- rowSums(major)

######################################################################
#                  Adjust to an anhydrous basis                      #
######################################################################

    adjust <- function(m)
    {##function to an anhydrous adjust
    anhydrous <- round((100*m)/(Sums.maj-major$LOI), digits=3)
    }

    adj<- adjust (major[,1:16])#test out <- rowSums(adj)== 100

######################################################################
#          Adjustment of Fe-oxidation (Middlemost, 1989)             #
######################################################################
######################################################################

    TAS = data.frame(S = adj$SiO2, NK = adj$K2O + adj$Na2O)

#Generate a dataframe
rock.type <- data.frame(matrix(ncol = 11, nrow = length(data[,"Sample"]),
                      dimnames = list(NULL, c("R.Fe", "Rock", "Rock.Intrusive", "Type", "Volc", "Ox.Fe", "Magma" , "Rock.TAS" , "Adj", "Fe.ratio", "T.Fe"))))

#require(pracma) #Call pracma to define Fe.ratio

#Rhyolite#
#define the polygon
Rhyolite = cbind(a = c(77,100,100,69,69),
                 b = c(0,0,25,25,8))
#check which points fall within the polygon_inpolygon: Polygon Region
rhy <- inpolygon(TAS[,"S"], TAS[,"NK"], Rhyolite[,"a"], Rhyolite[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (rhy == TRUE, 0.5, 0)
rock.type$Rock <- ifelse (rhy == TRUE, "Rhyolite", "NA")
rock.type$Rock.Intrusive <- ifelse (rhy == TRUE, "Granite", "NA")
#polygon(Rhyolite)


#Phonolite#
#define the polygon
Phonolite = cbind(a = c(57.6,69,30),
                  b = c(11.7,17.73,24.15))
#check which points fall within the polygon_inpolygon: Polygon Region
pho <- inpolygon(TAS[,"S"], TAS[,"NK"], Phonolite[,"a"], Phonolite[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (pho == TRUE, 0.5, rock.type$R.Fe)
rock.type$Rock <- ifelse (pho == TRUE, "Phonolite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (pho == TRUE, "Nephelinic Syenite", rock.type$Rock.Intrusive)
#polygon(Phonolite)


#Trachyte/Trachydacite#
#define the polygon
Trachyte.Trachydacite = cbind(a = c(63,69,69,57.6),
                              b = c(7,8,17.73,11.7))
#check which points fall within the polygon_inpolygon: Polygon Region
tr.tr <- inpolygon(TAS[,"S"], TAS[,"NK"], Trachyte.Trachydacite[,"a"], Trachyte.Trachydacite[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (tr.tr == TRUE, 0.5, rock.type$R.Fe)
rock.type$Rock <- ifelse (tr.tr == TRUE, "Trachyte/Trachydacite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (tr.tr == TRUE, "Quartz Monzonite", rock.type$Rock.Intrusive)
#polygon(Trachyte.Trachydacite)


#Dacite#
#define the polygon
Dacite = cbind(a = c(63,77,69,63),
               b = c(0,0,8,7))
#check which points fall within the polygon_inpolygon: Polygon Region
dac <- inpolygon(TAS[,"S"], TAS[,"NK"], Dacite[,"a"], Dacite[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (dac == TRUE, 0.4, rock.type$R.Fe)
rock.type$Rock <- ifelse (dac == TRUE, "Dacite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (dac == TRUE, "Granodiorite", rock.type$Rock.Intrusive)
#polygon(Dacite)


#Tephriphonolite#
#define the polygon
Tephriphonolite = cbind(a = c(53,57.6,52.5,48.4),
                        b = c(9.3,11.7,14,11.5))
#check which points fall within the polygon_inpolygon: Polygon Region
teph <- inpolygon(TAS[,"S"], TAS[,"NK"], Tephriphonolite[,"a"], Tephriphonolite[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (teph == TRUE, 0.4, rock.type$R.Fe)
rock.type$Rock <- ifelse (teph == TRUE, "Tephriphonolite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (teph == TRUE, "Foide Syenite", rock.type$Rock.Intrusive)
#polygon(Tephriphonolite)


#Trachyandesite#
#define the polygon
Trachyandesite = cbind(a = c(57,63,57.6,53),
                       b = c(5.9,7,11.7,9.3))
#check which points fall within the polygon_inpolygon: Polygon Region
tr.an <- inpolygon(TAS[,"S"], TAS[,"NK"], Trachyandesite[,"a"], Trachyandesite[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (tr.an == TRUE, 0.4, rock.type$R.Fe)
rock.type$Rock <- ifelse (tr.an == TRUE, "Trachyandesite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (tr.an == TRUE, "Monzonite", rock.type$Rock.Intrusive)
#polygon(Trachyandesite)


#Andesite#
#define the polygon
Andesite = cbind(a = c(57,63,63,57),
                 b = c(0,0,7,5.9))
#check which points fall within the polygon_inpolygon: Polygon Region
and <- inpolygon(TAS[,"S"], TAS[,"NK"], Andesite[,"a"], Andesite[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (and == TRUE, 0.35, rock.type$R.Fe)
rock.type$Rock <- ifelse (and == TRUE, "Andesite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (and == TRUE, "Diorite", rock.type$Rock.Intrusive)
#polygon(Andesite)


#Phonotephrite#
#define the polygon
Phonotephrite = cbind(a = c(49.4,53,48.4,45),
                      b = c(7.3,9.3,11.5,9.4))
#check which points fall within the polygon_inpolygon: Polygon Region
pho.teph <- inpolygon(TAS[,"S"], TAS[,"NK"], Phonotephrite[,"a"], Phonotephrite[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (pho.teph == TRUE, 0.35, rock.type$R.Fe)
rock.type$Rock <- ifelse (pho.teph == TRUE, "Phonotephrite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (pho.teph == TRUE, "Foide Syenite", rock.type$Rock.Intrusive)
#polygon(Phonotephrite)


#Basaltictrachyandesite#
#define the polygon
Basaltictrachyandesite = cbind(a = c(52,57,53,49.4),
                               b = c(5,5.9,9.3,7.3))
#check which points fall within the polygon_inpolygon: Polygon Region
ba.tr.an <- inpolygon(TAS[,"S"], TAS[,"NK"], Basaltictrachyandesite[,"a"], Basaltictrachyandesite[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (ba.tr.an == TRUE, 0.35, rock.type$R.Fe)
rock.type$Rock <- ifelse (ba.tr.an == TRUE, "Basaltictrachyandesite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (ba.tr.an == TRUE, "Monzodiorite", rock.type$Rock.Intrusive)
#polygon(Basaltictrachyandesite)


#Basalticandesite#
#define the polygon
Basalticandesite = cbind(a = c(52,57,57,52),
                         b = c(0,0,5.9,5))
#check which points fall within the polygon_inpolygon: Polygon Region
ba.an <- inpolygon(TAS[,"S"], TAS[,"NK"], Basalticandesite[,"a"], Basalticandesite[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (ba.an == TRUE, 0.3, rock.type$R.Fe)
rock.type$Rock <- ifelse (ba.an == TRUE, "Basalticandesite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (ba.an == TRUE, "Gabrodiorite", rock.type$Rock.Intrusive)
#polygon(Basalticandesite)


#Tephrite/Basanite#
#define the polygon
Tephrite.Basanite = cbind(a = c(41,41,45,49.4,47),
                          b = c(6,7,9.4,7.3,6))
#check which points fall within the polygon_inpolygon: Polygon Region
teph.bas <- inpolygon(TAS[,"S"], TAS[,"NK"], Tephrite.Basanite[,"a"], Tephrite.Basanite[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (teph.bas  == TRUE, 0.3, rock.type$R.Fe)
rock.type$Rock <- ifelse (teph.bas  == TRUE, "Tephrite/Basanite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (teph.bas == TRUE, "Foid Monzogabbro/Diorite", rock.type$Rock.Intrusive)
#polygon(Tephrite.Basanite)


#Trachybasalt#
#define the polygon
Trachybasalt = cbind(a = c(45,52,49.4),
                     b = c(5,5,7.3))
#check which points fall within the polygon_inpolygon: Polygon Region
tr.ba <- inpolygon(TAS[,"S"], TAS[,"NK"], Trachybasalt[,"a"], Trachybasalt[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (tr.ba  == TRUE, 0.3, rock.type$R.Fe)
rock.type$Rock <- ifelse (tr.ba  == TRUE, "Trachybasalt", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (tr.ba == TRUE, "Monzogabbro", rock.type$Rock.Intrusive)
#polygon(Trachybasalt)


#Basalt#
#define the polygon
Basalt = cbind(a = c(45,45,52,52),
               b = c(0,5,5,0))
#check which points fall within the polygon_inpolygon: Polygon Region
bas<- inpolygon(TAS[,"S"], TAS[,"NK"], Basalt[,"a"], Basalt[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (bas == TRUE, 0.2, rock.type$R.Fe)
rock.type$Rock <- ifelse (bas  == TRUE, "Basalt", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (bas == TRUE, "Gabbro", rock.type$Rock.Intrusive)
#polygon(Basalt)


#Tephrite/Basanite#
#define the polygon
Basanite.Tephrite = cbind(a = c(41,41,47,45,45),
                          b = c(3,6,6,5,3))
#check which points fall within the polygon_inpolygon: Polygon Region
ba.teph<- inpolygon(TAS[,"S"], TAS[,"NK"], Basanite.Tephrite[,"a"], Basanite.Tephrite[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (ba.teph == TRUE, 0.2, rock.type$R.Fe)
rock.type$Rock <- ifelse (ba.teph == TRUE, "Tephrite/Basanite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (ba.teph == TRUE, "Foid Monzogabbro/Diorite", rock.type$Rock.Intrusive)
#polygon(Basanite.Tephrite)


#Picrobasalt#
#define the polygon
Picrobasalt = cbind(a = c(41,45,45,41),
                    b = c(0,0,3,3))
#check which points fall within the polygon_inpolygon: Polygon Region
pic <- inpolygon(TAS[,"S"], TAS[,"NK"], Picrobasalt[,"a"], Picrobasalt[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (pic  == TRUE, 0.15, rock.type$R.Fe)
rock.type$Rock <- ifelse (pic  == TRUE, "Picrobasalt", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (pic  == TRUE, "Gabbro", rock.type$Rock.Intrusive)
#polygon(Picrobasalt)


#Foidite.1#
#define the polygon
Foidite.1 = cbind(a = c(46,52.5,30,0,0),
                  b = c(10,14,24.15,24.15,10))
#check which points fall within the polygon_inpolygon: Polygon Region
foi1 <- inpolygon(TAS[,"S"], TAS[,"NK"], Foidite.1[,"a"], Foidite.1[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (foi1  == TRUE, 0.4, rock.type$R.Fe)
rock.type$Rock <- ifelse (foi1  == TRUE, "Foidite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (foi1  == TRUE, "Foidite", rock.type$Rock.Intrusive)
#polygon(Foidite.1)


#Foidite.2#
#define the polygon
Foidite.2 = cbind(a = c(0,46,41,0),
                  b = c(10,10,7,7))
#check which points fall within the polygon_inpolygon: Polygon Region
foi2 <- inpolygon(TAS[,"S"], TAS[,"NK"], Foidite.2[,"a"], Foidite.2[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (foi2  == TRUE, 0.3, rock.type$R.Fe)
rock.type$Rock <- ifelse (foi2  == TRUE, "Foidite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (foi2  == TRUE, "Foidite", rock.type$Rock.Intrusive)
#polygon(Foidite.2)


#Foidite.3#
#define the polygon
Foidite.3 = cbind(a = c(0,41,41,0),
                  b = c(7,7,3,3))
#check which points fall within the polygon_inpolygon: Polygon Region
foi3 <- inpolygon(TAS[,"S"], TAS[,"NK"], Foidite.3[,"a"], Foidite.3[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (foi3  == TRUE, 0.2, rock.type$R.Fe)
rock.type$Rock <- ifelse (foi3  == TRUE, "Foidite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (foi3  == TRUE, "Foidite", rock.type$Rock.Intrusive)
#polygon(Foidite.3)


#Foidite.4#
#define the polygon
Foidite.4 = cbind(a = c(0,41,41,0),
                  b = c(0,0,3,3))
#check which points fall within the polygon_inpolygon: Polygon Region
foi4 <- inpolygon(TAS[,"S"], TAS[,"NK"], Foidite.4[,"a"], Foidite.4[,"b"], boundary = TRUE)#
rock.type$R.Fe <- ifelse (foi4  == TRUE, 0.15, rock.type$R.Fe)
rock.type$Rock <- ifelse (foi4  == TRUE, "Foidite", rock.type$Rock)
rock.type$Rock.Intrusive <- ifelse (foi4  == TRUE, "Ijolite", rock.type$Rock.Intrusive)
#polygon(Foidite.4)


######################################################################
#                     Define type of magma                           #
######################################################################
######################################################################
#Hipersilicic#
#define the polygon
Hypersilicic = cbind(a = c(66,100,100,66),
                     b = c(0,0,24,24))
#create check which points fall within the polygon
hip <- inpolygon(TAS[,"S"], TAS[,"NK"], Hypersilicic[,"a"], Hypersilicic[,"b"], boundary = TRUE)#
rock.type$Type <- ifelse (hip  == TRUE, "Hypersilicic", NA)


#Intermediate#
#define the polygon
Intermediate = cbind(a = c(52,66,66,52),
                     b = c(0,0,24,24))
#create check which points fall within the polygon
int <- inpolygon(TAS[,"S"], TAS[,"NK"], Intermediate[,"a"], Intermediate[,"b"], boundary = TRUE)#
rock.type$Type <- ifelse (int  == TRUE, "Intermediate", rock.type$Type)


#Basic#
#define the polygon
Basic = cbind(a = c(45,52,52,45),
              b = c(0,0,24,24))
#create check which points fall within the polygon
bas <- inpolygon(TAS[,"S"], TAS[,"NK"], Basic[,"a"], Basic[,"b"], boundary = TRUE)#
rock.type$Type <- ifelse (bas  == TRUE, "Mafic", rock.type$Type)


#Ultrabasic#
#define the polygon
Ultrabasic = cbind(a = c(0,45,45,0),
                   b = c(0,0,24,24))
#create check which points fall within the polygon
Ubas <- inpolygon(TAS[,"S"], TAS[,"NK"], Ultrabasic[,"a"], Ultrabasic[,"b"], boundary = TRUE)#
rock.type$Type <- ifelse (Ubas  == TRUE, "Ultramafic", rock.type$Type)

######################################################################
#           Adjustment of Fe-oxidation (LeMaitre, 1976)              #
######################################################################
rock.type$Volc <- ifelse (Type == "Volcanic", rep(TRUE, length (data$Sample)) , rep(FALSE, length (data$Sample)))
rock.type$Ox.Fe <- ifelse(rock.type$Volc == TRUE, 0.93-(0.0042 * adj$SiO2)-(0.022*(adj$Na2O + adj$K2O)), 0.88-(0.0016 * adj$SiO2)-(0.027*(adj$Na2O + adj$K2O)))

######################################################################
#                      Define FeOx output                            #
######################################################################
rock.type$Magma <- rock.type$Type
rock.type$Rock.TAS<- ifelse (rock.type$Volc == TRUE, rock.type$Rock, rock.type$Rock.Intrusive)
Fe.adjustment <- rep(Fe.adjustment, length (data$Sample))
rock.type$Fe.ratio <- ifelse(Fe.adjustment == "Middlemost", rock.type$R.Fe, rock.type$Ox.Fe)
rock.type$Fe.ratio <- ifelse(Fe.adjustment == "Le Maitre", rock.type$Ox.Fe, rock.type$Fe.ratio)
rock.type$Fe.ratio <- ifelse(Fe.adjustment == "Fe+3/Fe+2", adj$Fe2O3/adj$FeO, rock.type$Fe.ratio)
rock.type$Fe.ratio <- ifelse(is.finite (rock.type$Fe.ratio), rock.type$Fe.ratio, rock.type$R.Fe)
rock.type$Fe.ratio <- round(rock.type$Fe.ratio, digits=2)

######################################################################

######################################################################
#                Define Fe.output (Pruseth, 2009)                    #
######################################################################
rock.type$T.Fe <- adj$Fe2O3 + (adj$FeO*1.111348)
adj$FeO <- round((rock.type$T.Fe/(1+(rock.type$Fe.ratio * 0.899809))) *0.899809, digits = 3)#
adj$Fe2O3  <- round(rock.type$T.Fe-(rock.type$T.Fe/(1+(rock.type$Fe.ratio *0.899809))), digits = 3)#

######################################################################
#     ##Recalculate major elements data on an anhydrous basis##      #
######################################################################
######################################################################

major_adj <- data.frame(adj$SiO2, adj$TiO2, adj$Al2O3, adj$Fe2O3, adj$FeO, adj$MnO, adj$MgO, adj$CaO, adj$Na2O, adj$K2O, adj$P2O5, adj$CO2, adj$SO3, adj$S, adj$F.trace, adj$Cl)
col.major_adj<-c("sio2","tio2","al2o3","fe2o3","feo","mno","mgo","cao","na2o","k2o","p2o5","co2","so3","s","f","cl")
colnames(major_adj) <- col.major_adj
major_adj [is.na(major_adj)] <- 0 ##Change values NA->0
Sums.maj <- rowSums(major_adj)
                    
######################################################################
#                  Adjust to an anhydrous basis2                     #
######################################################################

    adjust2 <- function(m)
    {##function to an anhydrous adjust
    anhydrous <- round((100*m)/(Sums.maj),digits = 3)
    }

    major_adj <- adjust2 (major_adj)#test out <- rowSums(adj)== 100
    Sums.total <- round (rowSums(major_adj), 3)

######################################################################
#                         Define Output                              #
######################################################################
major_adj[major_adj==0]<-NA ##Change values 0->NA
output <- data.frame(data$Sample, format(major_adj[,1:11], nsmall = 3), format(Sums.total, nsmall = 3), format(major_adj[,12:16], nsmall = 3), rock.type$Magma, rock.type$Rock.TAS, rock.type$Fe.ratio)

##Define output##
col.output<-c("Sample", "SiO2 adj", "TiO2 adj", "Al2O3 adj", "Fe2O3 adj", "FeO adj", "MnO adj", "MgO adj", "CaO adj", "Na2O adj", "K2O adj", "P2O5 adj", "Total adj", "CO2 adj", "SO3 adj", "S adj", "F adj", "Cl adj", "Magma", "TAS class", "Fe ratio")
colnames(output) <- col.output
return(output)

}
## End(Not run)
