#'
#' @export
#'
#' @description
#' Starts the interim application in the client's browser.
#'
#' @param host host link (defaults to the local machine "127.0.0.1")
#' @param port port number (randomly chosen unless specified as a certain number)
#' @param browser path to browser exe (defaults to standard browser)
#'
#' @import graphics
#' @import shiny
#' @return A shiny app
 
shinyNORRRM <- function(host = "127.0.0.1", port = NULL, browser=NULL) {

   #### User Interface (ui) ####


  ui = navbarPage(inverse = TRUE,
                title = strong(em("shinyNORRRM", style = "color:white")),
                theme = shinytheme("cerulean"),
                position = "static-top",
                tabPanel("Input data", id ="Input data", icon=icon("database"),
                           sidebarLayout(
                             sidebarPanel(width = 2,
                               fileInput("file",tags$div("Upload the file (*.csv)",
                               br(),
                               "Default max. file size is 50MB")),
                               radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Tab='\t', Semicolon=';', Space=''), 
                                            selected = ',', inline = FALSE),
                               hr(style = "border-top: 2px solid #3385d6;", .noWS = "before-end", .renderHook = NULL),
                               radioButtons("var1", "Type of rock", list("Volcanic", "Plutonic"), selected = "Volcanic", inline = TRUE),
                               radioButtons("var2", "Fe adjustment", list("Middlemost", "Le Maitre", "Fe+3/Fe+2"), "Middlemost"),
                               radioButtons("var3", "Cancrinite", list (FALSE, TRUE), selected = FALSE, inline = TRUE),
                               radioButtons("var4", "Calcite", list (FALSE, TRUE), selected = FALSE, inline = TRUE),
                               br(),
                               actionButton("run_button", "Run Analysis", icon = icon("play"), color = "success"),
                               br(),
                               progressBar(
                                   id = "pb",
                                   value = 0,
                                   total = 100,
                                   title = "Progress bar",
                                   display_pct = F
                                 ),
                               helpText(style="text-align:left","This app is based on R"),
                               tags$a(href="https://www.r-project.org/", img(
                                 src = "logos/Rlogo.png", align = "center",
                                 height = 50, # originally 350
                                 style="inline-block;margin:5px 5px"),
                                 br()
                               )),
                             mainPanel(width = 10,
                                       tableOutput("table")
                             )
                           )
                ),
                tabPanel("Major and minor elements (% adj.)", id = "Major and minor elements (% adj.)", icon=icon("table"),  
                         sidebarLayout(
                           sidebarPanel(width = 1,
                                        tags$a(strong("Anhydrous basis data")),
                                        br(),
                                        helpText("Click on the download button to download the dataset (.adj) in *.csv format"),
                                        downloadButton('downloadMajor', 'Download')
                           ),
                           mainPanel(width = 11,
                                     tableOutput("table_adjRock")))),
                  tabPanel("Normative minerals", id = "Normative minerals", icon=icon("code"),
                           sidebarLayout(
                             sidebarPanel(width = 1,
                                          tags$a(strong("NORRRM")),
                                          br(),
                                          helpText("Click on the download button to download the Normative minerals in *.csv format"),
                                          downloadButton("downloadCIPW", "Download")
                             ),
                             mainPanel(width = 11,
                                       tableOutput("table_CIPW"),
									   textOutput("CIPWcom")))),
                  tabPanel("Indices", id = "Indices", icon=icon("list-alt"),
                           sidebarLayout(
                             sidebarPanel(width = 1,
                                          tags$a(strong("Indices")),
                                          br(),
                                          helpText("Click on the download button to download the common Indices used in igneous petrology in *.csv format"),
                                          downloadButton("downloadIndices", "Download")
                             ),
                             mainPanel(width = 11,
                               tableOutput("table_Indices"),
							   textOutput("Indicescom")))),
                  tabPanel("TAS", id = "TAS", icon = icon("fas fa-chart-line"),
                           sidebarLayout(
                             sidebarPanel(width = 1,
                                          tags$a(strong("TAS diagram")),
                                          br(),
                                          helpText("Select the download format"),
                                          radioButtons("color", "Color of the points", choices = c("green", "red", "blue"), selected = "green"),
                                          br(),
                                          radioButtons("format", "Download file format", choices = c( "png", "pdf"), selected = "png"),
                                          helpText("Click on the download button to download the TAS plot"),
                                          downloadButton("downloadTAS", "Download")
                             ),
                             mainPanel(width = 11,
                                       plotOutput("plot", height=600, width = 600),
                                       textOutput("TAScom")))),
				  tabPanel("Feldspar diagram", id = "Feldspar diagram", icon = icon("fas fa-chart-line"),
                           sidebarLayout(
                             sidebarPanel(width = 1,
                                          tags$a(strong("Ternaries")),
                                          br(),
                                          helpText("Select the download format"),
                                          radioButtons("color2", "Color of the points", choices = c("green", "red", "blue"), selected = "green"),
                                          br(),
                                          radioButtons("format2", "Download file format", choices = c( "png", "pdf"), selected = "png"),
                                          helpText("Click on the download button to download the Ternary diagram"),
                                          downloadButton("downloadTernaries", "Download")
                             ),
                             mainPanel(width = 11,
                                     plotOutput("plot2", height=620, width = 620),
                                       textOutput("Feldsparcom")))
									   ),
                tabPanel("Mafic diagrams", id = "Mafic diagrams", icon = icon("fas fa-chart-line"),
                           sidebarLayout(
                             sidebarPanel(width = 1,
                                          tags$a(strong("Mafic")),
                                          br(),
                                          helpText("Select the download format"),
                                          radioButtons("color3", "Color of the points", choices = c("green", "red", "blue"), selected = "green"),
                                          br(),
                                          radioButtons("format3", "Download file format", choices = c( "png", "pdf"), selected = "png"),
                                          helpText("Click on the download button to download the Ternaries diagrams"),
                                          downloadButton("downloadMafic", "Download")
                             ),
                             mainPanel(width = 11,
                                     plotOutput("plot3", height=1240, width = 1240),
                                       textOutput("maficomm")))
									   ),
          tabPanel("U-mafic diagram", id = "U-mafic diagram", icon = icon("fas fa-chart-line"),
                           sidebarLayout(
                             sidebarPanel(width = 1,
                                          tags$a(strong("UMafic")),
                                          br(),
                                          helpText("Select the download format"),
                                          radioButtons("color4", "Color of the points", choices = c("green", "red", "blue"), selected = "green"),
                                          br(),
                                          radioButtons("format4", "Download file format", choices = c( "png", "pdf"), selected = "png"),
                                          helpText("Click on the download button to download the Ternary diagram"),
                                          downloadButton("downloadUmafic", "Download")
                             ),
                             mainPanel(width = 11,
                                     plotOutput("plot4", height=620, width = 620),
                                       textOutput("Umafic")))
									   ),
  				  tabPanel("About", id = "About", icon=icon("clipboard"),
                           sidebarLayout( 
                           sidebarPanel(width = 2,
                             h2("Thanks for using:",
                                br(),
                                tags$a(href="https://github.com/TheRFrog/shinyNORRRM", img(src = "logos/logo.png", heigth=230, width=230)))),
                           mainPanel(
                             tags$a(href="https://www.gnu.org/licenses/gpl-3.0.en.html", 
                                    h4(strong("This software is distributed under the terms of the GNU General Public License Version 3 or later."))),
                             br(),
                             h5("Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files, 
                      to deal in the software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, 
                      and/or sell copies of the app, and to permit persons to whom the software is furnished to do so. You are welcome to redistribute it under 
                      certain conditions. shinyNORRRM is free software and comes with ABSOLUTELY NO WARRANTY."),
                             br(),
                             h6("shinyNORRRM is maintained by:", 
                                br(),
                                "Renee Gonzalez-Guzman",
                                br(),
                                "rguzman@geociencias.unam.mx",align = "right")
                              )
                              ))           
                  )

#### Server ####

  server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=50*1024^2)
  
  data <- reactive({
    database <- input$file
    if(is.null(database)){return(NULL)}
    read.table(file=database$datapath, sep=input$sep, row.names = NULL , header = TRUE, na.strings = "NA", stringsAsFactors = FALSE)
  })

  # This reactive output contains the dataset and display the dataset in table format###
  output$table <- renderTable({
    if(is.null(data())){return (NULL)}
    data()
  })

  dataAdj <- reactive({
    return(suppressWarnings(adjRock(data(), Type = as.character(input$var1), Fe.adjustment = as.character(input$var2), Cancrinite = as.logical(input$var3), Calcite = as.logical(input$var4))))
  })
  
  datanorm <- reactive({
    return(suppressWarnings(ultimateCIPW(data(), Type = as.character(input$var1), Fe.adjustment = as.character(input$var2), Cancrinite = as.logical(input$var3), Calcite = as.logical(input$var4))))
  })
  
  output$table_adjRock <- renderTable({
    if(input$run_button==0) return(NULL)
    if(is.null(data())){return (NULL)}
    return(dataAdj())
  })

  output$downloadMajor <- downloadHandler(
    filename = function() {
      paste('adjusted_data', '.csv', sep='')
    },
    content = function(file) {
      df <- dataAdj()
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$table_CIPW <- renderTable({
    if(input$run_button==0) return(NULL)
    if(is.null(data())){return (NULL)}
    return(datanorm())
  })
  
  output$CIPWcom <- renderText({
    if(input$run_button==0) return(NULL)
    if(is.null(data())){return (NULL)}
    CIPWcom <- "CIPW Norm (Based on Verma, 2002; 2003). Minerals: Zrn: Zircon, C: Corundum, Q: Quartz, En: Enstatite, Hm: Hematite, Ru: Rutile, An: Anorthite, Di: Diopside,
	Wo: Wollastonite, Dcs: Dicalcium silicate, Tn: Sphene, Pf: Perovskite, Ap: Apatite, Cc: Calcite, Fs: Hypersthene, Fa: Fayalite, Fo: Forsterite, Mt: Magnetite,
	Il: Ilmenite, Ab: Albite, Ne: Nepheline, Th: Thenardite, Nc: Sodium Carbonate, Ac: Acmite, Ns: Sodium metasilicate, Or: Orthoclase, Lc: Leucite, Kp: Kaliophilite, Ks: Potassium metasilicate,
	Cm: Chromite, NaCl: Halite, Fr: Fluorite, Pr: Pyrite. Salic = Q + Or + Ab + An, Femic = (Di-Mg) + (Di-Fe) + (Hy-Mg) + (Hy-Fe) + Fo + Fa + Mt + Il + Hm, C.I. (Crystallization Index) = An + 2.1570577(Di-Mg) + Fo + 0.7007616(Hy-Fe), 
	D.I. (Differentiation Index) = Qz + Or + Ab + Ne + Lc, Density: Theorical density (g/cm3), FSSI (Feldspathoid Silica-Saturation Index): Qz-[Lc+2*(Ne+Kp)]."
	#print(CIPWcom)
  })

  output$downloadCIPW <- downloadHandler(
    filename = function() {
      paste('norm_data', '.csv', sep='')
    },
    content = function(file) {
      df2 <- suppressWarnings(ultimateCIPW(data(), Type = as.character(input$var1), Fe.adjustment = as.character(input$var2), Cancrinite = as.logical(input$var3), Calcite = as.logical(input$var4)))
      write.csv(df2, file, row.names = FALSE)
    }
  )

  output$table_Indices <- renderTable({
    if(input$run_button==0) return(NULL)
    if(is.null(data())){return (NULL)}
    return(suppressWarnings(Indices(dataAdj(), Calcite = as.logical(input$var4))))
  })

  output$Indicescom <- renderText({
    if(input$run_button==0) return(NULL)
    if(is.null(data())){return (NULL)}
    Indicescom <- "S.I. (Solidification Index): 100 * MgO/(MgO + FeO + Fe2O3 + Na2O + K2O), A.R. (Alkalinity Ratio): Al2O3 + CaO + 2Na2O, CIA (Chemical Index of Alteration): molar(Al2O3/[Al2O3 + CaO* + Na2O + K2O] * 100), 
	CIW (Chemical Index of Weathering): molar[A1203/(Al203 + CaO+Na20)] * 100, Mg# molar (MgO/(MgO + FeO) x 100, Fe*: FeO + (0.9 * Fe2O3) / (FeO + (0.9 * Fe2O3 + MgO)), MALI (Modified Alkali Lime Index): Na2O + K2O-CaO, ACNK (Alumina-Saturation Index): Al/(Ca-1.67P + Na + K),
	A/NK (Alumina Index): Al/(Na + K), AI (Alkalinity Index): Al-(K-Na), tau: (Al2O3-Na2O)/TiO2, sigma: (K2O + Na2O**2)/(SiO2-43), Temp_SiO2 and Temp_MgO (C) after Duan et al., 2022 (https://doi.org/10.1007/s00710-021-00769-5)."
	#print(Indicescom)
  })

  output$downloadIndices <- downloadHandler(
    filename = function() {
      paste('indices_data', '.csv', sep='')
    },
    content = function(file) {
      df3 <- suppressWarnings(Indices(dataAdj(), Calcite = as.logical(input$var4)))
      write.csv(df3, file, row.names = FALSE)
    })

    shinyTAS<- function (data){
    TAS <- (dataAdj())
    irvine <- data.frame (x=c(39.2,40,43.2,45,48,50,53.7,55,60,65,77), y=c(0,0.4,2,2.8,4,4.75,6,6.4,8,8.8,10))
    lines1 <- data.frame(x=c(41,41,52.5), y=c(0,7,14))
    lines2 <- data.frame(x=c(45,45,61), y=c(0,5,13.5))
    lines3 <- data.frame(x=c(45,52,69), y=c(5,5,8))
    lines4 <- data.frame(x=c(45,49.4,52,52), y=c(9.4,7.3,5,0))
    lines5 <- data.frame(x=c(48.4,53,57,57), y=c(11.5,9.3,5.9,0))
    lines6 <- data.frame(x=c(52.5,57.6,63,63), y=c(14,11.7,7,0))
    lines7 <- data.frame(x=c(69,69,77.5), y=c(13,8,0))
    lines8 <- data.frame(x=c(41,45), y=c(3,3))
    lines9 <- data.frame(x=c(61,64), y=c(13.5,15))
    lines10 <- data.frame(x=c(52.5,49), y=c(14,15.5))

    par(mgp = c(2, 0.6, 0))
    xl <- expression(bold('SiO'[2]*' (% wt.)'))
    yl <- expression(bold('Na'[2]*'O'+'K'[2]*'O (% wt.)'))
    TAS.legend <- data.frame (a=c(74,67,60,54.5,48.5,43,64,64,57.5,52.5,48.86,57.5,53,49,45,43,42.5), b=c(11,4.5,4.0,3.5,3.0,2.0,12.8,10,9,7.5,6,15,11.5,9.3,7.5,5,12.5))
    TAS.names <- c("R","D","A","BA","B","PB","T","TD","TA","BTA","TB","P","TP","PT","Te","Ba","F")
    x <- as.numeric (TAS$`SiO2 adj`)
    y <- as.numeric (TAS$`Na2O adj`) + as.numeric (TAS$`K2O adj`)
    bgcolor <- input$color
    plot(x=x, y=y, main = "TAS diagram", xlab = xl, ylab =  yl, pch=21, lwd=2, cex= 2.0, bg=bgcolor, col=1, cex.lab=1.3, cex.axis=1.2, xlim=c(35, 80), ylim=c(-1, 16.5))
    lines(lines1, col="gray50",lwd=2)
    lines(lines2, col="gray50",lwd=2)
    lines(lines3, col="gray50",lwd=2)
    lines(lines4, col="gray50",lwd=2)
    lines(lines5, col="gray50",lwd=2)
    lines(lines6, col="gray50",lwd=2)
    lines(lines7, col="gray50",lwd=2)
    lines(lines8, col="gray50",lwd=2)
    lines(lines9, col="gray50",lwd=2)
    lines(lines10, col="gray50",lwd=2)
    lines(irvine, col="darkred",lwd=2)
	text(x=TAS.legend$a, y=TAS.legend$b, label=TAS.names, col="gray50", cex=1.2)
    }

  # render the plot so could be used to display the plot in the mainPanel
  output$plot <- renderPlot({
    if(input$run_button==0) return(NULL)
    if(is.null(data())){return (NULL)}
    TASplot <- shinyTAS (data())
    print(TASplot)
  })

  output$TAScom <- renderText({
    if(input$run_button==0) return(NULL)
    if(is.null(data())){return (NULL)}
    TAScomment <- "Chemical classification of volcanic rocks using TAS (Total Alkali vs. Silica) diagram (Le Bas et al., 1986; Le Maitre et al., 2002).
                                       The red line dividing alkalic and subalkalic series is after Irvine and Baragar (1971). To decide on the shared fields Ba-Te and TD-T,
                                       must be used the normative mineralogy: Ba should contain [Ol] > 10%, Te should contain [Ol] < 10%, TD should contain [Q] > 20%, T should contain [Q] < 20%.
                                       Field names are as follows: B: Basalt, BA: Basalticandesite, A: Andesite, D: Dacite, R: Rhyolite, PB: Picrobasalt, TB: Trachybasalt, BTA: Basaltictrachyandesite,
                                       TA: Trachy- andesite, TD: Trachydacite, T: Trachyte, Ba: Basanite, Te: Tephrite, PT: Phonotephrite, TP: Tephriphonolite, P: Phonolite, F: Foidite."
    #print(TAScomment)
  })


  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$downloadTAS <- downloadHandler(
    filename =  function() {
      paste("TASplot", input$format, sep=".")
    },

    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$format == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device

      TAS <- shinyTAS (data())
      print(TAS)

      dev.off()  # turn the device off

    })

 shinyTernary<- function (data){
  dataA <- (dataAdj())
  dataN <- (datanorm())
  loginput<- ifelse (as.numeric(dataA[ , 2]) >52, TRUE, FALSE)
  norm<- subset(dataN, loginput == TRUE)
  
  line1 <- list(
  A = c(5, 80, 15),
  B = c(25, 37.5, 75)
  )

  line1b <- list(
  A = c(25, 37.5, 75),
  B = c(27, 0, 73)
  )  

  line2 <- list(
  A = c(0, 30, 70),
  B = c(25, 37.5, 75)
  )

  line3 <- list(
  A = c(40, 50, 10),
  B = c(12.5, 50, 37.5)
  )

  line4 <- list(
  A = c(50, 35, 15),
  B = c(16, 35, 49)
  )
  
  line5 <- list(
  A = c(60, 20, 20),
  B = c(21, 20, 59)
  )

  textT <- list(
  A = c(0.08, 0.54, 0.452),
  B = c(0.08, 0.15, 0.77),
  C = c(0.30, 0.425, 0.275),
  D = c(0.40, 0.225, 0.205),
  E = c(0.60, 0.10, 0.30)
  )

    inputF <- data.frame(Or = as.numeric(norm$`Or`),
                     An = as.numeric(norm$`An`),
					 Ab = as.numeric(norm$`Ab`))

    inputF[is.na(inputF)] <- 0.000 ##Change values NA->0  
    inputF <- inputF[rowSums(inputF[])>0,] ##Remove rows
    inputF[inputF == 0] <- 0.000001

#First ternary plot 
TernaryPlot(atip = "An",
            btip = "Or",
            ctip = "Ab",
			grid.lines = 5, grid.lty = "dotted",
			grid.minor.lines = 1, grid.minor.lty = "dotted")

    bgcolor2 <- input$color2
    # Add a title
    title("Ternary feldspars diagram", cex.main = 1.0)

    # Add data points
    AddToTernary(points, inputF[, c("An", "Or", "Ab")], pch=21, lwd=2, cex= 2.0, bg=bgcolor2, col=1, cex.lab=1.3, cex.axis=1.2)

    AddToTernary(lines, line1, col = "gray50",lwd=2)
	AddToTernary(lines, line1b, col = "gray50",lwd=2)
    AddToTernary(lines, line2, col = "gray50",lwd=2)
    AddToTernary(lines, line3, col = "gray50",lwd=2)
    AddToTernary(lines, line4, col = "gray50",lwd=2)
    AddToTernary(lines, line5, col = "gray50",lwd=2, lty= 2)
    TernaryText(textT, c("Gr", "Trondh","Mon", "Grdr", "Ton"), col="gray50", cex=1.2)

  }

# render the plot so could be used to display the plot in the mainPanel
  output$plot2 <- renderPlot({
    if(input$run_button==0) return(NULL)
    if(is.null(data())){return (NULL)}
    Ternary1 <- shinyTernary (data())
    print(Ternary1)
  })
  
  output$Feldsparcom <- renderText({
    if(input$run_button==0) return(NULL)
    if(is.null(data())){return (NULL)}
    Feldsparcomment <- "Normative Ab-An-Or ternary plot and classification of plutonic rocks. Only intermediate and hypersilicic samples are plotted
	                    Field names are as follows: Ton: Tonalite, Grdr: Granodiorite, Mon: Quartz Monzonite, Trondh: Trondhjemite, Gr: Granite."
	#print(Feldsparcomment)
  })

  
  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$downloadTernaries <- downloadHandler(
    filename =  function() {
      paste("Ternary", input$format2, sep=".")
    },

    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$format2 == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device

      Ternaries <-  shinyTernary (data())
      print(Ternaries)

      dev.off()  # turn the device off

    })
	
  shinyTernary2<- function (data){
  dataA <- (dataAdj())
  dataN <- (datanorm())
  par(mfrow = c(2, 1))
  loginput2<- ifelse (as.numeric(dataA[ , 2]) >45 & as.numeric(dataA[ , 2]) <52, TRUE, FALSE)
  norm2<- subset(dataN, loginput2 == TRUE)


######################

 line1 <- list(
  A = c(90, 10, 0),
  B = c(90, 0, 10)
  )
 
  line2 <- list(
  A = c(70, 30, 0),
  B = c(70, 0, 30)
  )
  
  line3 <- list(
  A = c(40, 60, 0),
  B = c(40, 0, 60)
  )

  line4 <- list(
  A = c(10, 90, 0),
  B = c(10, 0, 90)
  )

  line5 <- list(
  A = c(0, 10, 90),
  B = c(10, 0, 90)
  )

  line6 <- list(
  A = c(10, 90, 0),
  B = c(0, 90, 10)
  )

  line7 <- list(
  A = c(10, 5, 90),
  B = c(90, 5, 5)
  )

  line8 <- list(
  A = c(90, 5, 5),
  B = c(10, 95, 5)
  )

  line9 <- list(
  A = c(90, 5, 5),
  B = c(10, 50, 50)
  )
  
  line10 <- list(
  A = c(90, 5, 5),
  B = c(5, 90, 5)
  )
  
  line11 <- list(
  A = c(5, 90, 5),
  B = c(5, 5, 90)
  )
  
  line12 <- list(
  A = c(5, 5, 90),
  B = c(90, 5, 5)
  )

##Another database


  
inputB <- data.frame(Ab  = as.numeric(norm2$`Ab`),
                     An  = as.numeric(norm2$`An`),
                     Fo = as.numeric(norm2$`Fo`),
                     Fa = as.numeric(norm2$`Fa`),
                     Di = as.numeric(norm2$`Di`),
                     Wo = as.numeric(norm2$`Wo`),
                     En = as.numeric(norm2$`En`),
                     Fs = as.numeric(norm2$`Fs`))
				  
inputB[is.na(inputB)] <- 0.00000 ##Change values NA->0

inputP <- data.frame (Pl = (inputB$Ab + inputB$An),
                      Ol = (inputB$Fo + inputB$Fa),
					  Px = (inputB$Di + inputB$Wo + inputB$En + inputB$Fs),
					  Opx = (inputB$En + inputB$Fs),
					  Cpx = (inputB$Di + inputB$Wo)
					  )

inputP[inputP == 0] <- 0.000001

#Second Ternary plot
TernaryPlot(atip = "Pl",
            btip = "Ol",
            ctip = "Px",
			grid.lines = 5, grid.lty = "dotted",
			grid.minor.lines = 1, grid.minor.lty = "dotted")
			
        bgcolor3 <- input$color3
    # Add a title
    title("(a) Pl-Ol-Px diagram", cex.main = 1.0)

# Add data points
AddToTernary(points, inputP[, c("Pl", "Ol", "Px")], pch=21, lwd=2, cex= 2.0, bg=bgcolor3, col=1, cex.lab=1.3, cex.axis=1.2)
			
AddToTernary(lines, line1, col = "gray50",lwd=2)
AddToTernary(lines, line2, col = "gray50",lwd=2)
AddToTernary(lines, line3, col = "gray50",lwd=2)
AddToTernary(lines, line4, col = "gray50",lwd=2)
AddToTernary(lines, line5, col = "gray50",lwd=2)
AddToTernary(lines, line6, col = "gray50",lwd=2)
AddToTernary(lines, line7, col = "gray50",lwd=2)
AddToTernary(lines, line8, col = "gray50",lwd=2)

textT <- list(
  A = c(0.95, 0.025, 0.025),
  B = c(0.80, 0.025, 0.175),
  C = c(0.80, 0.1, 0.1),
  D = c(0.80, 0.175, 0.025),
  E = c(0.55, 0.025, 0.425),
  F = c(0.55, 0.225, 0.225),
  G = c(0.55, 0.425, 0.025),
  H = c(0.25, 0.025, 0.725),
  I = c(0.25, 0.375, 0.375),
  J = c(0.25, 0.725, 0.025),
  K = c(0.025, 0.025, 0.95),
  L = c(0.025, 0.375, 0.375),
  M = c(0.025, 0.725, 0.025)
  )

TernaryText(textT, c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13"), col="gray50", cex=1.2)

###########################################################			

#Third Ternary plot
TernaryPlot(atip = "Pl",
            btip = "Cpx",
            ctip = "Opx",
			grid.lines = 5, grid.lty = "dotted",
			grid.minor.lines = 1, grid.minor.lty = "dotted")
			
# Add a title
title("(b) Pl-Cpx-Opx diagram", cex.main = 1.0)

# Add data points
AddToTernary(points, inputP[, c("Pl", "Cpx", "Opx")], pch=21, lwd=2, cex= 2.0, bg=bgcolor3, col=1, cex.lab=1.3, cex.axis=1.2)
			
AddToTernary(lines, line1, col = "gray50",lwd=2)
AddToTernary(lines, line2, col = "gray50",lwd=2)
AddToTernary(lines, line3, col = "gray50",lwd=2)
AddToTernary(lines, line4, col = "gray50",lwd=2)
AddToTernary(lines, line5, col = "gray50",lwd=2)
AddToTernary(lines, line6, col = "gray50",lwd=2)
AddToTernary(lines, line7, col = "gray50",lwd=2)
AddToTernary(lines, line8, col = "gray50",lwd=2)
AddToTernary(lines, line9, col = "gray50",lwd=2)


textT <- list(
  A = c(0.95, 0.025, 0.025),
  B = c(0.80, 0.025, 0.175),
  C = c(0.75, 0.075, 0.175),
  D = c(0.75, 0.175, 0.075),
  E = c(0.80, 0.175, 0.025),
  F = c(0.55, 0.025, 0.425),
  G = c(0.50, 0.15, 0.35),
  H = c(0.50, 0.35, 0.15),
  I = c(0.55, 0.425, 0.025),
  J = c(0.25, 0.025, 0.725),
  K = c(0.20, 0.20, 0.60),
  L = c(0.20, 0.60, 0.20),
  M = c(0.25, 0.725, 0.025),
  N = c(0.025, 0.025, 0.95),
  O = c(0.025, 0.375, 0.375),
  P = c(0.025, 0.725, 0.025)
  )

TernaryText(textT, c("b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "b13", "b14", "b15", "b16"), col="gray50", cex=1.2)			
  }

# render the plot so could be used to display the plot in the mainPanel
  output$plot3 <- renderPlot({
    if(input$run_button==0) return(NULL)
    if(is.null(data())){return (NULL)}
    Ternary2 <- shinyTernary2 (data())
    print(Ternary2)
  })
 
  output$maficomm <- renderText({
    if(input$run_button==0) return(NULL)
    if(is.null(data())){return (NULL)}
    maficomm <- "Normative Pl-Ol-Px and Pl-Opx-Cpx ternaries diagrams. Only mafic samples are plotted. 
	Field names are as follows: a1: Anorthosite, a2: Leucogabbro/Leuconorite/Leucogabbronorite, a3: Olivine Leucogabbro/Leuconorite/Leucogabbronorite, a4: Leucotroctolite, 
	                            a5: Gabbro/Norite/Gabbronorite, a6: Olivine Gabbro/Norite/Gabbronorite, a7: Troctolite, a8: Melanogabbro/Melanonorite/Melanogabbronorite,
								a9: Olivine Melanogabbro/Melanonorite/Melanogabbronorite, a10: Melanotroctolite, a11: Pyroxenite, a12: Plagioclase-bearing Ultramafic Rocks, a13: Dunite. 
                                b1: Anorthosite, b2: Leuconorite, b3: Clinopyroxene Leuconorite, b4: Orthopyroxene Leucogabbro, b5: Leucogabbro, b6: Norite, b7: Clinopyroxene Norite,
								b8: Orthopyroxene Gabbro, b9: Gabbro, b10: Melanonorite, b11: Clinopyroxene Melanonorite, b12: Orthopyroxene Melanogabbro, b13: Melanogabbro, b14: Orthopyroxenite,
								b15: Plagioclase-bearing Ultramafic Rocks, b16: Clinopyroxenite.
	"
   #print(maficomm)
  })
   
  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$downloadMafic <- downloadHandler(
    filename =  function() {
      paste("Mafic", input$format3, sep=".")
    },

    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$format3 == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device

      Ternaries2 <-  shinyTernary2 (data())
      print(Ternaries2)

      dev.off()  # turn the device off

    })

shinyTernary3<- function (data){
  dataA <- (dataAdj())
  dataN <- (datanorm())
  
  
  ##last database

  loginput3<- ifelse (as.numeric(dataA[ , 2]) <45, TRUE, FALSE)
  norm3<- subset(dataN, loginput3 == TRUE)
  
inputU <- data.frame(Ab  = as.numeric(norm3$`Ab`),
                     An  = as.numeric(norm3$`An`),
                     Fo = as.numeric(norm3$`Fo`),
                     Fa = as.numeric(norm3$`Fa`),
                     Di = as.numeric(norm3$`Di`),
                     Wo = as.numeric(norm3$`Wo`),
                     En = as.numeric(norm3$`En`),
                     Fs = as.numeric(norm3$`Fs`))
				  
inputU[is.na(inputU)] <- 0.00000 ##Change values NA->0

inputUm <- data.frame (Pl = (inputU$Ab + inputU$An),
                      Ol = (inputU$Fo + inputU$Fa),
					  Px = (inputU$Di + inputU$Wo + inputU$En + inputU$Fs),
					  Opx = (inputU$En + inputU$Fs),
					  Cpx = (inputU$Di + inputU$Wo)
					  )

inputUm[inputUm == 0] <- 0.000001	
#inputUm[nrow(inputUm) + 1,] <- c(0, 0, 0, 0, 0)
		
######################

 line1 <- list(
  A = c(90, 10, 0),
  B = c(90, 0, 10)
  )
  
  line3 <- list(
  A = c(40, 60, 0),
  B = c(40, 0, 60)
  )

  line5 <- list(
  A = c(0, 10, 90),
  B = c(10, 0, 90)
  )

  line6 <- list(
  A = c(10, 90, 0),
  B = c(0, 90, 10)
  )

  line10 <- list(
  A = c(90, 5, 5),
  B = c(5, 90, 5)
  )
  
  line11 <- list(
  A = c(5, 90, 5),
  B = c(5, 5, 90)
  )
  
  line12 <- list(
  A = c(5, 5, 90),
  B = c(90, 5, 5)
  )


#Fourth Ternary plot
TernaryPlot(atip = "Ol",
            btip = "Cpx",
            ctip = "Opx",
			grid.lines = 5, grid.lty = "dotted",
			grid.minor.lines = 1, grid.minor.lty = "dotted")

bgcolor4 <- input$color4
			
# Add a title
title("Ol-Cpx-Opx diagram", cex.main = 1.0)	

# Add data points

AddToTernary(points, inputUm[, c("Ol", "Cpx", "Opx")], pch=21, lwd=2, cex= 2.0, bg=bgcolor4, col=1, cex.lab=1.3, cex.axis=1.2)			


AddToTernary(lines, line1, col = "gray50",lwd=2)
AddToTernary(lines, line3, col = "gray50",lwd=2)
AddToTernary(lines, line5, col = "gray50",lwd=2)
AddToTernary(lines, line6, col = "gray50",lwd=2)
AddToTernary(lines, line10, col = "gray50",lwd=2)
AddToTernary(lines, line11, col = "gray50",lwd=2)			
AddToTernary(lines, line12, col = "gray50",lwd=2)

textT <- list(
  A = c(0.95, 0.025, 0.025),
  B = c(0.70, 0.025, 0.275),
  C = c(0.60, 0.20, 0.20),
  D = c(0.70, 0.275, 0.025),
  G = c(0.25, 0.025, 0.725),
  H = c(0.20, 0.40, 0.40),
  I = c(0.25, 0.725, 0.025),
  J = c(0.025, 0.025, 0.95),
  K = c(0.025, 0.375, 0.375),
  L = c(0.025, 0.725, 0.025)
  )

TernaryText(textT, c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10"), col="gray50", cex=1.2)			
			
  }

# render the plot so could be used to display the plot in the mainPanel
  output$plot4 <- renderPlot({
    if(input$run_button==0) return(NULL)
    if(is.null(data())){return (NULL)}
    Ternary3 <- shinyTernary3 (data())
    print(Ternary3)
  })
  
  output$Umafic <- renderText({
    if(input$run_button==0) return(NULL)
    if(is.null(data())){return (NULL)}
    Umafic <- "Normative compositions plotted in Ol-Opx-Cpx classification diagram. Only ultramafic samples are plotted. Field names are as follows: 
	c1: Dunite, c2: Hazburguite, c3: Lherzolite, c4: Wherlite, c5: Olivine Orthopyroxenite, c6: Olivine Websterite, c7: Olivine Clinopyroxene, c8: Orthopyroxenite, c9: Websterite, c10: Clinopyroxenite."
	#print(Umafic)
  })

  
  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$downloadUmafic <- downloadHandler(
    filename =  function() {
      paste("UMafic", input$format4, sep=".")
    },

    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$format4 == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      Ternaries3 <-  shinyTernary3 (data())
      print(Ternaries3)

      dev.off()  # turn the device off

    })
	
  	  
   observeEvent(input$run_button, {
    for (i in 1:100) {
      updateProgressBar(
        #session = session,
        id = "pb",
        value = i, total = 100,
        title = paste("Process", trunc(i/25))
      )
      Sys.sleep(0.025)
    }
  })

}

  #### Run Shiny NORRRM ####

  shinyNORRRM <- shinyApp(ui = ui, server = server)
  on_ex_browser <- options()$browser
  on.exit(options(browser = on_ex_browser))
   if (!is.null(browser)) {options(browser = browser)}
  shiny::runApp(shinyNORRRM, host = host, port = port)
}