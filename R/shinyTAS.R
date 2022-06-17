#' shinyNORRRM - Launches the shinyNORRRM app
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

    shinyTAS<- function (data){
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
    x <- as.numeric (data$`SiO2 adj`)
    y <- as.numeric (data$`Na2O adj`) + as.numeric (data$`K2O adj`)
    bgcolor <- input$color
    plot(x=x, y=y, main = "TAS plot", xlab = xl, ylab =  yl, pch=21, lwd=2, cex= 1.6, bg=bgcolor, col=1, cex.lab=1.3, cex.axis=1.2, xlim=c(35, 80), ylim=c(-1, 16.5))
    lines(irvine, col="darkred",lwd=2)
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
    text(x=TAS.legend$a, y=TAS.legend$b, label=TAS.names, col="gray50", cex=1.2)
    print (TAS)
  }