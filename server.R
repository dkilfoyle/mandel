library(shiny)
library(stringr)

shinyServer(function(input, output, session) {
  
  getWidth = reactive({
    strtoi(str_sub(input$res, 1, (str_locate(input$res, "x")[1] - 1)))
  })
  
  getHeight = reactive({
    strtoi(str_sub(input$res, str_locate(input$res, "x")[1]+1, -1L))
  })
  
  observe({
    input$zoom
    updateSliderInput(session, "xr", value=c(-2,2))
    updateSliderInput(session, "yr", value=c(-1.5,1.5))
  })
  
  output$mandelPlot <- renderPlot({
    
    progress = shiny::Progress$new(session, min=1, max=input$n)
    on.exit(progress$close())
    progress$set(message="Iterating")
    
    # mandelbrot_naive.R
    # "Naive" implementation of Mandelbrot Set in R
    # Myles Harrison
    # http://www.everydayanalytics.ca
    
    # parameters
    cols <- colorRampPalette(c("blue","yellow","red","black"))(11)
    #     xmin = -2
    #     xmax = 2
    #     nx = 1000
    #     ymin = -1.5
    #     ymax = 1.5
    #     ny = 1000
    #     n=200
    
    # variables
    x <- seq(input$xr[1], input$xr[2], length.out=getWidth())
    y <- seq(input$yr[1], input$yr[2], length.out=getHeight())
    
    # c will be 2d array of values c=x+iy
    c <- outer(x,y*1i,FUN="+")
    
    # initialize z to 0
    z <- matrix(0.0, nrow=length(x), ncol=length(y))
    k <- matrix(0.0, nrow=length(x), ncol=length(y))
    
    # for each interation to max 200
    for (rep in 1:input$n) { 
      
      progress$set(rep)
      
      # which coords still have length(z) < 2
      # NB: Mod(imagnum) = sqrt(x^2 + y^2)
      index <- which(Mod(z) < 2)
      
      # if lengh(z) < 2 calculate new z = z^2 + c (where c=x+iy)
      z[index] <- z[index]^2 + c[index]
      k[index] <- k[index] + 1
    }
    
    # k will now be full of number of iterations until Z > 2

    par(mar=c(0,0,0,0))
    image(x, y, k, col=cols, axes=F)
    
  }, width=getWidth, height=getHeight)

})
