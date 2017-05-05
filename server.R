library(shiny)
library(EBImage)

options(EBImage.display = "raster")

source("fun.R")


shinyServer( function(input, output, session) {
  lab <- reactiveVal()
  
  output$selectUI <- renderUI({
    values <- divs()
    sel_values <- paste(paste0('"', values, '"'), collapse = ',')
    list(
      (HTML(
        sprintf('<script type="text/javascript">
                  $(document).ready(function() {
                  var vals = [%s];
                  $(\'#nx\').data(\'ionRangeSlider\').update(
                  {values:vals,
                   min: 0,
                   max: %s,
                   from:%s})
                  })
                 </script>', 
                sel_values, length(values) - 1L, which(values==8L) - 1L)))
    )
  })
  
  img <- reactive({
    cat("IMG\n")
    # reset precomputed LAB colorspace
    lab(NULL)
    
    # load the file
    if ( is.null(input$file) )
      readImage(system.file("images", "sample-color.png", package="EBImage"))
    else
      readImage(input$file$datapath, type = substr(input$file$type, 7, 10))
  })
  
  divs = reactive({
    cat("DIV\n")
    d = req(dim(img()))
    vals <- intersect(
      which(d[1L] %% seq_len(d[1L]) == 0L),
      which(d[2L] %% seq_len(d[2L]) == 0L)
    )[-1L]
    vals[vals<=16L]
  })
  
  output$slider <- renderUI({
    values = req(divs())
    sliderInput(inputId = "nx", label = "nx",
                min = 0, max = length(values) - 1L,
                step = 1, ticks = FALSE,
                value = which(values==8L) - 1L)
  })
  
  observe({
    if ( input$luminance && is.null(lab()) ) {
      cat("LAB\n")
      rgb = apply(img(), 3L, as.vector)
      lab(rgb2lab(rgb))
    }
  })
  
  luminance <- eventReactive(lab(), {
    cat("LUM\n")
    L = req(lab())[,1L]
    dim(L) = dim(img())[1:2]
    L
  })
  
  res <- reactive({
    req(input$nx)
    cat("res\n")
    nx = divs()[input$nx+1L]
    limit = input$limit
    keep.range = input$keep.range
    
    if (input$luminance) {
      cL = clahe(luminance(), nx=nx, ny=nx, limit=limit, keep.range=keep.range)
      res = lab()
      res[,1L] = as.vector(cL)
      Image(lab2rgb(res), dim(img()), colorMode(img()))
    }
    else {
      clahe(img(), nx=nx, ny=nx, limit=limit, keep.range=keep.range)
    }
  })
  
  output$img <- 
    renderImage({
      req(res())
      cat("r\n")
      outfile <- tempfile(fileext='.png')
      
      width  <- session$clientData$output_img_width
      
      writeImage(res(), outfile)
      
      list(src = outfile,
           width = width,
           alt = "Filtered image")
    }, deleteFile = TRUE)
  
  output$org <- renderImage({
    req(img())
    cat("o\n")
    outfile <- tempfile(fileext='.png')
    
    width  <- session$clientData$output_img_width
    
    writeImage(img(), outfile)
    
    list(src = outfile,
         width = width,
         alt = "Original image")
  }, deleteFile = TRUE)
  
  output$download <- downloadHandler(
    filename = function() {
      paste("filtered", input$file$name, sep="-")
    },
    content = function(file) {
      writeImage(res(), file)
    },
    contentType = input$file$type
  )
  
})
