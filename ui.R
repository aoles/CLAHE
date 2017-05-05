library(shiny)

shinyUI(fluidPage(
  tags$head(tags$script(src = "imgslider.min.js"),
            tags$link(rel = "stylesheet", type = "text/css", href = "imgslider.min.css")
  ),
  
  # Application title
  titlePanel("Contrast Limited Adaptive Histogram Equalization"),
  
  # Sidebar with inputs corresponding to function arguments
  sidebarLayout(
    sidebarPanel(
      uiOutput('selectUI'),
      
      fileInput("file", "Image file", accept=paste("image", c("jpeg", "png", "tiff"), sep="/")),
      
      uiOutput('slider'),
      
      sliderInput(inputId = "limit", label = "limit", min = 1, max = 10, step = .5, value = 2),
      
      checkboxInput("keep.range", "keep.range"),
      
      checkboxInput("luminance", "luminance"),
      
      downloadButton("download", label = "Download")
    ),
    
    mainPanel(
      div(class="slider",
          div(class="slider responsive",
              div(id = "org", class="shiny-image-output left image", style="width: 100%;"),
              div(id = "img", class="shiny-image-output right image", style="width: 100%;")
          )
      )
    )
  ),
  
  tags$script(type="text/javascript", "$('.slider').slider();")
))
