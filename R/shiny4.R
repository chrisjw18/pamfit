#' @title shiny app to view the results
#' @description shiny app to view the results
#' @return It does not return any object, it for visualization only
#' @keywords external
#' @examples
#'
#'
#' @export
shiny4 <- function(my.data){

  #(re) define projections that will be used in the polgon fitting within the app

  leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137
  +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"
  #define polygon projection
  poly.proj <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  my.data <- my.data

  #determine classes of different objects in input list (to pull out all bricks)
  my.classes <- c()
  for(i in 1:length(my.data)){
    my.classes[i] <- class(my.data[[i]])
  }
  my.bricks <- which(my.classes == 'RasterStack' | my.classes == 'RasterBrick')
  brick.dat <- my.data[my.bricks]
  brick.names <- names(brick.dat)

  #extract info for settings tab

  #model settings info


  #get important settings to show in a table in UI
  settings.names <-

app4 <- shiny::shinyApp(

ui  = shiny::fluidPage(
  shiny::titlePanel(title = 'View data and settings'),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput('selectBrick', 'Choose raster brick', choices = brick.names),# objects from input that are raster bricks
      shiny::htmlOutput('selectLayer'),

      shiny::hr(),
      shiny::hr(),
      shiny::actionButton('closeWindow', 'Close Window')
        ),#end of sidear

    shiny::mainPanel(
      shiny::tabsetPanel(id = 'mainPanel',
                         shiny::tabPanel('Image',
                               leaflet::leafletOutput('rasterImage')
                               ),
                         shiny::tabPanel('Settings',
                                         shiny::fluidRow(
                                         shiny::column(6,
                                         shiny:: htmlOutput('input.settings'))
                              ) )
                      )
        )#end of main panel
      )#sidebarLayout

    ),#end of ui

server = function(input, output, session){

  app.data <<- shiny::reactiveValues(brick = NULL)

  shiny::observeEvent(input$selectBrick, {
    app.data$brick <- input$selectBrick
  })

  shiny::observeEvent(input$selectLayer,{
    app.data$layer <- input$selectLayer
  })

  #render layer chooser
  output$selectLayer <- shiny::renderUI({
    layer.names <- names(my.data[[app.data$brick]])
    shiny::selectInput('selectLayer', 'Select brick layer to view', choices = layer.names)
  })

  #render raster map
  output$rasterImage <- leaflet::renderLeaflet({
    my.rast <- raster::raster(my.data[[app.data$brick]], app.data$layer)
    pal <- leaflet::colorNumeric(palette = 'magma',
                 domain = c(min(values(my.rast), na.rm = T), max(values(my.rast), na.rm = T)),
                 na.color = 'transparent')
     leaflet::leaflet() %>%
       leaflet::addRasterImage(my.rast, project = T, colors = pal) %>%
       leaflet::addLegend(pal = pal,
                 values = values(my.rast)#,
                 #labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
                 )
   })

  #render input settings info
  output$input.settings <- shiny::renderUI({
    paste0(
      shiny::HTML('<br>Input PAR levels</br>'),
    my.data$par.levels,
    shiny::HTML('<b>Degree of smoothing applied</b>'),
    my.data$smooth,
    shiny::HTML('<b>Absorbance image used in ETR calculation</b>'),
    my.data$abs
    )
  })

  shiny::observeEvent(input$closeWindow, {
    stopApp()
  })
  session$onSessionEnded(function(){
    shiny::stopApp
  })

}#end of server
)#end of app4

shiny::runApp(app4)

} #end of shiny4

