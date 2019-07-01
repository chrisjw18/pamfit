#' @title shiny app to input a file and read in light levels
#' @description shiny app to input a file and read in light levels (COMPLETE)
#' @return Returns an object (Raster brick?) with all the images from the imaging PAM file (ADD DETAILS)
#' @keywords external
#' @importFrom magrittr %>%
#' @examples
#'
#'
#' @export
#shiny app to input a file and read in light levels

shiny1 <- function(){

  #library(shiny)
  #library(tiff)
  #library(raster)
  #couldn't solve the problem of CRS needing the library loaded (BJ,27/06/19)
  library(sp)
  #library(DT)
#library(magrittr)
#  `%>%` <- magrittr::`%>%`

  #this increases uplaod file size to 30MB
  options(shiny.maxRequestSize = 30*1024^2)

  #projection to apply to uploaded data for later leaflet raster plotting
  leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"

  #initiate shiny app
  app <- shiny::shinyApp(

    #define user interface
    ui <- shiny::fluidPage(

      shiny::titlePanel(title = 'Data and light levels input'),
      shiny::hr(),
      shiny::fluidRow(shiny::column(6,
                             shiny::fileInput('fileIn', '1. Upload your TIFF file',
                                multiple = F, accept = c(".tif",".tiff")),
                             shiny::hr(),
                             shiny::HTML('<b>2. Input light step PAR levels</b>'),
                             shiny::br(),
                             DT::dataTableOutput('lightSteps')
      ),
      shiny::column(6,
             shiny::plotOutput('img1'),
             shiny::radioButtons('smooth', '3. Choose degree of pixel smoothing',
                          choices = c(0,3,5,7,9,11,13,15), selected = 0,inline = T),
             shiny::hr(),
             shiny::radioButtons('abs', '4. Identify whether data contains an absorbance image',
                          choices = c('Yes' = 'yes', 'No' = 'no'), inline = T, selected = 'no'),
             shiny::hr(),
             shiny::actionButton('confirm', 'Submit All', width='100%')
      )
      )

    ),

    server <- function(input, output, session){

      app.data <<- shiny::reactiveValues(data = NULL)

      shiny::observeEvent(input$fileIn, {
        #get file details - also makes this reactive on input$fileIn
        my.file <- input$fileIn

        #load the data
        dat <- tiff::readTIFF(my.file$datapath, all = T)
        app.data$data <- dat

        #load in as stack
        my.brick <- raster::stack()
        for(i in 1:length(dat)){
          my.brick <- raster::stack(my.brick, raster::raster(as.matrix(dat[[i]])))
        }

        #set extent based on image properties and set our desired CRS
        raster::extent(my.brick) <- c(0,ncol(my.brick), 0, nrow(my.brick))
        raster::projection(my.brick) <- sp::CRS(leafletProj)

        #push to app.data
        app.data$my.brick <- my.brick

      })


      output$img1 <- shiny::renderPlot({
        my.smooth <- input$smooth %>% as.numeric
        if(!is.null(app.data$my.brick)){
          my.brick <- app.data$my.brick
          my.image <- raster::raster(my.brick, 4)

          if(my.smooth > 0){
            my.mat <- matrix(1, nrow = my.smooth, ncol = my.smooth)
            my.image <- raster::focal(my.image, w = my.mat, fun = mean, pad = T, padValue = NA, na.rm = T)
          }

          #plot image to UI to show user what has been uploaded
          plot(my.image, main = 'Uploaded Data', legend = FALSE)
        }

        #output smooth level selected to app.data
        app.data$smooth <- my.smooth
      })

      #function to interatively add textInput with an ID and initial value to dataframe
      shinyInput = function(FUN, len, id, int.value, ...) {
        #validate(need(character(len)>0,message=paste("")))
        inputs = character(len)
        for (i in seq_len(len)) {
          inputs[i] = as.character(FUN(paste0(id, i), label = NULL,
                                       value = int.value[i], width = '100px',...))
        }
        inputs
      }

      #render output light levels data table
      output$lightSteps <- DT::renderDataTable({

        if(!is.null(app.data$my.brick)){

          my.brick <- app.data$my.brick

          #establish number of light levels to have
          no.light.steps <- (raster::nlayers(my.brick) - 4) / 2

          #check to see if light levels already loaded (object tmp.light in global environment)
          if(exists('tmp.light')){
            my.int.par <- tmp.light
            #check right number of light levels apparent
            if(length(my.int.par) != no.light.steps){
              my.int.par <- rep('NA', no.light.steps)
            }

          } else {
            my.int.par <- rep('NA', no.light.steps)
          }


          DT::datatable(
            data.frame('Light_Step' = 1:no.light.steps,
                       'PAR_level' = shinyInput(shiny::textInput,no.light.steps,"ls_", my.int.par)),
            selection="multiple",
            rownames = FALSE,
            escape = FALSE,
            #filter = list(position = 'top', clear = FALSE),
            #extensions = list("ColReorder" = NULL, "Buttons" = NULL),
            options = list(
              dom = 't',
              ColReorder = TRUE,
              #extensions = list("ColReorder" = NULL, "Buttons" = NULL),
              autoWidth=TRUE,
              lengthMenu = list(c(no.light.steps), c(as.character(no.light.steps))),
              rowCallback = DT::JS("function(r,d) {$(r).attr('height', '50px')}"),
              preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
              drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } '),
              columnDefs = list(list(targets=c(0), visible = T, width = '100px'))
            ))

        }#end of if is.NULL my.brick
      })

      #actions on confirm button selection
      shiny::observeEvent(input$confirm, {
        no.light.steps <- (raster::nlayers(app.data$my.brick) - 4) / 2
        my.par <- vector()
        for(i in 1:no.light.steps){
          my.in <- paste('ls_',i,sep='')
          my.par[i] <- input[[my.in]] %>% as.numeric
        }

        #send to app.data and send par.levels to s1.env to check on reload...
        app.data$par.levels <- my.par
        #writeout tmp.light to global environment?
        assign('tmp.light', my.par, envir = .GlobalEnv)
      })

      shiny::observe({
        input$abs
        app.data$abs <- input$abs
      })

      #close box on submit button
      shiny::observeEvent(input$confirm, {
        shiny::stopApp()
      })
      session$onSessionEnded(function(){
        shiny::stopApp
      })
      return(shiny::isolate(app.data))
    }
  )
  shiny::runApp(app)

  #pull out data from app.data reactive shiny list and input to returnable list
  all.data <- list()
  all.data$original.data <- shiny::isolate(app.data$my.brick)
  all.data$par.levels <- shiny::isolate(app.data$par.levels)
  all.data$smooth <- shiny::isolate(app.data$smooth)
  all.data$abs <- shiny::isolate(app.data$abs)
  #apply smoothing to original data set
  if(all.data$smooth > 0){
    my.mat <- matrix(1, nrow = all.data$smooth, ncol = all.data$smooth)
    for(i in 1:(raster::nlayers(all.data$original.data))){
      all.data$original.data[[i]] <- raster::focal(raster::raster(all.data$original.data,i), fun = mean, w = my.mat, pad = T, padValue = NA, na.rm = T)
    }
  }
  #want to apply names to each layer of the original data - need details on order from Bruno
  ls.nos <- 1:length(all.data$par.levels)
  names(all.data$original.data) <- c('Fo_initial', 'Fm_initial', 'NIR', 'Red',c(rbind(paste('F_', ls.nos, sep=''), paste('Fm_', ls.nos, sep=''))))
  return(all.data)

}#end of shiny1 function call
