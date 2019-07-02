library(shiny)
library(tiff)
library(raster)
#couldn't solve the problem of CRS needing the library loaded (BJ,27/06/19)
library(sp)
library(DT)
library(magrittr)
library(shinydashboard)
library(mapview)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(parallel)
library(snow)


#this increases uplaod file size to 30MB
options(shiny.maxRequestSize = 30*1024^2)

#projection to apply to uploaded data for later leaflet raster plotting
leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"

#define polygon projection
poly.proj <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#create environment
e2 <- new.env()


ui <- dashboardPage(
  
  dashboardHeader(title = "Bruno's PAM Machine"),
  
  dashboardSidebar(
    #dynamic sidebarMenu driven by folder heirachry
    sidebarMenu(id = 'sidebarmenu',
                menuItem("1. Upload Image", tabName = "uploadImage",  icon = icon("group", lib="font-awesome")),
                menuItem("2. Select ROI", tabName = "selectROI", icon = icon("check-circle", lib = "font-awesome")),
                menuItem("3. Select Fit Parameters", tabName = "selectFitParameters", icon = icon("check-circle", lib = "font-awesome")),
                menuItem("4. Summary Statistics", tabName = "summaryStatistics", icon = icon("check-circle", lib = "font-awesome")),
                conditionalPanel("input.sidebarmenu === 'summaryStatistics'",
                                 shiny::uiOutput('selectBrick'),
                                 shiny::htmlOutput('selectLayer')
                )
                
    )
  ),#end of dashboardSidebar
  
  dashboardBody(
    tabItems(
      tabItem(
        #this is our shiny 1 tab
        tabName = 'uploadImage',
        
        shiny::fluidPage(
          
          shiny::titlePanel(title = 'Input data and initial settings'),
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
                        shiny::HTML('<b>5. Confirm Settings to Proceed</b>'),
                        shiny::actionButton('confirm', 'Confirm', width='100%')
          )
          )
          
        )
      ),
      tabItem(tabName = 'selectROI',
              shiny::fluidRow(
                leaflet::leafletOutput("leafmap", height = '600px'),
                shiny::actionButton('confirm2', 'Confirm ROI', width = '100%')
              )
      ),
      tabItem(tabName = 'selectFitParameters',
              shiny::fluidPage(
                
                shiny::titlePanel(title = 'Examine rETR and select starting parameters'),
                shiny::hr(),
                
                #panel showing image to select pixel from to display ETR
                shiny::fluidRow(shiny::column(4,
                                              leaflet::leafletOutput('pixelSelector', width = '100%', height = '400px')
                ),#end of column
                shiny::column(3,
                              shiny::uiOutput('renderLayers'),
                              #shiny::selectInput('layers', 'Number of light steps to plot',
                               #                  choices = 4:raster::nlayers(app.data$etr), selected = raster::nlayers(app.data$etr)), #might cause a problem - needs to be rendered server side.
                              shiny::radioButtons('model', 'Select model fit',
                                                  choices = c('none','Jassby & Platt', 'Platt', 'Eilers & Peeters'),
                                                  selected = 'none'),
                              shiny::br(),
                              #output of etrmax, alpha as potential starting parameters
                              shiny::htmlOutput('currentParams'),
                              shiny::br()
                ),
                shiny::column(5,
                              plotly::plotlyOutput('etrPlot', width = '100%', height='400px')
                )#end of column
                ),#end of fluid row
              shiny::fluidRow(shiny::column(12,
                                            #confirm button
                                            shiny::actionButton('useParams', 'Use Current Model and Initial Parameters?', width = '100%')
                                            )
              )#end of 2nd fluid row
      )), #end of fluid page & tab items
      
      tabItem(tabName = 'summaryStatistics',
              shiny::fluidPage(
                shiny::titlePanel(title = 'View data and settings'),
                
                # shiny::sidebarLayout(
                #   shiny::sidebarPanel(
                #     shiny::selectInput('selectBrick', 'Choose raster brick', choices = brick.names),# objects from input that are raster bricks
                #     shiny::htmlOutput('selectLayer'),
                #     
                #     shiny::hr(),
                #     shiny::hr(),
                #     shiny::actionButton('closeWindow', 'Close Window')
                #   ),#end of sidear
                  
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
              )#fluidPage
    )#tabItems
    
  )#dashboardBody
  
)#end of UI


server <- function(input, output, session){
  
  app.data <<- shiny::reactiveValues(data = NULL)
  
  ####################### Shiny 1 code ##################################
  #file upload
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
  
  #render Image 1
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
      #make datatable for rendering
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
  
  #actions on confirm button selection (save light levels; save to external env; smooth image; smooth raster stack; label raster stack)
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
    
    #apply smoothing to original data set
    if(app.data$smooth > 0){
      my.mat <- matrix(1, nrow = app.data$smooth, ncol = app.data$smooth)
      for(i in 1:(raster::nlayers(app.data$my.brick))){
        app.data$my.brick[[i]] <- raster::focal(raster::raster(app.data$my.brick,i), fun = mean, w = my.mat, pad = T, padValue = NA, na.rm = T)
      }
    }
    #apply names to each layer of the original data
    ls.nos <- 1:length(app.data$par.levels)
    names(app.data$my.brick) <- c('Fo_initial', 'Fm_initial', 'NIR', 'Red',c(rbind(paste('F_', ls.nos, sep=''), paste('Fm_', ls.nos, sep=''))))
    
    #check to see if abs image incorporated in fileInput
    app.data$abs <- input$abs
    
    #check to ensure step 1 (file upload is compelete)
    app.data$step_1 <- 'complete'
  })
  
  
  ####################### Shiny 2 code ##################################
  #want this page to be rendered depedent on step1 (file upload) being complete
  
  #render leafmap plot of uploaded TIFF 
  output$leafmap <- leaflet::renderLeaflet({
    if(!is.null(app.data$step_1) && app.data$step_1 == 'complete'){
    my.rast <- raster::raster(app.data$my.brick,1)
    leaflet::leaflet() %>%
      leaflet::addRasterImage(my.rast, project = T) %>%
      leaflet.extras::addDrawToolbar( polylineOptions = F, circleOptions = F, rectangleOptions = T,
                                      markerOptions = F, circleMarkerOptions = F, singleFeature = T,
                                      polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = F), editOptions = leaflet.extras::editToolbarOptions()
      )
    } else {
      'Complete step 1'
    }
  })
  
  #capture the drawn polygon
  shiny::observeEvent(input$leafmap_draw_new_feature, {
    input$leafmap_draw_new_feature
    
    poly.coords <- input$leafmap_draw_new_feature$geometry$coordinates %>% unlist
    poly.mat <- data.frame(x = poly.coords[c(TRUE, FALSE)], y = poly.coords[c(FALSE, TRUE)])
    xy <- sp::SpatialPoints(poly.mat)
    proj4string(xy) <- poly.proj
    xy <- as.data.frame(spTransform(xy, leafletProj))
    final.poly <- mapview::coords2Polygons(as.matrix(xy), ID='chris')
    my.crop <- app.data$my.brick %>% raster::crop(raster::extent(final.poly)) %>% raster::mask(final.poly)
    
    app.data$my.crop <- my.crop
    app.data$poly.coords <-xy
    app.data$poly.draw <- final.poly
    
  })
  
  ###****The below needs to be sorted - currently not working
  # #capture coordinates of an edited polygon - delete if cant get to work and switch off edit options in UI
  # #taken from: https://stackanswers.net/questions/how-to-update-coordinates-after-dragging-a-marker-in-leaflet-shiny
  # observeEvent(input$leafmap_draw_edited_features, {
  #   
  #   input$leafmap_draw_edited_features
  #   
  #   edited <<- input$leafmap_draw_edited_features
  #   drawn <<- input$leafmap_draw_new_feature
  #   # find the edited features and update drawn
  #   # start by getting the leaflet ids to do the match
  #   ids <- unlist(lapply(drawn, function(x){x$properties$`_leaflet_id`}))
  #   # now modify drawn to match edited
  #   map(edited$features, function(x){
  #     loc <- match(x$properties$`_leaflet_id`, ids)
  #     drawn[loc] <<- list(x) })
  #   
  #   #this is a test
  #   poly.edit.coords <<- input$leafmap_draw_edited_features %>% unlist
  #   poly.mat2 <- data.frame(x = poly.edit.coords[c(TRUE, FALSE)], y = poly.edit.coords[c(FALSE, TRUE)])
  #   xyz <- SpatialPoints(poly.mat2)
  #   proj4string(xyz) <- poly.proj
  #   xyz <- as.data.frame(spTransform(xyz, leafletProj))
  #   final.poly2 <- coords2Polygons(as.matrix(xyz), ID='chris')
  #   app.data$poly.draw2 <- final.poly2
  #   
  # })
  
  #data handling on confirm ROI
  shiny::observeEvent(input$confirm2,{
    
    #process rasters to determine f, fm, yield and etr images
    this.dat <- app.data$my.crop[[-c(1:4)]]
    #num_samples <- this.dat %>% raster::nlayers
    num_samples <-  raster::nlayers(this.dat)
    
    #alternate sequnces to be used for subsetting Fo / Fm images, respectively
    my.seq <- 1:num_samples
    odd_sequence <- my.seq[c(TRUE, FALSE)]
    even_sequence <- my.seq[c(FALSE, TRUE)]
    
    #1 - extract F images
    f_img <- this.dat[[odd_sequence]]
    #2 - extract Fm images
    fm_img <- this.dat[[even_sequence]]
    #3 - calculate Yield images (this is a brick object)
    fv_fm_img <- (fm_img-f_img)/fm_img
    fv_fm_img[fv_fm_img < 0.01] <- NaN
    fv_fm_img[fv_fm_img > 0.85] <- NaN
    
    #4 - calculate ETR images
    light <- app.data$par.levels
    abs <- app.data$abs
    if(abs == 'yes'){
      nir <- raster::subset(all.data$cropped.data, 'NIR')
      red <- raster::subset(all.data$cropped.data, 'Red')
      abs.rast <- 1 - red / nir
    }
    
    etr.fun.1 <- function(x) { x * light * 0.5 }
    #etr.fun.2 <- function(x) {x * light * 0.5 * abs.rast}
    
    if(abs == "no"){
      etr <- raster::calc(fv_fm_img, etr.fun.1)
    } else {
      etr <- raster::calc(fvm_fm_img, etr.fun.1)
      etr <- etr * abs.rast
    }
    
    #push data to app.data reactive values list
    app.data$f_img <- f_img
    names(app.data$f_img) <- paste('F', 1:(raster::nlayers(app.data$f_img)), sep='_')
    app.data$fm_img <- fm_img
    names(app.data$fm_img) <- paste('Fm', 1:(raster::nlayers(app.data$fm_img)), sep='_')
    app.data$yield <- fv_fm_img
    names(app.data$yield) <- paste('Yield', 1:(raster::nlayers(app.data$yield)), sep='_')
    app.data$etr <- etr
    names(app.data$etr) <- paste('ETR', 1:(raster::nlayers(app.data$etr)), sep='_')
    
    #confirmation that step 3 complete
    app.data$step_2 <- 'complete'
  })
  
  
  ####################### Shiny 3 code ##################################
  
  #render selectInput layers
  output$renderLayers <- shiny::renderUI({
    if(!is.null(app.data$step_2) && app.data$step_2 == 'complete'){
    my.layers <- raster::nlayers(app.data$etr)
    shiny::selectInput('layers', 'Number of light steps to plot',
                       choices = 4:my.layers, selected = my.layers)
    }
  })
  
  shiny::observeEvent(input$pixelSelector_click, {
    app.data$click <- input$pixelSelector_click
  })
  
  shiny::observeEvent(input$model, {
    app.data$model <- input$model
  })
  
  shiny::observeEvent(input$layers,{
    app.data$layers <- input$layers
  })
  
  
  output$pixelSelector <- leaflet::renderLeaflet({
    if(!is.null(app.data$step_2) && app.data$step_2 == 'complete'){
    leaflet::leaflet() %>%
      leaflet::addRasterImage(raster::raster(app.data$yield, 1), project = T)
    }
  })
  
  #extract click coordinates and use to extract etr data from raster brick
  shiny::observeEvent(input$pixelSelector_click, {
    if(!is.null(app.data$step_2) && app.data$step_2 == 'complete'){
    #get coords from click object and project back to raster CRS
    my.x.coord <- app.data$click$lng
    my.y.coord <- app.data$click$lat
    coord.mat <- data.frame(x = my.x.coord, y = my.y.coord)
    xy <- sp::SpatialPoints(coord.mat)
    sp::proj4string(xy) <- poly.proj
    click.coord <- as.data.frame(sp::spTransform(xy, leafletProj))
    
    #make available
    app.data$click.coord <- click.coord
    
    #extract associated data from raster brick
    app.data$current.etr <- raster::extract(app.data$etr, click.coord) %>% as.vector
    }
    
  })
  
  #fit particular model depending on input$model (and add to plot)
  shiny::observe({
    if(!is.null(app.data$step_2) && app.data$step_2 == 'complete' && !is.null(app.data$layers)){
      
    #no model currently selected (default starting parameter?)
    if(app.data$model == 'none'){
      no.obs <- app.data$layers %>% as.numeric
      model.etr <- app.data$current.etr[1:no.obs]
      model.par <- app.data$par.levels[1:no.obs]
      app.data$model.etr <- model.etr
      app.data$model.par <- model.par
    }
    
    #Jassby & Platt model selected
    if(app.data$model == 'Jassby & Platt'){
      no.obs <- app.data$layers %>% as.numeric
      model.etr <- app.data$current.etr[1:no.obs]
      model.par <- app.data$par.levels[1:no.obs]
      my.res<-tryCatch({
        minpack.lm::nlsLM(model.etr ~ etrmax * tanh((alpha * model.par) / etrmax),
                          start=list(alpha = 0.1, etrmax = 40), algorithm="port", trace=F, lower = c(0,0),
                          control=stats::nls.control(maxiter=1024))
      },error=function(e){NaN}
      )
      
      #make available
      app.data$model.etr <- model.etr
      app.data$model.par <- model.par
      
      if(!is.na(my.res[1])){
        coefs <- stats::coef(my.res)
        my.alpha <- coefs[1] %>% as.numeric
        my.etrmax <- coefs[2] %>% as.numeric
        
        #run predict to get fit line
        new.dat <- data.frame(model.par = seq(0,max(model.par), by=1))
        pred <- stats::predict(my.res, new.dat)
        
        #make available
        app.data$alpha <- my.alpha
        app.data$etrmax <- my.etrmax
        app.data$pred.par <- new.dat$model.par
        app.data$pred <- pred
      }
    }#end of if
    
    #Platt model selected - currently defaults to plot1() driven by no model
    if(app.data$model == 'Platt'){
      no.obs <- app.data$layers %>% as.numeric
      model.etr <- app.data$current.etr[1:no.obs]
      model.par <- app.data$par.levels[1:no.obs]
      
      my.res <- tryCatch({
        minpack.lm::nlsLM(model.etr ~ Ps*(1-exp(-alpha*model.par/Ps))*exp(-beta*model.par/Ps),
                          start = list(alpha = 0.2, Ps = 2000, beta=150), algorithm = "port",
                          trace = F, control = stats::nls.control(maxiter=1024),
                          lower = c(0,0,0))
      }, error = function(e) {NaN} )
      
      #make available
      app.data$model.etr <- model.etr
      app.data$model.par <- model.par
      
      #extract parameters
      if(!is.na(my.res[1])){
        coefs <- stats::coef(my.res)
        
        my.alpha <- coefs[1] %>% as.numeric
        my.ps <- coefs[2] %>% as.numeric
        my.beta <- coefs[3] %>% as.numeric
        
        #run predict to get fit line
        new.dat <- data.frame(model.par = seq(0,max(model.par), by=1))
        pred <- stats::predict(my.res, new.dat)
        
        #make available
        app.data$alpha <- my.alpha
        app.data$ps <- my.ps
        app.data$beta <- my.beta
        app.data$pred.par <- new.dat$model.par
        app.data$pred <- pred
      }
      
    }#end of platt model if
    
    #Eilers and Peeters model selected
    if(app.data$model == 'Eilers & Peeters'){
      no.obs <- app.data$layers %>% as.numeric
      model.etr <- app.data$current.etr[1:no.obs]
      model.par <- app.data$par.levels[1:no.obs]
      
      my.res <- tryCatch({
        minpack.lm::nlsLM(model.etr ~ model.par/(model.par^2*(1/(alpha*Eopt^2))+(model.par/etrmax)-((2*model.par)/(alpha*Eopt))+(1/alpha)),
                          start=list(alpha = 0.4, etrmax = 40, Eopt=150), algorithm="port", trace=F,
                          control=stats::nls.control(maxiter=1024),lower=c(0,0,0))
      },error=function(e){NaN}
      )
      
      #make available
      app.data$model.etr <- model.etr
      app.data$model.par <- model.par
      
      if(!is.na(my.res[1])){
        coefs <- stats::coef(my.res)
        
        my.alpha <- coefs[1] %>% as.numeric
        my.etrmax <- coefs[2] %>% as.numeric
        my.eopt <- coefs[3] %>% as.numeric
        
        #run predict to get fit line
        new.dat <- data.frame(model.par = seq(0,max(model.par), by=1))
        pred <- stats::predict(my.res, new.dat)
        
        #make available
        app.data$alpha <- my.alpha
        app.data$etrmax <- my.etrmax
        app.data$eopt <- my.eopt
        app.data$pred.par <- new.dat$model.par
        app.data$pred <- pred
      }
    }
    
  }
  })
  
  #reactive plot when no model OR Platt (not made) selected
  plot1 <- shiny::reactive({
    plotly::plot_ly(x = app.data$model.par, y = app.data$model.etr, type='scatter', mode='line') %>%
      plotly::layout(xaxis = list(title = 'PAR'), yaxis = list(title = 'rETR'))
  })
  
  #reactive plot when Jassby & Platt OR EP model selected
  plot2 <- shiny::reactive({
    plotly::plot_ly(x = app.data$model.par, y = app.data$model.etr, type='scatter', mode='line', name = 'data') %>%
      plotly::layout(xaxis = list(title = 'PAR'), yaxis = list(title = 'rETR'), legend = list(x = 0.1, y = 0.95)) %>%
      plotly::add_trace(x = app.data$pred.par, y = app.data$pred, name = app.data$model)
  })
  
  #select which plot to use
  myGraph <- shiny::reactive({
    switch(app.data$model,
           'none' = plot1(),
           'Jassby & Platt' = plot2(),
           'Eilers & Peeters' = plot2(),
           'Platt' = plot2())
  })
  
  #send to UI
  output$etrPlot <- plotly::renderPlotly({
    myGraph()
  })
  
  
  #determine which model parameters to print to screen
  no.param <- reactive({
    my.info <- c('<b>Current model parameters</b>',
                 'No model selected')
    shiny::HTML(paste0(my.info, sep='',collapse='<br/>'))
  })
  
  jp.param <- shiny::reactive({
    if(!is.null(app.data$click[[1]])){
      if(!is.null(app.data$etrmax) & !is.null(app.data$alpha)){
        etrmax <- round(app.data$etrmax,2)
        alpha <- round(app.data$alpha,2)
      } else {
        etrmax <- NA
        alpha <- NA}
      my.info <- c('<b>Current Model Parameters</b>',
                   paste('<b>ETRmax</b>: ', etrmax, sep=''),
                   paste('<b>Alpha</b>: ', alpha, sep=''))
      shiny::HTML(paste0(my.info, sep='',collapse='<br/>'))
      
    }
  })
  
  ep.param <- shiny::reactive({
    if(!is.null(app.data$click[[1]])){
      if(!is.null(app.data$etrmax) & !is.null(app.data$alpha) & !is.null(app.data$eopt)){
        etrmax <- round(app.data$etrmax,2)
        alpha <- round(app.data$alpha,2)
        eopt <- round(app.data$eopt,2)
      } else {
        etrmax <- NA
        alpha <- NA
        eopt <- NA
      }
      my.info <- c('<b>Current Model Parameters</b>',
                   paste('<b>ETRmax</b>: ', etrmax, sep=''),
                   paste('<b>Alpha</b>: ', alpha, sep=''),
                   paste('<b>Eopt</b>: ', eopt, sep=''))
      shiny::HTML(paste0(my.info, sep='',collapse='<br/>'))
    }
  })
  
  p.param <- shiny::reactive({
    if(!is.null(app.data$click[[1]])){
      if(!is.null(app.data$alpha) & !is.null(app.data$ps) & !is.null(app.data$beta)){
        alpha <- round(app.data$alpha,2)
        ps <- round(app.data$ps,2)
        beta <- round(app.data$beta,2)
      } else {
        alpha <- NA
        ps <- NA
        beta <- NA}
      my.info <- c('<b>Current Model Parameters</b>',
                   paste('<b>Alpha</b>: ', alpha, sep=''),
                   paste('<b>Ps</b>: ', ps, sep=''),
                   paste('<b>Beta</b>: ', beta, sep=''))
      shiny::HTML(paste0(my.info, sep='',collapse='<br/>'))
    }
  })
  
  #check with model selected
  my.params <- shiny::reactive({
    switch(app.data$model,
           'none' = no.param(),
           'Jassby & Platt' = jp.param(),
           'Eilers & Peeters' = ep.param(),
           'Platt' = p.param()
    )
  })
  
  output$currentParams <- shiny::renderUI({
    my.params()
  })
  
  
  #data processing on confirmation of model and parameter selection
  shiny::observeEvent(input$useParams, {
    #assign our desired objects back to our my.data list
    #save model parameters dependingon what model is selected
    my.model <- app.data$model
    cut.light.steps <- app.data$layers %>% as.numeric
    
    #run the model selected on the nlayers chosen over the entire cropped image..
    #clip down data and light levels too desirced number of light steps
    clipped.etr <- app.data$etr[[1:as.numeric(app.data$layers)]]
    clipped.par <- app.data$par.levels[1:as.numeric(app.data$layers)]

    
    ####################
    #Define models - should be read in from other file in final version
    jassby.platt.mod<-function(x){
      
      light <- local(light, env = e2)
      s.alpha <- local(s.alpha, env = e2)
      s.etrmax <- local(s.etrmax, env = e2)
      
      if(is.na(x[[1]])){
        my.res <- c(NaN,NaN,NaN,NaN,NaN,NaN)
      }else{
        my.res <- tryCatch({
          minpack.lm::nlsLM(x ~ etrmax * tanh((alpha * light) / etrmax),
                            start=list(alpha = s.alpha, etrmax = s.etrmax), algorithm="port", trace=F,
                            control=stats::nls.control(maxiter=1024),lower=c(0,0))
        },error=function(e){NaN}
        )
        if(is.na(my.res[1])){
          my.res <- c(NaN,NaN,NaN,NaN,NaN,NaN)
        }else{
          my.res <- c(summary(my.res)$coefficients[1:2],
                      (summary(my.res)$coefficients[2]/summary(my.res)$coefficients[1]),
                      summary(my.res)$coefficients[3:4],
                      summary(my.res)$sigma)}
        
        return(my.res)
      }
    }
    ######
    platt.mod <- function(x){
      
      light <- local(light, env = e2)
      s.alpha <- local(s.alpha, env = e2)
      s.ps <- local(s.ps, env = e2)
      s.beta <- local(s.beta, env = e2)
      
      if (is.na(x[[1]]) ){
        my.res <- c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN)
      }else{
        my.res <- tryCatch({
          minpack.lm::nlsLM(x ~ Ps*(1-exp(-alpha*light/Ps))*exp(-beta*light/Ps),
                            start = list(alpha = s.alpha, Ps = s.ps, beta=s.beta), algorithm = "port",
                            trace = F, control = stats::nls.control(maxiter=1024),
                            lower = c(0,0,0))
        }, error = function(e) {NaN} )
        if (is.na(my.res[1])) {
          my.res <- c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN)
        }else{
          my.res <- c(summary(my.res)$coefficients[1:3],
                      summary(my.res)$coefficients[2]*(summary(my.res)$coefficients[1]/(summary(my.res)$coefficients[1]+summary(my.res)$coefficients[3]))*(summary(my.res)$coefficients[3]/(summary(my.res)$coefficients[1]+summary(my.res)$coefficients[3]))^(summary(my.res)$coefficients[3]/summary(my.res)$coefficients[1]),
                      summary(my.res)$coefficients[2]*(summary(my.res)$coefficients[1]/(summary(my.res)$coefficients[1]+summary(my.res)$coefficients[3]))*(summary(my.res)$coefficients[3]/(summary(my.res)$coefficients[1]+summary(my.res)$coefficients[3]))^(summary(my.res)$coefficients[3]/summary(my.res)$coefficients[1])/summary(my.res)$coefficients[1],
                      summary(my.res)$coefficients[4:6],
                      summary(my.res)$sigma)}
        return(my.res)
      }
    }
    ######
    ep.mod <- function(x){
      
      light <- local(light, env = e2)
      s.alpha <- local(s.alpha, env = e2)
      s.etrmax <- local(s.etrmax, env = e2)
      s.eopt <- local(s.eopt, env = e2)
      
      if(is.na(x[[1]])){
        my.res <- c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)
      }else{
        my.res <- tryCatch({
          minpack.lm::nlsLM(x ~ light/(light^2*(1/(alpha*Eopt^2))+(light/etrmax)-((2*light)/(alpha*Eopt))+(1/alpha)),
                            start=list(alpha = s.alpha, etrmax = s.etrmax, Eopt=s.eopt), algorithm="port", trace=F,
                            control=stats::nls.control(maxiter=1024),lower=c(0,0,0))
        },error=function(e){NaN}
        )
        
        if (is.na(my.res[1])){
          my.res <- c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)
        }else{
          my.res <- c(summary(my.res)$coefficients[1:3],#alpha, etrmax, eopt
                      summary(my.res)$coefficients[2]/summary(my.res)$coefficients[1],#ek
                      summary(my.res)$coefficients[4:6],summary(my.res)$sigma)#se alpha, etermax, eopt, RMSE
        }
        return(my.res)
      }
    }
    
    ####################
    #run desired model - with progress bar...
    
    shiny::withProgress(message = 'Running model', { ##***this could be improved ###
    #######
    #Jassby and Platt
    ######
    if(my.model == 'Jassby & Platt'){
      #light needs to be available as object to the functions below
      light <- assign('light', clipped.par, envir = e2)
      s.etrmax <- assign('s.etrmax', app.data$etrmax, envir = e2)
      s.alpha <- assign('s.alpha', app.data$alpha, envir = e2)
      
      no.cores <- parallel::detectCores()
      if(no.cores == 1){
        app.data$model.outputs <-  raster::calc(clipped.etr, jassby.platt.mod, progress='text')
      } else {
        raster::beginCluster((no.cores-1))
        app.data$model.outputs <- raster::clusterR(clipped.etr, raster::calc, args=list(fun=jassby.platt.mod))
        raster::endCluster()
      }
      names(app.data$model.outputs) <- c('alpha','etrmax', 'Ek', 'se_alpha',' se_etrmax', 'RMSE')
    }
    
    #######
    #Platt
    #######
    if(my.model == 'Platt'){
      #light needs to be available as object to the functions below
      light <- assign('light', clipped.par, envir = e2)
      s.alpha <- assign('s.alpha', app.data$alpha, envir = e2)
      s.ps <- assign('s.ps', app.data$ps, envir = e2)
      s.beta <- assign('s.beta', app.data$beta, envir = e2)
      
      no.cores <- parallel::detectCores()
      if(no.cores == 1){
        app.data$model.outputs <-  raster::calc(clipped.etr, platt.mod, progress='text')
      } else {
        
        raster::beginCluster((no.cores-1))
        app.data$model.outputs <- raster::clusterR(clipped.etr, raster::calc, args=list(fun=platt.mod))
        raster::endCluster()
      }
      
      names(app.data$model.outputs) <- c('alpha','ps', 'beta', 'etrmax','ek', 'se_alpha',' se_ps', 'se_beta', 'RMSE')
    }
    
    #######
    #Eilers & Peeters
    ######
    if(my.model == 'Eilers & Peeters'){
      #light needs to be available as object to the functions below
      light <- assign('light', clipped.par, envir = e2)
      s.alpha <- assign('s.alpha', app.data$alpha, envir = e2)
      s.etrmax <- assign('s.etrmax', app.data$etrmax, envir = e2)
      s.eopt <- assign('s.eopt', app.data$eopt, envir = e2)
      
      no.cores <- parallel::detectCores()
      if(no.cores == 1){
        app.data$model.outputs <-  raster::calc(clipped.etr, ep.mod, progress='text')
      } else {
        raster::beginCluster((no.cores-1))
        app.data$model.outputs <- raster::clusterR(clipped.etr, raster::calc, args=list(fun=ep.mod))
        raster::endCluster()
      }
      
      names(app.data$model.outputs) <- c('alpha','etrmax', 'eopt', 'Ek', 'se_alpha',' se_etrmax', 'se_eopt', 'RMSE')
    }
    
    })#end of progress checker
    
    #completion check for step 3
    app.data$step_3 <- 'complete'
    
  })
  
  
  
  ####################### Shiny 4 code ##################################
  #observe brick selection
  shiny::observeEvent(input$selectBrick, {
    app.data$brick <- input$selectBrick
  })
  
  #observe layer when changed
  shiny::observeEvent(input$selectLayer,{
    app.data$layer <- input$selectLayer
  })
  
  #render selectInput for selectBrick 
  output$selectBrick <- shiny::renderUI({
    if(!is.null(app.data$step_3) && app.data$step_3 == 'complete'){
      
      #determine classes of different objects in input list (to pull out all bricks)
      my.classes <- c()
      for(i in 1:length(names(app.data))){
        my.classes[i] <- class(app.data[[names(app.data)[i]]])
      }
      print(my.classes)
      my.bricks <- which(my.classes == 'RasterStack' | my.classes == 'RasterBrick')
      #brick.dat <- app.data[[names(app.data)[my.bricks]]]
      brick.names <- names(app.data)[my.bricks]
      print(brick.names)
      
      shiny::selectInput('selectBrick', 'Choose raster brick',
                         choices = brick.names, selected = brick.names[1])
    }
  })

  
  #render htmlOutput for selectLayer based on brick selection
  output$selectLayer <- shiny::renderUI({
    if(!is.null(app.data$brick)){
    layer.names <- names(app.data[[names(app.data)[names(app.data) == app.data$brick]]])
    shiny::selectInput('selectLayer', 'Select brick layer to view', choices = layer.names, selected = layer.names[1])
    }
  })
  
  
  #render raster map
  output$rasterImage <- leaflet::renderLeaflet({
    if(!is.null(app.data$step_3) && app.data$step_3 == 'complete' && !is.null(app.data$brick)){
    my.rast <- raster::raster(app.data[[names(app.data)[names(app.data) == app.data$brick]]], app.data$layer)
    print(my.rast)
    pal <- leaflet::colorNumeric(palette = 'magma',
                                 domain = c(min(values(my.rast), na.rm = T), max(values(my.rast), na.rm = T)),
                                 na.color = 'transparent')
    leaflet::leaflet() %>%
      leaflet::addRasterImage(my.rast, project = T, colors = pal) %>%
      leaflet::addLegend(pal = pal,
                         values = values(my.rast)#,
                         #labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
      )
    }
  })
  
  #render input settings info
  output$input.settings <- shiny::renderUI({
    if(!is.null(app.data$step_3) && app.data$step_3 == 'complete'){
    paste0(
      shiny::HTML('<br>Input PAR levels</br>'),
      app.data$par.levels,
      shiny::HTML('<b>Degree of smoothing applied</b>'),
      app.data$smooth,
      shiny::HTML('<b>Absorbance image used in ETR calculation</b>'),
      app.data$abs
    )
    }
  })
  
}#end of server

shinyApp(ui, server)