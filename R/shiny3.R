#' @title shiny app to select the irradiance-ETR curve to fit and calculate the parameter images
#' @description shiny app to select the irradiance-ETR curve to fit and calculate the parameter images
#' @return Returns an object (TYPE?) with all the rlc parameter images (e.g.alpha','etrmax', 'eopt', 'Ek')
#' @keywords external
#' @examples
#'
#'
#' @export
shiny3 <- function(my.data){

  #library(shiny)
  #library(plotly)
  #library(magrittr)
  #library(parallel)
  #library(snow)
  library(sp)
  #`%>%` <- magrittr::`%>%`

  #create environment
  e2 <- new.env()

  #(re) define projections that will be used in the polgon fitting within the app
  leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"
  #define polygon projection
  poly.proj <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  #get our objects from our input
  my.brick <- my.data$yield
  my.etr <- my.data$etr
  my.rast <- raster::raster(my.brick, 1)
  my.light <- my.data$par.levels
  my.layers <- raster::nlayers(my.etr)

app3 <- shiny::shinyApp(

  ui <- shiny::fluidPage(

    shiny::titlePanel(title = 'Examine rETR and select starting parameters'),
    shiny::hr(),

    #panel showing image to select pixel from to display ETR
    shiny::fluidRow(shiny::column(4,
                    leaflet::leafletOutput('pixelSelector', width = '100%', height = '400px')
    ),#end of column
    shiny::column(3,
                  shiny::selectInput('layers', 'Number of light steps to plot',
                       choices = 4:my.layers, selected = my.layers),
                  shiny::radioButtons('model', 'Select model fit',
                        choices = c('none','Jassby & Platt', 'Platt', 'Eilers & Peeters'),
                        selected = 'none'),
                  shiny::br(),
           #output of etrmax, alpha as potential starting parameters
           shiny::htmlOutput('currentParams'),
           shiny::br(),
           #confirm button
           shiny::actionButton('useParams', 'Use Current Parameters?')
    ),
    shiny::column(5,
           plotly::plotlyOutput('etrPlot', width = '100%', height='400px')
    )#end of column
    ),#end of fluid row
    shiny::hr()
  ),

server <- function(input, output, session) {


    app.data <<- shiny::reactiveValues(click = NULL)

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
      leaflet::leaflet() %>%
        leaflet::addRasterImage(my.rast, project = T)
    })

    #extract click coordinates and use to extract etr data from raster brick
    shiny::observeEvent(input$pixelSelector_click, {

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
      app.data$current.etr <- raster::extract(my.etr, click.coord) %>% as.vector

    })

    #fit particular model depending on input$model (and add to plot)
    shiny::observe({

      #no model currently selected (default starting parameter?)
      if(app.data$model == 'none'){
        no.obs <- app.data$layers %>% as.numeric
        model.etr <- app.data$current.etr[1:no.obs]
        model.par <- my.light[1:no.obs]
        app.data$model.etr <- model.etr
        app.data$model.par <- model.par
      }

      #Jassby & Platt model selected
      if(app.data$model == 'Jassby & Platt'){
        no.obs <- app.data$layers %>% as.numeric
        model.etr <- app.data$current.etr[1:no.obs]
        model.par <- my.light[1:no.obs]
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
        model.par <- my.light[1:no.obs]

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
        model.par <- my.light[1:no.obs]

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

    shiny::observeEvent(input$useParams, {
      shiny::stopApp()
    })
    session$onSessionEnded(function(){
      shiny::stopApp
    })


  }#end of server

)#end of shinyApp

shiny::runApp(app3)


#assign our desired objects back to our my.data list
#save model parameters dependingon what model is selected
my.model <- shiny::isolate(app.data$model)
my.data$model <- my.model
my.data$cut.light.steps <- shiny::isolate(app.data$layers) %>% as.numeric
if(my.model == 'none'){
  my.data$start.etrmax <- 'no model selected in previous function'
  my.data$start.alpha <- 'no model selected in previous function'
}
if(my.model == 'Jassby & Platt'){
  my.data$start.etrmax <- shiny::isolate(app.data$etrmax)
  my.data$start.alpha <- shiny::isolate(app.data$alpha)
}
if(my.model == 'Platt'){
  my.data$start.alpha <- shiny::isolate(app.data$alpha)
  my.data$start.ps <- shiny::isolate(app.data$ps)
  my.data$start.beta <- shiny::isolate(app.data$beta)
}
if(my.model == 'Eilers & Peeters'){
  my.data$start.etrmax <- shiny::isolate(app.data$etrmax)
  my.data$start.alpha <- shiny::isolate(app.data$alpha)
  my.data$start.eopt <- shiny::isolate(app.data$eopt)
}

#run the model selected on the nlayers chosen over the entire cropped image..
#clip down data and light levels too desirced number of light steps
clipped.etr <- my.data$etr[[1:my.data$cut.light.steps]]
clipped.par <- my.data$par.levels[1:my.data$cut.light.steps]

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
#run desired model

#######
#Jassby and Platt
######
if(my.data$model == 'Jassby & Platt'){
  #light needs to be available as object to the functions below
  light <- assign('light', clipped.par, envir = e2)
  s.etrmax <- assign('s.etrmax', my.data$start.etrmax, envir = e2)
  s.alpha <- assign('s.alpha', my.data$start.alpha, envir = e2)

  no.cores <- parallel::detectCores()
  if(no.cores == 1){
    my.data$model.outputs <-  raster::calc(clipped.etr, jassby.platt.mod, progress='text')
  } else {
    raster::beginCluster((no.cores-1))
    my.data$model.outputs <- raster::clusterR(clipped.etr, raster::calc, args=list(fun=jassby.platt.mod))
    raster::endCluster()
  }
  names(my.data$model.outputs) <- c('alpha','etrmax', 'Ek', 'se_alpha',' se_etrmax', 'RMSE')
}

#######
#Platt
#######
if(my.data$model == 'Platt'){
  #light needs to be available as object to the functions below
 light <- assign('light', clipped.par, envir = e2)
 s.alpha <- assign('s.alpha', my.data$start.alpha, envir = e2)
 s.ps <- assign('s.ps', my.data$start.ps, envir = e2)
 s.beta <- assign('s.beta', my.data$start.beta, envir = e2)

 no.cores <- parallel::detectCores()
  if(no.cores == 1){
    my.data$model.outputs <-  raster::calc(clipped.etr, platt.mod, progress='text')
   } else {

     raster::beginCluster((no.cores-1))
     my.data$model.outputs <- raster::clusterR(clipped.etr, raster::calc, args=list(fun=platt.mod))
     raster::endCluster()
   }

  names(my.data$model.outputs) <- c('alpha','ps', 'beta', 'etrmax','ek', 'se_alpha',' se_ps', 'se_beta', 'RMSE')
}

#######
#Eilers & Peeters
######
if(my.data$model == 'Eilers & Peeters'){
  #light needs to be available as object to the functions below
  light <- assign('light', clipped.par, envir = e2)
  s.alpha <- assign('s.alpha', my.data$start.alpha, envir = e2)
  s.etrmax <- assign('s.etrmax', my.data$start.etrmax, envir = e2)
  s.eopt <- assign('s.eopt', my.data$start.eopt, envir = e2)

  no.cores <- parallel::detectCores()
  if(no.cores == 1){
    my.data$model.outputs <-  raster::calc(clipped.etr, ep.mod, progress='text')
  } else {
    raster::beginCluster((no.cores-1))
    my.data$model.outputs <- raster::clusterR(clipped.etr, raster::calc, args=list(fun=ep.mod))
    raster::endCluster()
  }

  names(my.data$model.outputs) <- c('alpha','etrmax', 'eopt', 'Ek', 'se_alpha',' se_etrmax', 'se_eopt', 'RMSE')
}


return(my.data)

}#end of shiny3 function
