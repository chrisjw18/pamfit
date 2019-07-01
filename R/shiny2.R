#' @title shiny app to select the region of interest to be analysed
#' @description shiny app to select the region of interest to be analysed, it also calculates PSII efficiency images and the equivalent rETR images
#' @return Returns an object (TYPE?) with all the PSII images and the rETR images cropped to the region of interest selected by the user
#' @keywords external
#' @examples
#'
#'
#' @export
shiny2 <- function(my.data){
#the input here is the output from the previous function (list of i: the raster brick, ii: par.levels, iii: smooth factor)

  #library(mapview)
  #library(leaflet)
  #library(leaflet.extras)
  library(sp)
  #library(tiff)
  #library(raster)
  #library(shiny)
  `%>%` <- magrittr::`%>%`
#(re) define projections that will be used in the polgon fitting within the app
leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"
#define polygon projection
poly.proj <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


#Interactive session to define AOI polygon
app2 <- shiny::shinyApp(

#define UI
ui <- shiny::fluidRow(leaflet::leafletOutput("leafmap", height = '600px'),
               shiny::actionButton('confirm', 'Confirm Selection', width = '100%')
               ),

#define server
server <- function(input, output, session) ({
  app.data <<- shiny::reactiveValues(poly.coords = NULL)

  output$leafmap <- leaflet::renderLeaflet({
    my.rast <- raster::raster(my.data$original.data,1)
    leaflet::leaflet() %>%
      leaflet::addRasterImage(my.rast, project = T) %>%
      leaflet.extras::addDrawToolbar( polylineOptions = F, circleOptions = F, rectangleOptions = T,
                      markerOptions = F, circleMarkerOptions = F, singleFeature = T,
                      polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = F), editOptions = leaflet.extras::editToolbarOptions()
      )
  })

  shiny::observeEvent(input$leafmap_draw_new_feature, {
    input$leafmap_draw_new_feature

    poly.coords <- input$leafmap_draw_new_feature$geometry$coordinates %>% unlist
    poly.mat <- data.frame(x = poly.coords[c(TRUE, FALSE)], y = poly.coords[c(FALSE, TRUE)])
    xy <- sp::SpatialPoints(poly.mat)
    proj4string(xy) <- poly.proj
    xy <- as.data.frame(spTransform(xy, leafletProj))
    final.poly <- mapview::coords2Polygons(as.matrix(xy), ID='chris')
    my.crop <- my.data$original.data %>% raster::crop(raster::extent(final.poly)) %>% raster::mask(final.poly)

    app.data$my.crop <- my.crop
    app.data$poly.coords <-xy
    app.data$poly.draw <- final.poly

  })

  #this observeEvent is our test for capturing coordinates of a edited polygon - delete if cant get to work and switch off edit options in UI
  #taken from: https://stackanswers.net/questions/how-to-update-coordinates-after-dragging-a-marker-in-leaflet-shiny
  observeEvent(input$leafmap_draw_edited_features, {

    input$leafmap_draw_edited_features

    edited <<- input$leafmap_draw_edited_features
    drawn <<- input$leafmap_draw_new_feature
    # find the edited features and update drawn
    # start by getting the leaflet ids to do the match
    ids <- unlist(lapply(drawn, function(x){x$properties$`_leaflet_id`}))
    # now modify drawn to match edited
    map(edited$features, function(x){
      loc <- match(x$properties$`_leaflet_id`, ids)
      drawn[loc] <<- list(x) })

    #this is a test
    poly.edit.coords <<- input$leafmap_draw_edited_features %>% unlist
    poly.mat2 <- data.frame(x = poly.edit.coords[c(TRUE, FALSE)], y = poly.edit.coords[c(FALSE, TRUE)])
    xyz <- SpatialPoints(poly.mat2)
    proj4string(xyz) <- poly.proj
    xyz <- as.data.frame(spTransform(xyz, leafletProj))
    final.poly2 <- coords2Polygons(as.matrix(xyz), ID='chris')
    app.data$poly.draw2 <- final.poly2

  })



  observeEvent(input$confirm, {
    shiny::stopApp()
  })
  session$onSessionEnded(function(){
    shiny::stopApp
  })



})
)#end of app call
#run the app
shiny::runApp(app2)

#use to work on my.data input list from previous function
my.data$poly.coords <- shiny::isolate(app.data$poly.coords)
my.data$poly.draw <- shiny::isolate(app.data$poly.draw)
my.data$cropped.data <- shiny::isolate(app.data$my.crop)
names(my.data$cropped.data) <- names(my.data$original.data)

#process rasters to determine f, fm, yield and etr images
this.dat <- my.data$cropped.data[[-c(1:4)]]
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
light <- my.data$par.levels
abs <- my.data$abs
if(abs == 'yes'){
  nir <- raster::subset(all.data$cropped.data, 'NIR')
  red <- raster::subset(all.data$cropped.data, 'Red')
  abs.rast <- 1 - red / nir
}

etr.fun.1 <- function(x) { x * light * 0.5 }
etr.fun.2 <- function(x) {x * light * 0.5 * abs.rast}

if(abs == "no"){
  etr <- raster::calc(fv_fm_img, etr.fun.1)
} else {
  etr <- raster::calc(fvm_fm_img, etr.fun.2)
  }

my.data$f_img <- f_img
names(my.data$f_img) <- paste('F', 1:(raster::nlayers(my.data$f_img)), sep='_')
my.data$fm_img <- fm_img
names(my.data$fm_img) <- paste('Fm', 1:(raster::nlayers(my.data$fm_img)), sep='_')
my.data$yield <- fv_fm_img
names(my.data$yield) <- paste('Yield', 1:(raster::nlayers(my.data$yield)), sep='_')
my.data$etr <- etr
names(my.data$etr) <- paste('ETR', 1:(raster::nlayers(my.data$etr)), sep='_')


return(my.data)

}
