#Fits the light-photosynthesis curve model suggested by Platt et al. (1980)
#Platt, T., Gallegos, C.L., Harrison, W.G., 1980. Photoinhibition of 
#photosynthesis in natural assemblages of marine phytoplankton. J. Mar. Res. 38, 687-701.
#It is more complex than the Jassby & Platt model (1976) but has the advantage of including a photo-inhibition parameter.
#the first input parameter controls the number of cpu cores to used in the calculations. The default is one core. The number of available cores can be checked with the function test_OS().
#the second input parameter determines if plots should be produced for each pixel. The default is TRUE (O) but changes to FALSE if more than one core is used because multicore will crash R if any attemps are made to plot anything during processing.
#it is advised that a first run is carried out with a subset of the image to check model fitting plotting the fit curve and using only one core


#' @title Check Multicore
#' @description Makes sure that multicore is not activated 
#' when using windows based OS
#' @param ncores number of cores
#' @return the number of cores
#' @keywords internal
#' @export
check_multicore <- function(ncores) {
  OS <- .Platform$OS.type
  if (OS != "unix") {
    ncores <- 1
  }
  ncores
}


#' @title Creates Rapid Light Curve
#' @description Creates a bigmemory object to store all the results
#' @param x2 number of ...
#' @param y2 number of ...
#' @param ncol number of columns of big matrix
#' @keywords internal
#' @export
create_rlc <- function(x2, y2, ncol) {
  big.matrix(x2 * y2, ncol)
}


#' @title Initialize RLC parameters
#' @description Creates default values to be updated during the fitting
#' process
#' @param model name of the fitted model
#' @return list with various parameters
#' @keywords internal
#' @export
initialize_rlc_params <- function(model = 'jassby') {
  pars <- c(
    'alpha2' = NaN,
    'etrmax2' = NaN,
    'Ek2' = NaN,
    'WSSR' = NaN,
    'mnb' = NaN,
    'rmse' = NaN,
    'nonFit' = NaN,
    'graf' = 1,
    'Ps2'= NaN,
    'beta2' = NaN
  )
  if (model == 'jassby') {
    return(as.list(pars[1:8]))
  } else {
    return(as.list(pars))
  }
}



#' @title Get Prediction
#' @description Compute predicted value
#' @param params list of RLC parameters
#' @param pe3 data frame with light and etr
#' @param model name of the fitted model
#' @return predicted value
#' @keywords internal
#' @export
get_prediction <- function(rlc_params, pe3, model = 'jassby') {
  switch(
    model,
    'jassby' = rlc_params$etrmax2 * tanh((rlc_params$alpha2 * pe3$light) / rlc_params$etrmax2),
    'platt' = rlc_params$Ps2*(1-exp(-rlc_params$alpha2*pe3$light/rlc_params$Ps2))*exp(-rlc_params$beta2*pe3$light/rlc_params$Ps2)
  )
}


#' @title Weighted sum-of-squared-residuals
#' @description Weighted sum-of-squared-residuals
#' @param predicted predicted value from \code{\link{get_prediction}}
#' @param pe3 data frame with light and etr
#' @keywords internal
#' @export
get_wssr <- function(predicted, pe3) {
  sum((pe3$etr - predicted) ^ 2)
}


#' @title Mean Normalized Bias
#' @description Mean normalized bias in percentage
#' @param predicted predicted value from \code{\link{get_prediction}}
#' @param pe3 data frame with light and etr
#' @keywords internal
#' @export
mean_normalized_bias <- function(predicted, pe3) {
  clean_pred <- na.omit((predicted - pe3$etr) / pe3$etr)
  sum(clean_pred / (length(pe3$etr) - 1)) * 100
}


#' @title Get Mean Normalized Bias
#' @description Mean normalized bias in percentage
#' @param predicted predicted value from \code{\link{get_prediction}}
#' @param pe3 data frame with light and etr
#' @keywords internal
#' @export
get_mnb <- function(predicted, pe3) {
  mnb <- tryCatch(
    mean_normalized_bias(predicted, pe3), 
    error = function(e) "NaN"
  )
  if (is.character(mnb)) NaN else mnb
}


#' @title Residual Mean Square Error
#' @description Residual Mean Square Error
#' @param predicted predicted value from \code{\link{get_prediction}}
#' @param pe3 data frame with light and etr
#' @keywords internal
#' @export
res_mean_square_error <- function(predicted, pe3) {
  sqrt(sum(predicted - pe3$etr) ^ 2 / (length(pe3$etr) - 2))
}


#' @title Get Residual mean square error (RMSE)
#' @description Residual mean square error (RMSE)
#' @param predicted predicted value from \code{\link{get_prediction}}
#' @param pe3 data frame with light and etr
#' @keywords internal
#' @export
get_rmse <- function(predicted, pe3) {
  rmse <- tryCatch(
    res_mean_square_error(predicted, pe3), 
    error = function(e) "NaN"
  )
  if (is.character(rmse)) NaN else rmse
}


#' @title ETR Convergence
#' @description update RLC parameters when etr converges to zero
#' @param etr_sim etr
#' @param rlc_params RLC parameters 
#' @param pe3 data frame with light and etr
#' @param model name of fitted model
#' @keywords internal
#' @export
etr_convergence <- function(etr_sim, rlc_params, pe3, model = 'jassby') {
  # the nonFit image
  rlc_params$nonFit <- 5
  #extraction of the parameters from the etr_sim object
  rlc_params$alpha2 <- etr_sim$par[1]
  
  if (model == 'jassby') {
    # jassby
    rlc_params$etrmax2 <- etr_sim$par[2]
  } else {
    # platt
    rlc_params$beta2 <- round(etr_sim$par[2],1)
    rlc_params$Ps2<-etr_sim$par[3]
    if (round(etr_sim$par[2],1)==0)
    {
      rlc_params$etrmax2<-max(pe3$etr) 
    } 
    else {
    rlc_params$etrmax2<-etr_sim$par[3]*(rlc_params$alpha2/(rlc_params$alpha2+ rlc_params$beta2))*( rlc_params$beta2/(rlc_params$alpha2+ rlc_params$beta2))^( rlc_params$beta2/rlc_params$alpha2)
    }
    }
    
    
  
  rlc_params$Ek2 <- rlc_params$etrmax2 / rlc_params$alpha2
  #print( etr_sim$par[1])
  #print( etr_sim$par[2])
  #print( etr_sim$par[3])
  
  #print(rlc_params$alpha2)
  #print(rlc_params$beta2)
  #print(rlc_params$Ps2)
  #print(rlc_params$etrmax2)
  #print(rlc_params$Ek2)
  
  # error statistics
  predicted <- get_prediction(rlc_params, pe3, model)
  #print(predicted)
  rlc_params$WSSR <- get_wssr(predicted, pe3)
  rlc_params$mnb <- get_mnb(predicted, pe3)
  rlc_params$rmse <- get_rmse(predicted, pe3)
  rlc_params
  #print(rlc_params)
}



#' @title Print completeness 
#' @description Print completeness percentage in the R console if using 
#' just one core when multiple cores are used it is not possible to plot the
#' percentage because of the multiple R instances running,
#' thus a message is printed
#' @param ncores number of cores
#' @param i counter
#' @param total number of pixels
#' @keywords internal
#' @export
print_completeness <- function(ncores, i, total) {
  if (ncores == 1) {
    sub_total <- i
    final <- (sub_total / total) * 100
    print(paste(round(final, 4), "%"))
  } else {
    print("Processing data, please wait...")
  }
}



#' @title Get Pixels
#' @description Calculates different types of pixels
#' @param rlc_matrix RLC big matrix object
#' @param x2 number of rows
#' @param y2 number fo columns
#' @keywords internal
#' @export
get_pixels <- function(rlc_matrix, x2, y2) {
  # calculations of the different types of pixels
  non_fit_image <- rlc_matrix[,7]
  # total number of pixels
  total_pixels <- x2 * y2
  # total without the pixels that are outside the region of interest
  total_pixels_inside <- total_pixels - sum(non_fit_image == 1)
  # total of pixels excluded by the noise reduction while importing
  # the data import_data()
  filtered <- sum(non_fit_image == 2)
  # total of pixels where model fit was attemped
  attempted <- total_pixels_inside - filtered
  # percentage of pixels excluded from the AOI
  perc_excluded <- filtered / total_pixels_inside * 100
  # percentage of pixels that did not converge to a solution
  non_fit <- sum(non_fit_image == 3) + sum(non_fit_image == 4)
  perc_non_fit <- non_fit / attempted * 100
  # percentage of pixels fitted
  fit <- sum(non_fit_image == 5)
  perc_fit <- fit / attempted * 100
  
  list(
    total_pixels_inside = total_pixels_inside,
    perc_excluded = perc_excluded,
    attempted = attempted,
    perc_fit = perc_fit,
    perc_non_fit = perc_non_fit)
}  


#' @title Print Pixel Messages
#' @description Prints messages about pixels
#' @param pixels_info information
#' @keywords internal
#' @export
print_pixel_messages <- function(pixels_info) {
  messages <- c(
    "Total number of pixels = ",
    "Percentage of excluded pixels = ",
    "Total number of pixels analyzed = ",
    "Percentage of fitted pixels = ",
    "Percentage of non-fitted pixels = ")
  
  # printing pixel messages
  for (m in seq_along(messages)) {
    print(paste0(messages[m], round(pixels_info[[m]], 0) ))
  }
}


#' @title Export Images
#' @description Export images as text files that can be imported by 
#' ImageJ as text Images
#' @param rlc_matrix RLC big matrix object
#' @param x2 number of columns
#' @param model name of fitted model
#' @keywords internal
#' @export
export_images <- function(rlc_matrix, x2, model = 'jassby') {
  # names of parameters
  img_params <- switch(
    model,
    'jassby' = c('alpha', 'etrmax', 'Ek', 'WWSR',
                  'MNB', 'RMSE', 'nonFit'),
    'platt' = c('alpha', 'etrmax', 'Ek', 'WWSR',
                'MNB', 'RMSE', 'nonFit', 'ps','beta')
  )
  # preparing images
  images <- vector(mode = "list", length(img_params))
  for (k in 1:length(img_params)) {
    images[[k]] <- t(matrix(rlc_matrix[ ,k], nrow = x2))
  }
  names(images) <- paste0(img_params, "_", model)
  # exporting image files
  image_files <- paste0(img_params, '_', model, '_model.txt')
  for (k in 1:length(img_params)) {
    write.table(
      images[[k]],
      file = image_files[k],
      col.names = FALSE,
      row.names = FALSE
    )
  }
}


#' @title Platt Model
#' @description Fits a Platt model
#' @param imgdim list with image dimensions and light values
#' (output from \code{\link{import_data}})
#' @param etr_total etr big matrix object
#' @param path character vector indicating full path where files
#' are located
#' @param ncores number of cores for parallel computing
#' @param plots number of plots. Only works when \code{ncores = 1}
#' @param alpha 0.35 by default
#' @param Ps 40 by default
#' @param beta 0 by default
#' @details multicore will crash if any attemps are made to plot 
#' anything during processing. Setting \code{plots} to \code{TRUE} when 
#' \code{ncores > 1} will only plot the maps at the end of model fitting.
#' It is advised that a first run is carried out with a subset of 
#' the image to check model fitting plotting the fit curve and 
#' using only one core.
#' It creates a bigmemory object to store all the results
#' @export
platt_model <- function(imgdim, etr_total, path = '.', ncores = 1, 
                        plots = 0, alpha = 0.35, beta = 2, Ps = 40)
{
  if (!exists('etr_total')) {
    stop('\netr object does not exist')
  }
  x2 <- imgdim$x2
  y2 <- imgdim$y2
  total <- imgdim$total
  light <- imgdim$light
  # multicore?
  ncores <- check_multicore(ncores)
  
  # create a bigmemory object to store all the results
  #rlc_matrix <<- create_rlc(path, x2, y2, ncol = 7)
  rlc_matrix <<- create_rlc(x2, y2, ncol = 9)
  
  # a counter to keep track of the fitting errors
  count_optim_errors <- 0
  sub_total <- 0
  
  # the function that will be used by mclapply()
  # ======= does it have to be included? =======
  myRLC <- function(i, light) {
    # initialize RLC parameters
    rlc_params <- initialize_rlc_params(model = 'platt')
    
    # update RLC parameters
    if (any(etr_total[i,] == "NaN")) {
      #skips any pixel that has non-values (areas outside AOI)
      rlc_params$nonFit <- 1
    } else if (any(etr_total[i,] == "99999")) {
      #skips any pixel that has a 99999 (noise)
      rlc_params$nonFit <- 2
    } else {
      # reset variable for the plot
      rlc_params$graf <- 0
      # set up the dataframe (pixel) to be processed
      pe3 <- as.data.frame(cbind(light, etr_total[i,]))
      names(pe3) <- c("light", "etr")
      # The tryCatch option allows the errors to be stored inside
      # an object and prevents the mclapply to stop when there is an error
      #      etr_sim <- get_etr_sim(alpha, ETRmax)
      
      minim.func <- function(params, data = pe3)
      {
        alpha <- params[[1]]
        beta <- params[[2]]
        Ps <- params[[3]]
        return( sum( (data$etr-Ps*(1-exp(-alpha*light/Ps))*exp(-beta*light/Ps))^2))
      }
      
      
      #function that finds the fit solution 
      #Optim with  L-BFGS-B is a general-purpose optimization based on a 
      #modification if the Nelder-Mead quasi Newton method that allows box 
      #constrains. This is important because both rETRmax and alpha need to be 
      #positive the tryCatch option allows the errors to be stored inside an 
      #object and prevents the mclapply to stop when there is an error
      etr_sim <-
        tryCatch(
          optim(
            par = c(alpha, beta, Ps),
            fn = minim.func,
            control = list("maxit" = 1000000, pgtol = 0.01),
            method = "L-BFGS-B",
            lower = c(0,0,0), 
            upper = c(Inf,Inf, Inf),
            hessian = TRUE
          )
          , error = function(e)
            "NaN"
        )
      
      # if error message reset graf and nonFit
      if (is.character(etr_sim)) {
        rlc_params$nonFit <- 3
        rlc_params$graf <- 1
      }
      
      if (is.list(etr_sim)) {
        if (etr_sim$convergence != 0) {
          count_optim_errors <<- count_optim_errors + 1
          rlc_params$nonFit <- 4
          rlc_params$graf <- 1
        }
        
        if (etr_sim$convergence == 0) {
          rlc_params <- etr_convergence(etr_sim, rlc_params, pe3, model = 'platt')
      
        }
        
      }
      
    } # end updating RLC parameters
    
    # store the parameters in the bigmemory object (rlc_matrix)
    #rlc_matrix[i, ] <- unlist(rlc_params)[-length(rlc_params)]
    rlc_matrix[i, ] <- unlist(rlc_params)[-8]
    
    
    print_completeness(ncores, i, total)  # (only for one core)
    
    #to plot data and fitted curves when ncores=1 and it is a valid pixel
    if (ncores == 1 & rlc_params$graf == 0 & plots == 1 & i %% 5 == 0) {
      #dev.new()
      plot(pe3$light, pe3$etr, pch = 21,
           xlab = "Light", ylab = "rETR")
     # print(rlc_params$Ps2*(1-exp(-rlc_params$alpha2*pe3$light/rlc_params$Ps2))*exp(-1*pe3$light/rlc_params$Ps2))
      lines(pe3$light, 
            rlc_params$Ps2*(1-exp(-rlc_params$alpha2*pe3$light/rlc_params$Ps2))*exp(-rlc_params$beta2*pe3$light/rlc_params$Ps2),
            col = "blue")
      legend("topright", legend = i)
      #dev.off()
    }
    
  }
  # ends myRLC()
  
  #lines(pe3$light, rlc_params$etrmax2 * tanh((rlc_params$alpha2 * pe3$light) / rlc_params$etrmax2)
  
  # mclapply exectutes the myRLC function above
  #print(system.time(
  #  mclapply(1:total, myRLC, mc.cores = ncores, light = light)
  #  )
  #)
  mclapply(1:total, myRLC, mc.cores = ncores, light = light)
  print("Analysis finished!")
  
  pixels_info <- get_pixels(rlc_matrix, x2, y2)
  print_pixel_messages(pixels_info)
  
  # exports the images to the working directory
  export_images(rlc_matrix, x2, model = 'platt') 
  
}



#' @title Jassby Model
#' @description Fits a Jassby model
#' @param imgdim list with image dimensions and light values
#' (output from \code{\link{import_data}})
#' @param etr_total etr big matrix object
#' @param path character vector indicating full path where files
#' are located
#' @param ncores number of cores for parallel computing
#' @param plots number of plots. Only works when \code{ncores = 1}
#' @param alpha 0.35 by default
#' @param ETRmax 40 by default
#' @details multicore will crash if any attemps are made to plot 
#' anything during processing. Setting \code{plots} to \code{TRUE} when 
#' \code{ncores > 1} will only plot the maps at the end of model fitting.
#' It is advised that a first run is carried out with a subset of 
#' the image to check model fitting plotting the fit curve and 
#' using only one core.
#' It creates a bigmemory object to store all the results
#' @export
jassby_model <- function(imgdim, etr_total, path = '.', ncores = 1, 
                         plots = 0, alpha = 0.35, ETRmax = 40)
{
  print("JASSBY")
  if (!exists('etr_total')) {
    stop('\netr object does not exist')
  }
  x2 <- imgdim$x2
  y2 <- imgdim$y2
  total <- imgdim$total
  light <- imgdim$light
  # multicore?
  ncores <- check_multicore(ncores)
  
  # create a bigmemory object to store all the results
  #rlc_matrix <<- create_rlc(path, x2, y2, ncol = 7)
  print("CREATING RLC-matrix")
  rlc_matrix <<- create_rlc(x2, y2, ncol = 7)
  
  # a counter to keep track of the fitting errors
  count_optim_errors <- 0
  sub_total <- 0
  
  # the function that will be used by mclapply()
  # ======= does it have to be included? =======
  myRLC <- function(i, light) {
    # initialize RLC parameters
    rlc_params <- initialize_rlc_params(model = 'jassby')
    
    # update RLC parameters
    if (any(etr_total[i,] == "NaN")) {
      #skips any pixel that has non-values (areas outside AOI)
      rlc_params$nonFit <- 1
    } else if (any(etr_total[i,] == "99999")) {
      #skips any pixel that has a 99999 (noise)
      rlc_params$nonFit <- 2
    } else {
      # reset variable for the plot
      rlc_params$graf <- 0
      # set up the dataframe (pixel) to be processed
      pe3 <- as.data.frame(cbind(light, etr_total[i,]))
      names(pe3) <- c("light", "etr")
      # The tryCatch option allows the errors to be stored inside
      # an object and prevents the mclapply to stop when there is an error
      #      etr_sim <- get_etr_sim(alpha, ETRmax)
      
      minim.func <- function(rlc_params, data = pe3)
      {
        alpha <- rlc_params[[1]]
        etrmax <- rlc_params[[2]]
        return(sum((data$etr - etrmax * tanh((alpha * light) / etrmax)) ^ 2))
      }
      
      #function that finds the fit solution 
      #Optim with  L-BFGS-B is a general-purpose optimization based on a 
      #modification if the Nelder-Mead quasi Newton method that allows box 
      #constrains. This is important because both rETRmax and alpha need to be 
      #positive the tryCatch option allows the errors to be stored inside an 
      #object and prevents the mclapply to stop when there is an error
      etr_sim <-
        tryCatch(
          optim(
            par = c(alpha, ETRmax),
            fn = minim.func,
            control = list("maxit" = 1000000, pgtol = 0.01),
            method = "L-BFGS-B",
            lower = c(0,0), 
            upper = c(Inf, Inf),
            hessian = TRUE
          )
          , error = function(e)
            "NaN"
        )
      
      # if error message reset graf and nonFit
      if (is.character(etr_sim)) {
        rlc_params$nonFit <- 3
        rlc_params$graf <- 1
      }
      
      if (is.list(etr_sim)) {
        if (etr_sim$convergence != 0) {
          count_optim_errors <<- count_optim_errors + 1
          rlc_params$nonFit <- 4
          rlc_params$graf <- 1
        }
        
        if (etr_sim$convergence == 0) {
          rlc_params <- etr_convergence(etr_sim, rlc_params, pe3, model = 'jassby')
        }
        
      }
      
    } # end updating RLC parameters
    
    # store the parameters in the bigmemory object (rlc_matrix)
    rlc_matrix[i, ] <- unlist(rlc_params)[-length(rlc_params)]
    
    print_completeness(ncores, i, total)  # (only for one core)
    
    #to plot data and fitted curves when ncores=1 and it is a valid pixel
    if (ncores == 1 & rlc_params$graf == 0 & plots == 1 & i %% 5 == 0) {
      #dev.new()
      plot(pe3$light, pe3$etr, pch = 21,
           xlab = "Light", ylab = "rETR")
      lines(pe3$light, 
            rlc_params$etrmax2 * tanh((rlc_params$alpha2 * pe3$light) / rlc_params$etrmax2),
            col = "blue")
      legend("topright", legend = i)
      #dev.off()
    }
    
  }
  # ends myRLC()
  
  
  # mclapply exectutes the myRLC function above
  #print(system.time(
  #  mclapply(1:total, myRLC, mc.cores = ncores, light = light)
  #  )
  #)
  print("MCLAPPLY")
  mclapply(1:total, myRLC, mc.cores = ncores, light = light)
  print("Analysis finished!")
  
  pixels_info <- get_pixels(rlc_matrix, x2, y2)
  print_pixel_messages(pixels_info)
  
  # exports the images to the working directory
  export_images(rlc_matrix, x2) 
  
}



