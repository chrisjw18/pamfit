#' @title Import Light Steps File
#' @description Imports the file with the light curve light steps
#' @details The file to be imported needs to be named "light.txt"
#' By default, light values must be separated by a comma
#' @param path a character vector with the full path name of the
#' directory where file light.txt is located
#' @param sep value separator character; comma by default
#' @return numeric vector with light steps
#' @keywords internal
import_light_steps <- function(path, sep = ',',i_light,f_light) {
  if(missing(i_light)|missing(f_light)){
    light_file <- paste0(path, '/light.txt')
    light <- scan(light_file,  sep = sep, quiet = TRUE)
  }else{
    light_file <- paste0(path, '/light.txt')
    light <- scan(light_file,  sep = sep, quiet = TRUE)
    light<-light[i_light:f_light]
  }
  light
}



#' @title Light Steps Error
#' @description error message for incorrect light steps in light.txt
#' @keywords internal
error_light_steps <- function() {
  paste(
    "The number of light steps does not match\n",
    "the number of PSII efficiency files.\n",
    "Please check the light.txt file to see\n",
    "if the number of light steps match the number of Fv/Fm files"
  )
}



#' @title Clean Fv,Fm file
#' @description removes Fv_Fm values that are either negative or
#' @param fvfm_file a character vector with the full path
#' name of the fv-fm file
#' @return cleaned fv-fm file
#' @keywords internal
clean_fvfm_file <- function(fvfm_file) {
  temp <- read.table(file = fvfm_file)
  temp <- replace(temp, temp < 0.000001, 99999)
  temp <- replace(temp, temp > 0.85, 99999)
  temp
}


#this is the original function above, kept it for the moment, remove if not needed
#'
#' @title Import Fv,Fm files
#' @description Imports the Fv-Fm files
#' @param path a character vector with the full path name of the
#' directory where fv-fm files are located
#' @param light numeric vector of light steps
#' @keywords internal
import_fvfm_files <- function(path, light,i_light,f_light) {
  if(missing(i_light)|missing(f_light)){
   fvfm_names <- list.files(path, pattern = "fv_fm")
   fvfm <- paste0(path, "/", fvfm_names)
  }else{
    fvfm_names <- list.files(path, pattern = "fv_fm")
    fvfm <- paste0(path, "/", fvfm_names)
    fvfm <-fvfm[i_light:f_light]
  }
   num_fvfm <- length(fvfm)

   print("Importing Fv/Fm files")
   Sys.sleep(1)
   print(paste(num_fvfm, "files will be imported"))

   clean_fvfm <- vector(mode = "list", num_fvfm)
   if (length(light) == num_fvfm) {
     for (y in 1:num_fvfm) {
       clean_fvfm[[y]] <- clean_fvfm_file(fvfm[y])
       print(paste(round(y / num_fvfm * 100, 0), "%"))
     }
     names(clean_fvfm) <- paste0("fv_fm", 1:num_fvfm)
     #print(str(clean_fvfm))
     #readline(prompt="Press [enter] to continue")
     return(clean_fvfm)
   } else {
     stop(error_light_steps())
   }
 }




#' @title Calculate Absorbance
#' @description Estimates rETR using absorbance information when
#' it is available
#' @param path a character vector with the full path name of the
#' directory where file fv-fm files are located
#' @param ab logical indicating whether absorbance is present
#' @keywords internal
get_absorbance <- function(path, ab) {
  if (ab)
  {
    print("Calculating Absorbance")
    Sys.sleep(1)
    nir <- read.table(paste0(path, "/NIR.txt"))
    red <- read.table(paste0(path, "/RED.txt"))
    abso <- 1 - red / nir
    write.table(abso,
      file = paste0(path,'absorbance.txt'),
      col.names = FALSE,
      row.names = FALSE
    )
  } else {
    abso <- 0.84
    print(sprintf("Default Absorbance: %s", abso))
  }
  abso
}



#' @title Clean ETR pixels
#' @description Some of the Fv/Fm will be noise pixels.
#' The second line of each ETR object replaces these pixels with
#' NA values to be skipped during further calculations.
#' @param fv_fm_file name of fv_fm file to be cleaned
#' @param light_value one light step value
#' @param abso absorbance value, either 0.84 or a value coming from the PAM absorbance
#'  calculation,i.e. Abs. = 1 - Red/NIR.
#' @return cleaned FvFm file
#' @keywords internal
clean_etr <- function(fv_fm_file, light_value, abso) {
  temp_etr <- light_value * 0.5 * abso * fv_fm_file
  temp_etr <- replace(temp_etr, is.na(temp_etr), NaN)
  temp_etr <- replace(temp_etr, temp_etr > 99999, 99999)
  temp_etr
}


#' @title Calculate rETR values
#' @description Cycle to calculate the rETR values
#' @description Some of the Fv/Fm will be noise pixels.
#' The second line of each ETR object replaces these pixels with
#' NA values to be skipped during further calculations.
#' @param fv_fm list with cleaned fv_fm files
#' @param light numeric vector of light steps
#' @param abso absorbance value
#' @return list with etr matrices
#' @keywords internal
get_retr <- function(fv_fm, light, abso) {
  print("Calculating rETR")
  Sys.sleep(1)

  list_etr <- vector(mode = "list", length(fv_fm))
  for (y in 1:length(light)) {
    list_etr[[y]] <- clean_etr(fv_fm[[y]], light[y], abso)
    print(paste(round(y / length(light) * 100, 0), "%"))
  }
  names(list_etr) <- paste0("etr", 1:length(fv_fm))
  list_etr
}


#' @title Image Dimension
#' @description Get image dimensions as total number of pixels
#' @param etr_matrix image matrix
#' @return list with \code{x2}, \code{y2}, and \code{total} values
#' @keywords internal
image_dimension <- function(etr_matrix) {
  x2 <- ncol(etr_matrix)
  y2 <- nrow(etr_matrix)
  total <- x2 * y2
  list(x2 = x2, y2 = y2, total = total)
}


#' @title ETR bigmatrix object
#' @description creates an empty big.matrix object to store
#' the rETR values of all the images it uses the bigmemory library
#' @param path input directory
#' @param total number of rows
#' @param light steps
#' @keywords internal
etr_bigmatrix <- function(path, total, light) {
  big.matrix(
    total, # number of rows
    length(light) # number of columns
  )
}


#this was an older version that created a temp file and often returned an error
#etr_bigmatrix_back <- function(path, total, light) {
#  filebacked.big.matrix(
#    total, # number of rows
#    length(light), # number of columns
#    backingfile = NULL, #"etr.bin",
#    descriptorfile = NULL, #"etr.desc",
#    backingpath = NULL #path
#  )
#}



#' @title Transform ETR
#' @description Cycle to fill the big.matrix object with
#' the rETR values with big images.
#' @details This will take same time to execute but having
#' all the rETR values in one big.matrix object facilitates the
#' calculations using \code{\link{mclapply}}, i.e. paralell computing.
#' @param etr_total big matrix object
#' @param list_etr list with etr matrices
#' @param img_dim image dimensions
#' @param path
#' @keywords internal
transform_etr <- function(etr_total, list_etr, img_dim, path)
{
  print("Transforming the etr matrices")
  Sys.sleep(1)
  counter <- 1
  partial <- 0
  for (i in 1:img_dim$y2)
  {
    for (v in 1:img_dim$x2)
    {
      etr_temp2 <- numeric()
      for (y in 1:length(list_etr))
      {
        a <- list_etr[[y]][i,v]
        etr_temp2 <- c(etr_temp2, a)
      }
      etr_total[counter, ] <- etr_temp2
      counter <- counter + 1
      partial <- counter - 1
      final <- (partial / img_dim$total) * 100
      print(paste(round(final, 3), "%"))
    }
  }
  # export big matrix
  # not sure this is needed
  write.big.matrix(
    etr_total,
    file.path(path, "etr_matrix.txt"))
}



#' @title Import Data Files
#' @description Imports the Fv_Fm files and calculates
#' the equivalent rETR objects
#' @param input_dir Input directory. Name of directory containing all the
#' input files: light, fv_fm, NIR, RED.
#' @param ab logical indicating whether absorbance parameter is present
#' When available, \code{ab} is used to estimate rETR.
#' @param light_dir location of the light file. Defaults to the input_dir folder.
#' @param output_dir Output directory. An alternate directory to write the
#' output file to (defaults to the directory of the input file).Not implemented yet.
#' @return list with \code{abso}, \code{x2}, \code{y2},
#' \code{total}, and \code{light} values
#' @export
import_data <- function(input_dir = ".", ab = FALSE,
                        light_dir=input_dir,output_dir = NULL,
                        initial_light,final_light) {
  # processing data
  light <- import_light_steps(light_dir,i_light = initial_light,f_light = final_light)
  fv_fm <- import_fvfm_files(input_dir, light,i_light = initial_light,f_light = final_light)
  abso <- get_absorbance(input_dir, ab)
  etr <- get_retr(fv_fm, light, abso)
  img_dim <- image_dimension(etr$etr1)
  etr_total <<- etr_bigmatrix(input_dir, img_dim$total, light)
  transform_etr(etr_total, etr, img_dim, input_dir)

  # return
  list(abso = abso,
       x2 = img_dim$x2,
       y2 = img_dim$y2,
       total = img_dim$total,
       light = light)
}


