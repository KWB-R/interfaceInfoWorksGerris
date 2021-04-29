#' Load the Infoworks Output file (csv)
#'
#' @param iw_output_folder Folder with InfoWorks csv-outpu
#' @param simulation_name Complete or unique part of the simulation name
#' 
#' @description The function looks up all files in the specified infoWorks output
#' folder. It chooses csv files that start with "link" and contain "ds" for 
#' donwstream and the specified simulation name. All parameter listed in the 
#' parameter conversion table are selected.
#' 
#' @return A list with all read csv Files
load_infoworks_output <- function(
  iw_output_folder, 
  simulation_name,
  pct
){
  # output parameters from parameter_conversion table
  output_paras <- 
    pct[
      !is.na(pct[,"id_infoworks"]),"id_infoworks"]
  
  # filter for the right files
  output_files <- list.files(path = iw_output_folder)
  output_files <- grep(pattern = simulation_name, x = output_files, value = TRUE)
  
  if(length(output_files) == 0){
    stop(paste("no Infoworks data named", simulation_name))
  }
  output_files <- grep(pattern = "\\.csv$", x = output_files, value = TRUE)
  output_files <- grep(pattern = "^Link", x = output_files, value = TRUE)
  output_files <- grep(pattern = "_ds_", x = output_files, value = TRUE)
  
  # load infoworks output data
  iw_out <- list()
  for(output_para in output_paras){
    this_file <- grep(pattern = output_para, x = output_files, value = T)
    if(length(this_file) == 0){
      warning("no data-file for parameter: ", output_para)
    } else {
      iw_out[[output_para]] <- 
        read.csv(file = paste0(iw_output_folder, this_file), 
                 header = T, sep = ",", dec = ".", stringsAsFactors = F)
      
      # only keep links listed in the outlet_conversion table
      ids <- ids_from_colnames(COLnames = colnames(iw_out[[output_para]]))
      missing_outlets <- which(!(outlet_conversion$upstream_link_id %in% ids))
      keep_those <- which(ids %in% outlet_conversion$upstream_link_id)
      iw_out[[output_para]] <- iw_out[[output_para]][c(1,2,keep_those)]
      
      message(output_para, " loaded")
    }
  }
  if(length(missing_outlets) > 0){
    warning(paste0("the following links are listed in the conversion table but not found in Infowokrs output: ", 
                   outlet_conversion$upstream_link_id[missing_outlets]))
  }
  iw_out
}


