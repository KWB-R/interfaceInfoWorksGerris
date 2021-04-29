################################################################################
############ functions needed for the infoworks-Gerris-Interface ###############
################################################################################

# 1. loading inworks Output ----------------------------------------------------
#' Title
#'
#' @param iw_output_folder
#' @param simulation_name
#'
#' @return
#' @export
#'
#' @examples
load_infoworks_output <- function(
  iw_output_folder,
  simulation_name
){
  # output parameters from parameter_conversion table
  output_paras <-
    parameter_conversion[
      !is.na(parameter_conversion[,"id_infoworks"]),"id_infoworks"]

  # filter for the right files
  output_files <- list.files(path = iw_output_folder)
  output_files <- grep(pattern = simulation_name, x = output_files, value = T)
  if(length(output_files) == 0){
    stop(paste("no Infoworks data named", simulation_name))
  }
  output_files <- grep(pattern = paste0("\\.csv$"), x = output_files, value = T)
  output_files <- grep(pattern = paste0("^Link"), x = output_files, value = T)
  output_files <- grep(pattern = paste0("_ds_"), x = output_files, value = T)

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
# 2. Flow correction -----------------------------------------------------------
correct_flow <- function(
  input_data,
  data_columns = 3:ncol(input_data),
  flow_threshold = 0.003
){
  # splitting time columns and flow data
  time_cols <- input_data[-data_columns]
  flow_mat <- as.matrix(input_data[,data_columns])

  # correct the flow
  flow_mat[flow_mat < flow_threshold] <- 0

  # joining time columns and corrected data
  cbind(time_cols, as.data.frame(flow_mat))
}

#' Keep overflows
#'
#' @param in_list in_list
#' @param flowID_name flowID_name
#'
#' @return filtered list
#' @keywords internal
#' @noRd
keep_overflows <- function(in_list, flowID_name){
  events <- which(in_list[[flowID_name]] != 0)
  beforeEvents <- events - 1
  afterEvents <- events + 1
  events <- unique(c(events, beforeEvents, afterEvents))
  if(any(events < 1)){
    events <- events[-which(events < 1)]
  }
  in_list[c(1, events),]
}

# 3. Change of Timesteps -------------------------------------------------------
adapt_timestep <- function(
  data_in,
  time_format = "%d/%m/%Y %H:%M:%S",
  time_column = 1,
  skip_hours = 24, # in hours (Infoworks needs time to warm up --> not meant for gerris)
  data_columns = 3:ncol(data_in),
  timestep_out = 15 # minutes
){
  # get time step information of infoworks simulation
  sim_beg <- as.POSIXct(data_in[1:2,time_column], format = time_format)
  timestep_in <- as.numeric(difftime(sim_beg[2],sim_beg[1], units = "mins"))

  sim_beg <- sim_beg + 60 * 60 * skip_hours

  # build table with average values in new time steps
  table_out <-
    as.data.frame(sapply(X = data_in[,data_columns], averaging_values,
                         timestep_in = timestep_in,
                         timestep_out = timestep_out))

  # create new time column
  time_table <-
    data.frame("Zeit" =
                 0:(nrow(table_out)-1) * timestep_out * 60  + sim_beg[1])

  # combine new time column with new average data
  cbind(time_table, table_out)
}

# 3a. Integrated function in Change of Timesteps
averaging_values <- function(
  values,
  timestep_in,
  timestep_out){
  # one-minute-vector
  v_1 <- rep(values, each = timestep_in)

  # averaged vector in new time steps
  sapply(1:floor(length(v_1) / timestep_out), function(x){
    mean(v_1[(1:timestep_out) + timestep_out * (x-1)], na.rm = T)
  })
}
# 4. Merge outlets that are not defined in Gerris with those that are ----------
integrate_missing_outlets <- function(
  data_input
){
  # all outlets with missing ID for Gerris
  missing <- outlet_conversion$upstream_link_id[
    which(is.na(outlet_conversion$gerris_id))]
  # pairing with the closest available outlet
  add_to <- lapply(missing,
                   closest_available_outlet,
                   outlet_df = outlet_conversion)

  # Adding the missing outlets mass flow to the closest available outlet
  # and deleting the missing outlet column
  for(x in add_to){
    data_input <- lapply(data_input, function(df){
      real_names <- ids_from_colnames(COLnames = colnames(df))
      df[,real_names == x["to"]] <- rowSums(df[,real_names %in% x])
      df[-which(real_names == x["from"])]
    })
    message(x["from"], " (",
            outlet_conversion$surface_water[
              outlet_conversion$upstream_link_id == x["from"]],
            " at km ",
            outlet_conversion$water_body_km[
              outlet_conversion$upstream_link_id == x["from"]],
            ") is now part of ", x["to"], " (",
            outlet_conversion$surface_water[
              outlet_conversion$upstream_link_id == x["to"]],
            " at km ",
            outlet_conversion$water_body_km[
              outlet_conversion$upstream_link_id == x["to"]],
            ")")
  }
  data_input
}
# 4a. Integrated function in intergrate missing outlets
closest_available_outlet <- function(
  missing_outlet,
  outlet_df
){
  # missing outlet inforamtion
  moi <- outlet_df[outlet_df$upstream_link_id == missing_outlet,]

  # filter for surface water and available outlets
  sw <- moi$surface_water
  av_outlets <- outlet_df[outlet_df$surface_water == sw &
                            !is.na(outlet_df$gerris_id),]

  # minimum distance between missing outlet and available outlets
  all_dist <- abs(av_outlets$water_body_km - moi$water_body_km)
  min_dist <- min(all_dist)

  # selevt outlet with minimal distance (if more than one outlet are equally
  # far away, the first one is selected)
  c("from" = missing_outlet,
    "to" = av_outlets$upstream_link_id[which(all_dist == min_dist)[1]])
}
# 5. calculating concentration from mass flows
get_concentrations <- function(input_data, flowID_infoworks){
  flow <- which(names(input_data) == flowID_infoworks)

  flow_table <- input_data[[flow]]
  input_data <- input_data[-flow]

  # return list with flow and concentrations -----------------------------------
  c("flow" = list(flow_table),
    lapply(input_data, function(table_in){
      # concentration is mass flow / flow * 1000 (from kg/s with m?/s to ng/L)
      concentration_mat <-
        as.matrix(table_in[,-1] / flow_table[,-1] * 1000) # without time columns
      concentration_mat[!is.finite(concentration_mat)] <- 0

      cbind("Zeit" = table_in[,1],
            as.data.frame(concentration_mat)) # add time colums
    }))
}
# 6. correction of Kjeldahl Nitrogen -------------------------------------------
kn_correction <- function(input_data){
  tkn <- grep(pattern = "tkn",x = tolower(names(input_data)))
  nh4 <- grep(pattern = "nh4",x = tolower(names(input_data)))
  mat1 <- as.matrix(input_data[[tkn]][,-1])
  mat2 <- as.matrix(input_data[[nh4]][,-1])
  to_small <- which(mat1 < mat2)
  mat1[to_small] <- mat2[to_small]
  mat1

  cbind("DatumUhrzei" = input_data$mftkntot[,1], as.data.frame(mat1))
}
# 7. no function here, directly processed in combining function ----------------
# 8. Renaming parameter IDs ----------------------------------------------------
rename_IDs <- function(input_data){
  id_old <- parameter_conversion[,"id_infoworks"]
  id_new <- parameter_conversion[,"id_gerris"]
  # parameters that are not defined in the id_gerris column are not to be used
  remove_parameters <- id_old[is.na(id_new)]
  to_change <- which(names(input_data) %in% id_old &
                       !(names(input_data) %in% remove_parameters))

  for(changing in to_change){
    names(input_data)[changing] <-
      id_new[which(id_old == names(input_data)[changing])]
  }

  rm_entry <- which(names(input_data) %in% remove_parameters)
  if(length(rm_entry > 0)){
    input_data <- input_data[-rm_entry]
    message("Parameters: ",  remove_parameters,
            " deleted --> not defined in gerris ID column")
  }

  input_data
}
# 9. Add constant values -------------------------------------------------------
add_constant_values <- function(
  input_data
){
  rows_constant <- which(!is.na(parameter_conversion$constant_value))
  constant_parameters <- parameter_conversion$id_gerris[rows_constant]
  constant_value <- parameter_conversion$constant_value[rows_constant]
  time_column <- input_data[[1]][,1]

  for(i in 1:length(constant_value)){
    df_process <- as.data.frame(
      matrix(data = constant_value[i],
             nrow = nrow(input_data[[1]]),
             ncol = ncol(input_data[[1]])-1))
    df_process <- cbind(time_column, df_process)
    colnames(df_process) <- colnames(input_data[[1]])

    input_data[[constant_parameters[i]]] <- df_process
  }
  input_data
}
# 10. Checking if the concentration lies within the gerris range ---------------
check_parameter_range <- function(
  input_data
){
  para_names <- names(input_data)

  for(i in 1:length(para_names)){
    # extract data matrix
    mat_process <- as.matrix(input_data[[i]][-1])
    # Min and Max values from parameter conversion table (if value is NA it
    # means there was no limit defined --> set to infinite)
    min_allowed <- parameter_conversion$min_gerris[
      which(parameter_conversion$id_gerris == para_names[i])]
    if(is.na(min_allowed)){min_allowed <- -Inf}
    max_allowed <- parameter_conversion$max_gerris[
      which(parameter_conversion$id_gerris == para_names[i])]
    if(is.na(max_allowed)){max_allowed <- Inf}

    # change values that are not within the Gerris limit
    to_small <- which(mat_process < min_allowed)
    to_high <- which(mat_process > max_allowed)
    if(length(to_small) > 0){
      mat_process[to_small] <- min_allowed
      warning(para_names[i], ": ", length(to_small),
              " values below Gerris limit and set to ", min_allowed)
    }
    if(length(to_high) > 0){
      mat_process[to_high] <- max_allowed
      warning(para_names[i], ": ", length(to_high),
              " values above Gerris limit and set to ", max_allowed)
    }

    # round to 3 significant digits
    mat_process <- signif(x = mat_process, digits = 3)

    # combine again
    input_data[[i]] <-  cbind("Zeit" = input_data[[i]][,1],
                              as.data.frame(mat_process))
  }
  input_data
}
# 11. Reshaping from parameter-tables to outlet-tables -------------------------
data_per_outlet <- function(input_data){

  outlet_names <- unique(unlist(lapply(input_data, colnames)))[-1]

  identical_outlets <- lapply(outlet_names, function(outlet){
    sapply(input_data, function(df){
      which(colnames(df) == outlet)
    })
  })

  names(identical_outlets) <- outlet_names

  lapply(identical_outlets, function(outlet){

    # per outlet look for the correct column in each parameter table
    df_out <- data.frame(mapply(function(n_para, n_out){
      input_data[[n_para]][[n_out]]
    },
    n_para = names(outlet),
    n_out = outlet))
    cbind("Zeit" = input_data[[1]][,1], df_out)
  })
}
# 12. small function to help creating a column with gerris outlet ID -----------
find_positions <- function(v_original, v_new){
  sapply(v_original, function(x){
    which(v_new == x)
  })
}
# 14. Reshaping to Gerris format -----------------------------------------------
build_gerris_table <- function(data_input){

  function_list <- lapply(data_input, function(x){
    all_parameters <-
      colnames(x)[which(!(colnames(x) %in% c("Zeit", "RbId")))]
    # reshape table
    function_table <- gather(x, ParamId, Wert,  all_of(all_parameters))
    # order by time
    function_table[order(function_table[,"Zeit"]),
                   c("RbId", "Zeit", "ParamId", "Wert")]
  })
  do.call(rbind, function_list)
}
# 15. Change time to gerris format ---------------------------------------------
change_time_format <- function(input_data, new_format = "%d.%m.%Y %H:%M"){
  input_data[,"Zeit"] <-
    format(input_data[,"Zeit"], new_format)
  input_data
}
# 16. Save interface output ----------------------------------------------------
save_gerris_input <- function(input_data, gerris_input_folder, output_filename){
  write.table(x = input_data,
              file = paste0(gerris_input_folder, output_filename, ".csv"),
              sep = ";",
              dec = ",",
              row.names = FALSE,
              col.names = c(colnames(input_data)[1:3], ""), quote = F)
}
# small functions (multiple times used) -----------------------------------------
ids_from_colnames <- function(COLnames){
  # Link IDs auf den Spaltennamen
  # Link IDs die mit einer Nummer anfangen bekommen von R ein X davor gesetzt
  # dieses muss entfernt werden
  link_ids <- COLnames
  rm_x <- grep(pattern = "^X[0-9]", link_ids)
  link_ids[rm_x] <-  substr(link_ids[rm_x], start = 2, stop = 20)
  link_ids
}


# special functions for more information about concentrations above
# gerris concentration limit --> see concentrations and the according flow
# iw_gerris_interace needs to be stopped at "5" to obtain the results table
check_high_concentration <- function(
  result,
  parameter_conversion,
  parameterID_infoworks,
  flowID_infoworks = "flow"
){
  print("for checking the concentration the iw_gerris_interface needs to
      be stopped at position 5")

  df <- result[[parameterID_infoworks]]
  gerris_limit <- parameter_conversion$max_gerris[which(
    parameter_conversion$id_infoworks == parameterID_infoworks)]
  if(length(gerris_limit) == 0){
    print("no limit found: check parameter name")
  }
  high <- lapply(2:ncol(df), function(x){
    here <- which(df[[x]] > gerris_limit )
    if(length(here) > 0){
      here
    } else {
      NA
    }
  })

  data.frame(
    "Parameter" =
      unlist(lapply(which(!is.na(high)), function(x){
        df[high[[x]], x+1]
      })),
    "flow" =
      unlist(lapply(which(!is.na(high)), function(x){
        result[[flowID_infoworks]][high[[x]], x+1]
      })),
    "outlet" =
      unlist(lapply(which(!is.na(high)), function(x){
        rep(colnames(df)[x+1], length(high[[x]]))
      }))
  )
}



