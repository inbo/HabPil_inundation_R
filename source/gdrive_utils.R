# gdrive_utils.R content

# function to authenticate in Google Drive
authenticate_gdrive <- function() {
  message("Attempting Google Drive authentication...")
  preferred_email <- "stien.heremans@inbo.be" # <--- Set your email here
  
  auth_success <- FALSE # Flag to track overall authentication success
  
  # Attempt 1: Authenticate with preferred email
  message(paste("Attempting authentication with preferred email:", preferred_email))
  tryCatch({
    googledrive::drive_auth(
      email = preferred_email,
      cache = ".secrets"
      # scopes = "https://www.googleapis.com/auth/drive" # Add if specific scopes are needed
    )
    
    # Verify that the correct account was authenticated
    if (!is.null(googledrive::drive_user()$email) && googledrive::drive_user()$email == preferred_email) {
      message(paste("Google Drive authentication successful for:", preferred_email))
      auth_success <- TRUE
    } else {
      warning("Authentication completed, but not for the preferred account. Token might be for a different user.")
      auth_success <- FALSE # Treat as failure for preferred account
    }
    
  }, error = function(e) {
    warning(paste("Automated authentication for", preferred_email, "failed. Error:", conditionMessage(e)))
    auth_success <- FALSE
  })
  
  # If preferred authentication failed, ask user for interactive retry
  if (!auth_success) {
    user_choice <- tolower(readline("Do you want to try a general interactive Google Drive authentication? (y/n): "))
    
    if (user_choice == "y" || user_choice == "yes") {
      message("Attempting general interactive authentication...")
      tryCatch({
        # Attempt 2: General interactive authentication (will prompt in browser to choose account)
        googledrive::drive_auth(
          email = TRUE, # Forces prompt to choose an email if multiple are recognized
          cache = ".secrets"
          # scopes = "https://www.googleapis.com/auth/drive" # Add if specific scopes are needed
        )
        if (googledrive::drive_has_token()) {
          message(paste("General interactive authentication successful for:", googledrive::drive_user()$email))
          auth_success <- TRUE
        } else {
          warning("General interactive authentication did not result in a valid token.")
        }
      }, error = function(e) {
        warning(paste("General interactive authentication failed. Error:", conditionMessage(e)))
      })
    } else {
      message("Skipping interactive authentication. Google Drive operations may fail.")
    }
  }
  
  return(auth_success)
}


#' Force Download All Files from a Google Drive Folder
#'
#' Downloads every file from a specified Google Drive folder to a local directory,
#' overwriting any existing local files by default. This ensures the local
#' directory is an exact, up-to-date copy of the remote folder's contents.
#'
#' @param gdrive_folder_id The ID of the Google Drive folder.
#' @param local_path The path to the local directory where files will be saved.
#' @return Invisibly returns TRUE on success.
force_download_gdrive_folder <- function(gdrive_folder_id, local_path) {
  # Ensure the local directory exists
  if (!dir.exists(local_path)) {
    message("Creating local cache directory: ", local_path)
    dir.create(local_path, recursive = TRUE)
  }
  
  # Get a list of all files in the Google Drive folder
  message("Listing all files in Google Drive folder...")
  gdrive_files <- googledrive::drive_ls(googledrive::as_id(gdrive_folder_id))
  
  if (nrow(gdrive_files) == 0) {
    warning("No files found in the specified Google Drive folder.")
    return(invisible(FALSE))
  }
  
  message(paste("Downloading", nrow(gdrive_files), "file(s) to local cache (overwriting existing)..."))
  
  # Download each file, overwriting by default
  for (i in 1:nrow(gdrive_files)) {
    file <- gdrive_files[i, ]
    target_path <- file.path(local_path, file$name)
    message(paste("-> Downloading:", file$name))
    googledrive::drive_download(file, path = target_path, overwrite = TRUE)
  }
  
  message("All remote files have been downloaded.")
  invisible(TRUE)
}


# function to download google drive files for shapefiles (multiple files needed)
download_shapefile_components <- function(gdrive_folder_id, target_shp_filename, base_local_cache_dir, local_target_subfolder) {
  message("\nProcessing dataset for target: ", target_shp_filename, " from GDrive folder ID: ", gdrive_folder_id)
  message("Target local subfolder: ", local_target_subfolder)
  
  shapefile_basename <- tools::file_path_sans_ext(target_shp_filename)
  if (shapefile_basename == "" || shapefile_basename == target_shp_filename) {
    stop("Could not determine a valid base name from target_shp_filename: ", target_shp_filename)
  }
  message("Shapefile base name for filtering: ", shapefile_basename)
  
  dataset_local_dir <- file.path(base_local_cache_dir, local_target_subfolder)
  if (!dir.exists(dataset_local_dir)) {
    message("Creating local directory for this dataset: ", dataset_local_dir)
    dir.create(dataset_local_dir, recursive = TRUE, showWarnings = TRUE)
  } else {
    message("Using existing local directory for this dataset: ", dataset_local_dir)
  }
  
  message("Listing files in Google Drive folder ID: ", gdrive_folder_id, "...")
  tryCatch({
    drive_files <- googledrive::drive_ls(as_id(gdrive_folder_id))
  }, error = function(e) {
    stop("Failed to list files in Google Drive folder ID '", gdrive_folder_id, "'. Error: ", conditionMessage(e))
  })
  
  if (nrow(drive_files) == 0) {
    message("No files found in Google Drive folder ID: ", gdrive_folder_id)
    return()
  }
  
  files_to_download <- drive_files[startsWith(drive_files$name, shapefile_basename), ]
  
  if (nrow(files_to_download) == 0) {
    message("No files found matching the base name '", shapefile_basename, "' in the GDrive folder.")
    main_shp_file_info <- drive_files[drive_files$name == target_shp_filename, ]
    if (nrow(main_shp_file_info) == 0) {
      message("The main target file '", target_shp_filename, "' was also not found in the GDrive folder listing.")
    } else {
      message("The main target file '", target_shp_filename, "' was found, but no other associated files with the base name '", shapefile_basename, "' were found.")
    }
    return()
  }
  
  message("Found ", nrow(files_to_download), " file(s) matching base name '", shapefile_basename, "':")
  print(files_to_download$name)
  
  # Loop through identified files and download only if missing locally
  for (i in 1:nrow(files_to_download)) {
    file_info <- files_to_download[i, ]
    local_file_path <- file.path(dataset_local_dir, file_info$name)
    if (file.exists(local_file_path)) {
      message("Local file already exists: '", file_info$name, "'. Skipping download.")
    } else {
      message("Downloading '", file_info$name, "' to '", local_file_path, "'...")
      tryCatch({
        googledrive::drive_download(
          file = as_id(file_info$id),
          path = local_file_path
        )
        message("Successfully downloaded '", file_info$name, "'.")
      }, error = function(e) {
        warning("Failed to download '", file_info$name, "'. Error: ", conditionMessage(e))
      })
    }
  }
  message("Finished processing dataset for target: ", target_shp_filename)
}


# function to actually achieve the Google Drive download
perform_project_data_acquisition <- function(datasets_config, base_cache_dir) {
  message("\n--- Starting Project Data Acquisition Process ---")
  
  downloaded_dataset_paths <- list() # Initialize list to store paths
  
  if (!exists(".gdrive_authenticated") || !.gdrive_authenticated) {
    .gdrive_authenticated <<- authenticate_gdrive()
    if (!.gdrive_authenticated) {
      message("GDrive authentication failed. Aborting data acquisition.")
      return(invisible(NULL)) 
    }
  } else {
    auth_check_ok <- tryCatch({ googledrive::drive_user(); TRUE }, error = function(e) { FALSE })
    if (!auth_check_ok) {
      message("GDrive token might be stale or invalid. Re-authenticating...")
      .gdrive_authenticated <<- authenticate_gdrive()
      if (!.gdrive_authenticated) {
        message("GDrive re-authentication failed. Aborting data acquisition.")
        return(invisible(NULL))
      }
    } else {
      message("GDrive authentication previously successful and token appears valid.")
    }
  }
  
  if (!dir.exists(base_cache_dir)) {
    message("Base local cache directory '", base_cache_dir, "' does not exist. Creating it.")
    dir.create(base_cache_dir, recursive = TRUE, showWarnings = TRUE)
  } else {
    message("Using existing base local cache directory: ", base_cache_dir)
  }
  
  if (length(datasets_config) == 0) {
    message("No datasets configured for download.")
    return(invisible(downloaded_dataset_paths)) # Return empty list
  }
  
  for (dataset_index in seq_along(datasets_config)) {
    dataset <- datasets_config[[dataset_index]]
    
    required_elements <- c("gdrive_id", "target_filename")
    if (!all(required_elements %in% names(dataset))) {
      warning("Skipping dataset at index ", dataset_index, 
              " due to missing configuration (must include 'gdrive_id' and 'target_filename'). ",
              "Provided keys: ", paste(names(dataset), collapse=", "))
      next
    }
    
    dataset_name_log <- ifelse(!is.null(dataset$name) && nzchar(dataset$name), dataset$name, dataset$target_filename)
    message("\n--- Configuring download for: ", dataset_name_log, " ---")
    
    local_target_subfolder_name <- NULL 
    
    if (!is.null(dataset$local_subfolder)) { 
      local_target_subfolder_name <- dataset$local_subfolder 
      message("User-defined local subfolder specified: '", local_target_subfolder_name, 
              "'. This will be used relative to base_cache_dir.")
    } else {
      message("Local subfolder not specified in config. Attempting to use Google Drive folder name for GDrive ID: ", dataset$gdrive_id)
      fetched_gdrive_folder_name <- tryCatch({
        folder_info <- googledrive::drive_get(as_id(dataset$gdrive_id))
        trimws(folder_info$name) 
      }, error = function(e) {
        warning("Could not fetch Google Drive folder name for ID '", dataset$gdrive_id, "'. Error: ", conditionMessage(e))
        NULL 
      })
      
      if (!is.null(fetched_gdrive_folder_name) && nzchar(fetched_gdrive_folder_name)) {
        local_target_subfolder_name <- fetched_gdrive_folder_name
        message("Using fetched Google Drive folder name as local subfolder: '", local_target_subfolder_name, "'")
      } else {
        local_target_subfolder_name <- tools::file_path_sans_ext(dataset$target_filename)
        warning("Could not determine local subfolder from GDrive folder name (or it was empty) and none was specified in config. ",
                "Using fallback based on target filename: '", local_target_subfolder_name, "'")
      }
    }
    
    if (is.null(local_target_subfolder_name)) {
      warning("Failed to determine a valid local subfolder name for dataset '", dataset_name_log, "'. Skipping this dataset.")
      next 
    }
    
    # Construct the full path to the directory for this dataset
    full_path_for_this_dataset_dir <- file.path(base_cache_dir, local_target_subfolder_name)
    
    # Determine the key for the returned list
    # Use dataset$name if provided, otherwise use the base of target_filename
    key_for_list <- if (!is.null(dataset$name) && nzchar(dataset$name)) {
      dataset$name
    } else {
      tools::file_path_sans_ext(dataset$target_filename)
    }
    downloaded_dataset_paths[[key_for_list]] <- full_path_for_this_dataset_dir
    
    # Call the download function
    download_shapefile_components(
      gdrive_folder_id = dataset$gdrive_id,
      target_shp_filename = dataset$target_filename,
      base_local_cache_dir = base_cache_dir, # Pass the overall base cache dir
      local_target_subfolder = local_target_subfolder_name # Pass the determined subfolder for this specific dataset
    )
  }
  
  message("\n--- Project Data Acquisition Process Completed ---")
  message("Check the specified subdirectories within '", base_cache_dir, "' for your downloaded files.")
  
  return(invisible(downloaded_dataset_paths)) # Return the list of paths invisibly
}
