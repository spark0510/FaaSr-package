faasr_test <- function(){

  svc <- .faasr_get_svc()
  faasr_wd <- svc$wd
  if (!dir.exists(faasr_wd)){
    faasr_wd <- getwd()
  }
  faasr <- svc$json
  cred <- faasr_collect_sys_env(faasr,svc$cred)
  faasr <- faasr_replace_values(faasr, cred)

  if (!dir.exists(faasr_data)){
    cli_alert_danger("faasr_data directory no found")
    return("")
  }
  
  setwd("faasr_data")

  if (dir.exists("temp")){
    unlink("temp", recursive=TRUE)
  }
  dir.create("temp/faasr_state_info", recursive=TRUE)
  
  cli_alert_success("Create test temporary folders")

  for (i in list.files("R")){
    source(paste0("R/",i))
  }

  cli_alert_success("Soured R files")

  setwd("temp")
  
  download.file("https://raw.githubusercontent.com/FaaSr/FaaSr-package/main/schema/FaaSr.schema.json", "FaaSr.schema.json")

  result <- faasr_test_start(faasr, faasr_wd)

  if (result == TRUE){
    setwd(faasr_wd)
    cli_alert_success("Function execution successfully")
  } else {
    setwd(faasr_wd)
    cli_alert_info("Wrong result: test close")
  }
}
.faasr_user$operations$faasr_test <- faasr_test

faasr_test_start <- function(faasr, faasr_wd){
  #clc <- function() cat(rep("\n", 30))
  cli::cli_h1("")

  current_func <- faasr$FunctionInvoke

  cli::cli_h2(paste0("Start testing: ",current_func))
  if (!dir.exists(current_func)){
    dir.create(current_func)
  }
  
  setwd(current_func)

  if (!dir.exists("logs")){
    dir.create("logs")
  }

  result <- faasr_configuration_check(faasr)
  if (result != TRUE){
    if (result == "next"){
      return(TRUE)
    } else{
      cli_alert_danger(result)
      return("")
    }
  }
  cli_alert_success("Configuration checked")

  check <- try(faasr_dependency_install(faasr, current_func), silent=TRUE)
  cli_alert_success("Dependencies installed")

  result <- faasr_user_function_check(faasr)
  if (result != TRUE){
    cli_alert_danger(result)
    return("")
  }
  cli_alert_success("User function checked")

  next_funcs <- faasr$FunctionList[[faasr$FunctionInvoke]]$InvokeNext
  if (is.null(next_funcs)){
    setwd("..")
    return(TRUE)
  }

  cat("\f")
  cli_alert_info(paste0("Success testing",current_func))

  for (next_func in next_funcs){
    faasr$FunctionInvoke <- next_func
    setwd("..")
    cli_alert_info("Trigger Next functions")
    result <- faasr_test_start(faasr, faasr_wd)
    if (result == ""){
      return("")
    }
  }

  return(TRUE)
}



faasr_user_function_check <- function(faasr){

    func_name <- faasr$FunctionList[[faasr$FunctionInvoke]]$FunctionName
    user_function <- try(get(func_name), silent=TRUE)

    if (is(user_function, "try-error")){
      return(paste0("Can't find User function ",func_name))
    }

    user_call <- trimws(deparse(user_function))
    for (i in 1:length(user_call)){
      user_call[i] <- gsub("FaaSr::faasr_put_file", "faasr_mock_put_file", user_call[i])
      user_call[i] <- gsub("faasr_put_file", "faasr_mock_put_file", user_call[i])

      user_call[i] <- gsub("FaaSr::faasr_get_file", "faasr_mock_get_file", user_call[i])
      user_call[i] <- gsub("faasr_get_file", "faasr_mock_get_file", user_call[i])

      user_call[i] <- gsub("FaaSr::faasr_delete_file", "faasr_mock_delete_file", user_call[i])
      user_call[i] <- gsub("faasr_delete_file", "faasr_mock_delete_file", user_call[i])

      user_call[i] <- gsub("FaaSr::faasr_log", "faasr_mock_log", user_call[i])
      user_call[i] <- gsub("faasr_log", "faasr_mock_log", user_call[i])      
    }   
    user_function <- eval(parse(text=user_call))

    user_args <- faasr_get_user_function_args(faasr) 

    faasr_result <- tryCatch({expr=do.call(user_function, user_args)}, error=function(e){e})

    if (is(faasr_result, "error")){
      check <- FALSE
      err_code <- deparse(faasr_result$call)

      for (i in 1:length(user_call)){
        if (err_code[1] == user_call[i]){
          check <- TRUE
          return(paste0("Line", i-1, " : ", faasr_result$message))
        }
      }   
      if (!check){
        return(faasr_result$message)
      }
    } else {
      write.table("TRUE", file=paste0("../faasr_state_info/", faasr$FunctionInvoke, ".done"), row.names=F, col.names=F)
      return(TRUE)
    }
}

faasr_configuration_check <- function(faasr){
  
  file.copy(from="../FaaSr.schema.json", to="FaaSr.schema.json")
  faasr <- try(faasr_parse(toJSON(faasr,auto_unbox=TRUE)), silent=TRUE)
  if (is(faasr, "error")){
    # schema errors
    return(attr(faasr, "condition"))
  } 

  for (datastore in names(faasr$DataStores)){
    endpoint <- faasr$DataStores[[datastore]]$Endpoint
    if ((!is.null(endpoint)) && (endpoint != "") && !startsWith(endpoint, "https")){
      # data store errors
      return("data store errors")
    }
  }

  log_server_name <- faasr$DefaultDataStore
  if (!log_server_name %in% names(faasr$DataStores)){
    # default server errors
    return("default server errors")
  }

  if (!is.null(faasr$LoggingDataStore)){
    log_server_name <- faasr$LoggingDataStore
    if (!log_server_name %in% names(faasr$DataStores)){
      # logging server errors
      return("logging server errors")
    }
  }

  pre <- try(faasr_check_workflow_cycle(faasr), silent=TRUE)
  if (is(pre, "try-error")){
    # cycle/unreachable faasr_state_info errors
    return(attr(pre, "condition"))
  }
  
  if (length(pre)>1){
    for (func in pre) {
      func_done <- paste0("../faasr_state_info/",func,".done")
      check_fn_done_list <- list.files("../faasr_state_info")
      if (!func_done %in% check_fn_done_list){
        return("next")
      }
    }
  }

  return(TRUE)
}



faasr_mock_put_file <- function(server_name=NULL, remote_folder="", remote_file, local_folder=".", local_file){
   
  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  remote_file <- sub("^/+", "", sub("/+$", "", remote_file))
  put_file_s3 <- paste0(remote_folder, "/", remote_file)

  local_folder <- sub("^/+", "", sub("/+$", "", local_folder))
  local_file <- sub("^/+", "", sub("/+$", "", local_file))
  put_file <- paste0(local_folder,"/",local_file)
  
  remote_path <- paste0("../../files/", put_file_s3)
  local_path <- put_file
  
  if (!dir.exists(paste0("./",local_folder))){
    dir.create(paste0("./",local_folder), recursive=TRUE)
  }

  if (!dir.exists(paste0("../../files/", remote_folder))){
    dir.create(paste0("../../files/", remote_folder), recursive=TRUE)
  }  

  file.copy(from=local_path, to=remote_path)

}

faasr_mock_get_file <- function(server_name=NULL, remote_folder="", remote_file, local_folder=".", local_file){
  
  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  remote_file <- sub("^/+", "", sub("/+$", "", remote_file))
  get_file_s3 <- paste0(remote_folder, "/", remote_file)

  local_folder <- sub("^/+", "", sub("/+$", "", local_folder))
  local_file <- sub("^/+", "", sub("/+$", "", local_file))
  get_file <- paste0(local_folder,"/",local_file)
  
  remote_path <- paste0("../../files/", get_file_s3)
  local_path <- get_file
  
  if (!dir.exists(paste0("./",local_folder))){
    dir.create(paste0("./",local_folder), recursive=TRUE)
  }

  if (!dir.exists(paste0("../../files/", remote_folder))){
    dir.create(paste0("../../files/", remote_folder), recursive=TRUE)
  }  

  file.copy(to=local_path, from=remote_path)

}

faasr_mock_delete_file <- function(server_name=NULL, remote_folder="", remote_file){
  
  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  remote_file <- sub("^/+", "", sub("/+$", "", remote_file))
  delete_file_s3 <- paste0(remote_folder, "/", remote_file)

  remote_path <- paste0("../../files/", delete_file_s3)
  unlink(remote_path, recursive=TRUE)

}

faasr_mock_log <- function(log_message){
  
  logs <- log_message
  log_file <- "logs/log.txt"
  write.table(logs, log_file, col.names=FALSE, row.names = FALSE, append=TRUE, quote=FALSE)
  
}

#system2(stderr=TRUE, stdout=TRUE)

faasr_dependency_install <- function(faasr, funcname, new_lib=NULL){

  # install CRAN packages
  packages <- faasr$FunctionCRANPackage[[funcname]]
  faasr_install_cran(packages, lib_path=new_lib)

  # install Git packages
  ghpackages <- faasr$FunctionGitHubPackage[[funcname]]
  faasr_install_git_package(ghpackages, lib_path=new_lib)

}


faasr_install_cran <- function(packages, lib_path=NULL){
  if (length(packages)==0){
  } else{
    for (package in packages){
	    install.packages(package, lib=lib_path)
	  }
  }
}

# function to help install "git packages"
faasr_install_git_package <- function(ghpackages, lib_path=NULL){
  if (length(ghpackages)==0){
  } else{
    for (ghpackage in ghpackages){
	    withr::with_libpaths(new=lib_path, devtools::install_github(ghpackage, force=TRUE))
	  }
  }
}


faasr_test_docker <- function(){

  svc <- .faasr_get_svc()
  faasr_wd <- svc$wd
  if (!dir.exists(faasr_wd)){
    faasr_wd <- getwd()
  }
  faasr <- svc$json
  cred <- faasr_collect_sys_env(faasr,svc$cred)
  faasr <- faasr_replace_values(faasr, cred)

  if (!dir.exists(faasr_data)){
    cli_alert_danger("faasr_data directory no found")
    return("")
  }
  
  setwd("faasr_data")

  if (dir.exists("temp")){
    unlink("temp", recursive=TRUE)
  }
  dir.create("temp/faasr_state_info", recursive=TRUE)
  
  download.file("https://raw.githubusercontent.com/FaaSr/FaaSr-package/main/schema/FaaSr.schema.json", "temp/FaaSr.schema.json")

  result <- faasr_test_start_docker(faasr, faasr_wd)

  if (result == TRUE){
    setwd(faasr_wd)
    cli_alert_success("Function execution successfully")
  } else {
    setwd(faasr_wd)
    cli_alert_danger(result)
    cli_alert_info("Wrong result: test close")
  }
}
.faasr_user$operations$faasr_test_docker <- faasr_test_docker



faasr_test_start_docker <- function(faasr, faasr_wd){
  
  current_func <- faasr$FunctionInvoke

  cli_alert_info(paste0("Using Docker: Start testing",current_func))
  
  faasr_input <- jsonlite::toJSON(faasr, auto_unbox=TRUE)
  faasr_input <- toString(faasr_input)
  faasr_input <- gsub("\"", "\\\\\\\"", faasr_input)

  result <- system(paste0("docker run --rm --name faasr-",current_func,
                    " --mount type=bind,source='${pwd}', target=/faasr_data spark77/test-docker:1.0.0.0-dev \"",
                    faasr_input, "\""), intern=TRUE, ignore.stderr = TRUE, ignore.stdout= TRUE)
  print(result)
  if (result[1] != "TRUE"){
    return(result[1])
  }

  next_funcs <- faasr$FunctionList[[faasr$FunctionInvoke]]$InvokeNext
  if (is.null(next_funcs)){
    setwd("..")
    return(TRUE)
  }
  
  cli_alert_info(paste0("Using Docker: Success testing",current_func))

  for (next_func in next_funcs){
    faasr$FunctionInvoke <- next_func
    cli_alert_info("Trigger Next functions")
    result <- faasr_test_start_docker(faasr, faasr_wd)
    if (result != TRUE){
      return(result)
    }
  }
  return(TRUE)
}

