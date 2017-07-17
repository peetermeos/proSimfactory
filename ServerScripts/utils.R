#
# Server helper scripts
#
# Author: Peeter Meos, Proekspert AS
# Date: 4. July 2017
#

library(mrsdeploy)
library(reshape2)

# Init parameters

srv <- list()
srv$url <- 
srv$user <- "admin"
srv$pwd <- "Proekspert1!"

# Dev locally
srv.dev <- srv
srv.dev$url <- "http://127.0.0.1:12800"

# Test at MEF3000
srv.test <- srv
srv.test$url <- "http://mef3000p.prx:12800"

# Live at Elva
srv.live <- srv
srv.live$url <- "http://gleu130.encnet.ead.ems:12800"


#' Login to remote R server
#'
#' @param srv list containing login url, username and password
#' @param session logical TRUE when opening an interactive session
#'
#' @return nothing
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
srvLogin <- function(srv, session = FALSE){
  require(mrsdeploy)
  
  if (is.null(srv))
    stop("Server not given!")
  
  remoteLogin(srv$url,
              username = srv$user,
              password = srv$pwd,
              session = session)
}

#' Title
#'
#' @param func 
#' @param title 
#' @param version 
#' @param description 
#' @param inputs 
#' @param outputs 
#'
#' @return
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
inject <- function(func, title, version, description="", inputs, outputs){
  s <- listServices()
  
  if (nrow(s[s$name == title & s$version == version, ]) == 1)
    type <-  "update"
  else
    type <- "publish"
  
  if (type == "publish"){
    api <- publishService(
      name = title,
      code = func,
      descr = description,
      inputs = inputs,
      outputs = outputs,
      v = version
    )
  }
  
  if (type == "update"){
    api <- updateService(
      name = title,
      code = func,
      descr = description,
      inputs = inputs,
      outputs = outputs,
      v = version
    )  
  }
  return(api)
}

#' Title
#'
#' @param metadata 
#' @param serviceCode 
#'
#' @return
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
deploy <- function(metadata = metadata, serviceCode = serviceCode){
  print("Deploying to...")
  inject(serviceCode, metadata$title, 
         metadata$version, 
         metadata$description, 
         metadata$inputs, 
         metadata$outputs)
}

#' Returns a data frame of listed services and versions
#'
#' @return data frame
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
listServices <- function(){
  l <- mrsdeploy::listServices()
  
  name <- ""
  version <- ""
  
  if (length(l) > 0){
    for(i in 1:length(l)){
      name[i] <- l[[i]]$name
      version[i] <- l[[i]]$version
    }
  } else {
    data.frame()
  }
  return(data.frame(name, version))
}

# Legacy stuff from testing
notRun <- function(){
  print(api$capabilities())
  
  result <- api$manualTransmission(120, 2.8)
  print(result$output("answer"))
  swagger <- api$swagger()
  cat(swagger)
  
  
  result <- deleteService(serviceName, "v1.0.0")
  print(result)
}
