source("ServerScripts/utils.R")

metadata <- list(
  title <- "CreateProductTree",
  version = "v0.3.0",
  description <- "Creates item hierarchy based on BOMs. Item cannot be empty. Regexp in item name is allowed. Empty revision takes all revisions.",
  inputs = list(item = "character", revision="character"),
  outputs = list(result = "character")
)

#' Creates item hierarchy based on BOMs
#' @version v0.3.0
#'
#' @return
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
serviceCode <- function(item = "", revision = ""){
  library("RODBC")
  library("reshape2")
  library("dplyr")
  library("jsonlite")
  library("Matrix")
  
  # Nice logging functionality
  printLog <- function(s){print(paste(Sys.time(), s, sep = ": "))}

  # Find child components
  getComponents <- function(parent, revision, db){
    printLog(paste("Getting component information for item :", parent, sep = ""))

    bom <- paste("BOMBO:EEEL1,", parent, ",U,0/", revision, sep = "")
    df.ret <- data.frame(item = parent, rev = revision)

    df <- sqlQuery(db, paste("SELECT
      ,[BOM_BO]
      ,[COMPONENT_GBO]
      ,[VALID_START]
      ,[VALID_END]
      ,[QTY]
      ,[CREATED_DATE_TIME]
      ,[MODIFIED_DATE_TIME]
      ,[BOM_COMPONENT_TYPE]
      ,[ORDER_ITEM_NUMBER]
       FROM [SAPMEWIP].[dbo].[BOM_COMPONENT]
       WHERE BOM_BO LIKE '", bom, "';", sep=""))

    # Now loop through the data frame, pick up child components
    for (i in 1:nrow(df)){
      s <- strsplit(df$COMPONENT_BO[i], ",")
      new.item <- s[[2]]
      new.rev <- strplit(s[[4]], "/")[[2]]
      if(nchar(new.item < 6)){
        df.ret <- rbind(df.ret, getComponents(new.item, new.rev, db))
      }
    }
    return(df.ret)
  }
  
  printLog("Init")
    
  # Stop if item name was not provided
  if (item == ""){
    stop("Item name must be provided.")
  }

  ##### Go through BOM components to find all sub-items #####
  sql <- list()
  sql$db.name <- "SAPMEWIP"
  sql$host.name <- "eeel163.encnet.ead.ems"
  sql$driver.name <- "SQL Server"
  sql$port <- ""
  sql$user.name <- "proekspert"
  sql$pwd <- "proekspert1!"
  
  s.odbc <- paste("DRIVER=", sql$driver.name,
                  ";Database=", sql$db.name,
                  ";Server=", sql$host.name,
                  ";Port=", sql$port,
                  ";PROTOCOL=TCPIP",
                  ";UID=", sql$user.name,
                  ";PWD=", sql$pwd,
                  ";TDS_Version=8.0", sep = "")
  
  db <- odbcDriverConnect(s.odbc)
  
  printLog("Getting component data")
  components <- getComponents(parent = item, revision = revision, db = db)
  
  ##### Get status for all items #####
  
  # Construct the list of items
  s <- ""
  for (i in 1:nrow(components)) {
    s <- paste(s, " LIKE ", components$item[i], sep = "")
    if (i != components$item[nrow(components)]) {
      s <- paste(s, "OR", sep = "")
    }
  }

  # Do the query
  df <- sqlQuery(db, paste("SELECT a.SFC, a.SHOP_ORDER_BO, a.QTY, a.QTY_DONE, a.QTY_SCRAPPED, 
                                   a.ACTUAL_COMP_DATE, a.MODIFIED_DATE_TIME, b.STATUS_DESCRIPTION,  
                           ITEM.ITEM 
                           FROM SFC AS a 
                           INNER JOIN STATUS AS b ON a.STATUS_BO = b.HANDLE 
                           INNER JOIN ITEM ON a.ITEM_BO = ITEM.HANDLE 
                           WHERE ITEM LIKE '", s, "'", sep = "")) 
  
  odbcClose(db)
  printLog("Done with the database")
  
  ##### Calculate medians and standard devs for all activities. #####
  # We are not going to do that yet
  
  ##### Create plot #####
  plotStr <- ""
  
  ##### Compose return data frame #####
  df.ret <- df
  
  s <- toJSON(list(result = df.ret, plot = plotStr), na = "string", null = "list")
  return(df)
}
