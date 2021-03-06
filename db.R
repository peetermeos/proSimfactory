#
# Connectivity for SAP ME ODS and WIP databases
#
# Author: Peeter Meos, Proekspert AS
# Date: 1. July 2017
#

library(RODBC)

#' Initalise SQL setup for data analysis
#'
#' @param type string c("ODS", "WIP")
#'
#' @return list of SQL setup parameters to be used by connectSQL()
#' @author Peeter Meos, Proekspert AS
#' 
#' @examples
#' initSQL("ODS")
initSQL <- function(type="ODS"){
  if (type != "ODS" & type != "WIP") {
    stop("type must be either ODS or WIP")
  }
  
  sql <- list()

  if (type == "ODS") {
    sql$db.name <- "SAPMEODS"
    sql$host.name <- "eeel164.encnet.ead.ems"
  }
  
  if (type == "WIP") {
    sql$db.name <- "SAPMEWIP"
    sql$host.name <- "eeel163.encnet.ead.ems"
  }

  sql$driver.name <- "SQL Server"
  sql$port <- ""
  sql$user.name <- "proekspert"
  sql$pwd <- "proekspert1!"
  
  return(sql)
}

#' Connect to SAPME database
#'
#' @param sql list holding connection parameters
#'
#' @return RODBC connection
#' @export
#'
#' @examples
#' connectSQL(initSQL("WIP"))
connectSQL <- function(sql){
  require(RODBC)
  
  s.odbc <- paste("DRIVER=", sql$driver.name,
                  ";Database=", sql$db.name,
                  ";Server=", sql$host.name,
                  ";Port=", sql$port,
                  ";PROTOCOL=TCPIP",
                  ";UID=", sql$user.name,
                  ";PWD=", sql$pwd,
                  ";TDS_Version=8.0", sep="")  
  db <- odbcDriverConnect(s.odbc)
  
  return(db)
}

#' disconnectSQL disconnects from the database
#'
#' @param db RODBC handle for data connection
#'
#' @return Returns nothing
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
disconnectSQL <- function(db){
  require(RODBC)
  
  odbcClose(db)
}

#' getTables retrieves list of all tables in given database
#'
#' @param db RODBC handle of the database
#' @param sql list from initSQL that contains database name
#'
#' @return data frame consisting table names
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
getListOfTables <- function(db, sql){
  require(RODBC)
  
  sql.str <- paste("SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES ",
                   "WHERE TABLE_TYPE = 'BASE TABLE' AND TABLE_CATALOG='", 
                   sql$db.name,  "'", sep = "")
  ret  <- sqlQuery(db, sql.str)
                                     
  return(ret)
}

#' Queries the given database and returns the required table 
#' within the period between t1 and t2. If not specified, brings 
#' the entire table
#'
#' @param db 
#' @param tblname
#' @param t1 beginning date
#' @param t2 end date
#' @param date.col the name of date column
#'
#' @return data frame containing the respective table
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
getTable <- function(db, tblname="", t1="", t2="", date.col="DATE_TIME"){
  if (t1 != "" & t2 != "")
    df  <- sqlQuery(db, paste("SELECT * FROM ", tblname, 
                              " WHERE ", date.col, " >='", t1, 
                              "' AND ", date.col, "  <= '", t2, "'", sep = ""))
  else
    df  <- sqlQuery(db, paste("SELECT * FROM ", tblname, sep = ""))
  
  return(df)
}

getAllProductionLog <- function(){
  for (i in 0:185){
    t1 <- as.POSIXct("2017-01-01") + 3600 * 24 * i
    t2 <- t1 + 3600 * 24 
    
    t1 <- format(t1, "%Y-%m-%d")
    t2 <- format(t2, "%Y-%m-%d")
    
    print(t1)
    
    df.ods <- sqlQuery(db, paste("SELECT SFC, OPERATION, ROUTER, STEP_ID, RESRCE, PASS1_QTY_STARTED,
                                        PASS1_QTY_COMPLETED, TIMES_PROCESSED, PASS1_ELAPSED_TIME,
                                  PASS1_ELAPSED_QUEUE_TIME, PREVIOUS_RESRCE, ODS_DATE_TIME,
                                  PASS1_NC_DEFECT_COUNT, SHOP_ORDER
                                  FROM dbo.ODS_PRODUCTION_LOG
                                  WHERE DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep=""))
    
    save(df.ods, file = paste("Data/", t1, ".RData", sep = ""))

  }
}

getSfcHistory <- function(){
  for (i in 0:185){
    t1 <- as.POSIXct("2017-01-01") + 3600 * 24 * i
    t2 <- t1 + 3600 * 24 
    
    t1 <- format(t1, "%Y-%m-%d")
    t2 <- format(t2, "%Y-%m-%d")
    
    print(t1)
    
    df.ods  <- sqlQuery(db, paste("SELECT * 
                                   FROM dbo.ODS_SFC_ID_HISTORY_WIP 
                                   WHERE (REASON = 'S' OR REASON = 'P') 
                                  AND DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep=""))
    
    save(df.ods, file = paste("Data/SFCHist/", t1, ".RData", sep = ""))
    
  }
}

getAllEventLog <- function(){
  for (i in 0:127){
    t1 <- as.POSIXct("2017-03-01") + 3600 * 24 * i
    t2 <- t1 + 3600 * 24 
    
    t1 <- format(t1, "%Y-%m-%d")
    t2 <- format(t2, "%Y-%m-%d")
    
    print(t1)
    
    df.ods <- sqlQuery(db, paste("SELECT SFC, ACTION_CODE, DATE_TIME, SFC, OPERATION, ITEM, ITEM_REVISION, 
                                  ROUTER, ROUTER_REVISION, STEP_ID, RESRCE, SHOP_ORDER_BO, PARTITION_DATE
                                  FROM dbo.ACTIVITY_LOG
                                  WHERE DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep=""))
    
    save(df.ods, file = paste("DataWip/", t1, ".RData", sep = ""))
    
  }
}

getBOM <- function(){
  df.bom <- sqlQuery(db, paste("SELECT [HANDLE]
      ,[CHANGE_STAMP]
      ,[SITE]
      ,[BOM]
      ,[BOM_TYPE]
      ,[DESCRIPTION]
      ,[STATUS_BO]
      ,[TMP_BOM]
      ,[COPIED_FROM_BOM_BO]
      ,[REVISION]
      ,[CURRENT_REVISION]
      ,[BOM_TEMPLATE]
      ,[HAS_BEEN_RELEASED]
      ,[EFF_START_DATE]
      ,[EFF_END_DATE]
      ,[EFFECTIVITY_CONTROL]
      ,[PREV_SITE]
      ,[ORIGINAL_TRANSFER_KEY]
      ,[CREATED_DATE_TIME]
      ,[MODIFIED_DATE_TIME]
      ,[PARTITION_DATE]
      ,[ERP_BILL_OF_MATERIAL]
  FROM [SAPMEWIP].[dbo].[BOM]", sep=""))
    
  save(df.bom, file = paste("Data/WIP/", "bom", ".RData", sep = ""))
  
}

getBOMComponent <- function(){
  for(i in c(2015, 2016, 2017)){
    print(i)
    
    t1 <- as.POSIXct(paste(i, "-01-01", sep = ""))
    t2 <- as.POSIXct(paste(i + 1, "-01-01", sep = "")) 
    t1 <- format(t1, "%Y-%m-%d")
    t2 <- format(t2, "%Y-%m-%d")
  
  df.bom.component <- sqlQuery(db, paste("SELECT [HANDLE]
      ,[BOM_BO]
      ,[COMPONENT_GBO]
      ,[SEQUENCE]
      ,[ENABLED]
      ,[VALID_START]
      ,[VALID_END]
      ,[USE_ITEM_DEFAULTS]
      ,[PRE_ASSEMBLED]
      ,[ASSY_DATA_TYPE_BO]
      ,[QTY]
      ,[TEST_PART]
      ,[BOM_TEMPLATE]
      ,[DISASSEMBLE_OPERATION_BO]
      ,[CREATE_TRACKABLE_SFC]
      ,[MAXIMUM_USAGE]
      ,[MAXIMUM_NC]
      ,[ASSEMBLE_AS_REQ]
      ,[AUTO_CONSUME_KIT]
      ,[CREATED_DATE_TIME]
      ,[MODIFIED_DATE_TIME]
      ,[BOM_COMPONENT_TYPE]
      ,[PARENT_SEQUENCE]
      ,[ORDER_ITEM_NUMBER]
      ,[ERP_CHANGE_NUMBER]
      ,[ERP_SEQUENCE]
       FROM [SAPMEWIP].[dbo].[BOM_COMPONENT]
       WHERE CREATED_DATE_TIME  >= CONVERT(datetime, '", t1,"') AND CREATED_DATE_TIME  < CONVERT(datetime, '", t2, "')",
      sep=""))
  
  save(df.bom.component, file = paste("Data/WIP/", "bomComponent", t1,  ".RData", sep = ""))
  }
}

mergeAllProductionLog <- function(){
  require(dplyr)
  for (i in 1:185){
    t1 <- as.POSIXct("2017-01-01") + 3600 * 24 * i
    t2 <- t1 + 3600 * 24 
    
    t1 <- format(t1, "%Y-%m-%d")
    t2 <- format(t2, "%Y-%m-%d")
    
    print(t1)
    load(file = paste("Data/", t1, ".RData", sep = ""))
    df.ods$SHOP_ORDER <- as.character(df.ods$SHOP_ORDER)
    
    if(i == 1){
      df.production.log <- df.ods
    } else { 
      df.production.log <- dplyr::bind_rows(df.production.log, df.ods)
    }   
  }
  save(df.production.log, file="Data/production_log.RData")
}

mergeAllActivityLog <- function(){
  require(dplyr)
  file.names <- dir("DataWip", pattern =".RData")
  
  for (i in 1:(length(file.names)-1)){
    t1 <- as.POSIXct("2017-01-01") + 3600 * 24 * i
    t2 <- t1 + 3600 * 24 
    
    t1 <- format(t1, "%Y-%m-%d")
    t2 <- format(t2, "%Y-%m-%d")
    
    print(t1)
    load(file = paste("DataWip/", t1, ".RData", sep = ""))
    df.ods$ROUTER_REVISION <- as.character(df.ods$ROUTER_REVISION)
    
    if(i == 1){
      df.production.log <- df.ods
    } else { 
      df.production.log <- dplyr::bind_rows(df.production.log, df.ods)
    }   
  }
  save(df.production.log, file="DataWip/activity_log.RData")
}

# Legacy stuff, thats not really a function but ODS database retrieval
notRun <- function(){

#s.odbc <- "Data Source=eeel164.encnet.ead.ems;Initial Catalog=SAPME_ODS;Integrated Security=False;Pooling=False;MultipleActiveResultSets=True;enlist=false"

# That is our archive database - 
# AR_* are archive folders (currentli >180 days old data) -will be 100 days old data
# ODS_* are max 1h old agregated data from production"

tbl1 <- "ODS_PRODUCTION_LOG"
tbl2 <- "ODS_SHOP_ORDER"
tbl3 <- "ODS_ASSEMBLY_HISTORY"
tbl4 <- "ODS_SFC_ID_HISTORY_WIP"
tbl5 <- "ODS_SFC_WIP"

#df.prod <- sqlQuery(db, paste("SELECT TOP 50000 * FROM dbo.ODS_PRODUCTION_LOG
#                               WHERE DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep=""))

df.prod <- sqlQuery(db, paste("SELECT SFC, OPERATION, ROUTER, STEP_ID, RESRCE, PASS1_QTY_STARTED,
                                        PASS1_QTY_COMPLETED, TIMES_PROCESSED, PASS1_ELAPSED_TIME,
                                        PASS1_ELAPSED_QUEUE_TIME, PREVIOUS_RESRCE, ODS_DATE_TIME,
                                        PASS1_NC_DEFECT_COUNT, SHOP_ORDER
                              FROM dbo.ODS_PRODUCTION_LOG
                              WHERE DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep=""))

df.sfc.order.hist.wip  <- sqlQuery(db, paste("SELECT * 
                                              FROM dbo.ODS_SFC_ID_HISTORY_WIP 
                                              WHERE (REASON = 'S' OR REASON = 'P') 
                                                AND DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep=""))


df.order <- sqlQuery(db, paste("SELECT * FROM dbo.ODS_SHOP_ORDER
                                WHERE ACTUAL_START_DATE >='", t1, "' AND ACTUAL_START_DATE   <= '", t2, "'", sep=""))

df.hist <- sqlQuery(db, "SELECT TOP 100 * FROM dbo.ODS_ASSEMBLY_HISTORY")
df.hist.wip <- sqlQuery(db, "SELECT TOP 100 * FROM dbo.ODS_SFC_ID_HISTORY_WIP")
df.sfc.wip  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.ODS_SFC_WIP")
df.shop.order  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.ODS_SHOP_ORDER")
df.bom  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.ODS_BOM")
df.bom.operation  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.ODS_BOM_OPERATION")
df.bom.component  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.ODS_BOM_COMPONENT")

df.op.production  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.ODS_OPERATION_PRODUCTION")

# Ressursi kasutuse summary
df.res.time.log  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.ODS_RESOURCE_TIME_LOG")

# Mõttetu
df.res.util  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.ODS_RESOURCE_UTILIZATION")

# See on mõttetu
df.wip.router  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.WIP_ROUTER")

# Siin sees on routingu sammud!
df.wip.router.step  <- sqlQuery(db, "SELECT TOP 100000 * FROM dbo.WIP_ROUTER_STEP")

# Seob operatsiooni ja ressursi
df.wip.resrce  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.WIP_RESRCE")

# Orderite ajalugu
df.wip.shop.order  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.WIP_SHOP_ORDER")


#SELECT TOP 1000 *
#  FROM [SAPMEODS].[dbo].[AR_SFC]
#where ITEM_BO like 'ItemBO:EEEL1,41908%'
#order by ACTUAL_COMP_DATE desc


}