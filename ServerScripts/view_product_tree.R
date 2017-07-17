source("ServerScripts/utils.R")

metadata <- list(
  title <- "CreateProductTree",
  version = "v0.1.1",
  description <- "Creates SFC hierarchy",
  inputs = list(),
  outputs = list(result = "character")
)

#' Creates summary of failed SFCs in past 24 hrs
#'
#' @return
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
serviceCode <- function(){
  library("RODBC")
  library("reshape2")
  library("dplyr")
  library("jsonlite")
  library("Matrix")
  
  printLog <- function(s){print(paste(Sys.time(), s, sep = ": "))}
  
  findParents <- function(child){
    r <- ""
    p <- df.m$parent[df.m$child == child]
    if(length(p) > 0){
      for (i in p){
        r <- paste(r, i, sep = ":")
        r <- paste(r, findParents(i), sep = ":")
      }
    }
    return(r)
  }
  
  printLog("Init")
  
##### Database import #####
  
  printLog("Loading started, getting last 24 hrs")
  
  load("Data/SFCHist/2017-01-18.RData")
  df.link <- df.ods
  
  load("Data/ODS/2017-01-18.RData")

  # t2 <- as.POSIXct(Sys.Date())
  # t1 <- t2 - 3600 * 24
  # 
  # t1 <- format(t1, "%Y-%m-%d")
  # t2 <- format(t2, "%Y-%m-%d")
  # 
  # sql <- list()
  # sql$db.name <- "SAPMEWIP"
  # sql$host.name <- "eeel163.encnet.ead.ems"
  # sql$driver.name <- "SQL Server"
  # sql$port <- ""
  # sql$user.name <- "proekspert"
  # sql$pwd <- "proekspert1!"
  # 
  # s.odbc <- paste("DRIVER=", sql$driver.name,
  #                 ";Database=", sql$db.name,
  #                 ";Server=", sql$host.name,
  #                 ";Port=", sql$port,
  #                 ";PROTOCOL=TCPIP",
  #                 ";UID=", sql$user.name,
  #                 ";PWD=", sql$pwd,
  #                 ";TDS_Version=8.0", sep = "")
  # db <- odbcDriverConnect(s.odbc)
  # 
  # df <- sqlQuery(db, paste("SELECT SFC, ACTION_CODE, DATE_TIME, SFC, OPERATION, ITEM, ITEM_REVISION,
  #                          ROUTER, ROUTER_REVISION, STEP_ID, RESRCE, SHOP_ORDER_BO, PARTITION_DATE
  #                          FROM dbo.ACTIVITY_LOG
  #                          WHERE DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep = ""))
  # 
  # odbcClose(db)
  # if (nrow(df) > 2)
  #   printLog("Loading successful")
  # else{
  #   printLog("Loading failed")
  #   stop()
  # }

##### Analysis #####
  
  df.link$child <- gsub("^.*,","", as.character(df.link$SFC_BO))
  df.link$child <- factor(df.link$child)
  
  # Take out lines where SFC has mutated into itself
  df.link <- df.link[as.character(df.link$child) != as.character(df.link$SFC), ]
  
  # Rename SFC to parent
  names(df.link)[names(df.link) %in% "SFC"] <- "parent"
  
  # We use only parent child and datetime
  df.link <- df.link[, c("DATE_TIME","parent", "child")]
  
  # Create SFC shop order summary table
  df.sfc.shop.order.link <- unique(df.ods[, c("SFC", "SHOP_ORDER")])
  
  # Merge shop orders to the link table
  df.link <- merge(df.link, df.sfc.shop.order.link, by.x="parent", by.y = "SFC", all.y = FALSE)
  names(df.link)[names(df.link) %in% "SHOP_ORDER"] <- "so.parent"

  df.link <- merge(df.link, df.sfc.shop.order.link, by.x="child", by.y = "SFC", all.y = FALSE)
  names(df.link)[names(df.link) %in% "SHOP_ORDER"] <- "so.child"
  
  # Lets try aggregation into sparse matrix
  df.link$count <- 1
  df.m <- aggregate(data = df.link, count ~ parent + child, FUN = "sum")
  
  df.m$ancestry <- sapply(df.m$child, findParents)
  #df.m <- sparseMatrix(i = as.numeric(df.m$parent), j = as.numeric(df.m$child), x = df.m$count)
##### Plot creation  ######
  
  printLog("Creating the plot")
  c <- "[]"
  
  plotStr <- paste("var data = ", c, ";",
                   "var layout = {
                        title: 'Product tree',
                        };
                   myChart = document.getElementById('myChart');
                   Plotly.newPlot(myChart, data, layout);", sep = "");    

##### Results #####    
  #printLog("Returning dataset")
  #s <- toJSON(list(result = df, plot = plotStr))
  return(df.link)
}
