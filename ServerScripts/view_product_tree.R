source("ServerScripts/utils.R")

metadata <- list(
  title = "CreateProductTree",
  version = "v0.3.2",
  description = "Creates hierarchy of item statuses based on BOMs. Item cannot be empty. Regexp in item name is allowed. Empty revision takes all revisions.",
  inputs = list(item = "character", revision = "character"),
  outputs = list(result = "character")
)

#' Creates item hierarchy based on BOMs
#' @version v0.3.2
#'
#' @return
#' @export
#' @author Peeter Meos, Proekspert AS
#'
#' @examples
serviceCode <- function(item = "", revision = ""){
  library("RODBC")
  library("reshape2")
  library("jsonlite")

  # Nice logging functionality
  printLog <- function(s){print(paste(Sys.time(), s, sep = ": "))}

  # Find child components
  revStr <- ""
  getComponents <- function(parent, revision, inherits = "", db){
    ifelse((revision == "" | revision == "*"), revStr <- "%", revStr <- revision) 
    
    bom <- paste("BOMBO:EEEL1,", parent, ",U,0/", revStr, sep = "")
    printLog(paste("Getting component information for item: ", bom, sep = ""))
    
    df.ret <- data.frame(item = parent, rev = revision, required.by = inherits)
    df <- sqlQuery(db, paste("SELECT
       [BOM_BO]
      ,[COMPONENT_GBO]
      ,[VALID_START]
      ,[VALID_END]
      ,[QTY]
      ,[CREATED_DATE_TIME]
      ,[MODIFIED_DATE_TIME]
      ,[BOM_COMPONENT_TYPE]
      ,[ORDER_ITEM_NUMBER]
       FROM [SAPMEWIP].[dbo].[BOM_COMPONENT]
       WHERE BOM_BO LIKE '", bom, "';", sep = ""))

    # Now loop through the data frame, pick up child components
    if (length(df) == 0 | nrow(df) == 0) {
      return(df.ret)
    }
    df$COMPONENT_GBO <- as.character(df$COMPONENT_GBO)
    for (i in 1:nrow(df)) {
      s <- strsplit(df$COMPONENT_GBO[i], ",")
      new.item <- s[[1]][2]
      new.rev <- strsplit(s[[1]][3], "/")[[1]][2]
      if (is.na(new.rev)) new.rev <- "*"
      if (nchar(new.item) < 6) {
        df.ret <- rbind(df.ret, getComponents(new.item, new.rev, paste(parent, " rev ", revision, sep = ""), db))
      }
    }
    return(df.ret)
  }
  
  printLog("Init")
    
  # Stop if item name was not provided
  if (item == "") {
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
  printLog("Getting status info for the components")
  s <- ""
  for (i in 1:nrow(components)) {
    s <- paste(s, " (ITEM.ITEM = '", components$item[i], "'", sep = "") 
    if (!is.na(components$rev[i]) & components$rev[i] != "*") s <- paste(s,  " AND ITEM.REVISION = '0/", components$rev[i], "'", sep = "") 
    s <- paste(s, ")", sep = "")
    
    #s <- paste(s, "(ITEM.ITEM = ", paste("'", components$item[i], "')", sep = ""), sep = "")
    if (i != nrow(components)) {
      s <- paste(s, " OR", sep = "")
    }
  }
  #print(s)
   
  # Do the query
  sqlStr <- paste("SELECT a.SFC, a.SHOP_ORDER_BO, a.QTY, a.QTY_DONE, a.QTY_SCRAPPED, 
                                   a.ACTUAL_COMP_DATE, a.MODIFIED_DATE_TIME, b.STATUS_DESCRIPTION,  
                           ITEM.ITEM, ITEM.REVISION 
                           FROM SFC AS a 
                           INNER JOIN STATUS AS b ON a.STATUS_BO = b.HANDLE 
                           INNER JOIN ITEM ON a.ITEM_BO = ITEM.HANDLE 
                           WHERE", s, sep = "")
  
  df <- sqlQuery(db, sqlStr)
  odbcClose(db)
  printLog("Done with the database")
  #return(df)
  odbcCloseAll()

  ##### Calculate medians and standard devs for all activities. #####
  # We are not going to do that yet
  
  ##### Prepare for plot #####
  # Summarise for plotting 
  df$ITEM <- factor(df$ITEM)
  df <- aggregate(data = df, QTY ~ ITEM  + REVISION + STATUS_DESCRIPTION, FUN = "sum", na.rm = TRUE)
  
  df$ITEM.REV <- paste(df$ITEM, df$REVISION, sep = "rev")
  # Order levels alphabetically
  #df$ITEM.REV <- factor(df$ITEM.REV, levels = levels(df$ITEM.REV)[order(levels(df$ITEM.REV))])
  df$ITEM.REV <- factor(df$ITEM.REV)
  df$STATUS_DESCRIPTION <- factor(df$STATUS_DESCRIPTION, 
                                  levels = levels(df$STATUS_DESCRIPTION)[order(levels(df$STATUS_DESCRIPTION))])
  
  li <- levels(df$ITEM.REV)
  ls <- levels(df$STATUS_DESCRIPTION)
  
  ##### Creating a plot ######
  printLog("Creating the plot")
  plotStr <- "var data = [";
  
  first.row <- TRUE  
  for (i in li) {
    y <- rep(0, length(li))
    for (j in ls) {
      # Y vector
      y[which(ls == j)] <- ifelse(length(df$QTY[df$ITEM.REV == i & df$STATUS_DESCRIPTION == j]) > 0, 
                                  df$QTY[df$ITEM.REV == i & df$STATUS_DESCRIPTION == j], 0)
    }
    
    if (sum(y) > 0) {
      ifelse(!first.row, plotStr <- paste(plotStr, ", ", sep = ""), first.row <- FALSE)
      
      #plotStr <- paste(plotStr, "{type: 'scatter', model: 'lines+markers', line: {shape: 'spline'},", sep = "")
      plotStr <- paste(plotStr, "{type: 'bar',", sep = "")
      plotStr <- paste(plotStr, "name: '", i, "',", sep = "")
      # Assemble x and y
      plotStr <- paste(plotStr, "x: ['", paste(ls[y > 0], collapse = "','"),"'],", sep = "")
      plotStr <- paste(plotStr, "y: [", paste(y[y > 0], collapse = ","),"]", sep = "")
      
      plotStr <- paste(plotStr, "}", sep = "")
    }
  }
  
  plotStr <- paste(plotStr, "];",
                   #"var layout = {title: 'Item statuses over past 24hrs as of ", t2,"'};",
                   "var layout = {title: 'Item statuses for item  ", item ," rev", revision, " hierarchy', barmode: 'stack'};",
                   "myChart = document.getElementById('myChart');
                              Plotly.newPlot(myChart, data, layout);", sep = "");   
  #write.table(plotStr, file="text.txt")
  ##### Compose return data frame #####
  df.ret <- dcast(data = df, ITEM + REVISION ~ STATUS_DESCRIPTION, fun.aggregate = sum, value.var = "QTY")
  names(components) <- c("ITEM", "REVISION", "REQUIRED BY")
  df1 <- data.frame(ITEM = components[, 1])
  df.ret <- merge(df.ret, df1, by = "ITEM", all.y = TRUE)
  s <- toJSON(list(ComponentHierarchy = components, ComponentStatus = df.ret, plot = plotStr), na = "string", null = "list")
  return(s)
}
