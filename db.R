library(RODBC)

driver.name <- "SQL Server"
#db.name <- "master"
db.name <- "SAPMEODS"
host.name <- "eeel164.encnet.ead.ems"
port <-""
user.name <-"proekspert"
pwd <- "proekspert1!"



# Use a full connection string to connect to a SAMPLE database
s.odbc <- paste("DRIVER=",driver.name,
                  ";Database=",db.name,
                  ";Server=",host.name,
                  ";Port=",port,
                  ";PROTOCOL=TCPIP",
                  ";UID=", user.name,
                  ";PWD=",pwd, sep="")


#s.odbc <- "Data Source=eeel164.encnet.ead.ems;Initial Catalog=SAPME_ODS;Integrated Security=False;Pooling=False;MultipleActiveResultSets=True;enlist=false"

# That is our archive database - 
# AR_* are archive folders (currentli >180 days old data) -will be 100 days old data
# ODS_* are max 1h old agregated data from production"

tbl1 <- "ODS_PRODUCTION_LOG"
tbl2 <- "ODS_SHOP_ORDER"
tbl3 <- "ODS_ASSEMBLY_HISTORY"
tbl4 <- "ODS_SFC_ID_HISTORY_WIP"
tbl5 <- "ODS_SFC_WIP"

db <- odbcDriverConnect(s.odbc)

t1 <- "2017-01-14"
t2 <- "2017-01-31"

df.prod <- sqlQuery(db, paste("SELECT TOP 10000 SFC, OPERATION, STEP_ID, RESRCE, PASS1_QTY_STARTED,
                                         PASS1_QTY_COMPLETED, TIMES_PROCESSED, PASS1_ELAPSED_TIME, 
                                         PASS1_ELAPSED_QUEUE_TIME, PREVIOUS_RESRCE, ODS_DATE_TIME,
                                         PASS1_NC_DEFECT_COUNT 
                               FROM dbo.ODS_PRODUCTION_LOG
                               WHERE DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep=""))

df.sfc.order.hist.wip  <- sqlQuery(db, paste("SELECT TOP 100000 * 
                                              FROM dbo.ODS_SFC_ID_HISTORY_WIP 
                                              WHERE (REASON = 'S' OR REASON = 'P') 
                                                AND DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep=""))


df.order <- sqlQuery(db, "SELECT TOP 100 * FROM dbo.ODS_SHOP_ORDER")
df.hist <- sqlQuery(db, "SELECT TOP 100 * FROM dbo.ODS_ASSEMBLY_HISTORY")
df.hist.wip <- sqlQuery(db, "SELECT TOP 100 * FROM dbo.ODS_SFC_ID_HISTORY_WIP")
df.sfc.wip  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.ODS_SFC_WIP")
df.shop.order  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.ODS_SHOP_ORDER")
df.bom  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.ODS_BOM")
df.bom.operation  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.ODS_BOM_OPERATION")
df.bom.component  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.ODS_BOM_COMPONENT")

df.op.production  <- sqlQuery(db, "SELECT TOP 1000 * FROM dbo.ODS_OPERATION_PRODUCTION")

#SELECT TOP 1000 *
#  FROM [SAPMEODS].[dbo].[AR_SFC]
#where ITEM_BO like 'ItemBO:EEEL1,41908%'
#order by ACTUAL_COMP_DATE desc


odbcClose(db)
