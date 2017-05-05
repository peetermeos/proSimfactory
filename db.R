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

df.prod <- sqlQuery(db, "SELECT TOP 2000 * FROM dbo.ODS_PRODUCTION_LOG")
df.order <- sqlQuery(db, "SELECT TOP 100 * FROM dbo.ODS_SHOP_ORDER")
df.hist <- sqlQuery(db, "SELECT TOP 100 * FROM dbo.ODS_ASSEMBLY_HISTORY")
df.hist.wip <- sqlQuery(db, "SELECT TOP 100 * FROM dbo.ODS_SFC_ID_HISTORY_WIP")
df.sfc.wip  <- sqlQuery(db, "SELECT TOP 100 * FROM dbo.ODS_SFC_WIP")

odbcClose(db)
