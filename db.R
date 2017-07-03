#
# Elva ENICS database connectivity
#
# Author: Peeter Meos, Proekspert AS
# Date: 30. June 2017
#

library(RODBC)

sql <- list()

sql$driver.name <- "SQL Server"
sql$port <-""
#db.name <- "master"

# 164 is ODS database
sql$host.name <- "eeel164.encnet.ead.ems"
sql$db.name <- "SAPMEODS"

# 163 is WIP database 
sql$host.name <- "eeel163.encnet.ead.ems"
sql$db.name <- "SAPMEWIP"

# Credentials
sql$user.name <-"proekspert"
sql$pwd <- "proekspert1!"


# Use a full connection string to connect to a SAMPLE database
s.odbc <- paste("DRIVER=",sql$driver.name,
                ";Database=",sql$db.name,
                ";Server=",sql$host.name,
                ";Port=",sql$port,
                ";PROTOCOL=TCPIP",
                ";UID=", sql$user.name,
                ";PWD=",sql$pwd, sep="")


db <- odbcDriverConnect(s.odbc)

t1 <- "2017-05-12"
t2 <- "2017-05-13"

# WIP database
res <- sqlQuery(db, paste("SELECT COUNT(*) 
                            FROM dbo.ACTIVITY_LOG WHERE DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep=""))

# SFCde ajalugu
df.sfc  <- sqlQuery(db, paste("SELECT * FROM dbo.SFC WHERE MODIFIED_DATE_TIME >='", t1, "' AND MODIFIED_DATE_TIME   <= '", t2, "'", sep=""))

# ACTIVITY_LOG ajalugu
df.activity.log  <- sqlQuery(db, paste("SELECT SFC, OPERATION, ACTION_CODE, DATE_TIME 
                                       FROM dbo.ACTIVITY_LOG WHERE DATE_TIME >='", t1, "' AND DATE_TIME   <= '", t2, "'", sep=""))

# Statuses
df.status  <- sqlQuery(db, paste("SELECT * FROM dbo.STATUS", sep=""))

# Items
df.items  <- sqlQuery(db, paste("SELECT * FROM dbo.ITEM WHERE MODIFIED_DATE_TIME >='", t1, "' AND MODIFIED_DATE_TIME   <= '", t2, "'", sep=""))

odbcClose(db)
