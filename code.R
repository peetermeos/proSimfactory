#
# Server sample trial
#
# Author: Peeter Meos, Proekspert AS
# Date: 4. July 2017
#

library(mrsdeploy)
library(reshape2)

remoteLogin("http://localhost:12800",
            username = "admin",
            password = "Proekspert1!",
            session = FALSE)

remoteLogin("http://localhost:12800",
            username = "admin",
            password = "Proekspert1!",
            session = TRUE)

serviceName <- paste0("mtService", round(as.numeric(Sys.time()), 0))
serviceName <- "RandomGenerator"


print(api$capabilities())

result <- api$manualTransmission(120, 2.8)
print(result$output("answer"))
swagger <- api$swagger()
cat(swagger)


result <- deleteService(serviceName, "v1.0.0")
print(result)

serviceAll <- listServices()
mtServiceAll <- listServices(serviceName)
print(serviceAll)
