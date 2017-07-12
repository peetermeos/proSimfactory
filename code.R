
library(mrsdeploy)

randomService <- function(num) {
  #print("Console output")
  rnorm(1)
}

carsModel <- glm(formula = am ~ hp + wt, data = mtcars, family = binomial)

remoteLogin("http://192.168.40.97:12800",
            username = "admin",
            password = "Proekspert1!",
            session = FALSE)

serviceName <- "randomService"

api <- publishService(
  serviceName,
  code = randomService,
  model = carsModel,
  inputs = list(num = "numeric"),
  outputs = list(answer = "numeric"),
  v = "v1.5.0"
)

print(api$capabilities())

#result <- api$manualTransmission(120, 2.8)
#print(result$output("answer"))
#swagger <- api$swagger()
#cat(swagger)


result <- deleteService("randomService", "v1.3.0")
#print(result)

serviceAll <- listServices()
mtServiceAll <- listServices("mtService1499165570")
print(serviceAll)
