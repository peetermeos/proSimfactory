manualTransmission <- function(hp, wt) {
  newdata <- data.frame(hp = hp, wt = wt)
  predict(carsModel, newdata, type = "response")
}

carsModel <- glm(formula = am ~ hp + wt, data = mtcars, family = binomial)

remoteLogin("http://mef3000.prx:12800",
            username = "admin",
            password = "Proekspert1!",
            session = FALSE)

serviceName <- paste0("mtService", round(as.numeric(Sys.time()), 0))

api <- publishService(
  serviceName,
  code = manualTransmission,
  model = carsModel,
  inputs = list(hp = "numeric", wt = "numeric"),
  outputs = list(answer = "numeric"),
  v = "v1.0.0"
)

print(api$capabilities())

result <- api$manualTransmission(120, 2.8)
print(result$output("answer"))
swagger <- api$swagger()
cat(swagger)


result <- deleteService("mtService1499165570", "v1.0.0")
print(result)

serviceAll <- listServices()
mtServiceAll <- listServices("mtService1499165570")
print(mtServiceAll)
