library(rsconnect)
deployApp(appDir = paste0(getwd(),"/code"),
          appName = basename(getwd()))
