library(data.table)

# You will need to change your working directory
setwd("C:/Users/riwh/OneDrive - Folkehelseinstituttet/git/xx_03")

fileSources = file.path("code_task2", list.files("code_task2", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# code goes here