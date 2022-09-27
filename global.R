
library(dplyr)
library(DT)
library(fst)
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)

source("configure.R")
clinicalStudyList <- read.fst("ReLiSyRClinicalStudiesApr2021.fst")
invivoStudyList <- read.fst("ReLiSyRInVivoStudiesApr2021.fst")
entityOfInterest <- read.csv("entityOfInterest.csv")

diseaseOfInterest <- entityOfInterest[entityOfInterest$Type == "diseaseOfInterest", ]$Item
drugOfInterest <- entityOfInterest[entityOfInterest$Type == "drugOfInterest", ]$Item
msTypes <- entityOfInterest[entityOfInterest$Type == "msTypeOfInterest", ]$Item