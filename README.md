# Project name GageLocationComparison
Project aims to compare available US gage data and locations for the nation.
It uses NLDI, NHDPLUS, NWIS, GAGES & StreamStats data.
Application is delivered with Rstudio & Shiny framework on shinyapps.io 

## Motivation
Often, data sets were collected from various sources and no/or little information is given on how well they correspond to each other.


## Features
Leaflet map structure with dynamic interaction.
Web app helps to easily access and examine location of gages from various sources within 6 classification groups.
Groups determined by the proximity of the location of gages.
Description file gives more explanation on statistics of reviewed data sets.

## Requirements
App can be run locally or by using already existing web host https://sabitovty.shinyapps.io/shinyapp/
There are no requirements to run it on the existing host. However host has a limited capabilities and limited uptime.
To run it locally user has to have installed R & Rstudio https://rstudio.com/ compiler. In addition external libraries must be installed.


## How to use
To run locally.
Download and open app.R

If packages are missing:
Packages can be installed using default function install.packages ("package name")
List of required packages is following:
 shiny
 leaflet
 RColorBrewer
 ggmap
 rmapshaper
 clipr
 e1071
 gridExtra
 labeling
 plogr
 readr
 reshape2
 viridisLite
 xfun

Navigate to Session  --> Set Working Directory --> To Source File Location
Select Run App or combination of keys 1) Ctrl+A; 2) Ctrl+ENTER/RETURN

Result should look like this.
![](screenshot.png)
