# App-cherry_picker

A Shiny app for mapping School specific variables by Local Education Authority with Leaflet in R.

## The live app

[App-cherry_picker](https://mammykins.shinyapps.io/App-cherry_picker/) allows the user to investigate correlations between two made-up variables (we call them apples and pears, more interesting names than X1 and X2) drawn from a uniform distribution. 
It demonstrates how [cherry picking](https://en.wikipedia.org/wiki/Cherry_picking), or finding data that supports your viewpoint (and ignoring data that does not) can be easily done even with two totally uncorrelated variables. We also use this appletunity for fruit related puns or [punnet](https://en.wikipedia.org/wiki/Punnet)-try.

## Getting Started

This app was built to demonstrate how to combine Leaflet and Shiny to create a pretty app for geospatial data while reminding users to be careful of interpreting the app. 
The development is described in this [blog post](https://www.machinegurning.com/).

## Sharing the app

[See here](http://shiny.rstudio.com/tutorial/lesson7/).

### Prerequisites

If you want to download the code and run the app locally to understand how it works (or use for your own mapping app), 
you'll need R Studio and several packages installed (see the head of the global.R file). You'll also need the school data and map polygon data for the LAs provided in this repo. 
Clone this repo and then open either the global.R, server.R or ui.R file, in the top right corner you'll see a "Run app" button; click this to run the app.

## Shiny

[Shiny](http://shiny.rstudio.com/tutorial/) has some great tutorials.

## Interactive maps in Shiny

[Leaflet for Shiny](https://blog.rstudio.org/2015/06/24/leaflet-interactive-web-maps-with-r/)

### Developing your Leaflet maps

[Leaflet for R](https://rstudio.github.io/leaflet/)

## Pretty data tables

[DT](https://rstudio.github.io/DT/) package provides a R / Javascript interface to make pretty tables that people expect in an app / dashboard.

## Pun suggestions and improvements

Raise an issue please.

