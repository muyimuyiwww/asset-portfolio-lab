.libPaths(c(file.path(getwd(),'.Rlib'),.libPaths()))
shiny::runApp('.',host='127.0.0.1',port=8765,launch.browser=TRUE)
