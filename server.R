library(dygraphs)
library(xts)
library(zoo)


# Read rainfall data
rain_data_frame <- readRDS(paste("data", "RainGuage.rds", sep = '/'))
# Read observed and model depth data
observed_depth_data <- readRDS(paste("data", "Observed_Depth.rds", sep = '/'))
# Read observed and model flow data
observed_volume_data <- readRDS(paste("data", "Observed_Volume.rds", sep = '/'))
# Read observed and model velocity data
observed_velocity_data <- readRDS(paste("data", "Observed_Velocity.rds", sep = '/'))

shinyServer(function(input, output, session) {
  
  rain_data <- reactive({
   withProgress(message = 'Loading rain data', {
     rain_guage <- xts(rain_data_frame[input$rainguage], order.by = rain_data_frame$Date, tz=Sys.timezone()) 
   })
  })
  
  depth_data <- reactive({
    withProgress(message = 'Loading depth data', {
      observed_depth_ts <- xts(observed_depth_data[input$flowmeters], order.by = observed_depth_data$Date, tz=Sys.timezone())
      observed_depth_ts
    })
  })
  
  volume_data <- reactive({
    withProgress(message = 'Loading volume data', {
      observed_volume_ts <- xts(observed_volume_data[input$flowmeters], order.by = observed_volume_data$Date, tz=Sys.timezone())
      observed_volume_ts
    })
  })
  
  velocity_data <- reactive({
    withProgress(message = 'Loading velocity data',{
      observed_velocity_ts <- xts(observed_velocity_data[input$flowmeters], order.by = observed_velocity_data$Date, tz=Sys.timezone())
      observed_velocity_ts
    })
  })
  
    
  output$rainintensity <- renderDygraph({
    withProgress(message = 'Loading graphs',{
      dygraph(rain_data(), group = "sync") %>%
        dyAxis("y", "Rainfall intensity  (Inches/Hr)")%>%
        dyOptions(includeZero = TRUE, retainDateWindow = TRUE, plotter = 
                    "function barChartPlotter(e) {
                var ctx = e.drawingContext;
                var points = e.points;
                var y_bottom = e.dygraph.toDomYCoord(0);  // see     http://dygraphs.com/jsdoc/symbols/Dygraph.html#toDomYCoord
                
                // This should really be based on the minimum gap
                var bar_width = 2/3 * (points[1].canvasx - points[0].canvasx);
                ctx.fillStyle = e.color;
                
                // Do the actual plotting.
                for (var i = 0; i < points.length; i++) {
                var p = points[i];
                var center_x = p.canvasx;  // center of the bar
                
                ctx.fillRect(center_x - bar_width / 2, p.canvasy,
                bar_width, y_bottom - p.canvasy);
                ctx.strokeRect(center_x - bar_width / 2, p.canvasy,
                bar_width, y_bottom - p.canvasy);
                }
  }")
    })
  })
  
  output$depth <- renderDygraph({
    withProgress(message = 'Loading graphs', {
      dygraph(depth_data(), group = "sync") %>%
        dyAxis("y", "Depth (Ft)")%>%
        dySeries(input$flowmeters,label="Observed") %>%
        dyOptions(includeZero = TRUE, retainDateWindow = TRUE, colors = "green")
    })
  })
  
  output$volume <- renderDygraph({
    withProgress(message = 'Loading graphs',{
      dygraph(volume_data(), group = "sync") %>%
        dyAxis("y", "Flow (MGD)")%>%
        dySeries(input$flowmeters,label="Observed") %>%
        dyOptions(includeZero = TRUE, retainDateWindow = TRUE, colors = "green")
    })
  })
  
  output$velocity <- renderDygraph({
    withProgress(message = 'Loading graphs', {
      dygraph(velocity_data(), group = "sync") %>%
        dyAxis("y", "Velocity (Ft / Sec)")%>%
        dySeries(input$flowmeters,label="Observed") %>%
        dyOptions(includeZero = TRUE, retainDateWindow = TRUE, colors = "green")
    })
  })
  
})