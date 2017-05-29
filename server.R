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
  
  react_rain_data <- reactive({
    if (!is.null(input$rainintensity_date_window)){
      start_utc = as.POSIXct(input$rainintensity_date_window[1], "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
      end_utc = as.POSIXct(input$rainintensity_date_window[2], "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
      start_sys = format(start_utc, tz = Sys.timezone(), usetz = T)
      end_sys = format(end_utc, tz = Sys.timezone(), usetz = T)
      rain_subset <- window(rain_data(), start = start_sys, end = end_sys)
      rain_subset
    }
  })
  
  react_depth_data <- reactive({
    if (!is.null(input$depth_date_window)){
      start_utc = as.POSIXct(input$depth_date_window[1], "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
      end_utc = as.POSIXct(input$depth_date_window[2], "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
      start_sys = format(start_utc, tz = Sys.timezone(), usetz = T)
      end_sys = format(end_utc, tz = Sys.timezone(), usetz = T)
      depth_subset <- window(depth_data(), start = start_sys, end = end_sys)
      depth_subset
    }
  })
  
  react_volume_data <- reactive({
    if (!is.null(input$volume_date_window)){
      start_utc = as.POSIXct(input$volume_date_window[1], "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
      end_utc = as.POSIXct(input$volume_date_window[2], "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
      start_sys = format(start_utc, tz = Sys.timezone(), usetz = T)
      end_sys = format(end_utc, tz = Sys.timezone(), usetz = T)
      volume_subset <- window(volume_data(), start = start_sys, end = end_sys)
      volume_subset
    }
  })
  
  react_velocity_data <- reactive({
    if (!is.null(input$velocity_date_window)){
      start_utc = as.POSIXct(input$velocity_date_window[1], "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
      end_utc = as.POSIXct(input$velocity_date_window[2], "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
      start_sys = format(start_utc, tz = Sys.timezone(), usetz = T)
      end_sys = format(end_utc, tz = Sys.timezone(), usetz = T)
      velocity_subset <- window(velocity_data(), start = start_sys, end = end_sys)
      velocity_subset
    }
  })
  
  output$rainintensity <- renderDygraph({
    withProgress(message = 'Loading graphs',{
      dygraph(rain_data(), group = "sync") %>%
        dyAxis("y", "Rainfall intensity  (Inches/Hr)") %>%
        dySeries(input$rainguage,label="Rainfall intensity  (Inches/Hr)") %>%
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
        dySeries(input$flowmeters,label="Depth (Ft)") %>%
        dyOptions(includeZero = TRUE, retainDateWindow = TRUE, colors = "green")
    })
  })
  
  output$volume <- renderDygraph({
    withProgress(message = 'Loading graphs',{
      dygraph(volume_data(), group = "sync") %>%
        dyAxis("y", "Flow (MGD)")%>%
        dySeries(input$flowmeters,label="Flow (MGD)") %>%
        dyOptions(includeZero = TRUE, retainDateWindow = TRUE, colors = "green")
    })
  })
  
  output$velocity <- renderDygraph({
    withProgress(message = 'Loading graphs', {
      dygraph(velocity_data(), group = "sync") %>%
        dyAxis("y", "Velocity (Ft / Sec)")%>%
        dySeries(input$flowmeters,label="Velocity (Ft / Sec)") %>%
        dyOptions(includeZero = TRUE, retainDateWindow = TRUE, colors = "green")
    })
  })
  
  output$rainfallstats <- renderText({
    paste0(
      "<br>",
      "<b>Min: <b>",
      if (!is.null(react_rain_data())){
        sapply(sapply(react_rain_data(), min, na.rm = TRUE), signif, 3)
      },
      "<br>",
      "<b>Max: <b>",
      if (!is.null(react_rain_data())){
        sapply(sapply(react_rain_data(), max, na.rm = TRUE), signif, 3)
      },
      "<br>",
      "<b>Mean: <b>",
      if (!is.null(react_rain_data())){
        sapply(sapply(react_rain_data(), mean, na.rm = TRUE), signif, 3)
      }
    )
  })
  
  output$depthstats <- renderText({
    paste0(
      "<br>",
      "<b>Min: <b>",
      if (!is.null(react_depth_data())){
        sapply(sapply(react_depth_data(), min, na.rm = TRUE), signif, 3)
      },
      "<br>",
      "<b>Max: <b>",
      if (!is.null(react_depth_data())){
        sapply(sapply(react_depth_data(), max, na.rm = TRUE), signif, 3)
      },
      "<br>",
      "<b>Mean: <b>",
      if (!is.null(react_depth_data())){
        sapply(sapply(react_depth_data(), mean, na.rm = TRUE), signif, 3)
      }
    )
  })
  
  output$volumestats <- renderText({
    paste0(
      "<br>",
      "<b>Min: <b>",
      if (!is.null(react_volume_data())){
        sapply(sapply(react_volume_data(), min, na.rm = TRUE), signif, 3)
      },
      "<br>",
      "<b>Max: <b>",
      if (!is.null(react_volume_data())){
        sapply(sapply(react_volume_data(), max, na.rm = TRUE), signif, 3)
      },
      "<br>",
      "<b>Mean: <b>",
      if (!is.null(react_volume_data())){
        sapply(sapply(react_volume_data(), mean, na.rm = TRUE), signif, 3)
      }
    )
  })
  
  output$velocitystats <- renderText({
    paste0(
      "<br>",
      "<b>Min: <b>",
      if (!is.null(react_velocity_data())){
        sapply(sapply(react_velocity_data(), min, na.rm = TRUE), signif, 3)
      },
      "<br>",
      "<b>Max: <b>",
      if (!is.null(react_velocity_data())){
        sapply(sapply(react_velocity_data(), max, na.rm = TRUE), signif, 3)
      },
      "<br>",
      "<b>Mean: <b>",
      if (!is.null(react_velocity_data())){
        sapply(sapply(react_velocity_data(), mean, na.rm = TRUE), signif, 3)
      }
    )
  })
})