shinyServer(function(input, output, session) {
  ## Page 1
  filteredData <- reactive({
    newdata <- data[sample(nrow(data),input$number,  replace=F),]
    newdata <- newdata[newdata$Consumption >= input$range[1] & 
                         newdata$Consumption <= input$range[2],]
    newdata <- newdata[newdata$age.index >= input$age[1] & 
                         newdata$age.index <= input$age[2],]
    newdata <- newdata[newdata$Number.of.identical.boilers >= input$boilers[1] &
                         newdata$Number.of.identical.boilers <= input$boilers[2],]
    newdata <- newdata[newdata$Year.constructed >= input$year[1] &
                         newdata$Year.constructed <= input$year[2],]
    newdata <- newdata[newdata$Number.of.total.units >= input$people[1] &
                         newdata$Number.of.total.units <= input$people[2],]
    if(input$buildtype != "All")
    {
      newdata <- newdata[which(newdata[,12]==input$buildtype),]
    }
    if(input$legend1) {
      newdata <- newdata[which(newdata[,6]=="Yes"),]
    }
    if(input$legend2) {
      newdata <- newdata[which(newdata[,11]=="No"),]
    }
    if(input$fueltype == "All") {newdata}
    else if(input$fueltype == "#4")  {
      newdata <- newdata[which(newdata[,8]=="#4"),]
      newdata
    }
    else {
      newdata <- newdata[which(newdata[,8]=="#6"),]
      newdata
    }
  })
  
  output$map <- renderLeaflet({
    leaflet(data) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      fitBounds(~min(Long), ~min(Lat), ~max(Long), ~max(Lat))
  })
  
  observe({
    pal <- colorFactor(c("green","blue", "red"), domain=c("","#4","#6"))
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~sqrt(Consumption), weight = 1, stroke=F,
                 fillColor = ~pal(Primary.Fuel), fillOpacity = 0.5, popup = ~paste(Consumption)
      )
  })
  
  
  
  ### Page 2
  output$plot <- renderPlotly(
    {
      newdata <- df
      newdata <- newdata[newdata$age.index >= input$age3[1] & 
                           newdata$age.index <= input$age3[2],]
      if(input$legend13) {
        newdata <- newdata[which(newdata[,6]=="Yes"),]
      }
      if(input$legend23) {
        newdata <- newdata[which(newdata[,11]=="No"),]
      }
      if(input$fueltype3 == "All") {newdata}
      else if(input$fueltype3 == "#4")  {
        newdata <- newdata[which(newdata[,8]=="#4"),]
        newdata
      }
      else {
        newdata <- newdata[which(newdata[,8]=="#6"),]
        newdata
      }
      year <- seq(1845,2015,length=120)
      b <- matrix(c(seq(1845,2015,length=120),rep(0,120)),ncol=2)
      for(i in 1:120)
      {
        b[i,2] <- mean(newdata[which(newdata[,17]>=year[i] & newdata[,17]<year[i+1]),9])
      }
      b <- as.data.frame(b)
      b <- b[which(b[,2]>0),]
      names(b) <- c("Constructed Year", "Average Oil Consumption")

      plot_ly(data = b, x = b$`Constructed Year`, y = b$`Average Oil Consumption`, mode = "markers", 
              size = b$`Constructed Year`, color = b$`Average Oil Consumption`, 
              colorbar = list(title = "Gallon"),
              text = ~paste("Oil Consumption: ", b$`Average Oil Consumption`))%>%
        layout(title = 'Oil Consumption vs Building Constructed Year')%>%
        layout(autosize = F,width = 900, height = 600)
    }
  )
  

  ### Page 3
  output$Total_Area <- renderPlot({
    
    data <- oil %>%
      filter(Green %in% input$Green) %>%
      filter(Dual %in% input$Dual) %>%
      filter(Fuel_Type %in% input$Fuel_Type) %>%
      filter(No_Floors >= input$No_Floors[1] & 
               No_Floors <= input$No_Floors[2])
    
    data_mean <- data %>%
      group_by(Total_Area_Tile) %>%
      summarise(avg = mean(Low_Gallons))
    
    g <- ggplot(data = data_mean)
    g + geom_bar(aes(x = Total_Area_Tile, y = data_mean$avg, fill = factor(..x..)), 
                 stat = 'identity', show.legend = FALSE) +
      xlab('Total Area') +
      ylab("Average Consumption") +
      theme_classic() + 
      scale_x_discrete(limits=c('0 - 30720', 
                                '30725 - 42798', 
                                '42814 - 53000', 
                                '53010 - 65190', 
                                '65192 - 83650', 
                                '83743 - 118024', 
                                '118024 - 187973',
                                '187973 - 13435076')) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  output$Boiler_Age <- renderPlot({
    
    data <- oil %>%
      filter(Green %in% input$Green) %>%
      filter(Dual %in% input$Dual) %>%
      filter(Fuel_Type %in% input$Fuel_Type) %>%
      filter(No_Floors >= input$No_Floors[1] & 
               No_Floors <= input$No_Floors[2])
    
    data_mean <- data %>%
      group_by(Boiler_Age) %>%
      summarise(avg = mean(Low_Gallons))
    
    g <- ggplot(data = data_mean)
    g + geom_bar(aes(x = Boiler_Age, y = data_mean$avg, fill = factor(..x..)), 
                 stat = 'identity', show.legend = FALSE) +
      xlab('Boiler Age') +
      ylab("Average Consumption") +
      theme_classic() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  ### Page 4
  filteredData2 <- reactive({
    newdata <- df
    newdata <- newdata[newdata$age.index >= input$age2[1] & 
                         newdata$age.index <= input$age2[2],]
    newdata <- newdata[newdata$Number.of.identical.boilers >= input$boilers2[1] &
                         newdata$Number.of.identical.boilers <= input$boilers2[2],]
    newdata <- newdata[newdata$Year.constructed >= input$year2[1] &
                         newdata$Year.constructed <= input$year2[2],]
    newdata <- newdata[newdata$Number.of.total.units >= input$people2[1] &
                         newdata$Number.of.total.units <= input$people2[2],]
    # DUAL FUEL or not
    if(input$legend12) {
      newdata <- newdata[which(newdata[,6]=="Yes"),]
    }
    # Green Greater Buildings Law
    if(input$legend22) {
      newdata <- newdata[which(newdata[,11]=="No"),]
    }
    # Fuel Type
    if(input$fueltype2 == "All") {newdata}
    else if(input$fueltype2 == "#4")  {
      newdata <- newdata[which(newdata[,8]=="#4"),]
      newdata
    }
    else {
      newdata <- newdata[which(newdata[,8]=="#6"),]
      newdata
    }
  })
  
  output$buildingtype <- renderPlotly({
    data = filteredData2()
    
    x <- split(data, data$Building.Type)
    ave.con.buildingtype <- matrix(NA, nrow = 23, ncol=2)
    ave.con.buildingtype[1,] <- c(as.character(x$`Asylums & Homes`$Building.Type[1]), mean(x$`Asylums & Homes`$Consumption))
    ave.con.buildingtype[2,] <- c(as.character(x$`Churches, Synagogues, etc.`$Building.Type[1]), mean(x$`Churches, Synagogues, etc.`$Consumption))
    ave.con.buildingtype[3,] <- c(as.character(x$Condominiums$Building.Type[1]), mean(x$Condominiums$Consumption))
    ave.con.buildingtype[4,] <- c(as.character(x$`Cultural & Public Assembly`$Building.Type[1]), mean(x$`Cultural & Public Assembly`$Consumption))
    ave.con.buildingtype[5,] <- c(as.character(x$`Educational Structures`$Building.Type[1]), mean(x$`Educational Structures`$Consumption))
    ave.con.buildingtype[6,] <- c(as.character(x$`Elevator Apartments`$Building.Type[1]), mean(x$`Elevator Apartments`$Consumption))
    ave.con.buildingtype[7,] <- c(as.character(x$`Factory & Industrial Buildings`$Building.Type[1]), mean(x$`Factory & Industrial Buildings`$Consumption))
    ave.con.buildingtype[8,] <- c(as.character(x$`Garages & Gasoline Stations`$Building.Type[1]), mean(x$`Garages & Gasoline Stations`$Consumption))
    ave.con.buildingtype[9,] <- c(as.character(x$`Hospitals & Health`$Building.Type[1]), mean(x$`Hospitals & Health`$Consumption))
    ave.con.buildingtype[10,] <- c(as.character(x$Hotels$Building.Type[1]), mean(x$Hotels$Consumption))
    ave.con.buildingtype[11,] <- c(as.character(x$`Loft Buildings`$Building.Type[1]), mean(x$`Loft Buildings`$Consumption))
    ave.con.buildingtype[12,] <- c(as.character(x$Miscellaneous$Building.Type[1]), mean(x$Miscellaneous$Consumption))
    ave.con.buildingtype[13,] <- c(as.character(x$`Office Buildings`$Building.Type[1]), mean(x$`Office Buildings`$Consumption))
    ave.con.buildingtype[14,] <- c(as.character(x$`Outdoor Recreation`$Building.Type[1]), mean(x$`Outdoor Recreation`$Consumption))
    ave.con.buildingtype[15,] <- c(as.character(x$`Residence - Multi-Use`$Building.Type[1]), mean(x$`Residence - Multi-Use`$Consumption))
    ave.con.buildingtype[16,] <- c(as.character(x$`Selected Government`$Building.Type[1]), mean(x$`Selected Government`$Consumption))
    ave.con.buildingtype[17,] <- c(as.character(x$`Store Buildings`$Building.Type[1]), mean(x$`Store Buildings`$Consumption))
    ave.con.buildingtype[18,] <- c(as.character(x$Theatres$Building.Type[1]), mean(x$Theatres$Consumption))
    ave.con.buildingtype[19,] <- c(as.character(x$`Transportation Facilities`$Building.Type[1]), mean(x$`Transportation Facilities`$Consumption))
    ave.con.buildingtype[20,] <- c(as.character(x$`Utility Bureau`$Building.Type[1]), mean(x$`Utility Bureau`$Consumption))
    ave.con.buildingtype[21,] <- c(as.character(x$`Vacant Land`$Building.Type[1]), mean(x$`Vacant Land`$Consumption))
    ave.con.buildingtype[22,] <- c(as.character(x$`Walk-Up Apartments`$Building.Type[1]), mean(x$`Walk-Up Apartments`$Consumption))
    ave.con.buildingtype[23,] <- c(as.character(x$Warehouses$Building.Type[1]), mean(x$Warehouses$Consumption))
    
    colnames(ave.con.buildingtype) <- c("building.type","Consumption")
    ave.con.buildingtype <- as.data.frame(ave.con.buildingtype)
    ave.con.buildingtype[,2] <- as.character(ave.con.buildingtype[,2])
    ave.con.buildingtype[,2] <- as.numeric(ave.con.buildingtype[,2])
    x <- list(
      title = "Average Oil Consumption"
    )
    y <- list(
      title = ""
    )
    
    plot_ly(ave.con.buildingtype, x = ~ave.con.buildingtype[,2], y = ~ave.con.buildingtype[,1], 
            color = ~ave.con.buildingtype[,1], size = ave.con.buildingtype[,2], mode = "markers")%>%
      layout(title = "Building Type v.s. Consumption",xaxis = x, yaxis = y)%>%
      layout(autosize = F,width = 900, height = 600)
  })
  
  ## Zhao's server
  output$table <- renderDataTable({
    coln=vector()
    if(input$add==T){coln=c(coln,"Facility Address")}
    if(input$nga==T){coln=c(coln,"Natural Gas Utility")}
    if(input$noboil==T){coln=c(coln,"Number of Boilers")}
    if(input$bcap==T){coln=c(coln,"Boiler Capacity")}
    if(input$binstal==T){coln=c(coln,"Boiler Installation Date")}
    if(input$dualboil==T){coln=c(coln,"DUAL FUEL")}
    if(input$ageboil==T){coln=c(coln,"Age of Boiler")}
    if(input$primfuel==T){coln=c(coln,"Primary Fuel")}
    if(input$lcons==T){coln=c(coln,"Oil Consumption")}
    if(input$comply==T){coln=c(coln,"GGB Law")}
    if(input$btype==T){coln=c(coln,"Building Type")}
    if(input$blot==T){coln=c(coln,"Area of Building")}
    if(input$nof==T){coln=c(coln,"Number of floors")}
    if(input$nou==T){coln=c(coln,"Number of total units")}
    if(input$yrc==T){coln=c(coln,"Year Constructed")}

    mydata=oil1[,coln]
    
    if (input$ngasu != "All") {
      mydata <- mydata[mydata$`Natural Gas Utility` == input$ngasu,]
    }
    if (input$ages != "All") {
      mydata <- mydata[mydata$`Age of Boiler` == input$ages,]
    }
    if (input$prifuel != "All") {
      mydata <- mydata[mydata$`Primary Fuel` == input$prifuel,]
    }
    mydata
  })
  
})
