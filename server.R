#May need to open the webpage in browser to have full functionality

library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyalert)
library(rgdal)
library(tmaptools)
library(methods)
library(mgcv)
library(leaflet)
library(fmsb)
library(plotly)
#install recharts
#install.packages("devtools")
#devtools::install_github('yihui/recharts')
library(recharts)
source("https://raw.githubusercontent.com/mwang27/recharts/master/R/echartR.R")
#read data
all2 = read.csv('all2.csv')
newscatter = read.csv('newscatter.csv')
background = read.csv('background.csv')
all_shape_new = read.csv('all_shape_new.csv')
newAgeFemale = read.csv('newAgeFemale.csv')
newAgeMale = read.csv('newAgeMale.csv')
averageAge = read.csv('averageAge.csv')
all_shape = read.csv('all_shape.csv')
radardata = read.csv('radardata2.csv', row.names = 1)
#some pre-processing
#data for radar chart
colors_border=c( rgb(0.2,0.5,0.7,0.9), rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.7,0.4), rgb(0.7,0.5,0.1,0.4) )
radardata = rbind(c(1000,30,200,80,0.42) , c(0,0,150,70,0.39) , radardata)
background = background[-1]
#set levels for bar chart
newAgeFemale$YearSex = factor(newAgeFemale$YearSex, levels = c('2010 Female','2020 Female', '2010 Male', '2020 Male'))
newAgeFemale$Age_group = factor(newAgeFemale$Age_group, levels = c('<20','20~24','25~29','30~34','>=35'))
newAgeMale$YearSex = factor(newAgeMale$YearSex, levels = c('2010 Female','2020 Female', '2010 Male', '2020 Male'))
newAgeMale$Age_group = factor(newAgeMale$Age_group, levels = c('<20','20~24','25~29','30~34','>=35'))
# prepare data for leaflet map

world_map <- readOGR("ne_50m_admin_0_countries.shp")
newscatterEast = subset(newscatter, Background == 'East')
newscatterWest = subset(newscatter, Background == 'West')
colordata <- newscatter$Value.y[match(world_map$admin, newscatter$IOC)]*1000
colordataEast <- newscatterEast$Value.y[match(world_map$admin, newscatterEast$IOC)]*1000
colordataWest <- newscatterWest$Value.y[match(world_map$admin, newscatterWest$IOC)]*1000
qpal <- colorQuantile(c("darkgreen", "yellow", "orangered"), colordata,10,na.color = '#C0C0C0') # prepare the color mapping
# customised function for add legends to the map
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity))
}
# function for the labels shown upon hovering on the map
ioc = newscatter$IOC[match(world_map$admin, newscatter$IOC)]
ratio = newscatter$ratio[match(world_map$admin, newscatter$IOC)]
g = newscatter$Value.y[match(world_map$admin, newscatter$IOC)]

labels <- sprintf(
  "<strong>%s</strong><br/>GDP: %s K<br/>Player ratio: %s /10 mil",
  ioc, g, ratio) %>% lapply(htmltools::HTML)
# linear models for rank prediction
Male = lm(CH~Height+Weight+Background,subset(all_shape, sex=='Male'))
Female = lm(CH~Height+Weight+Background,subset(all_shape, sex=='Female'))
#data for density plot
newdata = na.omit(subset(all2,select = c(CH_age,Background)))

server <- function(input, output) {
  
  output$myMap <- renderLeaflet({
    leaflet() %>% # create a blank canvas
      addTiles() %>% # add tile
      addLegend(pal = qpal, values = colordata,position="bottomright",
                title = "GDP Percentile") %>%
      addLegendCustom(colors = c("blue", "blue", "blue"), 
                      labels = c("100 tennis players/10mil population", "50 tennis players/10mil population", "25 tennis players/10mil population"), sizes = c(20, 10, 5)) %>%
      setView(lng = 0, lat = 45, zoom = 2)
    
    
  }
  )
  observeEvent(input$observation, {
    # Show a modal when the button is pressed
    shinyalert("Notice something?", "It could be easily seen that eastern countries generally have lower GDP and lower player ratio.
               The 2 plots also show that GDP has a heavier influence on the ratio of players in eatern countries.
               Indeed, tennis is called a richman's sport in the east.",
               type = "info", callbackR = function(value) { shinyalert(text = "What about the players? Have they performed differently?
                                                                       Click on the Player Perfomance section in the menu to continue your exploration") })
  })
  observeEvent(input$ins1, {
    # Show a modal when the button is pressed
    shinyalert("Radar Chart", "It seems that both eastern and western players achieved their career high ranking at the same age.
               However, western players generally achieve better ranking and they are also physically stronger.No doubt, tennis is a traditional western sport.
               We can see that western players are maintaining this tradition fairly well. They are still more 'advantaged' than eastern players",
               type = "info")
  })
  observeEvent(input$ins2, {
    # Show a modal when the button is pressed
    shinyalert("Physical Features", "Generally speaking, physical strength is more important for male players. The inluence does not vary much within female players of different regions. Whereas, for male players, the inluence on western players is more significant.
               You may explore further to look at the most ideal value for each physical features.",
               type = "info")
  })
  observeEvent(input$ins3, {
    # Show a modal when the button is pressed
    shinyalert("Career Ranking", "Although the average age at their highest ranking are the quite similar, the density plot tells us that 
               western players has more proportion for the age range below 20. This shows that they mature ealier than eastern players.This is probably due to the long history of tennis in the west which has cultivated good foundation and tennis culture.
               On the other hand, the violin plot shows that overall, western players' career achievement is more polarised with more players at the top and the bottom",
               type = "info")
  })
  observeEvent(input$trend, {
    # Show a modal when the button is pressed
    shinyalert("Career Length", "The two graph acutally shows that an aging trend or in another word, a longer career life is happening for all groups of players. You might notice that the aging trend is more significant for female players. The reason could be that there is a general increase in the prize money for female players
               due to the 'Equal Pay' campaign, which results in better physical treatment and encourages female players focus more on their career. 
               Moreover, eastern players have also shown a greater increase in career length with a larger increase in the ratio of players above 25. Indeed, eastern players have become more adapted to professional tennis career over the years.
               They have made many breakthroughs in tennis after 2010 with famous players like Li Na and Naomi Osaka winning Grand slams in recent years.",
               type = "info")
  })
  observe({ # to highlight the selected region
    
    proxy = leafletProxy("myMap", data = world_map)
    if (input$region == 'Both'){
      proxy %>% addPolygons( # draw polygons on top of the base map (tile)
        stroke = T,
        weight = 1,
        fillOpacity = 0.8,
        color = ~qpal(colordata),
        highlight = highlightOptions(weight = 2,
                                     color = "blue",
                                     bringToFront = FALSE
                                    ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))%>%
          addCircleMarkers(data = newscatter, radius=newscatter$ratio/10,stroke = FALSE,fillOpacity = 0.7)
      
    }else if(input$region == 'East'){
      proxy %>% addPolygons( # draw polygons on top of the base map (tile)
        stroke = T,
        weight = 1,
        fillOpacity = 0.8,
        color = ~qpal(colordataEast),
        highlight = highlightOptions(weight = 2,
                                     color = "blue",
                                     bringToFront = FALSE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))%>%
        addCircleMarkers(data = newscatterEast, radius=newscatterEast$ratio/10,stroke = FALSE,fillOpacity = 0.7)
    }else {
      proxy %>% addPolygons( # draw polygons on top of the base map (tile)
        stroke = T,
        weight = 1,
        fillOpacity = 0.8,
        color = ~qpal(colordataWest),
        highlight = highlightOptions(weight = 2,
                                     color = "blue",
                                     bringToFront = FALSE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))%>%
        addCircleMarkers(data = newscatterWest, radius=newscatterWest$ratio/10,stroke = FALSE,fillOpacity = 0.7)
    }
  })
  
  output$plot1 <- renderPlot({
    if(input$region == 'East'){
      ggplot(subset(newscatter, Background == 'East'), aes(Value.y, ratio, color = Background))+
      geom_point(color = 'grey')+
      geom_smooth(method = 'glm', formula = y~sqrt(x), se = FALSE)+
      xlab('GDP per capita(thousand)')+ylab('Tennis Player Ratio')+
      scale_colour_manual(values=c("#e6e200"))+
      facet_wrap(~Background)+theme_bw()
    }else if(input$region == 'West'){
      ggplot(subset(newscatter, Background == 'West'), aes(Value.y, ratio, color = Background))+
               geom_point(color = 'grey')+
               geom_smooth(method = 'glm', formula = y~sqrt(x), se = FALSE)+
               xlab('GDP per capita(thousand)')+ylab('Tennis Player Ratio')+
               scale_colour_manual(values=c("#3de600"))+
               facet_wrap(~Background)+theme_bw()
    }else {
      ggplot(newscatter, aes(Value.y, ratio, color = Background))+
                    geom_point(color = 'grey')+
                    geom_smooth(method = 'glm', formula = y~sqrt(x), se = FALSE)+
                    xlab('GDP per capita(thousand)')+ylab('Tennis Player Ratio')+
                    scale_colour_manual(values=c("#e6e200","#3de600"))+
                    facet_wrap(~Background)+theme_bw()
    }})
  output$plot2 <- renderPlot({
    ggplot(newscatter, aes(x=reorder(IOC, Value.y), y=Value.y, fill=Background)) +
      geom_bar(stat="identity")+
      theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
      geom_line(aes(x=reorder(IOC, Value.y), y=ratio/2, linetype = 'Player ratio'), group = 1, color = 'purple')+
      scale_y_continuous(sec.axis = sec_axis(~./2, name = "Players per 10m population"))+
      ylab('GDP per capita (k$)')+
      xlab('Countries(not shown)')+
      scale_fill_manual(values=c("#e6e200","#3de600"))
    })
  
  output$radar1 <- renderEChart({
    echartR(background, x=~variable, y=~value, series=~Background, type='radarfill',
            symbolList='none', palette=c('firebrick1','dodgerblue'),
            title='Comparison between Eastern Players and Western Players')
    })
  output$radar2 <- renderPlot({
    radarchart( radardata  , axistype=2 , 
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=0.5,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="blue", caxislabels=seq(0,20,5), cglwd=0.8,palcex = 0.8,
                #custom labels
                vlcex=0.8,
                title = 'Comparison between Eastern Players and Wester Players')
     legend(x=0.7, y=1, legend = c('Eastern Players','Western Players'), bty = "n", pch=20 , col=colors_in ,
            text.col = "black", cex=1, pt.cex=3)
  })
  output$plotlyF <- renderPlotly({
    ggplotly(
      ggplot(data = filter(all_shape_new, sex %in% input$sex), aes(x = value, y = CH, color = Background) ) +
      geom_point(size = 0.7) +
      ylab('Career High Ranking')+
      ggtitle('Correlation between height, weight, body ratio and career high ranking')+
      theme_light() +
      facet_wrap(~variable, scales = "free")+
      geom_smooth(method = 'gam', se = FALSE,formula =y~poly(x,2)))
  })
  output$density <- renderPlotly({
      density = ggplotly(ggplot(newdata, aes(x=CH_age, color=Background, fill=Background)) +
               geom_density(alpha=0.4) +
               theme_light()+
               ylab("Ratio") +
               xlab("Age at Career High Ranking")+
               ggtitle("Player Distribution for Age at Career High Ranking"),
             tooltip = c('x','y'))
      violin = ggplotly(ggplot(all2, aes(x=Background, y=CH, fill=Background, color=Background)) +
                        geom_violin(width=1, size=0.2, alpha = 0.4)+
                        coord_flip()+ # This switch X and Y axis and allows to get the horizontal version
                        xlab("") +
                        ylab("Career High Rank")+
                        theme_light()+
                        ggtitle('Player Distribution for Career High Ranking'), tooltip = c('x','y'))
    
      plotly::subplot(density,violin, titleX = TRUE, titleY = TRUE)
  })
  output$predict <- renderText({
    inputdata = data.frame(Height = as.numeric(input$height),Weight = as.numeric(input$weight), Background= input$background)
    if(input$method == 'Male'){
      predict(object = Male, newdata = inputdata)}
    else{
      predict(object = Female, newdata = inputdata)
    }
  })
  output$animation <- renderPlotly({
    if(input$gender == 'Female'){
    plot_ly(newAgeFemale, size = I(30), alpha  = 0.6) %>%
      add_segments(
        x = ~Percentage, xend = 0, 
        y = ~Age_group, yend = ~Age_group, 
        frame = ~Year,
        color = ~factor(Background),
        colors = c('red','blue')) %>% 
        layout(title="Player Age Distribution")
    } else{
      plot_ly(newAgeMale, size = I(30), alpha  = 0.6) %>%
        add_segments(
          x = ~Percentage, xend = 0, 
          y = ~Age_group, yend = ~Age_group, 
          frame = ~Year,
          color = ~factor(Background),
          colors = c('red','blue')) %>% 
        layout(title="Player Age Distribution")
    }
  })
  output$averageAge <- renderPlotly({
    ggplotly(ggplot(averageAge, aes( fill=YearSex, x=Background, y=Average_age)) + 
               geom_bar(position="dodge", stat = 'identity')+
               theme_bw()+
               ggtitle('Average age')+
               scale_fill_manual(values = alpha(c("pink", "lightblue",'red','blue'), .5)))
  })
}


