## ui.R ##


ui <- dashboardPage(skin = 'green',
  dashboardHeader(title = "Development of Tennis in Eastern and Western Countries", titleWidth = 700),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Popularity & GDP", tabName = "pop", icon = icon("user-friends")),
      menuItem("Player Performance", tabName = "per", icon = icon("poll-h")),
      menuItem("Career Length", tabName = "age", icon = icon("angle-double-up")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("dashboard")),
      menuItem("Data Source", tabName = "data", icon = icon("database"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")),
    tabItems(
      # First tab content
      tabItem(
      tabName = "overview",
      h1(span("The origin of tennis dates back thousands of  years  and it all started from a 12th century handball game in France called “Paume”.Nowadays, it is one of the most valued sports in the world and its influence has even spread among non-European countries.",tags$br(),tags$br(),
              "In this dashboard, a comparative analysis is done on the development of tennis in eastern and western countries in multiple perspectives. Here, we define eastern countries as asian countries and western countries as north-american, european, and oceanian countries.",tags$br(),tags$br(),
              " Click on the dashboard menu to start your exploration!", 
              style = "font-weight: 400;line-height: 1.5"), 
         style = "font-family: 'Arial'; color: #fff; font-size:20px; text-align: left;
         background-image: url('tennis-ball.jpg');padding-left:100px;padding-right:300px;padding-top:150px;padding-bottom:100px")
      ),
      
      # Second tab content
      tabItem(tabName = "pop",
              tags$head(includeCSS("mystyle.css")),
              leafletOutput("myMap", width= 1100, height= 600), 
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, 
                            draggable = TRUE, top = 50, left = 300, right = 750, bottom = 10, 
                            width = 350, height = 700, 
                            h5("Select a region and move your mouse over the countries to check their GDP and active player ratio.",tags$br(),
                               "What do you observe? Based on the 2 plots below, how has GDP influenced western and eastern countries differently? Click on Observation.",
                               style = "font-family: 'Helvetica Neue', Helvetica; font-size:14px; text-align: left;padding:15;line-height: 1.5"), 
                            
                            selectInput("region", h5("Select region"), 
                                        choices = list("","Both", "East","West"),
                                        selected = NULL),
                            useShinyalert(),  # Set up shinyalert
                            actionButton("observation", "Observation"),
                            #radioButtons('region', inline = TRUE, label = 'Select a region:',choices = c('Both','East','West'),selected = NULL),
                            plotOutput("plot1", height = 180), 
                            plotOutput("plot2", height = 200)) 
                            
              ),
      #third tab content
      tabItem(tabName = 'per',
              fluidRow(
                box(height = "420px", 
                    h4("Now let's look at how eastern players and western players have performed differently in their career.", tags$br(),
                    "The radar chart on the right shows a comparison with multiple parameters. 
                    A non-scaled version which amplifies the differences is also available.
                    For the detailed comparison, navigate through the tab boxes below to gain more insights into the differences.
                    ",tags$br(),
                    "You may move your mouse over the chart to look at the specific data or click on the legend to hide a specific group of data. You might also want to zoom in to particular section of the graph by drawing a square.
                    For the physical features, use the buttons below to select female/male players. What differences have you observed? Click on the 3 action buttons below for more insights.",
                    tags$br(),"You might want to input your own data to explore and predict your own ranking with the tab box as well!",
                       style = "font-family: 'Helvetica Neue', Helvetica; font-size:14px; text-align: left;padding:15;line-height: 1.5"),
                    radioButtons("sex", h3("Select sex"),
                                 choices = list("Male", "Female"),selected = "Male")),
                tabBox(
                  title = "Radar chart",
                  id = "tabset1", height = "250px",
                  tabPanel("Scaled", eChartOutput("radar1", width = 520)),
                  tabPanel("Non-scaled", plotOutput('radar2', width = 520)))
              ),
              fluidRow(
                box(
                  width = 2, height = 60,background = "aqua",
                  useShinyalert(),  # Set up shinyalert
                  actionButton("ins1", "Radar Chart")
                ),
                box(
                  width = 2, height = 60, background = "orange",
                  useShinyalert(),  # Set up shinyalert
                  actionButton("ins2", "Physical Feature")
                ),
                box(
                  width =2, height = 60, background = "green",
                  useShinyalert(),  # Set up shinyalert
                  actionButton("ins3", "Career Ranking")
                )
              ),
              fluidRow(
                tabBox( title = "Detailed Comparison",
                        id = "tabset2",height = 500, width = 750,
                        tabPanel(("Body Features"),plotlyOutput(outputId = "plotlyF", width = "100%")),
                        tabPanel(("Career High Ranking"),plotlyOutput(outputId = "density", width = "100%")),
                        tabPanel(("Predict Your Own Rank"),
                                 fluidRow( column(width = 4,
                                   sliderInput(
                                   inputId = "height",
                                   label = "Height(cm)",
                                   value = 0,
                                   min = 150,
                                   max = 230
                                   ),
                                 sliderInput(
                                   inputId = "weight",
                                   label = "Weight(kg)",
                                   value = 0,
                                   min = 30,
                                   max = 130
                                 )), column(width = 2,
                                 radioButtons(
                                   inputId = "background", label = "Region :", 
                                   choices = c("East" , "West") 
                                 ),
                                 radioButtons(
                                   inputId = "method", label = "Gender :", 
                                   choices = c("Male", "Female")
                                   )), column ( width = 4,
                                  style="font-family: 'Helvetica Neue', Helvetica; font-size:20px; text-align: left;padding:15;line-height: 1.5",
                                  box(
                                    width = NULL, status = "warning",
                                    h3("Your predicted ranking is:"),
                                 textOutput('predict'))))
                                 )
                        )
                  )
              ),
      tabItem(tabName = "age",
              fluidRow(
                box(height = "440px",h4('This page illustrates the aging trend among players of different background and gender from 2010 to 2020. The age of the active players could indirectly reflect the career length of players.',tags$br(),
                    "First, take a look at the bar chart on the right. It shows the average age among different groups of players based on year and sex.
                    You might want to click on the legend to hide particular groups and do a comparison based on your own interest (e.g. Hide all male players to compare female players).",
                    "The pyramid chart below shows the comparison on the distribution of players at different age groups. Use the slider at the bottom to get an animated view and observe the change.
                    You could also use the buttons here to choose the gender group that you are interested in. What trend do you notice?",
                    style = "font-family: 'Helvetica Neue', Helvetica; font-size:14px; text-align: left;padding:15;line-height: 1.5"),
                    width = 6,
                    radioButtons("gender", h3("Select sex"),
                                 choices = list("Male", "Female"),selected = "Male"),
                    useShinyalert(),  # Set up shinyalert
                    actionButton("trend", "Click Here for the Trend")),
                box(height = '440px', width = 6,
                    plotlyOutput("averageAge"))
                ),
              plotlyOutput("animation", width = '100%')
              ),
      tabItem(
        tabName = "conclusion",
        h1(span("In this dashboard, we have compared the development of tennis in eastern countries and western countries. As a traditional western sport,
                Tennis is found to be more popular in western coutries and western players generally outperformed eastern players with higher physical strength and higher ranking. Eastern players appear
                to mature later than western players. Nonetheless, both groups have shown longer career life over the years and we can see how professional tennis has made its progress in eastern countries since 2010.
                Let's hope tennis could spread further in the east and more eastern players could appear in the tournaments. Thank you!", 
                style = "font-weight: 400;line-height: 1.5"), 
           style = "font-family: 'Arial'; color: #fff; font-size:20px; text-align: left;
         background-image: url('tennis-ball.jpg');padding-left:100px;padding-right:300px;padding-top:150px;padding-bottom:100px")
      ),
      tabItem(
        tabName = "data",
        tags$a(href="https://www.rank-tennis.com/en/home", "Player Profiles"), tags$br(),
        tags$a(href="http://api.worldbank.org/v2/en/indicator/NY.GDP.PCAP.CD?downloadformat=xml", "GDP Data"),tags$br(),
        tags$a(href="https://www.kaggle.com/tomvebrcz/countriesandcontinents", "Country Data")
        )
      ) 
  )
)
