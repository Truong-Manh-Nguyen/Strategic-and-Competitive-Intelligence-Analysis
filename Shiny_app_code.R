# ==== Drone Show Intelligence Dashboard ====

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(visNetwork)
library(igraph)
library(plotly)
library(scales)
library(stringr)
library(purrr)

setwd("/Users/gagliardi_pietro/Desktop/codice_supremo") #RIMUOVILO E CAMBIALO IN BASE AL FOLDER CHE USI

# ---- Dataset 1: Top Technologies per Year ----
tcounts <- read_csv("/Users/gagliardi_pietro/Desktop/technology_counts_by_year.csv")
names(tcounts) <- gsub(" ", "_", names(tcounts))
tcounts$Year <- as.integer(tcounts$Year)
names(tcounts)[names(tcounts)=="Mapped_Technologies"] <- "Technology"

# ---- Dataset 2: Google Trends ----
trend_data <- read_csv("/Users/gagliardi_pietro/Desktop/multiTimeline_cleaned.csv", skip=1)
names(trend_data) <- c("Week","DroneLightShow","LaserShow","HologramConcert")
trend_data$Week <- as.Date(trend_data$Week)
trend_data <- trend_data %>%
  pivot_longer(cols=-Week, names_to="Technology", values_to="Interest")

# ---- Dataset 3: Company & Market Overview ----
companies <- data.frame(
  Company = c(
    "Intel Corporation","Vimdrones Inc Limited","Dronisos SA","Geoscan Group",
    "Pixel Rain Digital Ltd.","TAIT Towers Inc.","Airworks LLC","Verge Aero Inc.",
    "Firefly Drone Shows LLC","HighGreat Technology Co. Ltd.","BotLab Dynamics Inc.",
    "Sky Elements Drones S.L.","Skymagic Global Ltd.","SPH Engineering Limited",
    "CollMot Entertainment Kft.","Cyberdrone Drone Shows Ltd.","Open Sky Productions Pte. Ltd.",
    "The BLINK Entertainment Ltd.","Drone Light Show Company LLC","FlightShows Inc.",
    "Flyby Guys LLC","DJI Technology Co. Ltd.","Verity Studios AG","Airstage GmbH",
    "KMel Robotics LLC","MicroMultiCopter Aero Technology Co. Ltd."
  ),
  Country = c(
    "United States","China","France","Russia","United States","United States",
    "United States","United States","United States","China","India","United States",
    "United Kingdom","Latvia","Hungary","United Kingdom","Singapore","Singapore",
    "United States","United States","Finland","China","Switzerland","Germany",
    "United States","China"
  )
)

# ---- Dataset 4: Geo Trends ----
geo_data <- read_csv("/Users/gagliardi_pietro/Desktop/geoMap_cleaned_READY_complete.csv")

# ---- Dataset 5: Market Growth ----
years <- 2024:2034
start_value <- 7.47
cagr <- 0.225
growth <- data.frame(
  Year = years,
  MarketSize = start_value * (1 + cagr)^(years - 2025)
)

# ---- Dataset 6: Event Types ----
event_data <- rbind(
  data.frame(
    Region = "Worldwide",
    Event = c(
      "municipal","commercial company","not specified","sport","art festival","trade show",
      "music festival","resort","independence/national day","education","religion celebration",
      "government sector","concert","new year celebration","birthday","wedding","medical",
      "adventure park","proposal","museum","farm","political event","theatre",
      "valentine day","casino","bank","food festival","conference","mother day celebration"
    ),
    Percentage = c(
      18.01,14.55,9.53,9.42,9.42,4.82,3.56,3.35,3.25,2.83,2.30,2.30,
      1.99,1.88,1.68,1.57,1.36,1.26,1.15,1.15,1.15,0.73,0.63,0.52,0.52,0.52,0.21,0.21,0.10
    )
  ),
  data.frame(
    Region = "Europe",
    Event = c(
      "municipal","commercial company","art festival","not specified","sport","music festival",
      "trade show","museum","adventure park","theatre","concert","birthday",
      "new year celebration","independence/national day","resort","proposal","medical",
      "education","conference"
    ),
    Percentage = c(
      23.33,19.33,12.67,10.00,7.33,6.00,3.33,3.33,2.67,2.00,2.00,2.00,
      1.33,1.33,0.67,0.67,0.67,0.67,0.67
    )
  )
)

# ---- Dataset 7: Social Network ----
scopus_data <- read_csv("/Users/gagliardi_pietro/Desktop/scopus_updated.csv")

tech_per_paper <- scopus_data %>%
  select(Title, `Keywords Found`, Year) %>%
  filter(!is.na(`Keywords Found`)) %>%
  mutate(tech_list = str_split(`Keywords Found`, ";|,")) %>%
  unnest(tech_list) %>%
  mutate(tech_list = str_trim(tech_list)) %>%
  filter(tech_list != "")

nodes <- tech_per_paper %>%
  count(tech_list, name="size") %>%
  rename(id=tech_list, value=size) %>%
  mutate(label=id)

edges <- tech_per_paper %>%
  group_by(Title) %>%
  summarise(techs=list(unique(tech_list)), .groups="drop") %>%
  filter(lengths(techs)>=2) %>%
  mutate(pairs=map(techs, ~combn(.x,2,simplify=FALSE))) %>%
  unnest(pairs) %>%
  mutate(Source=map_chr(pairs,1), Target=map_chr(pairs,2)) %>%
  count(Source, Target, name="weight") %>%
  mutate(from=Source, to=Target, weight=rescale(weight, to=c(0.1,1)))

cluster_colors <- c(
  "#1A237E","#D32F2F","#388E3C","#FBC02D","#1976D2",
  "#F57C00","#7B1FA2","#0097A7","#C2185B","#455A64"
)

# ==== UI ====
ui <- navbarPage(
  "Drone Show Intelligence Dashboard",
  
  # 1) Introduction
  tabPanel("Introduction",
           fluidPage(
             titlePanel("Introduction to Drone Show Market Analysis"),
             tabsetPanel(type="tabs", id="new_introduction_subtabs",
                         tabPanel("Overall",
                                  fluidRow(
                                    column(10, offset=1, br(),
                                           img(src="Overall-market.png",
                                               style="width:100%;height:auto;max-height:850px;border:1px solid #ddd;padding:5px;"))
                                  )
                         ),
                         tabPanel("Average Cost by Region",
                                  fluidRow(
                                    column(12, br(),
                                           tags$div(
                                             style="border:1px solid #ddd;padding:15px;margin-top:30px;background:#f8f8f8;border-radius:8px;",
                                             tags$h3("THE AVERAGE COST OF DRONE LIGHT SHOW BY REGION",
                                                     style="text-align:center;color:#2C3E50;"),
                                             fluidRow(
                                               column(6,
                                                      tags$h4("The average cost of a show in 2023 varies from region to region",
                                                              style="color:#34495E;"),
                                                      tags$h2("$25,500", style="color:#2980B9;"),
                                                      tags$p("The lowest average cost of a drone show is in Latin American countries."),
                                                      tags$p("The reason for this is that small drone shows are produced in this region.")
                                               ),
                                               column(6,
                                                      plotOutput("averageCostBar", height="400px")
                                               )
                                             )
                                           )
                                    )
                                  )
                         ),
                         tabPanel("Key Trends",
                                  fluidPage(
                                    titlePanel("Types of Drone Light Show Customers"),
                                    fluidRow(
                                      column(6,
                                             tags$h4("In Europe (2023)"),
                                             plotOutput("keyTrendsEurope", height="600px"),
                                             tags$p(HTML(
                                               "<b>23%</b> of shows held in Europe were ordered by municipalities.<br/>
                        <b>20%</b> by commercial companies.<br/>
                        <b>13%</b> by festivals.<br/>
                        <b>4%</b> by museums."
                                             ))
                                      ),
                                      column(6,
                                             tags$h4("Global Overview"),
                                             plotOutput("keyTrendsGlobal", height="600px"),
                                             tags$p(HTML(
                                               "Orders from city governments account for <b>18%</b> of all drone shows.<br/>
                        <b>15%</b> from commercial companies.<br/>
                        <b>9%</b> each from sports and festival operators."
                                             ))
                                      )
                                    )
                                  )
                         ),
                         tabPanel("Key Figure",
                                  fluidPage(
                                    tags$div(
                                      style="background:#222;color:white;padding:30px;font-family:'Arial Black',Arial;",
                                      fluidRow(
                                        column(12,
                                               tags$h2(HTML("KEY FIGURE <span style='color:#17a2b8;'>IN EUROPE</span>"),
                                                       style="font-weight:900;color:white;"))
                                      ),
                                      fluidRow(
                                        column(4,
                                               tags$h3("France", style="color:#17a2b8;"),
                                               tags$p("the most shows per year in"),
                                               tags$h1("182", style="color:#17a2b8;"),
                                               tags$p("average number of drones in the show")
                                        ),
                                        column(4,
                                               tags$h3("Malta", style="color:#17a2b8;"),
                                               tags$p("the fewest shows per year in"),
                                               tags$h1("877", style="color:#17a2b8;"),
                                               tags$p("number of shows per year")
                                        ),
                                        column(4,
                                               tags$div(
                                                 style="border:2px solid #17a2b8;padding:20px;margin-top:30px;text-align:center;",
                                                 tags$h4("TOP 3 SHOW PROVIDER WITH SHARES", style="color:#ffa500;"),
                                                 fluidRow(
                                                   column(4, tags$h2("48%", style="color:#17a2b8;"), tags$p("DRONISOS", style="color:white;")),
                                                   column(4, tags$h2("13,5%", style="color:#17a2b8;"), tags$p("UMILES GROUP", style="color:white;")),
                                                   column(4, tags$h2("10%", style="color:#17a2b8;"), tags$p("DRONE SHOW EUROPE", style="color:white;"))
                                                 ),
                                                 tags$h5("OF 48 PLAYERS", style="color:#ffa500;")
                                               )
                                        )
                                      )
                                    )
                                  )
                         )
             )
           )
  ),
  
  # 2) Market Analysis
  tabPanel("Market Analysis",
           fluidPage(
             titlePanel("Detailed Market Analysis"),
             tabsetPanel(type="tabs", id="market_analysis_subtabs",
                         tabPanel("Porter's Five Forces Analysis",
                                  fluidRow(column(10, offset=1, br(),
                                                  img(src="five_forces_analysis.png", style="width:80%;border:1px solid #ddd;padding:5px;")
                                  ))
                         ),
                         tabPanel("SWOT Analysis",
                                  fluidRow(column(10, offset=1, br(),
                                                  img(src="swot_analysis.png", width="100%", style="border:1px solid #ddd;padding:5px;")
                                  ))
                         ),
                         tabPanel("Conclusion",
                                  fluidRow(br(),
                                           column(8, offset=2,
                                                  tags$h4("Conclusion from SWOT",
                                                          style="text-align:center;font-weight:900;font-size:28px;"),
                                                  tags$div(
                                                    style="border:1px solid #ddd;padding:15px;background:#f9f9f9;font-weight:700;font-size:20px;line-height:1.4;overflow-y:auto;height:350px;",
                                                    tags$p("From SWOT, we conclude that the key strategy to compete in Drone show market is: Technological Innovation and Marketing Strategy and Expansion of the market")
                                                  )
                                           )
                                  )
                         )
             )
           )
  ),
  
  # 3) Key Intelligence Questions
  tabPanel("Key Intelligence Questions",
           fluidPage(
             titlePanel("Key Intelligence Questions"),
             tabsetPanel(type="tabs", id="key_intelligence_subtabs",
                         tabPanel("Road Map",
                                  br(),
                                  fluidRow(column(12,
                                                  img(src="roadmap_key_intelligence_questions.png", alt="Roadmap",
                                                      style="max-width:90%;border:1px solid #ddd;padding:10px;box-shadow:3px 3px 5px #888;max-height:75vh;")
                                  ))
                         ),
                         tabPanel("Questions",
                                  br(),
                                  fluidRow(column(12,
                                                  tags$p("Here we have finally 5 questions to answer:"),
                                                  tags$ol(
                                                    tags$li("Question 1: What is the most developed market for drone shows?"),
                                                    tags$li("Question 2: What technologies typically go together at drone shows?"),
                                                    tags$li("Question 3: What technology can be competitive advantages?"),
                                                    tags$li("Question 4: Which emerging entertainment formats are gaining the most consumer attention?"),
                                                    tags$li("Question 5")
                                                  )
                                  ))
                         )
             )
           )
  ),
  
  # 4) Key Technologies (Social Network)
  tabPanel("Key Technologies",
           fluidPage(
             titlePanel("What are the most important technologies?"),
             fluidRow(
               column(7, visNetworkOutput("network_plot", height="600px")),
               column(5,
                      sliderInput("year_filter", "Filter by Year:",
                                  min = min(scopus_data$Year, na.rm=TRUE),
                                  max = max(scopus_data$Year, na.rm=TRUE),
                                  value = range(scopus_data$Year, na.rm=TRUE),
                                  sep="", step=1),
                      plotOutput("barplot_top_techs", height="550px", width="100%")
               )
             ),
             hr(),
             fluidRow(
               column(1, tableOutput("modularity_table")),
               column(2, tableOutput("degree_table")),
               column(2, tableOutput("betweenness_table")),
               column(2, tableOutput("eigenvector_table")),
               column(2, tableOutput("closeness_table")),
               column(3,
                      h4("Observations"),
                      tags$ul(
                        tags$li("High graph density leads to low clustering values."),
                        tags$li("IA and ML are the most connected nodes with highest degree."),
                        tags$li("Thermal Radar acts as a bridge to the 'sensor' node."),
                        tags$li("ML, DL, AI are influential due to many connections."),
                        tags$li("Inspection node can reach others quickly (important position).")
                      )
               )
             )
           )
  ),
  
  # 5) Consumer Attention
  tabPanel("Consumer Attention",
           fluidPage(
             h3("KIQ: Which entertainment formats are gaining the most consumer attention?"),
             plotOutput("trendPlot", height="400px"),
             hr(),
             h4("Global Distribution of Search Interest by Technology"),
             plotlyOutput("geoMap", height="600px")
           )
  ),
  
  # 6) Industry Landscape
  tabPanel("Industry Landscape",
           fluidPage(
             h3("KIQ: What is the structure of the drone show industry and how fast is it growing?"),
             fluidRow(
               column(6, plotOutput("companyPlot")),
               column(6, plotOutput("growthPlot"))
             ),
             hr(),
             selectInput("region","Select Region for Event Types:", choices=unique(event_data$Region)),
             plotOutput("eventPlot")
           )
  )
  
) # end navbarPage

# ==== SERVER ====
server <- function(input, output, session) {
  
  # -- Social Network outputs --
  output$network_plot <- renderVisNetwork({
    min_weight <- 0.11
    ef <- edges %>% filter(weight >= min_weight)
    cn <- unique(c(ef$from, ef$to))
    nf <- nodes %>% filter(id %in% cn)
    g <- graph_from_data_frame(ef, vertices=nf, directed=FALSE)
    cl <- cluster_louvain(g, weights=E(g)$weight)
    memb <- membership(cl)
    cols <- rep(cluster_colors, length.out=length(unique(memb)))
    nf_mod <- nf %>%
      mutate(
        cluster=as.factor(memb[id]),
        color=cols[as.numeric(cluster)],
        degree=degree(g, v=id),
        mass=1/degree
      )
    ef_mod <- ef %>% mutate(width=ifelse(weight>mean(weight),4,1))
    legend <- data.frame(
      label=c("Occur.: <500","Occur.: <1000","Occur.: >1000", paste0("Cluster ", unique(nf_mod$cluster))),
      shape=c(rep("dot",3), rep("square", length(unique(nf_mod$cluster)))),
      size=c(5,15,20, rep(20, length(unique(nf_mod$cluster)))),
      color=c("lightgray","gray","darkgray", cols[1:length(unique(nf_mod$cluster))]),
      id=paste0("l",1:(3+length(unique(nf_mod$cluster))))
    )
    visNetwork(nf_mod, ef_mod) %>%
      visNodes(mass=nf_mod$mass, color=list(background=nf_mod$color), scaling=list(min=5,max=40)) %>%
      visEdges(color=list(color="lightgray",highlight="purple")) %>%
      visOptions(highlightNearest=list(enabled=TRUE,degree=1,hover=TRUE), nodesIdSelection=TRUE) %>%
      visLegend(addNodes=legend, useGroups=FALSE, position="left") %>%
      visPhysics(
        solver="repulsion",
        repulsion=list(nodeDistance=200, centralGravity=0.2, springLength=200, springConstant=0.01),
        stabilization=list(enabled=TRUE, iterations=300)
      ) %>%
      visLayout(randomSeed=42)
  })
  
  output$barplot_top_techs <- renderPlot({
    fd <- scopus_data %>%
      filter(Year >= input$year_filter[1], Year <= input$year_filter[2]) %>%
      filter(!is.na(`Keywords Found`)) %>%
      mutate(tech_list=str_split(`Keywords Found`,";|,")) %>%
      unnest(tech_list) %>%
      mutate(tech_list=str_trim(tech_list)) %>%
      filter(tech_list!="")
    top <- fd %>%
      count(tech_list, name="value") %>%
      arrange(desc(value)) %>%
      slice_head(n=10)
    ggplot(top, aes(x=reorder(tech_list,value), y=value)) +
      geom_col(fill="#1A237E") +
      coord_flip() +
      labs(title="Top 10 Technologies (by Year Filter)", x="Technology", y="Occurrences") +
      theme_minimal()
  })
  
  output$modularity_table <- renderTable({
    g0 <- graph_from_data_frame(edges %>% filter(weight >= 0.11), vertices=nodes, directed=FALSE)
    data.frame(Modularity = round(modularity(cluster_louvain(g0,weights=E(g0)$weight)),4))
  })
  output$degree_table <- renderTable({
    dg <- degree(graph_from_data_frame(edges %>% filter(weight>=0.11), vertices=nodes, directed=FALSE))
    head(data.frame(Node=names(dg),Degree=dg) %>% arrange(desc(Degree)), 10)
  })
  output$betweenness_table <- renderTable({
    btw <- betweenness(graph_from_data_frame(edges %>% filter(weight>=0.11), vertices=nodes, directed=FALSE))
    head(data.frame(Node=names(btw),Betweenness=round(btw,2)) %>% arrange(desc(Betweenness)), 10)
  })
  output$eigenvector_table <- renderTable({
    evc <- eigen_centrality(graph_from_data_frame(edges %>% filter(weight>=0.11), vertices=nodes, directed=FALSE))$vector
    head(data.frame(Node=names(evc),Eigenvector=round(evc,4)) %>% arrange(desc(Eigenvector)),10)
  })
  output$closeness_table <- renderTable({
    cls <- closeness(graph_from_data_frame(edges %>% filter(weight>=0.11), vertices=nodes, directed=FALSE), normalized=TRUE)
    head(data.frame(Node=names(cls),Closeness=round(cls,4)) %>% arrange(desc(Closeness)),10)
  })
  
  # -- Consumer Attention outputs --
  output$trendPlot <- renderPlot({
    ggplot(trend_data, aes(x=Week, y=Interest, color=Technology)) +
      geom_line(size=1) +
      scale_color_manual(values=c(
        DroneLightShow="#FF2400",
        LaserShow="#1f78b4",
        HologramConcert="#33a02c"
      )) +
      theme_minimal()
  })
  
  output$geoMap <- renderPlotly({
    geo_long <- geo_data %>%
      pivot_longer(cols=-Region, names_to="Technology", values_to="Value")
    plot_geo(geo_long) %>%
      add_trace(
        z=~Value,
        color=~Technology,
        locations=~Region,
        locationmode='country names',
        frame=~Technology,
        type='choropleth',
        colorscale='Viridis'
      ) %>%
      layout(
        geo=list(showframe=FALSE, showcoastlines=FALSE,
                 projection=list(type='natural earth'))
      )
  })
  
  # -- Industry Landscape outputs --
  output$companyPlot <- renderPlot({
    companies %>%
      count(Country) %>%
      ggplot(aes(x=reorder(Country, -n), y=n)) +
      geom_col(fill="#FF2400") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45, hjust=1))
  })
  
  output$growthPlot <- renderPlot({
    ggplot(growth, aes(x=Year, y=MarketSize)) +
      geom_line(color="darkgreen", size=1.5) +
      geom_point(color="black") +
      theme_minimal()
  })
  
  output$eventPlot <- renderPlot({
    event_data %>%
      filter(Region==input$region) %>%
      ggplot(aes(x=reorder(Event, Percentage), y=Percentage)) +
      geom_col(fill="steelblue") +
      coord_flip() +
      theme_minimal()
  })
  
}

# ==== Run App ====
shinyApp(ui, server)

getwd()