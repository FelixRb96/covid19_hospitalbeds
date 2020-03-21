## COVID-2019 interactive mapping tool
## Edward Parker, London School of Hygiene & Tropical Medicine (edward.parker@lshtm.ac.uk), February 2020

## includes code adapted from the following sources:
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

# update data with automated script
#source("jhu_data_update.R")
source("jhu_data_full.R")

# set mapping colour for each outbreak
covid_col = "#cc4c02"
covid_other_col = "#662506"

# import data
cv_cases = read.csv("input_data/coronavirus.csv")
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("input_data/countries.geo.json", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv")

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
if (any(grepl("/", cv_cases$date))) { 
  cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") }
cv_cases$date = as.Date(cv_cases$date)
cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$per100k = as.numeric(format(round(cv_cases$cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$newper100k = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$activeper100k = as.numeric(format(round(cv_cases$active_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)

# add variable for days since 100th case and 1st death
cv_cases$days_since_case100 = cv_cases$days_since_death1 = 0
for (i in 1:length(unique(cv_cases$country))) {
  country_name = as.character(unique(cv_cases$country))[i]
  country_db = subset(cv_cases, country==country_name)
  country_db$days_since_case100[country_db$cases>=100] = 1:sum(country_db$cases>=100)
  country_db$days_since_death1[country_db$deaths>=1] = 1:sum(country_db$deaths>=1)
  cv_cases$days_since_case100[cv_cases$country==country_name] = country_db$days_since_case100
  cv_cases$days_since_death1[cv_cases$country==country_name] = country_db$days_since_death1
}

# creat variable for today's data
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_case_count_China = sum(cv_today$cases[cv_today$country=="Mainland China"])
current_case_count_other = sum(cv_today$cases[cv_today$country!="Mainland China"])
current_death_count = sum(cv_today$deaths)

# write current day's data
write.csv(cv_today %>% select(c(country, date, update, cases, new_cases, deaths, new_deaths,
                                recovered, new_recovered, active_cases, 
                                per100k, newper100k, activeper100k,
                                days_since_case100, days_since_death1)), "input_data/coronavirus_today.csv")

### plot of cases per 100k
# cv_sub = read.csv("input_data/coronavirus_per100k.csv")
# cv_sub = cv_sub[order(cv_sub$cases),]
# cv_sub$country = factor(cv_sub$country, levels = cv_sub$country)
# g1 = ggplot(cv_sub, aes(x = country, y = per100k)) + geom_bar(position="identity", stat="identity", alpha=0.8, fill=covid_col) + 
#   geom_text(aes(label = per100k), hjust=-0.3) + ylim(0,25) +
#   #geom_bar(aes(x = subset(cv_sub, date=="01/03/2020")$country, y = subset(cv_sub, date=="01/03/2020")$per100k), position="identity", stat="identity", alpha=0.7, fill=covid_other_col) +
#   coord_flip() + theme_bw() + theme(legend.title = element_blank(), legend.position = "", text = element_text(size=10)) + ylab("Cases per 100,000 individuals") + xlab("") + facet_grid(.~date)
# g2 = ggplot(cv_sub, aes(x = country, y = cases)) + geom_bar(position="identity", stat="identity", alpha=0.8, fill=covid_col) + 
#   geom_text(aes(label = round(cases,1)), hjust=-0.3) + ylim(0,100000) +
#   #geom_bar(aes(x = subset(cv_sub, date=="01/03/2020")$country, y = subset(cv_sub, date=="01/03/2020")$per100k), position="identity", stat="identity", alpha=0.7, fill=covid_other_col) +
#   coord_flip() + theme_bw() + theme(legend.title = element_blank(), legend.position = "", text = element_text(size=10)) + ylab("Confirmed cases") + xlab("") + facet_grid(.~date)
# gridExtra::grid.arrange(g2,g1,ncol=2)

# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$id)
if (all(cv_large_countries$alpha3 %in% worldcountry$id)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,1,10,20,50,100)
cv_pal <- colorBin("Oranges", domain = cv_large_countries$per100k, bins = bins)
plot_map <- worldcountry[worldcountry$id %in% cv_large_countries$alpha3, ]

# creat cv base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("2019-COVID (active)", "2019-COVID (new)", "2019-COVID (cumulative)"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-COVID (new)", "2019-COVID (cumulative)"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  #fitBounds(~-100,-50,~80,80) %>%
  addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$per100k,
            title = "<small>Active cases per 100,000</small>") %>%
  fitBounds(0,-25,90,65) # alternative coordinates for closer zoom

# sum cv case counts by date
cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
cv_aggregated_China = aggregate(subset(cv_cases, country=="Mainland China")$cases, by=list(Category=subset(cv_cases, country=="Mainland China")$date), FUN=sum)
cv_aggregated_other = aggregate(subset(cv_cases, country!="Mainland China")$cases, by=list(Category=subset(cv_cases, country!="Mainland China")$date), FUN=sum)
names(cv_aggregated) = names(cv_aggregated_China) = names(cv_aggregated_other) = c("date", "cases")

# add variable for new cases in last 24 hours
for (i in 1:nrow(cv_aggregated)) { 
  if (i==1) { cv_aggregated$new[i] = cv_aggregated_China$new[i] = cv_aggregated_other$new[i] = NA }
  if (i>1) { 
    cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] 
    cv_aggregated_China$new[i] = cv_aggregated_China$cases[i] - cv_aggregated_China$cases[i-1] 
    cv_aggregated_other$new[i] = cv_aggregated_other$cases[i] - cv_aggregated_other$cases[i-1] 
  }
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated_China$region = "Mainland China"
cv_aggregated_other$region = "Other"
cv_aggregated = rbind(cv_aggregated, cv_aggregated_China, cv_aggregated_other)
cv_aggregated$region = factor(cv_aggregated$region, levels=c("Global", "Mainland China", "Other"))

# function to plot cumulative cases by date
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")
cumulative_plot = function(cv_aggregated, plot_date) {
  plot_df = subset(cv_aggregated, date<=plot_date & region!="Global")
  plot_df_new = subset(cv_aggregated, date<=plot_date & region=="Global")
  g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
   ylab("cumulative cases") + theme_bw() + 
   scale_colour_manual(values=c(covid_col, covid_other_col)) +
   xlim(c(cv_min_date,current_date)) + 
   scale_y_continuous(limits=c(0,current_case_count_other+1000), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
   theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
         plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot new cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
  plot_df_new = subset(cv_aggregated, date<=plot_date & region!="Global")
  g1 = ggplot(plot_df_new, aes(x = date, y = new, fill = region)) + 
    geom_bar(position="stack", stat="identity") + 
    ylab("new cases") + theme_bw() + 
    scale_fill_manual(values=c(covid_col, covid_other_col)) +
    xlim(c(cv_min_date,current_date)) + 
    scale_y_continuous(limits=c(0,20000), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
    g1
}

# test function
# cumulative_plot(cv_aggregated, current_date)
# new_cases_plot(cv_aggregated, current_date)

# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
country_cols = cls[1:length(unique(cv_cases$country))]
names(country_cols) = unique(cv_cases$country)

country_cases_plot = function(cv_cases) {
  g1 = ggplot(cv_cases, aes(x = date, y = new_outcome, fill = country, 
                               text = paste0(format(date, "%d %B %Y"), "\n", country, ": ",new_outcome))) + 
    geom_bar(position="stack", stat="identity") + 
    ylab("new") + theme_bw() + 
    scale_fill_manual(values=country_cols) +
    xlim(c(cv_min_date,current_date+1)) + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

country_cases_cumulative = function(cv_cases) {
  g1 = ggplot(cv_cases, aes(x = date, y = outcome, colour = country, group = 1,
                            text = paste0(format(date, "%d %B %Y"), "\n", country, ": ",outcome))) + 
    geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative") + theme_bw() + 
    scale_colour_manual(values=country_cols) +
    xlim(c(cv_min_date,current_date+1)) + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

country_cases_cumulative_log = function(cv_cases) {
  g1 = ggplot(cv_cases, aes(x = date, y = outcome, colour = country, group = 1,
                            text = paste0(format(date, "%d %B %Y"), "\n", country, ": ",outcome))) + 
    geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative (log10)") + theme_bw() + 
    scale_y_continuous(trans="log10") +
    scale_colour_manual(values=country_cols) +
    xlim(c(cv_min_date,current_date+1)) + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# load epidemic comparison data
epi_comp = as.data.frame(data.table::fread("input_data/epi_comp.csv"))
epi_comp$outbreak = factor(epi_comp$outbreak, levels = epi_comp$outbreak)
epi_comp$cases[1] = current_case_count
epi_comp$deaths[1] = current_death_count
epi_comp$countries[1] = nrow(subset(cv_today, country!="Diamond Princess Cruise Ship"))
epi_comp$cfr[1] = round(epi_comp$deaths[1]/epi_comp$cases[1]*100,1)
epi_comp$cfr = round(epi_comp$cfr,2)

# function to render plotly of depending on selected outcome
comparison_plot = function(epi_comp, comparison) {
  epi_comp$outcome = epi_comp[,comparison] 
  epi_comp = epi_comp[order(epi_comp$outcome),]
  epi_comp$outbreak = factor(epi_comp$outbreak, levels=epi_comp$outbreak)
  
  p1 <- ggplot(epi_comp, aes(x = outbreak, y = outcome, fill=outbreak, text = paste0(outbreak, ": ",outcome))) + geom_bar(alpha = 0.8, stat="identity") +
    ylab("N") + xlab("") + theme_bw() + 
    scale_fill_manual(values=c("2019-COVID"=covid_col)) +
    theme(legend.position = "")
  
  if(comparison == "cfr") { p1 = p1 + ylab("%") }
  if(comparison == "deaths") { p1 = p1 + scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) }
  if(comparison == "cases") { p1 = p1 + scale_y_continuous(trans='log10', limits = c(1,1e8), breaks=c(1,1000,1e6,1e9), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) }
  ggplotly(p1 + coord_flip(), tooltip = c("text")) %>% layout(showlegend = FALSE)
}

# ----------------------------------------------------------------------- #

user_base <- data_frame(
  user = c("RT_1", "TUE_2", "bob"),
  password = c("pass1", "pass2", "123"), 
  password_hash = sapply(c("pass1", "pass2", "123"), sodium::password_store), 
  permissions = c("krankenhaus", "krankenhaus", "standard"),
  name = c("Klinikum Reutlingen", "Tuebinger Universitaetsklinikum", "Besorgter Buerger")
)


# ----------------------------------------------------------------------- #
#
# DER SHINY-TEIL
#




# create Shiny ui
ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 "COVID-19 tracker", id="nav",
                 
                 tabPanel("Krankenhausbetten Deutschland",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              leafletOutput("mymap", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, left = 20, width = 250, fixed=TRUE,
                                            draggable = TRUE, height = "auto",
                                            
                                            h3(textOutput("reactive_case_count"), align = "right"),
                                            h4(textOutput("reactive_death_count"), align = "right"),
                                            span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
                                            span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
                                            h6(textOutput("clean_date_reactive"), align = "right"),
                                            h6(textOutput("reactive_country_count"), align = "right"),
                                            tags$i(h6("Updated once daily. For more regular updates, refer to: ", tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard"))),
                                            plotOutput("epi_curve", height="130px", width="100%"),
                                            plotOutput("cumulative_plot", height="130px", width="100%"),
                                            span(h6(textOutput("reactive_case_count_China"), align = "right"), style="color:#cc4c02"),
                                            span(h6(textOutput("reactive_case_count_row"), align = "right"), style="color:#662506"),

                                            sliderInput("plot_date",
                                                        label = h5("Select mapping date"),
                                                        min = as.Date(cv_min_date,"%Y-%m-%d"),
                                                        max = as.Date(current_date,"%Y-%m-%d"),
                                                        value = as.Date(current_date),
                                                        timeFormat = "%d %b", 
                                                        animate=animationOptions(interval = 2000, loop = FALSE))
                              ),
                              
                              absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                            tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                                            
                              absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                                          actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                                       onclick = sprintf("window.open('%s')", 
                                                                        "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                          
                              
                          )
                 ),
                 
                 tabPanel("Bundeslaender Zahlen",
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              pickerInput("outcome_select", "Outcome:",   
                                          choices = c("Cases", "Deaths", "Recovered"), 
                                          selected = c("Cases"),
                                          multiple = FALSE),
                              
                              pickerInput("country_select", "Country:",   
                                          choices = as.character(cv_today[order(-cv_today$cases),]$country), 
                                          options = list(`actions-box` = TRUE),
                                          selected = cv_today$country,
                                          multiple = TRUE), 
                              "Select outcome and countries from drop-down menues to update plots."
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("New", plotlyOutput("country_plot")),
                                tabPanel("Cumulative", plotlyOutput("country_plot_cumulative")),
                                tabPanel("Cumulative (log10)", plotlyOutput("country_plot_cumulative_log"))
                              )
                            )
                          )
                 ),
                  tabPanel("Daten hinzufuegen",
                           
                           dashboardHeader(title = "#WirVsVirus",
                                           tags$li(class = "dropdown", style = "padding: 8px;",
                                                   shinyauthr::logoutUI("logout")),
                                           tags$li(class = "dropdown", 
                                                   tags$a(icon("github"), 
                                                          href = "https://github.com/FelixRb96/covid19_hospitalbeds",
                                                          title = "Hackathon"))
                           ),
                           
                           dashboardSidebar(collapsed = FALSE, 
                                            div(textOutput("Willkommen!"), style = "padding: 20px")
                           ),
                           
                           dashboardBody(
                             shinyjs::useShinyjs(),
                             tags$head(tags$style(".table{margin: 0 auto;}"),
                                       tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                                                   type="text/javascript"),
                                       includeScript("returnClick.js")
                             ),
                             uiOutput("first_message"),
                             shinyauthr::loginUI("login"),
                             uiOutput("user_table"),
                             uiOutput("testUI"),
                             HTML('<div data-iframe-height></div>')
                           ),
                              
                         # numericInput("maxrows", "Rows to show", 25),
                         # verbatimTextOutput("rawtable"),
                 ),
                           
                 tabPanel("Ueber diese Webseite",
                          tags$div(
                            tags$h4("Last update"), 
                            h6(paste0(update)),
                            "This site is updated once daily. At this time of rapid escalation of the COVID-19 pandemic, the following resources offer the latest numbers of known cases:",tags$br(),
                            tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "WHO COVID-19 dashboard"),tags$br(),
                            tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University COVID-19 dashboard"),tags$br(),
                            "The aim of this site is to complement the above resources by providing several interactive features not currently available elsewhere, including the timeline function, 
                            the ability to overlay past outbreaks, and an emphasis on normalised counts (per 100,000 individuals).",tags$br(),
                            tags$br(),tags$h4("Background"), 
                            "In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China. 
                            These were caused by a new type of coronavirus, and the disease is now commonly referred to as COVID-19.
                            The number of COVID-19 cases started to escalate more quickly in mid-January and the virus soon spread beyond China's borders. 
                            This story has been rapidly evolving ever since, and each day we are faced by worrying headlines regarding the current state of the outbreak.",
                            tags$br(),tags$br(),
                            "In isolation, these headlines can be hard to interpret. 
                            How fast is the virus spreading? Are efforts to control the disease working? How does the situation compare with previous epidemics?
                            This site is updated daily based on data published by Johns Hopkins University. 
                            By looking beyond the headlines, we hope it is possible to get a deeper understanding of this unfolding pandemic.",
                            tags$br(),tags$br(),
                            "An article discussing this site was published in ",tags$a(href="https://theconversation.com/coronavirus-outbreak-a-new-mapping-tool-that-lets-you-scroll-through-timeline-131422", "The Conversation. "),
                            "The map was also featured on the BBC World Service program",tags$a(href="https://www.bbc.co.uk/programmes/w3csym33", "Science in Action."),
                            tags$br(),tags$br(),tags$h4("Code"),
                            "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
                            tags$br(),tags$br(),tags$h4("Sources"),
                            tags$b("2019-COVID cases: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "Johns Hopkins Center for Systems Science and Engineering github page,")," with additional information from the ",tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO's COVID-19 situation reports."),
                            " In previous versions of this site (up to 17th March 2020), updates were based solely on the WHO's situation reports.",tags$br(),
                             tags$a(href="https://www.who.int/csr/disease/swineflu/updates/en/", "WHO situation reports"),tags$br(),
                            "Model estimates from ", tags$a(href="https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1001558", "GLaMOR Project"),tags$br(),
                            tags$br(),
                            tags$br(),
                            tags$b("Country mapping coordinates: "), tags$a(href="https://gist.github.com/tadast/8827699", "Github"),tags$br(),
                            tags$br(),tags$br(),tags$h4("Authors"),
                            "Dr Edward Parker, The Vaccine Centre, London School of Hygiene & Tropical Medicine",tags$br(),
                            "Quentin Leclerc, Department of Infectious Disease Epidemiology, London School of Hygiene & Tropical Medicine",tags$br(),
                            tags$br(),tags$br(),tags$h4("Contact"),
                            "edward.parker@lshtm.ac.uk",tags$br(),tags$br(),
                            tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
                          )
                 )
)

server = function(input, output) {
  
  # covid tab 
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(input$plot_date),"%d %B %Y")
  })
  
  reactive_db = reactive({
    cv_cases %>% filter(date == input$plot_date)
  })
  
  reactive_db_last24h = reactive({
    cv_cases %>% filter(date == input$plot_date & new_cases>0)
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$id)
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  reactive_db_large_last24h = reactive({
    large_countries = reactive_db_last24h() %>% filter(alpha3 %in% worldcountry$id)
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  reactive_polygons = reactive({
    worldcountry[worldcountry$id %in% reactive_db_large()$alpha3, ]
  })
  
  reactive_polygons_last24h = reactive({
    worldcountry[worldcountry$id %in% reactive_db_large_last24h()$alpha3, ]
  })
  
  output$reactive_case_count <- renderText({
    paste0(formatC(sum(reactive_db()$cases), big.mark=","), " cases")
  })
  
  output$reactive_death_count <- renderText({
    paste0(formatC(sum(reactive_db()$death), big.mark=","), " deaths")
  })
  
  output$reactive_recovered_count <- renderText({
    paste0(formatC(sum(reactive_db()$recovered), big.mark=","), " recovered")
  })
  
  output$reactive_active_count <- renderText({
    paste0(formatC(sum(reactive_db()$active_cases), big.mark=","), " active cases")
  })
  
  output$reactive_case_count_China <- renderText({
    paste0("Mainland China: ", formatC(sum(subset(reactive_db(), country=="Mainland China")$cases), big.mark=",")," (",
           formatC((cv_aggregated %>% filter(date == input$plot_date & region=="Mainland China"))$new, big.mark=",")," new)")
  })
  
  output$reactive_case_count_row <- renderText({
    paste0("Other: ", formatC(sum(subset(reactive_db(), country!="Mainland China")$cases), big.mark=",")," (",
           formatC((cv_aggregated %>% filter(date == input$plot_date & region=="Other"))$new, big.mark=",")," new)")
  })
  
  output$reactive_country_count <- renderText({
    paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
  })
  
  output$reactive_new_cases_24h <- renderText({
    paste0((cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new, " new in last 24h")
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
    clearShapes() %>%
    addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$activeper100k)) %>% #group = "2019-COVID (cumulative)",
                  #label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed COVID cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db_large()$country, reactive_db_large()$cases, reactive_db_large()$deaths, reactive_db_large()$recovered, reactive_db_large()$per100k) %>% lapply(htmltools::HTML),
                  #labelOptions = labelOptions(
                  #             style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                  #             textsize = "15px", direction = "auto")) %>%
      
      #addLegend(pal = cv_pal(reactive_db_large()$per100k), values = ~reactive_db_large()$per100k, position = "topright") %>%
      
      addCircles(data = reactive_db_last24h(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/4)*2.5e4*penalty, 
                 fillOpacity = 0.1, color = covid_col, group = "2019-COVID (new)",
                # label = sprintf("<strong>%s (past 24h)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db_last24h()$country, reactive_db_last24h()$new_cases, reactive_db_last24h()$new_deaths, reactive_db_last24h()$new_recovered, reactive_db_last24h()$newper100k) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                   textsize = "15px", direction = "auto")) %>%
      
      addCircles(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4)*2.5e4*penalty, 
                 fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                # label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths,reactive_db()$recovered, reactive_db()$per100k) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                   textsize = "15px", direction = "auto")) %>%

      addCircles(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/4)*2.5e4*penalty, 
                 fillOpacity = 0.1, color = covid_col, group = "2019-COVID (active)",
                # label = sprintf("<strong>%s (active)</strong><br/>Confirmed cases: %g<br/>Cases per 100,000: %g<br/><i><small>Excludes individuals known to have<br/>recovered (%g) or died (%g).</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$activeper100k, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                   textsize = "15px", direction = "auto")) 
      
  })
  
  output$cumulative_plot <- renderPlot({
    cumulative_plot(cv_aggregated, input$plot_date)
  })
  
  output$epi_curve <- renderPlot({
    new_cases_plot(cv_aggregated, input$plot_date)
  })
  
  # comparison plot
  output$comparison_plot <- renderPlotly({
    comparison_plot(epi_comp, input$comparison_metric)
  })
  
  # add footnote for cases
  output$epi_notes_1 <- renderText({
    if(input$comparison_metric=="cases") { paste0("Note that the axis is on a log10 scale so moves in 10-fold increments.
                                                  The 60.8 million estimated cases of H1N1 dwarf all other outbreaks of plotted on a standard linear scale.") }
  })
   
  # add note for cfr
  output$epi_notes_3 <- renderText({
    if(input$comparison_metric=="cfr") { 
      paste0("For COVID-19, this displays the proportion of confirmed cases who have subsequently died. When factoring in mild or asymptomatic infections that are not picked up by case surveillance efforts, current estimates place the case fatality rate in the range of 0.3-1%.")
    }
  })
  
  # create dataframe with selected countries
  country_reactive_db = reactive({
    if (input$outcome_select=="Cases") { 
      cv_cases$outcome = cv_cases$cases
      cv_cases$new_outcome = cv_cases$new_cases
      }
    if (input$outcome_select=="Deaths") { 
      cv_cases$outcome = cv_cases$deaths 
      cv_cases$new_outcome = cv_cases$new_deaths 
    }
    if (input$outcome_select=="Recovered") { 
      cv_cases$outcome = cv_cases$recovered 
      cv_cases$new_outcome = cv_cases$new_recovered
    }
    cv_cases %>% filter(country %in% input$country_select)
  })
  
  # country-specific plots
  output$country_plot <- renderPlotly({
    country_cases_plot(country_reactive_db())
  })
  
  # country-specific plots
  output$country_plot_cumulative <- renderPlotly({
    country_cases_cumulative(country_reactive_db())
  })
  
  # country-specific plots
  output$country_plot_cumulative_log <- renderPlotly({
    country_cases_cumulative_log(country_reactive_db())
  })
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("COVID_data_", cv_today$date[1], ".csv", sep="")
    },
    content = function(file) {
      write.csv(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                       recovered, new_recovered, active_cases, 
                                       per100k, newper100k, activeper100k)), file)
    }
  )
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                     recovered, new_recovered, active_cases, 
                                     per100k, newper100k, activeper100k)), input$maxrows), row.names = FALSE)
    options(orig)
  })
  
  ## ---------------- ##
  ## Log-in ##
  
  credentials <- callModule(shinyauthr::login, "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password_hash,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  
  logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
  
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  output$first_message <- renderUI({
    # only show pre-login
    if(credentials()$user_auth) return(NULL)
    
    HTML('<h1>Bitte loggen Sie sich mit Ihrem Krankenhauszugang ein.</h1>')
  })
  
  output$user_table <- renderUI({
    # only show pre-login
    if(credentials()$user_auth) return(NULL)
    
    tagList(
      tags$p("Zum Testen:", class = "text-center"),
      
      renderTable({user_base[, -3]})
    )
  })
  
  user_info <- reactive({credentials()$info})
  
  user_data <- reactive({
    req(credentials()$user_auth)
    
    if (user_info()$permissions == "krankenhaus") {
      iris
    } else if (user_info()$permissions == "standard") {
      iris[1:5,]
    }
    
  })
  
  # output$welcome <- renderText({
  #   req(credentials()$user_auth)
  #   
  #   glue("Sie sind eingeloggt als: {user_info()$name}")
  # })
  
  output$testUI <- renderUI({
    req(credentials()$user_auth)
    
    fluidRow(
      column(
        width = 12,
        tags$h2(glue("Willkommen!
                     Sie sind angemeldet als: {user_info()$name}.
                     Ihre Zugangskategorie ist: {user_info()$permissions}.")),
        box(width = NULL, status = "primary",
            title = ifelse(user_info()$permissions %in% c('krankenhaus'), "Daten hochladen:", "Fehler"),
            DT::renderDT(user_data(), options = list(scrollX = TRUE))
        )
      )
    )
  })
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")
