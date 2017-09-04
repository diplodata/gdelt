# A Shiny interface to GDELT 'doc 2.0' and 'geo 2.0' APIs
# 2017, Robin Edwards, @geotheory
# Shared under GNU General Public License v3.0

require(shiny)
require(shinyBS)
require(shinyjs)
require(shinythemes)
require(dplyr)
require(stringr)
require(lubridate)

# setwd('~/Documents/shinyapps.io/gdelt/')

# some time calculation functions
now = as.Date(Sys.Date())
add_days = function(d, n){
  d <- ymd(d)
  d %m+% days(n)
}
add_hours = function(d, n){
  #d = as.POSIXct(d)
  d <- ymd_hms(d)
  d %m+% minutes(n*60)
}

# paths for GDELT's doc' and 'geo' APIs, documented at:
# https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/
# https://blog.gdeltproject.org/gdelt-geo-2-0-api-debuts/
root =     'https://api.gdeltproject.org/api/v2/doc/doc?query='
geo_root = 'https://api.gdeltproject.org/api/v2/geo/geo?query='

# Menus for the respective API modes available, with 'doc' API  modes grouped appropriately
content_modes = c('MEDIA - a simple list of news articles that matched the search' = 'ArtList',
                  'MEDIA-ART - art layout of news articles that matched the search AND that include a social sharing image' = 'ArtGallery',
                  'IMAGE COLLAGE - matching images. Most relevant when used with the image-related search terms.' = 'ImageCollage',
                  'IMAGE COLLAGE INFO - as for IMAGE COLLAGE but includes links' = 'ImageCollageInfo',
                  'IMAGE GALLERY - alternative layout to IMAGE COLLAGE INFO' = 'ImageGallery',
                  'IMAGE COLLAGE SHARE - lists social sharing images found in the matching articles, where present.' = 'ImageCollageShare')

timeline_modes = c('VOLUME - % of global news coverage' = 'TimelineVol', 
                   'VOLUMEINFO - as VOLUME but includes interactive URL links to top sources' = 'TimelineVolInfo', 
                   'SENTIMENT - average "tone" of all matching coverage' = 'TimelineTone',
                   'LANGUAGE - volume breakdown by language (65 supported)' = 'TimelineLang',
                   'SOURCE COUNTRY - which countries are focusing the most on the topic' = 'TimelineSourceCountry')

geo_modes = c('POINTDATA - displays a dot at each location mentioned in proximity to your search term. Image functionality is disabled' = 'PointData',
              'IMAGEPOINTDATA - as above, but for image searches. Search terms are disabled' = 'ImagePointData',
              'POINTHEATMAP - heatmap of the locations most closely associated with your search term (GeoJSON only)' = 'PointHeatmap',
              'IMAGEPOINTHEATMAP - as above, but for image searches' = 'ImagePointHeatmap',
              'POINTANIMATION - a series of heatmaps in 15 minute increments over the past 24 hours (GeoJSON only)' = 'PointAnimation',
              'IMAGEPOINTANIMATION - as above, but for image searches' = 'ImagePointAnimation',
              'COUNTRY - aggregates all locations to country level. Also performs normalization, dividing number of mentions in context of your search by total mentions' = 'Country',
              'IMAGECOUNTRY - as above, but for image searches' = 'ImageCountry',
              'SOURCECOUNTRY - reflects the country or origin of your search results' = 'SourceCountry',
              'IMAGESOURCECOUNTRY - as above, but for image searches' = 'ImageSourceCountry',
              'ADM1 - as COUNTRY but higher administrative granularity' = 'ADM1',
              'IMAGEADM1 - as above, but for image searches' = 'ImageADM1')

# Options selection lists to read in
country_codes = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-COUNTRIES.TXT', sep='\t', stringsAsFactors = F, header = F)
country_codes = c('', setNames(as.character(country_codes$V1), country_codes$V2))
lang_codes = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-LANGUAGES.TXT', sep = '\t', stringsAsFactors = F, header = F)
lang_codes = c('', English = 'eng', setNames(as.character(lang_codes$V1), lang_codes$V2))
image_tags = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGETAGS.TXT', sep='\t', stringsAsFactors = F, header = F) %>% .[['V1']]
themes = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT', sep = '\t', stringsAsFactors = F, header = F) %>% 
  arrange(V1) %>% mutate(V1 = str_replace_all(V1, '_', ' ')) %>% .[['V1']] # underscores removed for aesthetic. Need to re-add to URL calls
adm1 = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-ADM1S.TXT', header=F, sep='\t', stringsAsFactors = F)
adm1 = c('', setNames(as.character(adm1$V1), adm1$V2))

#------------------------------------------------------------

server <- function(input, output, session) {
  
  # feed large selections from server (instead of pre-loading)
  updateSelectizeInput(session = session, inputId = 'image_tags', choices = image_tags, server = TRUE)
  updateSelectizeInput(session = session, inputId = 'themes', choices = themes, server = TRUE)
  updateSelectizeInput(session = session, inputId = 'geo_adm1', choices = adm1, server = TRUE)
  
  observe({
    # logically build up the API search string. This forms part of the final URL call. Seperate items (AND logic) are seperated by space '%20'
    search = ''
    if(input$search_terms != '') search = input$search_terms
    search = str_replace_all(search, ' ', '%20')
    if(input$search_domain != '') search = paste0(search, '%20domain:', input$search_domain)
    if(input$search_country != '') search = paste0(search, '%20sourcecountry:', input$search_country)
    if(input$source_lang != '') search = paste0(search, '%20sourcelang:', input$source_lang)
    if(length(input$image_tags) > 0) for(i in input$image_tags) search = paste0(search, '%20imagewebtag:%22', i, '%22')
    if(length(input$themes) > 0) for(i in input$themes) search = paste0(search, '%20theme:', str_replace_all(i,' ','_'))
    if(input$output_tab == 'GEO24'){
      if(input$geo_near != '') search = paste0(search, '%20near:', input$geo_near)
      if(input$geo_cc != '') search = paste0(search, '%20locationcc:', input$geo_cc)
      if(input$geo_adm1 != '') search = paste0(search, '%20locationadm1:', input$geo_adm1)
      if(input$geo_loc != ''){
        location = str_replace_all(input$geo_loc, '["\']', ''); 
        search = paste0(search, '%20location:%22', location, '%22')
      }
    } else{ # params not supported by GEO API
      if(input$search_lang != '') search = paste0(search, '%20searchlang:', input$search_lang)
    }
    
    # logically build up full URL, incorporating API search string
    if(input$output_tab == 'GEO24'){ # GEO MODE
      url = paste0(geo_root, search, '&mode=', input$geo_mode)
      if(input$timespan != 1440) url = paste0(url, '&timespan=', input$timespan) # timespan in mins
      if(input$geo_format != '') url = paste0(url, '&format=', input$geo_format) # output format
      # Manage widget fuctionality
      shinyjs::disable("timeline_daterange")
      shinyjs::disable("timeline_hours")
      shinyjs::disable("search_lang")
    } else{
      # CONTENT or TIMELINE modes
      # date params
      if(is.na(input$timeline_hours)){ # period defined by date range
        end_date = ifelse(input$timeline_daterange[2] == as.character(today()),
                          format(Sys.time(), '%Y%m%d%H%M%S'), 
                          format(as.Date(input$timeline_daterange[2]), paste0('%Y%m%d', '235959')))
        start_date = format(as.Date(input$timeline_daterange[1]), paste0('%Y%m%d', '000000'))
      } else{                         # period defined in past hours
        end_date = format(Sys.time(), '%Y%m%d%H%M%S')
        start_date = format(add_hours(Sys.time(), -round(input$timeline_hours,1)), '%Y%m%d%H%M%S')
      }
      
      # build URL
      url = paste0(root, search)
      if(input$output_tab == 'CONTENT'){
        url = paste0(url, '&mode=', input$content_mode)
        if(input$max_records > 75) url = paste0(url, '&maxrecords=', input$max_records)
      }
      if(input$output_tab == 'TIMELINE'){
        url = paste0(url, '&mode=', input$timeline_mode, '&TIMELINESMOOTH=', input$smooth)
        if(input$timeline_data != '') url = paste0(url, '&format=', input$timeline_data)
        shinyjs::enable("timeline_daterange")
      }
      url = paste0(url, '&startdatetime=', start_date, '&enddatetime=', end_date)
      
      # Manage widget fuctionality
      if(is.na(input$timeline_hours)) shinyjs::enable("timeline_daterange") else shinyjs::disable("timeline_daterange")
      shinyjs::enable("timeline_hours")
      shinyjs::enable("search_lang")
    }
    
    # render to interface for reference and external use
    output$url = renderUI({ HTML(paste0('<p style="color:#999;font-size: 90%; word-wrap: break-word;">', url, '</p><p  style="color:#999;font-size: 80%;">(2017 Robin Edwards)</p>')) })
    
    # Title to detail search parameters
    search_title = ''
    if(input$search_terms != '') search_title = input$search_terms
    if(length(input$image_tags) > 0){
      if(search_title != '') search_title = paste(search_title, '+')
      search_title = paste(search_title, paste('image tags:', paste(input$image_tags, collapse=', ')))
    }
    if(length(input$themes) > 0){
      if(search_title != '') search_title = paste(search_title, '+')
      search_title = paste(search_title, paste('themes:', paste(input$themes, collapse=', ')))
    }
    output$plot_title = renderUI({ HTML(paste0('<h3>', search_title, '</h3>')) })
    
    # iframe container for API response
    output$frame <- renderUI({
      # responsiveness. ~2/3 screen width for desktops; full-width for mobile devices
      w = ifelse(input$dimension[1] > 767, -15 + 2 * input$dimension[1]/3, input$dimension[1])
      h = ifelse(input$dimension[1] > 767, input$dimension[2]-70, input$dimension[2])
      new_iframe <- tags$iframe(src = url, width = w, height = h)
      new_iframe
    })
  })
}

#--------------------------------------------------------------------------------
pad = 'padding:0px 5px 0px 5px;'

ui <- fluidPage(
  theme = shinytheme("flatly"),
  # select existing textInput contents when clicking on it
  tags$script('$(document).ready(function(){ $("input").focus(function() { $(this).select(); } ); });'),
  # return screen dimensions (inc. responsively) to server to manage iframe dims
  tags$head(tags$script('
                        var dimension = 0;
                        $(document).on("shiny:connected", function(e) {
                        dimension = [window.innerWidth, window.innerHeight];
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension = [window.innerWidth, window.innerHeight];
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')
  ),
  tags$head(
    # custom css
    tags$style(HTML("
                    /* compress widgets a bit to fit on page */
                    hr {border-top: 1px solid #000; margin-top: 8px; margin-bottom: 12px; }
                    .form-control {height: 25px; margin-bottom: 0px;}
                    .selectize-input, .input-sm, .form-group, .shiny-input-container, .form-control 
                    { padding:0px 5px 0px 5px; margin-bottom: 0px; min-height: 25px; }
                    .selectize-control { padding:0px 0px 0px 0px; }
                    .item { font-size: 11px; }
                    
                    /* rescale iframe contents */
                    iframe { zoom: .667;
                    -moz-transform: scale(1);
                    -moz-transform-origin: 0 0;
                    -o-transform: scale(1);
                    -o-transform-origin: 0 0;
                    -webkit-transform: scale(1.5);
                    -webkit-transform-origin: 0 0;
                    } 
                    @media screen and (-webkit-min-device-pixel-ratio:0) { #scaled-frame { zoom: 1;} }
                    
                    /* remove numericInput increment buttons */
                    input[type=number]::-webkit-inner-spin-button, 
                    input[type=number]::-webkit-outer-spin-button { 
                    -webkit-appearance: none; 
                    margin: 0; 
                    }
                    "))
    ),
  # Custom tooltips for widgets
  shinyjs::useShinyjs(),
  bsTooltip(id = 'search_terms', title = 'Supports multiple terms separated by spaces, phrases in double quotes e.g. "cats and dogs", and OR if nested in parentheses, e.g. (cats OR dogs)', placement = "top", trigger = "hover"),
  bsTooltip(id = 'search_lang', title = 'Language of the terms you are searchig for, if not English. Matching content in this language is returned. (This feature seems to have some issues)', placement = "top", trigger = "hover"),
  bsTooltip(id = 'image_tags', title = 'Every image processed by GDELT is assigned one or more topical tags from a universe of more than 10,000 objects and activities recognized by Google algorithms', placement = "top", trigger = "hover"),
  bsTooltip(id = 'themes', title = 'Searches for any of the GDELT Global Knowledge Graph (GKG) Themes. GKG Themes offer a powerful way of searching for complex topics, since there can be numerous different phrases or names under a single heading. Key in likely relevant themes to find matching options. Words on the left denote the semantic hierarchy (NB. "TAX" seems to refer to taxonomy not taxation)', placement = "top", trigger = "hover"),
  bsTooltip(id = 'search_country', title = 'Country of media origin', placement = "top", trigger = "hover"),
  bsTooltip(id = 'search_domain', title = 'Internet domain of origin', placement = "top", trigger = "hover"),
  bsTooltip(id = 'source_lang', title = 'Language of content. You can specify e.g. French but use search terms in English. GDELT handles the interpretation', placement = "top", trigger = "hover"),
  bsTooltip(id = 'timeline_hours', title = 'Specify period in most recent hours', placement = "bottom", trigger = "hover"),
  bsTooltip(id = 'timeline_daterange', title = '(Functions when "Hours" is blank.) By default GDELT reports the most recent ~3 months, but you can specify any date range within this window', placement = "bottom", trigger = "hover"),
  bsTooltip(id = 'smooth', title = 'Line smooth option, using rolling average method', placement = "bottom", trigger = "hover"),
  bsTooltip(id = 'max_records', title = 'GDELT will return 75 by default, but this can be increased to 250', placement = "left", trigger = "hover"),
  bsTooltip(id = 'geo_near', title = 'Returns all matches within a certain radius (bounding box) of a given point. You specify a particular latitude and longitude and distance in either miles (default) or kms (e.g. for 100km from Paris "48.85,2.35,100km")', placement = "top", trigger = "hover"),
  bsTooltip(id = 'timespan', title = 'The geo portal searches the past 24 hours (1,440 mins), but this can be reduced further to a minimum timespan of 15 mins', placement = "top", trigger = "hover"),
  bsTooltip(id = 'geo_cc', title = 'Specify country of media mentions', placement = "top", trigger = "hover"),
  bsTooltip(id = 'geo_adm1', title = 'Specify ADM1 (top sub-national) geographical region of media mentions', placement = "top", trigger = "hover"),
  bsTooltip(id = 'geo_loc', title = 'Searches for a given word or phrase in the full formal name of the location - e.g. New York', placement = "top", trigger = "hover"),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 fluidRow(
                   column(3, h3("GDELT Project")),
                   column(9, p("GDELT monitors the world's news media in print, broadcast and web in 100+ languages. Monitoring uses machine-translation, so you can search globally in English (or ", a('supported', href = 'http://data.gdeltproject.org/api/v2/guides/LOOKUP-LANGUAGES.TXT', target="_blank")," other) for local-language content. (", a('GDELT home', href = 'https://www.gdeltproject.org/', target="_blank"), '/', a('reference', href = 'https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/', target="_blank"), ')'))
                 ),
                 
                 # INPUTS
                 hr(),
                 tabsetPanel(id = 'input_tab',
                             tabPanel("SEARCH TERMS",
                                      fluidRow(
                                        column(6, textInput(inputId = 'search_terms', label = 'search terms', value = 'Brexit'), style=pad),
                                        column(4, selectInput(inputId = 'search_lang', label = 'searchLang', selected = 'English', choices = lang_codes), style=pad)
                                      )
                             ),
                             tabPanel("IMAGE TAGS",
                                      fluidRow(
                                        column(12, selectizeInput(inputId = 'image_tags', label = 'Image tags', choices = NULL, selected = 1, multiple=T), style=pad)
                                      )
                             ),
                             tabPanel("THEMES",
                                      fluidRow(
                                        column(12, selectizeInput(inputId = 'themes', label = 'Themes', choices = NULL, selected = 1, multiple = T), style=pad) #  (from GDELT Global Knowledge Graph)
                                      )
                             )
                 ),
                 fluidRow(
                   column(6, selectInput(inputId = 'search_country', label = 'Country', choices = country_codes), style=pad),
                   column(6, textInput(inputId = 'search_domain', label = 'Domain', value = '', placeholder = 'e.g. "bbc.co.uk"'), style=pad)
                 ),
                 fluidRow(
                   column(7, dateRangeInput(inputId = "timeline_daterange", label = "Date range",
                                            start = add_days(now, -82), end = now,
                                            min = add_days(now, -82), max = now), style=pad),
                   column(2, numericInput(inputId = 'timeline_hours', label='Hours', value='24', min=.25, ), style=pad),
                   column(3, selectInput(inputId = 'source_lang', label = 'SourceLang', choices = lang_codes, selectize = T), style=pad)
                 ),
                 
                 # OUTPUTS       
                 hr(),
                 tabsetPanel(id = 'output_tab',
                             tabPanel("CONTENT",
                                      br(),
                                      fluidRow(
                                        column(10, selectInput(inputId = 'content_mode', label = 'Visualisation options', choices = content_modes, selected = 'Volume'), style=pad),
                                        column(2, sliderInput('max_records', 'Records', min=75, max=250, step=5, value=75), style=pad)
                                      )
                             ),
                             tabPanel("TIMELINE", 
                                      br(), p('Timeline-based stats/data for matching media activity'),
                                      fluidRow(
                                        column(12, selectInput(inputId = 'timeline_mode', label = 'Statistic', choices = timeline_modes, selected = 'Volume'), style=pad)
                                      ),
                                      fluidRow(
                                        column(3, sliderInput('smooth', 'Smooth', 0, 5, 3, step = 1), style=pad),
                                        column(4, selectInput(inputId = 'timeline_data', label = 'Data', choices = c('','csv','rss','json','jsonp'), selected = ''), style=pad)
                                      )
                             ),
                             tabPanel("GEO24", 
                                      br(), p('Geographic portal for matching media activity in the past 24 hours'),
                                      fluidRow(
                                        column(12, selectInput(inputId = 'geo_mode', label = 'Geo mode', choices = geo_modes, selected = ''), style=pad)
                                      ),
                                      fluidRow(
                                        column(4, sliderInput('timespan', 'Timespan', 15, 1440, 1440, step = 5), style=pad),
                                        column(4, selectInput(inputId = 'geo_format', label = 'Format', choices = c('','HTML','ImageHTML','ImageHTMLShow','GeoJSON','ImageGeoJSON'), selected = ''), style=pad),
                                        column(4, textInput(inputId = 'geo_near', label = 'Near', value = ''), style=pad)
                                      ),
                                      fluidRow(
                                        column(4, selectInput(inputId = 'geo_cc', label = 'Country', choices = country_codes, selected = 1, multiple=F), style=pad),
                                        column(5, selectizeInput(inputId = 'geo_adm1', label = 'ADM1', choices = NULL, selected = 1, multiple=F), style=pad),
                                        column(3, textInput(inputId = 'geo_loc', label = 'Location', value = ''), style=pad)
                                      )
                             )
                 ),
                 uiOutput('url')
    ),
    mainPanel(width = 8,
              uiOutput('plot_title'),
              fluidRow(htmlOutput("frame"))
    )
  )
)

shinyApp(ui, server)

