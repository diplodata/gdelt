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
    } else{
      # CONTENT or TIMELINE modes
      # date params
      shinyjs::enable("timeline_daterange")
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
      } else if(input$output_tab == 'TIMELINE'){
        url = paste0(url, '&mode=', input$timeline_mode, '&TIMELINESMOOTH=', input$smooth)
      }
      if(input$max_records > 75) url = paste0(url, '&maxrecords=', input$max_records)
      if(input$data_format != '') url = paste0(url, '&format=', input$data_format)
      url = paste0(url, '&startdatetime=', start_date, '&enddatetime=', end_date)
    }
    
    # Manage widget fuctionality
    if(input$output_tab == 'GEO24'){
      for(i in c('timeline_daterange', 'timeline_hours', 'search_lang')) shinyjs::disable(i)
      for(i in c('max_records', 'data_format')) shinyjs::hide(i)
    } else{
      for(i in c('timeline_daterange', 'timeline_hours', 'search_lang')) shinyjs::enable(i)
      for(i in c('max_records', 'data_format')) shinyjs::show(i)
      if(is.na(input$timeline_hours)) shinyjs::enable("timeline_daterange") else shinyjs::disable("timeline_daterange")
    }
    
    # render to interface for reference and external use
    output$url = renderUI({ HTML(paste0('<p style="color:#999;font-size: 90%; word-wrap: break-word;">', url, '</p><p  style="color:#999;font-size: 80%;">(2017 Robin Edwards. This site is an interface to the GDELT API but has has no formal affiliation.)</p>')) })
    
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
  
  # Help dialogue
  observeEvent(input$help, {
    showModal(modalDialog(
      h1("How to use GDELT"),
      hr(),
      p("GDELT is a web search tool for media, official and typically non-commercial content that offers some powerful alternative ways to find what you want (", a('GDELT home', href = 'https://www.gdeltproject.org/', target="_blank"), '/', a('reference', href = 'https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/', target="_blank"), ').' ," GDELT monitors global news media in multiple languages and processes it with advanced machine learning and deep learning algorithms to offer more intelligent search. Some usage examples include:"),
      tags$ul(
        tags$li('searching globally in English for content in local languages on a particular topic;'),
        tags$li('searching for all content posted in the past hour from Japan, or in Japanese, or from a particular website;'),
        tags$li('finding content based on the features and/or text found in its accompanying imagery;'),
        tags$li('finding content based on the themes it relates to; or'),
        tags$li('searching with a combination of these methods')
      ),
      p("The panel on the left structures the interface into two main groups - ", strong('inputs'), " and ", strong('outputs'), ". Inputs (top) are the search terms and other criteria that define the query. Outputs (below) are the CONTENT, TIMELINE and GEO24 tabs that group the various modes you can use to visualise and understand the results."),
      h3('INPUTS'),
      p('The service offers 3 ways to search for content:'),
      tags$ul(
        tags$li(strong('Search term'), " - any work or phrase. Phrases should be nested in double quotes - e.g. ", code('"the Ides of March"'),". Unquoted words seperated by spaces are interpretted as 'x and y' You can search for 'x or y' by enclosing terms in brackets and seperating with 'OR', e.g.", code('(cats OR dogs)'), '.'), 
        tags$li(strong('Image tags'), " - images within content are processed using deep learning algorithms to identify features and text they contain. Search for available tags in the dialogue box, or ", a(href = 'http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGETAGS.TXT', 'see the full list', target="_blank"), '.'), 
        tags$li(strong('Themes'), "- these offer a powerful way of searching for complex topics, since they can include hundreds or even thousands of different phrases or names under a single heading. Themes are based on GDELT's Global Knowledge Graph (GKG). Search for relevant themes in the dialogue box, or ", a(href = 'http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT', 'see the full list', target="_blank"), '.')
      ),
      p('These work together so e.g. you can specify a search for both search terms AND image tags. Note that when using the GEO24 output the available mode options tend to only work with either search terms or image tags - if you get an error message you may need to re-specify your criteria.'),
      h4('Other criteria'),
      p("You can further narrow down your query in the following ways:"),
      tags$ul(
        tags$li(strong('Search language'), " - defines the language of the search terms if not English. (This feature seems to have some bugs.)"), 
        tags$li(strong('Country'), " - defines the country where the media outlets are located. (", a('country list', href = 'http://data.gdeltproject.org/api/v2/guides/LOOKUP-COUNTRIES.TXT', target="_blank"), ')'), 
        tags$li(strong('Domain'), "- define web domain of content - e.g. website", code('bbc.co.uk'), ' or top-level domain ', code('.gov'), ' (US government content).'),
        tags$li(strong('Date range'), " - define any window for content dated in the past 3 months. The 'Hours' field must be empty to enable this."), 
        tags$li(strong('Hours'), " - set to return most recent content in terms of hours."),
        tags$li(strong('Source language'), " - defines the country where the media outlets are located. (", a('supported languages', href = 'http://data.gdeltproject.org/api/v2/guides/LOOKUP-LANGUAGES.TXT', target="_blank"), ')')
      ),
      h3('OUTPUTS'),
      tags$ul(
        tags$li(strong('CONTENT'), " - This tab offers the various modes to access the content matching the search."), 
        tags$li(strong('TIMELINE'), " - This offers the modes available to view volumetric trends for the query over time."), 
        tags$li(strong('GEO24'), " tab brings up a range of geographical tools to investigate media published in the past 24 hours. The ",em('"Geo mode"'), "dropdown gives the options. Some of the tools are image specific, and will only work for image tag searches (see ", em('IMAGE TAGS'), " tab). Others will work for search terms and not image tags. Most of the tools offer map-based insights not into media source locations but the places and countries mentioned in the content. You can specify your search in terms of countries, sub-national regions (NUTS1), place names or within a user-defined distance from a geographical point. This toolset uses a seperate API (", a('see documentation', href = 'https://blog.gdeltproject.org/gdelt-geo-2-0-api-debuts/', target="_blank"), ').')
      ),
      p("If you want to export the results from CONTENT or TIMELINE choose from the options under ", em("Format"),
        ". The URL at the bottom can be used to call the query output directly from the GDELT API."),
      easyClose = TRUE
      , size = 'l'))
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
                    iframe {
                      zoom: .667;
                      -webkit-zoom: .667;
                      -ms-zoom: .667;
                      -moz-transform: scale(.667, .667);
                      -webkit-transform: scale(1);
                      -o-transform: scale(1, 1);
                      -ms-transform: scale(1.5, 1.5);
                      transform: scale(1.5, 1.5);
                      -moz-transform-origin: top left;
                      -webkit-transform-origin: top left;
                      -o-transform-origin: top left;
                      -ms-transform-origin: top left;
                      transform-origin: top left;
                    }
                    @media screen and (-webkit-min-device-pixel-ratio:0) { #scaled-frame { zoom: .667;} }

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
  bsTooltip(id = 'max_records', title = 'GDELT will return 75 by default, but this can be increased to 250', placement = "right", trigger = "hover"),
  bsTooltip(id = 'data_format', title = 'Specify format for data export', placement = "top", trigger = "hover"),
  bsTooltip(id = 'geo_near', title = 'Returns all matches within a certain radius (bounding box) of a given point. You specify a particular latitude and longitude and distance in either miles (default) or kms (e.g. for 100km from Paris "48.85,2.35,100km")', placement = "top", trigger = "hover"),
  bsTooltip(id = 'timespan', title = 'The geo portal searches the past 24 hours (1,440 mins), but this can be reduced further to a minimum timespan of 15 mins', placement = "top", trigger = "hover"),
  bsTooltip(id = 'geo_cc', title = 'Specify country of media mentions', placement = "top", trigger = "hover"),
  bsTooltip(id = 'geo_adm1', title = 'Specify ADM1 (top sub-national) geographical region of media mentions', placement = "top", trigger = "hover"),
  bsTooltip(id = 'geo_loc', title = 'Searches for a given word or phrase in the full formal name of the location - e.g. New York', placement = "top", trigger = "hover"),
  bsTooltip(id = 'geo_format', title = 'Specify format for data export', placement = "top", trigger = "hover"),
  bsTooltip(id = 'tab-6471-3', title = 'Specify format for data export', placement = "top", trigger = "hover"),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 fluidRow(
                   column(3, h3("GDELT Project")),
                   column(9, p(a('GDELT', href = 'https://www.gdeltproject.org/', target="_blank"), " is a web search tool that offers more intelligent ways to find what you want. Search globally in English for local-language content. Find articles based on features in accompanying imagery. (", a('See documentation', href = 'https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/', target="_blank"), ')'))
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
                                        column(10, selectInput(inputId = 'content_mode', label = 'Visualisation options', choices = content_modes, selected = 'Volume'), style=pad)
                                      ),
                                      br()
                             ),
                             tabPanel("TIMELINE", 
                                      br(), #p('Timeline-based stats/data for matching media activity'),
                                      fluidRow(
                                        column(10, selectInput(inputId = 'timeline_mode', label = 'Statistic', choices = timeline_modes, selected = 'Volume'), style=pad),
                                        column(2, sliderInput('smooth', 'Smooth', 0, 5, 3, step = 1), style=pad)
                                      )
                             ),
                             tabPanel("GEO24",
                                      br(), #p('Geographic portal for matching media activity in the past 24 hours'),
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
                 fluidRow(
                   column(3, sliderInput('max_records', 'Records', min=75, max=250, step=5, value=75), style=pad),
                   column(4, selectInput(inputId = 'data_format', label = 'Format', choices = c('','csv','rss','json','jsonp'), selected = ''), style=pad)
                 ),
                 hr(), uiOutput('url')
    ),
    mainPanel(width = 8,
              fluidRow(
                column(11, uiOutput('plot_title')),
                column(1, actionButton("help", "Help"))
              ),
              fluidRow(htmlOutput("frame"))
    )
  )
)

shinyApp(ui, server)

