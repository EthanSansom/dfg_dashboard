# Preamble ---------------------------------------------------------------------
#
# Script name: app.R
#
# Purpose of script: Serve a Data for Good dashboard website.
#
# Author: Ethan Sansom
#
# Date Created: 2022-12-14

# Load Packages ----------------------------------------------------------------
library(shiny)         # Website
library(shinythemes)   # Website themes
library(shinyWidgets)  # Some nicer buttons
library(shinyjs)       # Some Javascript help
library(reactable)     # Reactive Tables
library(showtext)      # Google Fonts
library(tidyverse)     # Data Handling
library(glue)          # String Formatting
library(googlesheets4) # Accessing Google Sheets
library(htmltools)     # Handy HTML helpers
library(htmlwidgets)   # Even more help with HTML
library(leaflet)       # Interactive maps

# Access Google Sheets Data (sheet is public)
googlesheets4::gs4_deauth()
sheet_id <- "https://docs.google.com/spreadsheets/d/1-UDzzSR8FrnC04tcfvfl781D__BBZ-ih5ys9F2OWQuU/edit#gid=0"
org_data <- googlesheets4::read_sheet(sheet_id)

# Link to DfG Logo
logo_img <- "https://dataforgood.ca/wp-content/uploads/2021/04/logo-mobile.png"

# Set colors
dfg_red <- "#F28065"
dfg_purple <- "#A682B8"
dfg_green <- "#89C148"
dfg_blue <- "#73D0F5"

# Define Functions -------------------------------------------------------------

# Return CSS background color command for each type of organization
type_color <- function(type) {
  case_when(
    type == "Partner" ~ "background-color: var(--dfg-red);",
    type == "Affiliate" ~ "background-color: var(--dfg-green);",
    type == "Chapter" ~ "background-color: var(--dfg-blue);"
  )
}

# Convert shinytags to a character string of ready-to-use HMTL
html_to_char <- function(x) {
  # Convert shinytags HTML/CSS to text
  lapply(X = x, FUN = \(x) { as.character(renderTags(x)$html) }) |> 
    # Remove newline characters
    lapply(FUN = \(x) gsub("\n", "", x))
}

# Return a circle of specified size using HTML
circle_div <- function(diameter, fill, fill_opacity = 0.2, border_opacity = 0.5, style = "") {
  
  background_color <- paste(as.vector(col2rgb(fill)), collapse = ", ")
  
  div(style = glue(
    "
    background-color: rgba({background_color}, {fill_opacity});
    border-color: rgba({background_color}, {border_opacity});
    width: {diameter}px;
    height: {diameter}px;
    border-radius: 50%;
    border-style: solid;
    border-width: {floor(diameter/10)}px;
    {style}
    "
  ))
}

# Format all words in a string as proper nouns
# https://stackoverflow.com/questions/24956546/capitalizing-letters-r-equivalent-of-excel-proper-function
proper <- function(x) { gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl=TRUE) }

# Return the leadership team names with proper formatting and sorted alphabetically (by first name)
format_leadership <- function(x) {
  str_split(x, ",") |> 
    lapply(\(x) {trimws(proper(x))}) |> 
    unlist() |>
    sort() |>
    paste(collapse = ", ")
}

# Return the Data for Good Chapter given it's numeric index
index_to_chapter <- function(index) {
  case_when(
    index == 0 ~ "Canada",
    index == 1 ~ "Toronto",
    index == 2 ~ "Waterloo",
    index == 3 ~ "Regina"
  )
}

# Return the Organization Type given it's numeric index
index_to_type <- function(index) {
  case_when(
    index == 0 ~ "All",
    index == 1 ~ "Chapter",
    index == 2 ~ "Partner",
    index == 3 ~ "Affiliate"
  )
}

# Return an HTML page for each row of the reactable, containing long-form details about the organization
# Heavily inspired by: https://glin.github.io/reactable/articles/cran-packages/cran-packages.html
org_details <- function(index, data, popup = FALSE) {
  org <- data[index, ]
  
  # Add Icons for social media links (if any)
  socials <- div(class = "detail-socials")
  if (!is.na(org$twitter)) { socials <- tagAppendChild(socials, tags$a(href = org$twitter, tags$i(class = "fa-brands fa-twitter"))) }
  if (!is.na(org$insta)) { socials <- tagAppendChild(socials, tags$a(href = org$insta, tags$i(class = "fa-brands fa-instagram"))) }
  if (!is.na(org$link)) { socials <- tagAppendChild(socials, tags$a(href = org$link, tags$i(class = "fa-solid fa-link"))) }
  
  detail <- div(
    # Differentiate between popup details (used in leaflet map) and org details (used in reactable)
    class = if_else(popup, "org-popup", "org-detail"),
    
    # Organization Title
    div(
      class = "detail-sec detail-header",
      org$name,
      # span("x") is a hack to space the title text (adding margins adds problems)
      span("x", style = "color: transparent"),
      span(class = "detail-label", if_else(org$type == "Chapter", "", paste(org$type, "Organization")), style = "white-space: nowrap;")
    ),
    
    # Progress Bar Animation (as in https://dataforgood.ca/)
    div(class = "detail-sec progress-bar", div(class = "bar-start", style = type_color(org$type)), style = if_else(popup, "width: 100%;", "")),
    
    # Organization Sector
    if (!popup & org$type != "Chapter") {
      div(class = "detail-sec detail-label", "Sector", div(class = "detail-desc", org$sector))
    } else {
      NULL
    },
    
    # Organization Description
    div(class = "detail-sec detail-label", "About", div(class = "detail-desc", org$description)),
    
    # Add either the Chapter Leadership or Org's relationship with Data for Good
    if (popup & org$type == "Chapter") {
      NULL
    } else if (org$type == "Chapter") {
      div(class = "detail-sec detail-label", "Leadership Team", div(class = "detail-desc", org$leadership))
    } else {
      div(class = "detail-sec detail-label", style = "white-space: unset;", paste0("Data for Good and ", org$name), div(class = "detail-desc", org$dfg_relationship))
    },
    
    # Social media links
    socials
  )
  
  detail
}

# Pre-Process Data -------------------------------------------------------------

# Clean up the leadership team text
org_data <-
  org_data |>
  mutate(
    leadership = lapply(X = leadership, FUN = format_leadership)
  )

# Add size and color specifications for mapping
org_data <-
  org_data |>
  mutate(
    color =
      case_when(
        type == "Partner" ~ dfg_red,
        type == "Affiliate" ~ dfg_green,
        type == "Chapter" ~ dfg_blue
      ),
    size = if_else(type == "Chapter", 10, 5)
  )

# Add HTML for leaflet map pop-ups
org_data <- 
  org_data |> 
  mutate(
    # Generate shinytags HTML/CSS for each leaflet organization pop-up
    my_popup = lapply(X = row_number(), FUN = org_details, popup = TRUE, data = org_data),
    # Convert shinytags list to text
    my_popup = html_to_char(my_popup)
  )

### A Note on Leaflet Pop-Ups: ----
#
# When adding circle markers to a Leaflet map via leaflet::addCircleMarkers(),
# the `popup` parameter accepts a character which is used as the display text
# for the location popup.
#
# However, if you pass valid HTML as a character to the popup, this HTML will be
# rendered instead. This is the reason for the shinytags HTML to text conversion.

# Create the base-map ----------------------------------------------------------

# Create map
base_map <-
  leaflet(org_data, options = leafletOptions(minZoom = 2)) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  
  # Provide Chapter/Organization Map Markers
  addCircleMarkers(
    lng = ~lon, 
    lat = ~lat, 
    radius = ~size, 
    color = ~color, 
    weight = 2, 
    opacity = 0.5, 
    popup = ~my_popup
    ) |>
  
  # Set Initial View to Canada
  setView(lng = -108, lat = 60, zoom = 4) |>
  
  # Restrict map view to Earth (don't allow map wrapping)
  setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)

# Shiny Component --------------------------------------------------------------

## UI ----
ui <- bootstrapPage(
  
  useShinyjs(),
  
  # Source font awesome icons
  tags$script(src="https://kit.fontawesome.com/700fad3b84.js", crossorigin="anonymous"),
  
  # Javascript script to make media queries (window width)
  # https://stackoverflow.com/questions/47292295/shiny-dynamic-content-based-on-window-size-like-css-media-query
  tags$head(tags$script('
                        var width = 0;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        $(window).resize(function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        ')),
  
  navbarPage(
    # Replace the title with the Data for Good logo
    # title = div(img(src = logo_img)),
    title = tags$img(src = "dfg_logo.png", height = "100%"),
    windowTitle = "Data for Good",
    theme = shinytheme("flatly"),
    collapsible = TRUE, 
    
    header = tags$head(includeCSS("navbar_theme.css")),
    
    # Leaflet Map UI
    tabPanel(
      # Title
      "Map",
      
      # Style
      tags$head(includeCSS("leaflet_theme.css")),
      
      # Map Container
      div(class = "outer", leafletOutput("org_map", width = "100%", height = "100%"),
      
      # Info Button
      # NOTE: There's a fair bit of CSS to get this button to look the way
      #       that I wanted, which in hindsight is not entirely necessary. This
      #       absolutePanel is just a container for a customized prettyCheckbox.
      absolutePanel(
        bottom = 100, left = 15, width = "30px", height = "30px",
        style = "display: flex; flex-direction: column; background: white; border-radius: 4px; border: none; overflow: hidden;",
        
        div(
          style = "width: 30px; height: 30px; overflow: hidden;", class = "button-container",
          shinyWidgets::prettyCheckbox(inputId = "info", fill = FALSE, value = FALSE, label = "", icon = icon("question", style = "color: black;"))
        )
      ),
      
      # Info Menu; pops up when the Info Button is pressed
      absolutePanel(
        id = "infoPage", style = "box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.1); background: #FFFFFF; margin-top: 56px; padding: 1rem; border-radius: 4px; overflow: scroll;",
        bottom = 165, left = 15, width = "25%", fixed = TRUE, draggable = FALSE, height = "50%",
        
        # Menu Description
        div(class = "detail-sec detail-label", "About", div(class = "detail-desc", "Explore Data for Good Chapters, Partners, and Affiliate Organizations across Canada."), style = "white-space: nowrap;"),
        
        # Input Select Label
        div(class = "detail-sec detail-label", "Location Select", style = "margin-bottom: 5px;"),
        
        # Input Select; selects the Data for Good chapter location to view
        # NOTE: The "selectTable" and "selectMap" inputs are connected
        selectInput("selectMap", width = "100%", label = NULL, choices = list("Canada" = 0, "Toronto" = 1, "Waterloo" = 2, "Regina" = 3), selected = 0),
        
        # Map Legend
        div(class = "detail-sec detail-label", "Legend", style = "margin-bottom: 5px;"),
        div(circle_div(20, dfg_blue, style = "margin-right: 0.5rem;"), "DfG Chapter", style = "display: flex; align-items: center; padding: 5px;"),
        div(circle_div(15, dfg_red, style = "margin-left: 1px; margin-right: 0.5rem;"), "Partner Organization", style = "display: flex; align-items: center; padding: 5px;"),
        div(circle_div(15, dfg_green, style = "margin-left: 1px; margin-right: 0.5rem;"), "Affiliate Organization", style = "display: flex; align-items: center; padding: 5px;"),
        )
      )
    ),
    
    # Reactable UI
    tabPanel(
      # Title
      "Data Explorer",
      
      fluidPage(
          # Header and Filters
          column(3,
  
            # About
            div(
              class = "detail-sec detail-label", "About", style = "white-space: nowrap;",
              div(class = "detail-desc", "Click on any row for more details.") 
            ),
            
            # Input Select Label
            div(class = "detail-sec detail-label", "Location Select", style = "margin-bottom: 5px;"),
            
            # Input Select; filters reactable by Data for Good chapter location
            # NOTE: The "selectTable" and "selectMap" inputs are connected
            selectInput("selectTable", width = "100%", label = NULL, choices = list("Canada" = 0, "Toronto" = 1, "Waterloo" = 2, "Regina" = 3), selected = 0),
            
            # Input Select Label
            div(class = "detail-sec detail-label", "Organization Select", style = "margin-bottom: 5px;"),
    
            # Input Select; filters reactable by organization type
            selectInput("selectType", width = "100%", label = NULL, choices = list("All" = 0, "Chapter" = 1, "Partner" = 2, "Affiliate" = 3), selected = 0)
          ),
          
          # Reactable Output
          column(9, reactableOutput(outputId = "org_table", width = "100%", height = "auto", inline = FALSE))
      )
    ),
      
    # About Page UI
    tabPanel(
      # Title
      "About",
      
      # Website Preamble
      div(
        class = "detail-sec detail-header", "About this Website", style = "white-space: nowrap;", 
        div(class = "detail-desc detail-sec", "This website is not officially affiliated with Data for Good! It is, however, a side-project of the Data for Good Toronto Chapter Community Co-Lead (or DfGYYZCCC-L for short)."),
      ),
      
      # Details
      div(
        class = "detail-sec detail-header", "Data for Good", style = "white-space: nowrap;", 
        div(class = "detail-desc detail-sec", "Data for Good is a non-profit organization which allows people to use their data science expertise to make the world a better place. We were founded in 2013 by Joy Robson, a marketing & strategy executive, and Victor Anjos, Data Scientist & CTO. Data for Good began as a single chapter in Toronto, and has since grown nationwide, with chapters in Vancouver, Calgary, Edmonton, Regina, Ottawa, Toronto, Waterloo, and Montreal."),
        div(class = "detail-sec detail-desc", "See ", a(href = "https://dataforgood.ca/", "dataforgood.ca"), "to learn more about our organization and see how you can contribute to Data for Good.")
      ),
      
      # Code
      div(
        class = "detail-sec detail-header", "Code", style = "white-space: nowrap;", 
        div(class = "detail-desc detail-sec", "The code used to generate this Shiny is available on ", a(href = "https://github.com/EthanSansom/dfg_dashboard", "Github.")),
        div(class = "detail-desc detail-sec", "This website's design and implementation draw heavily from Greg Lin's {reactable} package demo, ", a(href = 'https://glin.github.io/reactable/articles/cran-packages/cran-packages.html', 'CRAN Packages'), ", and Edward Parker and Quentin Leclerc's ", a(href = 'https://vac-lshtm.shinyapps.io/ncov_tracker/#', 'Covid-19 Tracker.'))
      ),
      
      # Author
      div(
        class = "detail-sec detail-header", "Author", style = "white-space: nowrap;", 
        div(class = "detail-desc", "Ethan Sansom ", a(href = "https://twitter.com/EthanSansom2", tags$i(class = "fa-brands fa-twitter")), a(href = "https://github.com/EthanSansom", tags$i(class = "fa-brands fa-github")), a(href = "https://www.linkedin.com/in/ethansansom/", tags$i(class = "fa-brands fa-linkedin-in")))
      )
    )
  )
)

## Server ----
server <- function(input, output) {
  
  # Render Leaflet map
  output$org_map <- renderLeaflet({
    base_map
  })
  
  # Show/Hide the Help Menu
  observeEvent(input$info, {
    
    if (input$info) {
      shinyjs::show(id = "infoPage")
    } else {
      shinyjs::hide(id = "infoPage")
    }
  
  })
  
  # Set Leaflet Map location view via selectMap input
  observeEvent(input$selectMap, {
    
    if (input$selectMap == 1) {
      leafletProxy("org_map") |> setView(lat = 43.65332, lng = -79.38398, zoom = 13)
    } else if (input$selectMap == 2) {
      leafletProxy("org_map") |> setView(lat = 43.4643, lng = -80.5204, zoom = 13)
    } else if (input$selectMap == 3) {
      leafletProxy("org_map") |> setView(lat = 50.4452, lng = -104.6189, zoom = 13)
    } else {
      leafletProxy("org_map") |> setView(lng = -108, lat = 60, zoom = 4)
    }
    
  })
  
  # Filter Reactable via selectTable input
  observeEvent(input$selectTable, {
    
    if (input$selectTable == 1) {
      leafletProxy("org_map") |> setView(lat = 43.65332, lng = -79.38398, zoom = 13)
    } else if (input$selectTable == 2) {
      leafletProxy("org_map") |> setView(lat = 43.4643, lng = -80.5204, zoom = 13)
    } else if (input$selectTable == 3) {
      leafletProxy("org_map") |> setView(lat = 50.4452, lng = -104.6189, zoom = 13)
    } else {
      leafletProxy("org_map") |> setView(lng = -108, lat = 60, zoom = 4)
    }
    
  })
  
  # Connect the selectMap and selectTable inputs
  observeEvent(input$selectTable, {
    updateSliderInput(inputId = "selectMap", value = input$selectTable)
  })
  
  observeEvent(input$selectMap, {
    updateSliderInput(inputId = "selectTable", value = input$selectMap)
  })
  
  # Filter reactable input data via selectMap and selectTable inputs
  reactive_org_data <- reactive({
    
    reactive_data <- org_data
    
    if (input$selectType != 0) {
      reactive_data <- reactive_data |> filter(type == index_to_type(input$selectType))
    }
    
    if (input$selectTable != 0) {
      reactive_data <- reactive_data |> filter(chapter == index_to_chapter(input$selectTable))
    }
    
    reactive_data
    
  })
  
  # Reactive columns to display in reactable; determined by window width media query
  reactive_display_cols <- reactive({
    
    if (input$width > 1000) {
      reactive_cols <- c("name", "description", "sector", "chapter", "type")
    } else if (input$width > 600) {
      reactive_cols <- c("name", "sector", "chapter", "type")
    } else {
      reactive_cols <- c("name", "chapter", "type")
    }
    
    reactive_cols
    
  })
  
  # Render Reactable
  output$org_table <-
    renderReactable(
      
      # Reactable Body
      reactable(
        # Reactive input data and display columns
        reactive_org_data()[, reactive_display_cols()],
        
        # Table Parameters
        sortable = FALSE, 
        pagination = TRUE, 
        showPageInfo = FALSE,
        wrap = FALSE,
        resizable = TRUE,
        onClick = "expand",
        class = "orgs-table",
        
        # Column Details
        # NOTE: Reactable throws error if columns outside of input data are defined
        columns = if ("description" %in% reactive_display_cols()) {
          list(
            # NOTE: Setting aggregate to unique allows each row to display the same
            #       data when expanded and collapsed. Otherwise, the data will be 
            #       displayed differently depending on state.
            name = colDef(minWidth = 150, maxWidth = 200, name = "Name", aggregate = "unique"),
            description = colDef(name = "Description", style = "white-space: nowrap;"),
            sector = colDef(name = "Sector", aggregate = "unique", maxWidth = 200),
            chapter = colDef(name = "Chapter", aggregate = "unique", maxWidth = 200),
            type = colDef(name = "Type", aggregate = "unique", maxWidth = 200)
          )
        } else if ("sector" %in% reactive_display_cols()) {
          list(
            name = colDef(minWidth = 100, name = "Name", aggregate = "unique"),
            sector = colDef(name = "Sector", aggregate = "unique", minWidth = 100),
            chapter = colDef(name = "Chapter", aggregate = "unique", maxWidth = 100),
            type = colDef(name = "Type", aggregate = "unique", maxWidth = 100)
          )
        } else {
          list(
            name = colDef(minWidth = 100, name = "Name", aggregate = "unique"),
            chapter = colDef(name = "Chapter", aggregate = "unique", maxWidth = 80),
            type = colDef(name = "Type", aggregate = "unique", maxWidth = 80)
          )
        },
        
        # Defines the HTML to display beneath a row when clicked
        # See: https://glin.github.io/reactable/articles/cran-packages/cran-packages.html
        details = function(index) {tabPanel("", org_details(index, data = reactive_org_data()))}
      )
    )

}

## Run ----
shinyApp(ui = ui, server = server)
