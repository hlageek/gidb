#' data_editor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_editor_ui <- function(id) {
  ns <- NS(id)

  items <- list(
    bslib::accordion_panel(
      "Core Info",
      textInput(
        ns("game_external_ids"),
        "External IDs",
        value = "",
        width = "100%"
      )
    ),
    bslib::accordion_panel(
      "Classification",
      textInput(ns("genre"), "Genre", value = "", width = "100%")
    ),
    bslib::accordion_panel(
      "Developer",
      textInput(ns("game_dev"), "Developer", value = "", width = "100%"),
      textInput(
        ns("geonames_id"),
        span(
          "Developer location according to",
          a(
            "Geonames ID",
            href = "https://www.geonames.org/",
            target = "_blank"
          )
        ),
        value = "",
        width = "100%"
      ),
      actionButton(
        ns("retrive_geonames"),
        "Retrieve data for Geonames ID",
        icon = icon("search")
      ),
      textOutput(ns("geonames_output"))
    ),
    bslib::accordion_panel(
      "Notes",
      textAreaInput(ns("game_notes"), "Notes", value = "", width = "100%")
    )
  )
  rlang::inject(
    bslib::accordion(!!!items, id = ns("game_accordion"), open = FALSE)
  )
}

#' data_editor Server Functions
#'
#' @noRd
mod_data_editor_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues()

    observeEvent(input$retrive_geonames, {
      print("clicked")
      place <- get_geonames_place(as.integer(input$geonames_id), "pgps")
      print(place$placeName)
      loc$place_name <- place$name
      loc$country_name <- place$countryName
      loc$country_code <- place$countryCode
    })

    output$geonames_output <- renderText({
      print(paste(
        loc$place_name,
        loc$country_name,
        loc$country_code,
        sep = ", "
      ))
      paste(
        loc$place_name,
        loc$country_name,
        loc$country_code,
        sep = ", "
      )
    })
  })
}

## To be copied in the UI
# mod_data_editor_ui("data_editor_1")

## To be copied in the server
# mod_data_editor_server("data_editor_1")
