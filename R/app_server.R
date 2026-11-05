#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session, pool) {
  # Authentication
  user <- reactiveValues(name = NULL, editor_id = NULL, display_name = NULL)

  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      db = golem::get_golem_options(which = "credentials_path"),
      passphrase = golem::get_golem_options(which = "credentials_pass")
    ),
    timeout = 60
  )

  # Handle login - just set the username
  observeEvent(res_auth, {
    req(res_auth$user)
    user$name <- res_auth$user
  })

  # Look up or create editor record after login
  observeEvent(user$name, {
    req(user$name)

    # Look up editor record
    editor_id <- db_get_editor_by_login(pool, user$name)

    if (is.null(editor_id)) {
      # Editor doesn't exist - show registration modal
      showModal(
        modalDialog(
          title = "Welcome to GiDB!",
          p("Register your account to start editing."),
          textInput(
            session$ns("editor_first_name"),
            "First Name",
            placeholder = "Enter your first name"
          ),
          textInput(
            session$ns("editor_last_name"),
            "Last Name",
            placeholder = "Enter your last name"
          ),
          easyClose = FALSE,
          footer = tagList(
            actionButton(
              session$ns("editor_register"),
              "Register",
              class = "btn-primary"
            )
          )
        )
      )
    } else {
      # Editor exists - get their name
      user$editor_id <- editor_id
      user$display_name <- db_get_editor_name(pool, editor_id)
    }
  })

  # Handle editor registration
  observeEvent(input$editor_register, {
    req(input$editor_first_name, input$editor_last_name)
    req(
      nzchar(trimws(input$editor_first_name)),
      nzchar(trimws(input$editor_last_name))
    )

    first <- trimws(input$editor_first_name)
    last <- trimws(input$editor_last_name)

    editor_id <- db_create_editor(pool, user$name, first, last)

    if (!is.null(editor_id)) {
      user$editor_id <- editor_id
      user$display_name <- paste(first, last)
      removeModal()
      showNotification(
        paste("Welcome,", user$display_name, "!"),
        type = "message"
      )
    } else {
      showNotification("Failed to register. Please try again.", type = "error")
    }
  })

  # Game editor module
  game_editor_server <- mod_game_editor_server(
    "game_editor_1",
    pool = pool,
    user = user
  )

  # Render user display in top right corner
  output$user_display <- renderUI({
    req(user$name)
    if (!is.null(user$display_name) && nzchar(user$display_name)) {
      div(
        class = "d-flex align-items-center gap-2",
        bsicons::bs_icon("person-circle"),
        span(class = "text-muted small", "Logged in as"),
        strong(user$display_name)
      )
    } else {
      div(
        class = "d-flex align-items-center gap-2",
        bsicons::bs_icon("person-circle"),
        span(class = "text-muted small", "Logged in as"),
        strong(user$name)
      )
    }
  })
}
