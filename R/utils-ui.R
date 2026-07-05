# ── Helpers ──────────────────────────────────────────────────────────────────

#' textInput that fires only on blur.
#' @noRd
text_input_blur <- function(
  inputId,
  label,
  value = "",
  placeholder = NULL,
  width = "100%"
) {
  tag <- textInput(
    inputId,
    label,
    value = value,
    placeholder = placeholder,
    width = width
  )
  tag$children[[2]] <- tagAppendAttributes(
    tag$children[[2]],
    class = "blur-input"
  )
  tag
}

#' textAreaInput that fires only on blur.
#' @noRd
text_area_blur <- function(
  inputId,
  label,
  value = "",
  placeholder = NULL,
  width = "100%",
  rows = 3
) {
  tag <- textAreaInput(
    inputId,
    label,
    value = value,
    placeholder = placeholder,
    width = width,
    rows = rows
  )
  # The <textarea> is the second child of the wrapper div
  tag$children[[2]] <- tagAppendAttributes(
    tag$children[[2]],
    class = "blur-textarea"
  )
  tag
}


#' Dynamic rows UI helper.
#' @noRd
dynamic_rows_ui <- function(ns, n, add_btn_id, add_label, row_fn) {
  tagList(
    !!!purrr::map(seq_len(n), row_fn),
    actionButton(
      ns(add_btn_id),
      add_label,
      icon = icon("plus"),
      class = "btn-outline-secondary btn-sm mt-2"
    )
  )
}
