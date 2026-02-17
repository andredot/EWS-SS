library(quarto)
areas <- c("Lombardia", "Milano", "Valtellina")
for (a in areas) {
  # quarto_render(
  #   input = "report.qmd",
  #   execute_params = list(area = a),
  #   output_file = paste0("EWS_Report_", a, ".html")
  # )
  quarto_render(
    input = "validation_report.qmd",
    execute_params = list(area = a),
    output_file = paste0("EWS_Validation_Report_", a, ".html")
  )
}
