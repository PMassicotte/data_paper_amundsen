
pdf_fig <- fs::dir_ls("graphs/", regexp = "fig.*pdf$")

df <- tibble(pdf_fig) |>
  mutate(png_fig = fs::path_ext_set(pdf_fig, "png"))

future_walk2(df$pdf_fig, df$png_fig, ~ pdftools::pdf_convert(
  pdf = ..1,
  format = "png",
  filenames = ..2,
  dpi = 600
))
