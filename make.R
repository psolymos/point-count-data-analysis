# # Render Rmd files
# fl <- list.files(".", recursive=TRUE)
# fl <- fl[grep("\\.Rmd$", fl)]
# for (i in fl) {
#     rmarkdown::render(i)
# }

# Render qmd files
fl <- list.files(".", recursive = TRUE)
fl <- fl[grep("\\.qmd$", fl)]
for (i in fl) {
    quarto::quarto_render(i)
}
