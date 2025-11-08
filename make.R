# Rscript --vanilla make.R

# Render qmd files
fl <- list.files(".", recursive = TRUE)
fl <- fl[grep("\\.qmd$", fl)]
for (i in fl) {
    quarto::quarto_render(i)
}
