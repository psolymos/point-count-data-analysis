# Rscript --vanilla make.R
# Rscript --vanilla make.R --day 1

CONFIG <- rconfig::rconfig()
str(CONFIG)

DAY <- rconfig::value(CONFIG$day, "0")

dir <- switch(
    as.character(DAY[1]),
    "0" = ".",
    "1" = "day-01",
    "2" = "day-02",
    "3" = "day-03",
    stop("Invalid day argument")
)
fl <- list.files(dir, recursive = TRUE)
fl <- fl[grep("\\.qmd$", fl)]

# Render qmd files
for (i in fl) {
    try(quarto::quarto_render(file.path(dir, i), output_format = "all"))
}
