## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(data.table)
library(grepreaper)
library(ggplot2)

# Check if grep is available for chunks to use
has_grep <- nzchar(Sys.which("grep"))

vignette_wd <- file.path(tempdir(), "grepreaper_vignette")
if (!dir.exists(vignette_wd)) dir.create(vignette_wd, recursive = TRUE)

data.table::fwrite(ggplot2::diamonds, file.path(vignette_wd, "diamonds.csv"))

zip_path <- system.file("extdata", "ratings_data.zip", package = "grepreaper")

if (zip_path != "") {
  utils::unzip(zip_path, exdir = vignette_wd)
} else {
  r_dir <- file.path(vignette_wd, "ratings_data")
  if (!dir.exists(r_dir)) dir.create(r_dir)
  for(i in 1:10) {
    fwrite(data.frame(user=paste0("U",i), rating=5), file.path(r_dir, paste0("file_",i,".csv")))
  }
}

knitr::opts_knit$set(root.dir = vignette_wd)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(data.table)
library(grepreaper)
library(ggplot2)

# Check if grep is available for chunks to use
has_grep <- nzchar(Sys.which("grep"))

vignette_wd <- file.path(tempdir(), "grepreaper_vignette")
if (!dir.exists(vignette_wd)) dir.create(vignette_wd, recursive = TRUE)

data.table::fwrite(ggplot2::diamonds, file.path(vignette_wd, "diamonds.csv"))

zip_path <- system.file("extdata", "ratings_data.zip", package = "grepreaper")

if (zip_path != "") {
  utils::unzip(zip_path, exdir = vignette_wd)
} else {
  r_dir <- file.path(vignette_wd, "ratings_data")
  if (!dir.exists(r_dir)) dir.create(r_dir)
  for(i in 1:10) {
    fwrite(data.frame(user=paste0("U",i), rating=5), file.path(r_dir, paste0("file_",i,".csv")))
  }
}

knitr::opts_knit$set(root.dir = vignette_wd)

## ----eval = has_grep----------------------------------------------------------
diamonds <- fread(input = "diamonds.csv")
diamonds[1:5,]

## ----eval = has_grep----------------------------------------------------------
ideal <- diamonds[cut == "Ideal",]
ideal[1:5,]

## ----eval = has_grep----------------------------------------------------------
diamonds <- fread(cmd = "grep '' 'diamonds.csv'")
diamonds[1:5,]

## ----eval = has_grep----------------------------------------------------------
ideal <- fread(cmd = "grep 'Ideal' 'diamonds.csv'")
ideal[1:5,]

## ----eval = has_grep----------------------------------------------------------
diamonds <- grep_read(files = "diamonds.csv")
diamonds[1:5,]

## ----eval = has_grep----------------------------------------------------------
grep_read(files = "diamonds.csv", show_cmd = TRUE)

## ----eval = has_grep----------------------------------------------------------
ideal <- grep_read(files = "diamonds.csv", pattern = "Ideal")
ideal[1:5,]

## ----eval = has_grep----------------------------------------------------------
grep_read(files = "diamonds.csv", pattern = "Ideal", show_cmd = TRUE)

## ----eval = has_grep----------------------------------------------------------
multiple_cuts <- grep_read(files = "diamonds.csv", pattern = c("Ideal", "Very Good"))
multiple_cuts[1:5,]

## ----eval = has_grep----------------------------------------------------------
grep_read(files = "diamonds.csv", pattern = c("Ideal", "Very Good"), show_cmd = TRUE)

## ----eval = has_grep----------------------------------------------------------
grep_read(files = "diamonds.csv", pattern = c("SI2"), invert = TRUE)[1:5,]

## ----eval = has_grep----------------------------------------------------------
grep_read(files = "diamonds.csv", pattern = c("SI2"), invert = TRUE, show_cmd = TRUE)

## ----eval = has_grep----------------------------------------------------------
grep_read(files = "diamonds.csv", pattern = c("ideal"), ignore_case = TRUE)[1:5,]

## ----eval = has_grep----------------------------------------------------------
grep_read(files = "diamonds.csv", pattern = c("ideal"), ignore_case = TRUE, show_cmd = TRUE)

## ----eval = has_grep----------------------------------------------------------
grep_read(files = "diamonds.csv", pattern = "IdEaL", ignore_case = TRUE ,fixed = TRUE)[1:5,]

## ----eval = has_grep----------------------------------------------------------
grep_read(files = "diamonds.csv", pattern = "IdEaL", ignore_case = TRUE ,fixed = TRUE, show_cmd = T)

## ----eval = has_grep----------------------------------------------------------
grep_read(path = ".", recursive = TRUE, pattern = "Ideal", file_pattern = ".csv")[1:5,]

## ----eval = has_grep----------------------------------------------------------
cmd <- grep_read(path = ".", recursive = TRUE, pattern = "Ideal", file_pattern = ".csv", show_cmd = TRUE)
substring(text = cmd, first = 1, last = 100)

## ----eval = has_grep----------------------------------------------------------
grep_read(files = "diamonds.csv", pattern = "VS1", word_match = TRUE)

## ----eval = has_grep----------------------------------------------------------
grep_read(files = "diamonds.csv", pattern = "VS1", word_match = TRUE, show_cmd = TRUE)

## -----------------------------------------------------------------------------
grep_read(files = "diamonds.csv", include_filename = TRUE)[1:5]

## -----------------------------------------------------------------------------
grep_read(files = "diamonds.csv", include_filename = TRUE, show_cmd = TRUE)

## ----eval = has_grep----------------------------------------------------------
grep_read(files = "diamonds.csv", pattern = "Ideal", show_line_numbers = TRUE)[1:5]

## ----eval = has_grep----------------------------------------------------------
grep_read(files = "diamonds.csv", pattern = "Ideal", show_line_numbers = TRUE, show_cmd = T)

## ----eval = has_grep----------------------------------------------------------
two_files <- c("ratings_data/file_1.csv", "ratings_data/file_2.csv")
grep_read(files = two_files)

## ----eval = has_grep----------------------------------------------------------
grep_read(files = two_files, show_cmd = TRUE)

## ----eval = has_grep----------------------------------------------------------
ten_files <- sprintf("ratings_data/file_%d.csv", 1:10)
grep_read(files = ten_files)

## ----eval = has_grep----------------------------------------------------------
grep_read(files = ten_files, show_cmd = TRUE)

## ----eval = has_grep----------------------------------------------------------
all_files <- list.files(path = "ratings_data", pattern = ".csv", full.names = TRUE)
length(all_files)
all_files[1:10]

## -----------------------------------------------------------------------------
ratings <- grep_read(files = all_files)
ratings

## ----eval = has_grep----------------------------------------------------------
ratings_0kG80toKp2msfAut <- grep_read(files = all_files, pattern = "0kG80toKp2msfAut")
ratings_0kG80toKp2msfAut

## ----eval = has_grep----------------------------------------------------------
ratings_1fg4sLgEFzAtOqCa <- grep_read(path = "ratings_data", file_pattern = ".csv", pattern = "1fg4sLgEFzAtOqCa")
ratings_1fg4sLgEFzAtOqCa

## ----eval = has_grep----------------------------------------------------------
grep_read(files = "diamonds.csv", nrows = 3)

## ----eval = has_grep----------------------------------------------------------
grep_count(files = "diamonds.csv")

## ----eval = has_grep----------------------------------------------------------
grep_count(files = ten_files)

## ----eval = has_grep----------------------------------------------------------
grep_count(files = ten_files, include_filename = TRUE)

## ----eval = has_grep----------------------------------------------------------
grep_count(files = "diamonds.csv", pattern = "VVS1")

## ----eval = has_grep----------------------------------------------------------
grep_count(files = "diamonds.csv", pattern = "VVS1", invert = TRUE)

## ----eval = has_grep----------------------------------------------------------
grep_count(files = all_files, pattern = "5", word_match = TRUE)

