# Use optipng to reduce file size of PNG figures
# http://optipng.sourceforge.net/
# https://www.jumpingrivers.com/blog/knitr-image-png-jpeg-svg-rmarkdown/

# optipng.exe should be in your working directory
getwd()

# copy paste all figures into optipng folder
png_files = list.files(path = "Y:/Offshore/Assessment/2021/Presentations/Survey_summary/optipng/Mid",
                         pattern = "*\\.png$",
                       full.names = TRUE,
                       recursive = TRUE)

# review filenames
png_files

# shrink! THIS WILL OVERWRITE THE FILES IN THE optipng FOLDER! Takes about an hour to do all figures for all banks
for (png in png_files) {system2(command = "optipng", shQuote(png))}
for (png in png_files) {system2(command = "pngquant", shQuote(png))}


# or you can use functions
pngquant = function(dir = '.') {
  files = list.files(dir, '[.]png$', recursive = TRUE, full.names = TRUE)
  for (f in files) system2('pngquant', shQuote(f))
}

optipng = function(dir = '.') {
  files = list.files(dir, '[.]png$', recursive = TRUE, full.names = TRUE)
  for (f in files) system2('optipng', shQuote(f))
}


pngquant("Y:/Offshore/Assessment/2021/Presentations/Survey_summary/pngquant/Mid")
optipng("Y:/Offshore/Assessment/2021/Presentations/Survey_summary/optipng/Mid")
