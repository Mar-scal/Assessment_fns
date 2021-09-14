# Use optipng to reduce file size of PNG figures
# http://optipng.sourceforge.net/
# https://www.jumpingrivers.com/blog/knitr-image-png-jpeg-svg-rmarkdown/

# optipng.exe should be in your working directory
getwd()

# copy paste all figures into optipng folder
png_files = list.files(path = "Y:/Offshore/Assessment/2021/Presentations/Survey_summary/optipng",
                         pattern = "*\\.png$",
                       full.names = TRUE,
                       recursive = TRUE)

# review filenames
png_files

# shrink! THIS WILL OVERWRITE THE FILES IN THE optipng FOLDER! Takes about an hour to do all figures for all banks
for (png in png_files) system2("optipng", png)
