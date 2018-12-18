# This is a hack of a function to add transparency to a set of colours.  Stand-alone function created by DK in Nov 2018

# Arguements:

#1: colors  The colors you want to add transparency to.
#2: alpha   Do you want the colours to have some level of transparency.  0 = translucent, 1 = opaque, Default = 1

# 
# A way to add some transparancy to the colours if so desired.
addalpha <- function(colors, alpha=1.0) 
{
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}