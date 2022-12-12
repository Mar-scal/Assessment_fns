# To modify facet scales to compare abundance_bars and biomass_bars between years
# from https://dewey.dunnington.ca/post/2018/modifying-facet-scales-in-ggplot2/

scale_override <- function(which, scale) {
  if(!is.numeric(which) || (length(which) != 1) || (which %% 1 != 0)) {
    stop("which must be an integer of length 1")
  }
  
  if(is.null(scale$aesthetics) || !any(c("x", "y") %in% scale$aesthetics)) {
    stop("scale must be an x or y position scale")
  }
  
  structure(list(which = which, scale = scale), class = "scale_override")
}


CustomFacetWrap <- ggproto(
  "CustomFacetWrap", FacetWrap,
  init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
    # make the initial x, y scales list
    scales <- ggproto_parent(FacetWrap, self)$init_scales(layout, x_scale, y_scale, params)
    
    if(is.null(params$scale_overrides)) return(scales)
    
    max_scale_x <- length(scales$x)
    max_scale_y <- length(scales$y)
    
    # ... do some modification of the scales$x and scales$y here based on params$scale_overrides
    for(scale_override in params$scale_overrides) {
      which <- scale_override$which
      scale <- scale_override$scale
      
      if("x" %in% scale$aesthetics) {
        if(!is.null(scales$x)) {
          if(which < 0 || which > max_scale_x) stop("Invalid index of x scale: ", which)
          scales$x[[which]] <- scale$clone()
        }
      } else if("y" %in% scale$aesthetics) {
        if(!is.null(scales$y)) {
          if(which < 0 || which > max_scale_y) stop("Invalid index of y scale: ", which)
          scales$y[[which]] <- scale$clone()
        }
      } else {
        stop("Invalid scale")
      }
    }
    
    # return scales
    scales
  }
)



facet_wrap_custom <- function(..., scale_overrides = NULL) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_wrap(...)
  
  # sanitize scale overrides
  if(inherits(scale_overrides, "scale_override")) {
    scale_overrides <- list(scale_overrides)
  } else if(!is.list(scale_overrides) || 
            !all(vapply(scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
    stop("scale_overrides must be a scale_override object or a list of scale_override objects")
  }
  
  facet_super$params$scale_overrides <- scale_overrides
  
  ggproto(NULL, CustomFacetWrap,
          shrink = facet_super$shrink,
          params = facet_super$params
  )
}


# p_annoying_x_scale +
#   facet_wrap_custom(~facet_name, scales = "free", ncol = 4, scale_overrides = list(
#     scale_override(1, scale_x_continuous(breaks = c(5750, 5900))),
#     scale_override(6, scale_x_continuous(breaks = c(17800, 17900)))
#   ))

# create ggobj2 and ggobj using browser inside Survey_summary_figures_sf sections for abundance_bars and biomass_bars figures
# then hop over here and manually modify the y axes

png(paste(plot.dir,"/abundance_bars_2021.png",sep=""),units="in",
    width = 8.5, height = 11,res=420,bg="transparent")
ggobj2 +
  facet_wrap_custom(~variable, scales="free_y", ncol=1, scale_overrides = list(
    scale_override(1, scale_y_continuous(limits=c(0,1500))),
    scale_override(2, scale_y_continuous(limits=c(0,150))),
    scale_override(3, scale_y_continuous(limits=c(0,350)))
  ))
dev.off()

png(paste(plot.dir,"/abundance_bars_2022.png",sep=""),units="in",
    width = 8.5, height = 11,res=420,bg="transparent")
ggobj +
  facet_wrap_custom(~variable, scales="free_y", ncol=1, scale_overrides = list(
    scale_override(1, scale_y_continuous(limits=c(0,1500))),
    scale_override(2, scale_y_continuous(limits=c(0,150))),
    scale_override(3, scale_y_continuous(limits=c(0,350)))
  ))
dev.off()

png(paste(plot.dir,"/biomass_bars_2021.png",sep=""),units="in",
    width = 8.5, height = 11,res=420,bg="transparent")
ggobj2 +
  facet_wrap_custom(~variable, scales="free_y", ncol=1, scale_overrides = list(
    scale_override(1, scale_y_continuous(limits=c(0,3000))),
    scale_override(2, scale_y_continuous(limits=c(0,2000))),
    scale_override(3, scale_y_continuous(limits=c(0,8000)))
  ))
dev.off()

png(paste(plot.dir,"/biomass_bars_2022.png",sep=""),units="in",
    width = 8.5, height = 11,res=420,bg="transparent")
ggobj +
  facet_wrap_custom(~variable, scales="free_y", ncol=1, scale_overrides = list(
    scale_override(1, scale_y_continuous(limits=c(0,3000))),
    scale_override(2, scale_y_continuous(limits=c(0,2000))),
    scale_override(3, scale_y_continuous(limits=c(0,8000)))
  ))
dev.off()
