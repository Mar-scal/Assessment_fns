#####################################  This script takes all the inshore and offshore condition data from 2017 surveys and plots it up on one map... I hope..
#####################################  Function Summary ########################################################
####  

addalpha <- function(colors, alpha=1.0) 
{
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

# A way to plot our data spatially. This can be used to plot everywhere or used to plot a specific location
# This only works 
Scallop.plot.field = function(field, mesh, xlim=c(-65.88, -60.13), ylim=c(45.8, 49.1), zlim = c(0,1), dims = c(50, 50),
                              clip= NULL,lvls = seq(0,1,by=0.01),add_boxes = NULL,colors = c("blue","white","yellow","darkred"),alpha = 0.8) 
{ 
  stopifnot(length(field) == mesh$n)
  proj = inla.mesh.projector(mesh, xlim = xlim, ylim = ylim, dims=dims)
  field.proj = inla.mesh.project(proj, field)
  if(!is.null(clip)) 
  {
    pred.in <- inout(proj$lattice$loc,clip) 
    field.proj[!pred.in] <- NA
  } # end if(!is.null(clip)) 
  #windows(11,11)
  par(mar=c(4,4,1,1))
  #cols <- terrain.colors(length(lvls)-1)
  ScallopMap(xlim=xlim,ylim=ylim,plot.boundries = T)
  image(list(x = proj$x, y=proj$y, z = field.proj), xlim = xlim, ylim = ylim, zlim=zlim, axes=F,las=1,add=T, breaks=lvls, 
        axis.args=list(at=lvls,labels=round(exp(lvls))),
        col = addalpha(colorRampPalette(colors,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=alpha))
  if(!is.null(add_boxes)) addPolys(add_boxes,angle = 45,density=8,col="black",lwd=.5)
  # addPolys()
} # end function

# These are the functions used to within the heart of the code to make stuff happen
source("d:/r/Assessment_fns/Maps/ScallopMap.r")
source("d:/r/Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r")
require(viridis) || stop("Install the viridis package for the color ramps")
require(INLA) || stop("Install the INLA package for the spatial plots")
require(maps)|| stop("Install the maps package for the spatial plots")
require(maptools)|| stop("Install the maptools package for the spatial plots")
require(mapdata)|| stop("Install the mapdata package for the spatial plots")
require(rgeos)|| stop("Install the rgeos package for the spatial plots")
require(splancs)|| stop("Install the splancs package for the spatial plots")
require(boot)|| stop("Install the boot package for the spatial plots")
require(fields)|| stop("Install the fields package for the spatial plots")
require(PBSmapping)|| stop("Install the PBSmapping package for the spatial plots")
library("plotrix")
# 
yr <- 2017
load(paste0("Y:/Offshore scallop/Assessment//Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata"))  
BF.cf <- read.csv("Y:/INSHORE SCALLOP/BoF/2017/2017 Assessment/SurveyData/BFConditionforMap2017dd.csv")
BI.cf <- read.csv("Y:/INSHORE SCALLOP/BoF/2017/2017 Assessment/SurveyData/BIConditionforMap2017dd.csv")
GM.cf <- read.csv("Y:/INSHORE SCALLOP/BoF/2017/2017 Assessment/SurveyData/GMConditionforMap2017dd.csv")
SFA29.cf <- read.csv("Y:/INSHORE SCALLOP/SFA29/2018/dataoutput/ConditionforMap2017.csv")
SFA29.cf$DDSlat <- convert.dd.dddd(SFA29.cf$START_LAT)
SFA29.cf$DDSlon <- convert.dd.dddd(SFA29.cf$START_LONG)
bof.bound <- read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/BoFSFA29WExtent_wgs84.csv")
bof.bound <- as.PolySet(bof.bound,projection="LL")
Inshore.cf <- rbind(BF.cf[,c("TOW_NO","DDSlat","DDSlon","Condition")],BI.cf[,c("TOW_NO","DDSlat","DDSlon","Condition")],
                    GM.cf[,c("TOW_NO","DDSlat","DDSlon","Condition")],SFA29.cf[,c("TOW_NO","DDSlat","DDSlon","Condition")])


  ################################# Now for the INLA bit...
    areas <- c("inshore",names(surv.Live))
    areas <- areas[areas != "GB" & areas != "GBb"] # Don't need the spring survey for this and I'm going to combine GBa and GBb....
    n.areas <- length(areas)
    mod.res <- NULL # This will contain the model results for the spatial figures
    proj <- NULL
    field.proj <- NULL
    mesh <- NULL
    s.res <- c(1000,1000)
    for(i in 1:n.areas)
    {
      if(areas[i] == "inshore")
      {
        loc <- cbind(Inshore.cf$DDSlon,Inshore.cf$DDSlat)
        Condition <- Inshore.cf$Condition
        # We need to carve out Nova Scotia from the map so we can get the Inshore too be decent...
        ca <- map("worldHires", "Canada", fill=TRUE,
                  col="transparent", plot=FALSE)
        IDs <- sapply(strsplit(ca$names, ":"), function(x) x[1])
        prj4s <- CRS("+proj=longlat +datum=WGS84")
        ca.sp <- map2SpatialPolygons(
          ca, IDs=IDs, proj4string=prj4s)
        # This line is needed to clean up the Hi res data, get some repeated points in there that causes all sorts of issues with intersections...
        ca.sp <- gBuffer(ca.sp, byid=TRUE, width=0)
        ### define a non-convex hull boundary
        bound.sp <- PolySet2SpatialPolygons(bof.bound)
        # bound <- inla.nonconvex.hull(loc, 1, 1)
        # bound.sp <- SpatialPolygons(list(Polygons(
        #   list(Polygon(bound$loc, FALSE)), '0')), proj4=prj4s)
        ### which polygons from CA map are the islands?
        # windows(11,11)
        # plot(bound.sp)
        # points(loc)
        # plot(ca.sp, add=T, border=2)
        # for (j in 1:length(ca.sp@polygons[[1]]@Polygons))
        #   text(ca.sp@polygons[[1]]@Polygons[[j]]@labpt[1],
        #        ca.sp@polygons[[1]]@Polygons[[j]]@labpt[2], paste(j))
        
        #bound.sp <- SpatialPolygons(list(Polygons(list(Polygon(bound$loc, FALSE)), '0')), proj4=prj4s)
        ### select the overlapping area from the map
        ca.i <- SpatialPolygons(list(Polygons(
          ca.sp@polygons[[1]]@Polygons[c(7,22)],'0')), # 7 is Canada, 22 is GM island
          proj4string=ca.sp@proj4string)
        ### make that as holes in the boundary
        bound.d <- gDifference(bound.sp,ca.i)#,byid=T, drop_lower_td = TRUE)
        bound <- inla.sp2segment(bound.d)
        mesh[[areas[i]]] <- inla.mesh.2d(loc, 
                                max.edge=0.1, cutoff=0.02,
                                boundary=bound)
        #plot(mesh[[areas[i]]])
      } # end if(areas[i] == inshore)
      
      if(areas[i] != "inshore")
      {
        # For Middle bank Make a couple of boxes around the survey stations, these are entirely arbitrary...
        if(areas[i] == "Mid")  
        {
          bound.poly.surv <- rbind(c(1,	1,	1,	-60.78,	44.68),
                                   c(1,	1,	2,	-60.58,	44.68),
                                   c(1,	1,	3,	-60.58,	44.49),
                                   c(1,	1,	4,	-60.78,	44.49),
                                   c(1,	1,	5,	-60.78,	44.68),
                                   c(2,	1,	1,	-60.44,	44.53),
                                   c(2,	1,	2,	-60.33,	44.53),
                                   c(2,	1,	3,	-60.33,	44.42),
                                   c(2,	1,	4,	-60.44,	44.42),
                                   c(2,	1,	5,  -60.44,	44.53))
          bound.poly.surv <- as.data.frame(bound.poly.surv)
          names(bound.poly.surv) <- c("PID","SID","POS","X","Y")
          bound.survey.polys <- as.PolySet(bound.poly.surv,projection= "LL")
        } # end if(banks[i] == "Mid")  
        
        # For German bank the above polygon makes a mess of things, so we'll make our own based on the German Bank boundaries
        if(areas[i] == "Ger") 
        {
          g.bnds <- survey.bound.polys[survey.bound.polys$label==areas[i],]
          Y.range <- range(g.bnds$Y,na.rm=T)
          X.range <- range(g.bnds$X,na.rm=T)
          g.tmp <- newAreaPolys[newAreaPolys$label=="SFA26",]
          g.tmp$X[g.tmp$X < X.range[1]] <- X.range[1]
          g.tmp$X[g.tmp$X > X.range[2]] <- X.range[2]
          g.tmp$Y[g.tmp$Y > Y.range[2]] <- Y.range[2]
          g.tmp$Y[g.tmp$Y < Y.range[1]] <- Y.range[1]
          # Now I want to insert a segemnt into the boundary to run a diagonal line from around 43°9/-66.755 to 43/-66°24
          g.tmp[2,] <- c(5,2,-66.4,Y.range[1],"SFA26","Ger")
          g.tmp <- as.data.frame(rbind(g.tmp[c(1,2),],c(5,2,X.range[1],43.15,"SFA26","Ger"),g.tmp[3:nrow(g.tmp),]))
          for(k in 1:4) g.tmp[,k] <- as.numeric(g.tmp[,k]) # Silly rbind making everything characters...
          g.tmp$POS <- 1:nrow(g.tmp)
          bound.survey.polys <- as.PolySet(g.tmp,projection="LL")
        } # end if(banks[i] == "Ger") 
        if(areas[i] %in% c("BBn","BBs","Sab")) bound.survey.polys <- as.PolySet(survey.bound.polys[survey.bound.polys$label == areas[i],],projection = "LL")
        if(areas[i] %in% "GBa") bound.survey.polys <- as.PolySet(survey.bound.polys[survey.bound.polys$label == "GB",],projection = "LL")
        # Now convert this to an object for sp, this gets our bounding area for the survey.
        bound.poly.surv.sp <- PolySet2SpatialPolygons(bound.survey.polys)
        # Convert the sp boundary object to a mesh boundary for INLA.
        bound <- inla.sp2segment(bound.poly.surv.sp)
        #xyl <- rbind(x=range(bound$loc[,1]), y=range(bound$loc[,2])) # get the xy ranges of our survey extent.

        # The locations for each bank..
        if(areas[i] %in% c("Mid","Sab","Ger","BBn","BBs"))
        {   
          # These are the locations for the Condition and meat count data.
          loc <- cbind(CF.current[[areas[i]]]$lon[CF.current[[areas[i]]]$year == yr],
                          CF.current[[areas[i]]]$lat[CF.current[[areas[i]]]$year == yr])
          Condition <- CF.current[[areas[i]]]$CF
        }# end if(banks[i] %in% c("Mid","Sab","Ger","BBn","BBs","Ban","SPB","GB"))
        # I want 1 mesh for all of Georges bank summer survey.
        if(areas[i] %in% c("GBa")) 
        {
          # The condition and meat count data.
          loc <- cbind(c(CF.current[["GBa"]]$lon[CF.current[["GBa"]]$year == yr],CF.current[["GBb"]]$lon[CF.current[["GBb"]]$year == yr]),
                          c(CF.current[["GBa"]]$lat[CF.current[["GBa"]]$year == yr],CF.current[["GBb"]]$lat[CF.current[["GBb"]]$year == yr]))
          Condition <- cbind(c(CF.current[["GBa"]]$CF,CF.current[["GBb"]]$CF))
        } # end if(banks[i] %in% c("GBa") 
        # FInally we make our mesh
           mesh[[areas[i]]] <- inla.mesh.2d(loc, max.edge=c(0.05,0.075), offset=0.15)
      } # end if(areas[i] != "inshore")
      
      # Now make the A matrix
      A <- inla.spde.make.A(mesh[[areas[i]]] , loc)
      
      # We can just make the one spde object for all of these as well.
      spde <- inla.spde2.pcmatern(mesh[[areas[i]]],    
                                  prior.sigma=c(1,0.5), # The probabiliy that the marginal standard deviation (first number) is larger than second number
                                  prior.range=c(1,0.5)) # The Meidan range and the probability that the range is less than this..
      ## For conidtion a gaussian should do the trick...
      family1 <- "gaussian" # For CF and MC they are more normal so go with a gaussian.
      
      a0 <- 1 # intercept
      # Mostly just using stock priors, again fine for our purposes for the moment.
      pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))
      # Add an index to the data
      # The spatial model, simple model with a intercept (overall bank average) with the spde spatial component
      # basically the random deviations for each piece of the mesh.
      formula1 <- y ~ 0 + a0 + f(s, model=spde)
      
      # if we have maps to be made and we aren't simply loading in the INLA results we need to run this bit.
      
      # This is the stack for the INLA model
      stk <- inla.stack(tag="est",data=list(y = Condition, link=1L),
                        effects=list(a0 = rep(1, length(Condition)), s = 1:spde$n.spde),
                        A = list(1, A))
      # This is the INLA model itself
      mod <- inla(formula1, family=family1, data = inla.stack.data(stk),
                  control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
      
      # Now that this is done we need to make a prediction grid for projection onto our mesh,
      mod.res[[areas[i]]] <- mod$summary.random$s$mean + mod$summary.fixed$mean
      
      # Now that this is done we need to make a prediction grid for projection onto our mesh,
      proj[[areas[i]]] <- inla.mesh.projector(mesh[[areas[i]]], xlim = range(loc[,1]), ylim = range(loc[,2]), dims=s.res)
      
      # And this gives us our actual values for the grid...
      field.proj[[areas[i]]] = inla.mesh.project(proj[[areas[i]]], mod.res[[areas[[i]]]])
                                             
      # Now clip the data so we only have information from within the boundaries we want...
      #if(areas[i] != "Sab" && areas[i] != "Mid") 
      if(areas[i] != "Sab" && areas[i] != "Mid")pred.in <- inout(proj[[areas[i]]]$lattice$loc,bound$loc) 
      # Because there are holes in the survey strata on Sable and inshore things are a bit more complex...
      if(areas[i] %in% c("Sab","Mid","inshore"))
      {
      simplemesh <- inla.mesh.2d(boundary = bound,max.edge = 1e9)
      pred.in <- inla.mesh.projector(simplemesh,proj[[areas[i]]]$lattice$loc)$proj$ok
      } # end if(banks[i] == "Sab")
      
      field.proj[[areas[i]]][!pred.in] <- NA
                                             
    } # end for(i in 1:n.areas)
    
    min(field.proj$inshore,na.rm=T)
    min.cond <- floor(min(unlist(lapply(field.proj, function(x) min(x,na.rm=T)))))
    max.cond <- ceiling(max(unlist(lapply(field.proj, function(x) max(x,na.rm=T)))))
    lvls <- seq(min.cond,max.cond,by=2)
    colors <- rev(magma(n=(length(lvls)+1)))
    leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)):-length(lvls)],
                  paste(lvls[length(lvls)],'+',sep=''))
    windows(11,11)
    png(paste0(direct,"2018/Framework/Plankton_project/Condition_inshore_and_offshore_2017.png"),width=11,height=11,res=920,units="in")
    ScallopMap("SS",plot.boundries = T,plot.bathy = T,bathy.source = "quick",offshore.names = T,label.boundries = T)
    legend("topright",leg.lvls,fill=colors,border="black",pch=c(rep(NA,length(lvls))),title.adj = 0.2,
           pt.bg = c(rep(NA,length(lvls))),bg=NA,bty="n")
    draw.circle(-65.58, 42.55, 0.25, nv = 1000, border = NULL, col = addalpha("grey",0.5), lty = 1, lwd = 1) # Browns Bank south...
    
    for(i in 1:n.areas)
    {
      image(list(x = proj[[areas[i]]]$x, y=proj[[areas[i]]]$y, z = field.proj[[areas[i]]]),  zlim=range(field.pro[[areas[i]]],na.rm=T), 
            axes=F,las=1,add=T, breaks=lvls, axis.args=list(at=lvls,labels=round(exp(lvls))),
            col = addalpha(colorRampPalette(colors,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=0.8))
    }
    dev.off()
    
    
    
    