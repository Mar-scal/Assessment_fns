# Convert inla.mesh to sp objects, totally taken from Finn Lindgren here 
# # https://groups.google.com/forum/#!topic/r-inla-discussion-group/z1n1exlZrKM
#
# @param mesh An \code{\link{inla.mesh}} object
# @return A list with \code{sp} objects for triangles and vertices:
# \describe{
#   \item{triangles}{\code{SpatialPolygonsDataFrame} object with the triangles in
#   the same order as in the original mesh, but each triangle looping through
#   the vertices in clockwise order (\code{sp} standard) instead of
#   counterclockwise order (\code{inla.mesh} standard). The \code{data.frame}
#   contains the vertex indices for each triangle, which is needed to link to
#   functions defined on the vertices of the triangulation.
#   \item{vertices}{\code{SpatialPoints} object with the vertex coordinates,
#   in the same order as in the original mesh.}
# }
# @export
# 
#### Arguements
# 1: mesh   - An inla mesh object, before running this wouldn't hurt if you had specificed mesh$crs <- EPSG or coord system
#             
inla.mesh2sf <- function(mesh) 
{
  require(sp) || stop("Install sp, else thine code shan't work for thee")
  require(sf) || stop('Install sf or this code will be a mess')
  require(INLA) || stop("You need the R-INLA package for this, note that it's not crantastic...
                        install.packages('INLA', repos=c(getOption('repos'), INLA='https://inla.r-inla-download.org/R/stable'), dep=TRUE)")
  # Grab the CRS if it exists, NULL is fine
  crs <- inla.CRS(inla.CRSargs(mesh$crs))
  # Make sure the CRS isn't a geocentric one, which is won't be if yo look up geocentric..
  isgeocentric <- identical(inla.as.list.CRS(crs)[["proj"]], "geocent")
  
  # Look up geo-centric coordinate systems, nothing we'll need to worry about, but stop if so
  if (isgeocentric || (mesh$manifold == "S2")) {
    stop(paste0(
      "'sp' doesn't support storing polygons in geocentric coordinates.\n",
      "Convert to a map projection with inla.spTransform() before calling inla.mesh2sp()."))
  }
  # This pulls out from the mesh the triangles as polygons, this was the piece I couldn't figure out.
  triangles <- SpatialPolygonsDataFrame(Sr = SpatialPolygons(
                                               lapply(
                                                1:nrow(mesh$graph$tv),
                                                  function(x) 
                                                  {
                                                    tv <- mesh$graph$tv[x, , drop = TRUE]
                                                    Polygons(list(Polygon(mesh$loc[tv[c(1, 3, 2, 1)],1:2,drop = FALSE])),ID = x)
                                                  }
                                               ),
                                             proj4string = crs
                                             ),
                                        data = as.data.frame(mesh$graph$tv[, c(1, 3, 2), drop = FALSE]),
                                        match.ID = FALSE
                                        )
  # This one is easy, just grab the vertices (points)
  vertices <- SpatialPoints(mesh$loc[, 1:2, drop = FALSE], proj4string = crs)
  # Make these sf objects
  triangles <- st_as_sf(triangles)
  vertices <- st_as_sf(vertices)
  # and your output list.
  list(triangles = triangles, vertices = vertices)
} 