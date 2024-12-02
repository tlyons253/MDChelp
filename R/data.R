#' NLCD land cover by area for each county in MO odd numbered years 2001-2023.
#'
#' A dataset derived of Annual NLCD data- files were obtained via the FedData
#' package (NLCD) and tigir package (county boundaries). NLCD data was reprojected
#' to NAD83 UTM 15 N before areal calculations.
#'
#'See vignettes for examples on how to batch process
#'
#'  Cropping performed with terra::crop
#'
#' Area converted to square km based on a 30x30m cell size and summing all
#' all pixels with in a given class. No other processing occured.
#'
#'
#' @examples
#' \dontrun{
#' #pixel classes were condensed as follows:
#'
#'
#' terra::rast(nlcd)->Z
#'  mo.co.list[[county]]->Q
#'  terra::crop(Z,Q)%>%
#'  terra::freq()%>%
#'  filter(str_detect(value,
#'  'Water|Developed|Forest|Shrub|Grass|Hay|Crops|Wetlands'))%>%
#'  mutate(LC=case_when(str_detect(value,'Developed')~"develop",
#'                    str_detect(value,'Forest')~"forest",
#'                    str_detect(value,'Shrub|Grassland|Hay')~'grass',
#'                    str_detect(value,'Crops')~'crop',
#'                    str_detect(value,'Wetlands')~'wetland',
#'                    str_detect(value,'Water')~'water',
#'                    TRUE~as.character("other")
#'                    ))
#'                    }
#'
#' @format
#' \describe{
#'    \item{cty}{county name}
#'    \item{yr}{4 digit year}
#'    \item{crop.sqkm}{area in km^2 of crop}
#'    \item{develop.sqkm}{area in km^2 of developed}
#'    \item{forest.sqkm}{area in km^2 of forest}
#'    \item{grass.sqkm}{area in km^2 of grass}
#'    \item{water.sqkm}{area in km^2 of water}
#'    \item{wetland.sqkm}{area in km^2 of wetland}
#'    \item{other.sqkm}{area in km^2 of all other landcover types present}
#' }
#'
"nlcd.area.tab"



#'#' NLCD land cover by percent of county area for each county in MO
#' odd numbered years 2001-2023.
#'
#' A dataset derived of Annual NLCD data- files were obtained via the FedData
#' package (NLCD) and tigir package (county boundaries). NLCD data was
#' reprojected to NAD83 UTM 15 N before areal calculations.
#'
#'  Cropping performed with terra::crop
#'
#' Area converted to square km based on a 30x30m cell size and summing all
#' all pixels with in a given class. No other processing occured.
#'
#'
#' @examples
#' \dontrun{
#' #pixel classes were condensed as follows:
#'
#'
#' terra::rast(nlcd)->Z
#'  mo.co.list[[county]]->Q
#'  terra::crop(Z,Q)%>%
#'  terra::freq()%>%
#'  filter(str_detect(value,
#'  'Water|Developed|Forest|Shrub|Grass|Hay|Crops|Wetlands'))%>%
#'  mutate(LC=case_when(str_detect(value,'Developed')~"develop",
#'                    str_detect(value,'Forest')~"forest",
#'                    str_detect(value,'Shrub|Grassland|Hay')~'grass',
#'                    str_detect(value,'Crops')~'crop',
#'                    str_detect(value,'Wetlands')~'wetland',
#'                    str_detect(value,'Water')~'water',
#'                    TRUE~as.character("other")
#'                    ))
#'                    }
#'
#' @format
#' \describe{
#'    \item{cty}{county name}
#'    \item{yr}{4 digit year}
#'    \item{crop.pct}{area in % of crop}
#'    \item{develop.pct}{area in % of developed}
#'    \item{forest.pct}{area in % of forest}
#'    \item{grass.pct}{area in % of grass}
#'    \item{water.pct}{area in % of water}
#'    \item{wetland.pct}{area in % of wetland}
#'    \item{other.pct}{area in % of all other landcover types present}
#' }
#'
"nlcd.pct.tab"
