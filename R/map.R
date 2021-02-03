#' Open Street Map instance initiator
#'
#' @param place_name defult=NULL, will assume you copy bbox from https://openstreetmap.org
#'
#' @return
#' @export
#'
#' @examples none
OSM <- function(place_name=NULL, initiateLater=T) {
  osm <- new.env()

  osm$check <-
    list(
      browse_osmWeb=function(){browseURL("https://openstreetmap.org")},
      browse_features=function(){browseURL("https://wiki.openstreetmap.org/wiki/Map_Features")},
      list_features=function(){osmdata::available_features()}
    )

  osm$check$find_features=function(featurePattern, ignore_case=T){
    stringr::str_subset(
      osm$check$list_features(),
      stringr::regex(featurePattern, ignore_case = ignore_case)
    )
  }

  osm_buildFrom_clip(osm_instance = osm) -> osm$buildfrom_clip
  osm_buildFrom_name(osm_instance = osm) -> osm$buildfrom_name

  osm$query <- function() {
    osmdata::opq(
      osm$bbox
    ) -> osm$data

    osm$export_sf <- function() {
      osmdata::osmdata_sf(
        osm$data
      )
    }

    osm$add_features <- function(...) {
      osm$data <-
        osmdata::add_osm_feature(
          osm$data,
          ...
        )
    }
  }
  osm
}
#' Resolve the osm_sf in ggplot name error problem
#'
#' @param sf_object An simple feature dataframe exported from osmdata::osm_sf function
#'
#' @return
#' @export
#'
#' @examples
osm_rename <- function(sf_object){
  sf_object %>%
    sf::st_geometry() -> sfc_sf_object
  for(i in seq_along(sfc_sf_object)){
    names(sfc_sf_object[[i]][[1]]) <-
      1:length(names(sfc_sf_object[[i]][[1]]))
  }

  sf_object %>%
    sf::st_set_geometry(
      sfc_sf_object
    ) -> sf_object2
  return(sf_object2)
}


# helpers -----------------------------------------------------------------

osm_read_bboxclip <- function()
{

  # read clipboard from OSM bbox information from openstreetmap.org
  clip_bbox <- {
    clipr::read_clip()
  }

  # use stringr carefully rearrange your text clipboard into the format that can be the add_osm_feature's bbox input argument
  bbox4osm <- {
    legitimateClip <-
      as.numeric(stringr::str_subset(clip_bbox,"[0-9]+.[0-9]*"))

    names(legitimateClip) <- c("ymax", "xmin", "xmax", "ymin")

    clipMatrix <-
      rbind(
        legitimateClip[c("xmin", "xmax")],
        legitimateClip[c("ymin", "ymax")]
      )
    rownames(clipMatrix) <- c("x", "y")
    colnames(clipMatrix) <- c("min", "max")

    clipMatrix
  }

  bbox4osm_deparsed <-
    deparse(bbox4osm)

  clipr::write_clip(bbox4osm_deparsed)
}
osm_buildFrom_clip <- function(osm_instance){
  function(){
    osm_instance$bboxText <- {
      osm_read_bboxclip()
    }
    osm_instance$bbox <- {
      eval(parse(text = osm$bboxText))
    }
  }
}
osm_buildFrom_name <- function(osm_instance){
  function(place_name){
    osmdata::getbb(place_name) -> osm_instance$bbox
  }
}

