#------------------------------------------------------------------------------------------------------------------------
.minid <- setClass ("minid",
                    representation = representation(id="character",
                                                    metadata="list")
                    )

#------------------------------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------------------------------------------------
setGeneric("getCreator",               signature="obj", function(obj) standardGeneric ("getCreator"))
#------------------------------------------------------------------------------------------------------------------------
minid <- function(id)
{
   landing.page.prefix <- "http://minid.bd2k.org/minid/landingpage/ark:/"
   full.url <- paste(landing.page.prefix, id, sep="")
   request.object  <- GET(full.url, add_headers(Accept="application/json"))
   text.response <- httr::content(request.object, "text", encoding="UTF-8");
   stopifnot(jsonlite::validate(text.response))
   minid.JSON <- jsonlite::fromJSON(text.response)

   .minid(id=id, metadata=minid.JSON)

} # minid, the constructor
#------------------------------------------------------------------------------------------------------------------------
setMethod("getCreator", "minid",

   function (obj){
      obj@metadata$creator
      })

#------------------------------------------------------------------------------------------------------------------------
