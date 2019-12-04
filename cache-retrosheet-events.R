# This script downloads all of the gamelog data from retrosheets
# Use it with the cache function in the colindouglas/retrosheet to prevent
# accidentally DOSing the retrosheet website

years <- 2008

purrr::walk(years, function(year) {
    url <- paste0("https://www.retrosheet.org/events/", year, "eve.zip")
    download.file(url, destfile = paste0("data/retrosheet/events/", year, "eve.zip"))
})
