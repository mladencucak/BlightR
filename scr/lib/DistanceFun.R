# Computes the distance between two locations in meters. This uses an online
# map API and therefore an Internet connection is required for an accurate
# result. If no connection is found, this will use the Haversine formula
# to provide a rough estimate for the distance.
#

# Computes distance using Haversine formula.
#
# Returns the result in meters.
haversine <- function( lat1, lon1, lat2, lon2, radius = 6371 ) {
  # Convert decimal degrees to radians
  lon1 = lon1 * pi / 180
  lon2 = lon2 * pi / 180
  lat1 = lat1 * pi / 180
  lat2 = lat2 * pi / 180
  
  # Haversine formula
  dlon = lon2 - lon1
  dlat = lat2 - lat1
  a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
  c = 2 * atan2(sqrt(a), sqrt(1-a))
  
  return( radius * c * 1000 )
}
