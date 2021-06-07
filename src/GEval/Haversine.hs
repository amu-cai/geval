{-# LANGUAGE OverloadedStrings #-}

module GEval.Haversine
       (haversine)
       where


haversine :: ((Double, Double), (Double, Double)) -> Double
haversine ((longitude_1, latitude_1), (longitude_2, latitude_2)) = hav longitude_1_rad latitude_1_rad longitude_2_rad latitude_2_rad
  where
    longitude_1_rad = toRadians longitude_1
    latitude_1_rad = toRadians latitude_1
    longitude_2_rad = toRadians longitude_2
    latitude_2_rad = toRadians latitude_2

hav :: Double -> Double -> Double -> Double -> Double
hav longitude_1 latitude_1 longitude_2 latitude_2 = 2 * asin (sqrt h) * r
  where
    r = 6371.0 -- Radius of earth in kilometers. Use 3956 for miles
    longitude = longitude_2 - longitude_1
    latitude = latitude_2 - latitude_1
    h = hav_ longitude + cos latitude  * cos latitude * hav_ latitude

hav_ :: Double -> Double
hav_ x = sin(x / 2) ** 2

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180.0
