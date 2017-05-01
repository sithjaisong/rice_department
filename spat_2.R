library(sp)

# Print countries_sp
countries_sp

# Call summary() on countries_sp
summary(countries_sp)

# Call plot() on countries_sp
plot(countries_sp)

# Call str() on countries_sp
str(countries_sp)

# Call str() on countries_sp with max.level = 2
str(countries_sp, max.level = 2)

# Call summary() on countries_spdf and countries_sp
summary(countries_spdf)
summary(countries_sp)


# Call str() with max.level = 2 on countries_spdf
str(countries_spdf, max.level = 2)

# Plot countries_spdf
plot(countries_spdf)

#===
# 169th element of countries_spdf@polygons: one
one <- countries_spdf@polygons[[169]]

# Print one
one

# Call summary() on one
summary(one)

# Call str() on one with max.level = 2
str(one, max.level = 2)

one <- countries_spdf@polygons[[169]]

# str() with max.level = 2, on the Polygons slot of one
str(one@Polygons, max.level = 2)

# str() with max.level = 2, on the 6th element of the one@Polygons
str(one@Polygons[[6]], max.level = 2)

# Call plot on the coords slot of 6th element of one@Polygons
plot(one@Polygons[[6]]@coords)
# Since one@Polygons[[6]]@coords is just a matrix, this plot() call uses the default plot method, not the special one for spatial objects

#===
# Subset the 169th object of countries_spdf: usa
usa <- countries_spdf[169, ]

# Look at summary() of usa
summary(usa)

# Look at str() of usa
str(usa, max.level = 2)

# Call plot() on usa
plot(usa)

# Call head() and str() on the data slot of countries_spdf
head(countries_spdf@data)
str(countries_spdf@data)

# Pull out the name column using $
countries_spdf$name

# Pull out the subregion column using [[
countries_spdf[["subregion"]]

# Create logical vector: is_nz
is_nz <- countries_spdf$name == "New Zealand"

# Subset countries_spdf using is_nz: nz
nz <- countries_spdf[is_nz, ]

# Plot nz
plot(nz)

#===
library(sp)
library(tmap)

# Use qtm() to create a choropleth map of gdp
qtm(shp = countries_spdf, fill = "gdp")

library(sp)
library(tmap)

# Add style argument to the tm_fill() call
tm_shape(countries_spdf) +
  tm_fill(col = "population", style = "quantile") + tm_borders(col = "burlywood4")
# Add a tm_borders() layer 


# New plot, with tm_bubbles() instead of tm_fill()
tm_shape(countries_spdf) +
  tm_bubbles(size = "population", style = "quantile") + tm_borders(col = "burlywood4")