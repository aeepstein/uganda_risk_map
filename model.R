library(mgcv); library(rgdal); library(spdep)

#load in database with transformed covariates
db <- read.csv("db.csv") 

#load in parish shapefile
shapefile <- readOGR("parishes.shp") 

#create neighbor matrix
nb <- spdep::poly2nb(shapefile,
                     row.names = shapefile$parishcode,
                     snap = 0,
                     queen = TRUE) 
names(nb) <- attr(nb, "region.id")

ctrl <- list(nthreads = 30) #running on multiple threads to speed up process

#specify model
m1 <- mgcv::gam(cases ~ 
                  s(monthid, bs = "cr", k = 15) +
                  te(monthid, parishcode, bs = c("cr", "mrf"), k = c(4, 100),
                     xt = list(monthid = NULL, id = list(nb = nb))) +
                  s(EVI_l0, bs = 'cr') +
                  s(EVI_l1, bs = 'cr')  +
                  s(EVI_l3, bs = 'cr') +
                  s(LST_day_l0, bs = 'cr') +
                  s(LST_day_l3, bs = 'cr') +
                  s(LST_night_l0, bs = 'cr') +
                  s(LST_night_l1, bs = 'cr') +
                  s(LST_night_l2, bs = 'cr') +
                  s(LST_night_l3, bs = 'cr') +
                  s(precip_l0, bs = 'cr') +
                  s(precip_l1, bs = 'cr') + 
                  s(precip_l2, bs = 'cr') + 
                  s(precip_l3, bs = 'cr') +
                  s(housing,  bs = 'cr', k = 6) + 
                  s(dist_road, bs = 'cr', k = 6) + 
                  s(dist_water, bs = 'cr', k = 6) +
                  s(slope,  bs = 'cr', k = 6) + 
                  s(elevation, bs = 'cr', k = 6) + 
                  s(night_lights, bs = 'cr', k = 6) +
                  s(population_density, bs = 'cr', k = 6) + 
                  s(months_since_LLIN, bs = 'cr') + 
                  s(months_since_irs, bs = 'cr', by = irs_site) +
                  offset(log(denominator)),
                data = db,
                select = T,
                gamma = 1.4,
                family = "nb",
                drop.unused.levels = FALSE,
                control = ctrl)

save.image("m1.RData")