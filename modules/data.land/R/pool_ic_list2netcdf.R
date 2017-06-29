##' @name pool_ic_list2netcdf
##' @title pool_ic_list2netcdf
##' @export
##'
##' @param input list with two elements: list of netcdf dimensions (dims, with named values) and list of variables (vals, with named values)
##' @param outdir directory to write netcdf file
##' @param siteid site id
##' @author Anne Thomas

pool_ic_list2netcdf <- function(input, outdir,siteid){
  ##define dimensions available for netcdf
  #assuming dynamic dim names aren't necessary
  lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = input$dims$lon, 
                          longname = "station_longitude")
  lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = input$dims$lat, 
                          longname = "station_latitude")
  nsoil <- ncdf4::ncdim_def(name = "nsoil", units = "cm", vals = input$dims$nsoil, 
                            longname = "depth to bottom of layer")
  
  #sets of dims
  dims1 <- list(lon, lat)
  dims2 <- list(lon,lat,nsoil)
  
  #hardcoded vars (assumes these are the desirable variables)
  ic1 <- ncdf4::var_def(name = "AbvGrndWood", units = "kg C m-2", dim = dims1, -999)
  ic2 <- ncdf4::var_def(name = "TotSoilCarb", units = "kg C m-2", dim = dims2, -999)
  ic3 <- ncdf4::var_def(name = "litter_carbon_content", units = "kg C m-2", dim = dims1, -999) #made up
  ic4 <- ncdf4::var_def(name = "LAI", units = "m2 m-2", dim = dims1, -999)
  ic5 <- ncdf4::var_def(name = "SoilMoistFrac", units = "NA", dim = dims2, -999) #should this be dims2?
  #took out LitterMoistFrac (made up name, specific to Sipnet)
  ic6 <- ncdf4::var_def(name = "CoarseWoodyDebris", units = "NA", dim = dims1, -999) #made up
  ic7 <- ncdf4::var_def(name = "soilN", units = "mg kg-1 soil", dim = dims2, -999) #made up
  ic8 <- ncdf4::var_def(name = "soilP", units = "mg kg-1 soil", dim = dims2, -999) #made up
  ic9 <- ncdf4::var_def(name = "SWE", units = "kg m-2", dim = dims1, -999)
  ic10 <- ncdf4::var_def(name = "Microbial Biomass C", units = "mg C kg-1 soil", dim = dims1, -999) #made up
  
  ncvars <- list(ic1,ic2,ic3,ic4,ic5,ic6,ic7,ic8,ic9,ic10)
  
  #possibility: dynamic vars (would assume R list has standard names; need a lookup table for dims and units)
  ncvars <- vector(mode = list, length = length(names(input$vals))) #more efficient than appending to list()
  for(i in seq(input$vals)){
    #look up names(input$vals)[[i]] in pecan standard output table, get units and dimensions
    #var <- row from table
    #dim <- list of dims based on table
    #ncvars[[i]] <- ncdf4::var_def(name = var$name, units = var$units, dim = dim, -999)
  }
  
  #create nc file
  str_ns <- paste0(siteid %/% 1e+09, "-", siteid %% 1e+09)
  outfile <- file.path(outdir, paste0("IC_site_", str_ns))
  nc  <- ncdf4::nc_create(outfile, ncvars)
  
  #put variables in nc file
  for (i in seq(ncvars)) {
    #print(i)
    varname <- ncvars[[i]]$name
    if (varname %in% names(input$vals)){
      ncdf4::ncvar_put(nc, ncvars[[i]], input$vals[[varname]])
    }
    else{
      #will automatically add NA
    }
  }
  
  #close file
  ncdf4::nc_close(nc)
}