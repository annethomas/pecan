##' @name pool_ic_list2netcdf
##' @title pool_ic_list2netcdf
##' @export
##'
##' @param input list with two-elements: list of netcdf dimensions (dims; named values) and list of variables (vars; named values)
##' @param outdir directory to write netcdf file
##' @param siteid site id
##' @author Anne Thomas

pool_ic_list2netcdf <- function(input, outdir,siteid){
  ##define dimensions available for netcdf
  #assuming dynamic dim names aren't necessary
  lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = IClist$dims$lon, 
                          longname = "station_longitude")
  lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = IClist$dims$lat, 
                          longname = "station_latitude")
  nsoil <- ncdf4::ncdim_def(name = "nsoil", units = "cm", vals = IClist$dims$nsoil, 
                            longname = "depth to bottom of layer")
  
  #sets of dims
  dims1 <- list(lon, lat)
  dims2 <- list(lon,lat,nsoil)
  
  #hardcoded vars (assumes these are the desirable variables)
  ic1 = ncvar_def(name = "AbvGrndWood", units = "kg C m-2", dim = dims1, -999)
  ic2 = ncvar_def(name = "TotSoilCarb", units = "kg C m-2", dim = dims2, -999)
  ic3 = ncvar_def(name = "CoarseWoodyDebris", units = "kg C m-2", dim = dims1, -999)
  ic4 = ncvar_def(name = "LAI", units = "m2 m-2", dim = dims1, -999)
  ic5 = ncvar_def(name = "SoilMoistFrac", units = "NA", dim = dims2, -999)
  ic6 = ncvar_def(name = "LitterMoistFrac", units = "NA", dim = dims1, -999)
  ic7 = ncvar_def(name = "Nitrogen", units = "kg m-2", dim = dims2, -999)
  ic8 = ncvar_def(name = "Phosphorus", units = "kg m-2", dim = dims2, -999)
  ic9 = ncvar_def(name = "SWE", units = "kg m-2", dim = dims1, -999)
  ic10 = ncvar_def(name = "Microbe", units = "kg m-2", dim = dims1, -999)
  
  ncvars = list(ic1,ic2,ic3,ic4,ic5,ic6,ic7,ic8,ic9,ic10)
  
  #possibility: dynamic vars (would assume R list has standard names; need a lookup table for which variables need nsoil?)
  
  
  #create nc file
  #outdir <- "/fs/data3/aet4612/dbfiles"
  #note: test#
  str_ns <- paste0(siteid %/% 1e+09, "-", siteid %% 1e+09)
  outfolder <- file.path(outdir, paste0("_site_", str_ns))
  nc  <- ncdf4::nc_create(outfolder, ncvars)
  
  #put variables in nc file
  for (i in seq(ncvars)) {
    #print(i)
    varname <- ncvars[[i]]$name
    if (varname %in% names(IClist$vals)){
      ncdf4::ncvar_put(nc, ncvars[[i]], IClist$vals[[varname]])
    }
    else{
      #don't load
      #or
      #find length of nsoil dim (check if dim 3 really is nsoil or locate by name?)
      #numNA <- ncvars[[i]]$dim[[3]]$len 
      #ncdf4::ncvar_put(nc, ncvars[[i]], rep(times = numNA,NA))
    }
  }
  
  #close file
  ncdf4::nc_close(nc)
}