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
  #assuming additional dim names aren't necessary; could use process similar to ncvars if so
  lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = input$dims$lon, 
                          longname = "station_longitude")
  lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = input$dims$lat, 
                          longname = "station_latitude")
  nsoil <- ncdf4::ncdim_def(name = "nsoil", units = "cm", vals = input$dims$nsoil, 
                            longname = "depth to bottom of layer")
  
  dims <- list(lon = lon, lat = lat, nsoil = nsoil)
  
  #lookup table
  standard_vars <- data(standard_vars,package = "PEcAn.utils") #standard_vars replacing mstmip_vars; not yet merged
  
  ###function to lapply to all variables in input list and return a list of ncvars
  list_ncvars <- function(varname,standard_vars,dims){
    print(varname)
    var <- standard_vars[which(standard_vars$Variable.Name == varname),]
    #add: check var exists
    dimset <- var[,c("dim1","dim2","dim3","dim4")]
    dim <- dims[which(names(dims) %in% dimset)] #subset list of all dims for this variable
    print(class(dim[[1]]))
    #add: check that dim isn't 0
    ncvar <- ncdf4::ncvar_def(name = varname, units = var$Units, dim = dim, -999) #also add longname?
    if (var$Long.name != "na") {
      ncvar$longname <- as.character(var$Long.name)
    }
    return(ncvar)
  }
  
  ncvars = lapply(names(IClist$vals),list_ncvars,standard_vars,alldims)
  
  #create nc file
  str_ns <- paste0(siteid %/% 1e+09, "-", siteid %% 1e+09)
  outfile <- file.path(outdir, paste0("IC_site_", str_ns))
  nc  <- ncdf4::nc_create(outfile, ncvars)
  
  #put variables in nc file
  for (i in seq(ncvars)) {
    varname <- ncvars[[i]]$name
    ncdf4::ncvar_put(nc, ncvars[[i]], input$vals[[varname]])
  }
  
  #close file
  ncdf4::nc_close(nc)
}
