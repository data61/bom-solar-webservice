# bom-solar-webservice
Runs on port 8080, and needs the file `all-DNI-reformat.nc` in the same directory.

API is currently `:8080/v1/DNI/<lat>/<lon>`

## Todo:
- [x] Add date ranges
- [ ] Add proper configuration a la the other web services
- [x] add Swagger docs
- [ ] Support GHI?

## NetCDF data format
Expects to have data matching the following CDL definition:
```cdl
netcdf all-DNI-reformat {
dimensions:
time = 227928 ;
lat = UNLIMITED ; // (679 currently)
lon = 839 ;
variables:
char crs(time) ;
  crs:grid_mapping_name = "latitude_longitude" ;
  crs:longitude_of_prime_meridian = 0. ;
  crs:semi_major_axis = 6378137. ;
  crs:inverse_flattening = 298.257223563 ;
  crs:spatial_ref = "" ;
  crs:GeoTransform = "112.025 0.05 0 -10.025 0 -0.05 " ;
double lat(lat) ;
  lat:standard_name = "latitude" ;
  lat:long_name = "latitude" ;
  lat:units = "degrees_north" ;
double lon(lon) ;
  lon:standard_name = "longitude" ;
  lon:long_name = "longitude" ;
  lon:units = "degrees_east" ;
int Band1(lat, lon, time) ;
  Band1:long_name = "GDAL Band Number 1" ;
  Band1:_FillValue = -999 ;
  Band1:grid_mapping = "crs" ;
int time(time) ;
  time:long_name = "time" ;
  time:units = "hours since 1989-12-31 00:00:00 UTC" ;

// global attributes:
  :history = "Mon Apr 11 05:27:13 2016: ncpdq -D 3 -a lat,lon,time all-DNI.nc all-DNI-reformat.nc\n",
    "Sat Apr  9 09:22:32 2016: ncks -D 2 -A time_agg.nc all.nc\n",
    "Fri Apr  8 23:40:14 2016: ncecat -D 2 -u time -A -o all.nc\n",
  :nco_openmp_thread_number = 1 ;
  :Conventions = "CF-1.5" ;
  :GDAL = "GDAL 1.10.1, released 2013/08/26" ;
  :nco_input_file_number = 227928 ;
  :NCO = "4.4.2" ;
}
```
