A service for retrieving Solar DNI information for a given location from satellite data between Jan 1990 and Dec 2015.

# Licensing, Terms & Conditions
Solar radiation data derived from satellite imagery processed by the Bureau of Meteorology from the Geostationary Meteorological Satellite and MTSAT series operated by Japan Meteorological Agency and from GOES-9 operated by the National Oceanographic & Atmospheric Administration (NOAA) for the Japan Meteorological Agency.

The visualisation of Australian Hourly Solar Irradiance Gridded Data on the AREMI platform is attributed to the Commonwealth of Australia acting through the Bureau of Meteorology.

The Bureau of Meteorology grants NICTA a non-transferable, non-exclusive licence to make the Australian Hourly Solar Irradiance Gridded Data available on the AREMI platform.

If visitors of the AREMI website are interested in sourcing the Australian Hourly Solar Irradiance Gridded Data, they should contact the Bureau of Meteorology directly via:

http://www.bom.gov.au/climate/data-services/data-requests.shtml

Below is an extract from "Bureau Access Agreement Covering External Party Usage of Bureau Information" Access Agreement between the Commonwealth of Australia acting through the Bureau of Meteorology (the "Bureau") and the User (NICTA).

* 1.1 The Bureau grants NICTA, in respect of the data and information made available by the Bureau to NICTA for AREMI project, a non-transferable, non-exclusive licence to:
   *  (b) supply the Information to third parties where it is incorporated as part of a User product on the terms and conditions set out in this Agreement.
*  1.2 The User (i.e. NICTA) must not supply the Information to third parties as is, except as part of a User product (i.e. in this case, AREMI visualisation platform). Where a third party (i.e. AREMI user) requests access to the Information itself, the User must advise the third party to contact the Bureau for the purpose of obtaining the Information directly from the Bureau.

## Disclaimer

  The Australian Renewable Energy Mapping Infrastructure (AREMI) website implements a geospatial map viewer which is intended for viewing over the internet. The AREMI website is being developed by National ICT Australia Limited (NICTA) in collaboration with Geoscience Australia (GA) and with funding support from the Australian Renewable Energy Agency (ARENA). ARENA invests in renewable energy projects, supports research and development activities, boosts job creation and industry development, and increases knowledge about renewable energy. The AREMI website is an information service that is intended to be accessible to all interested external parties. The AREMI website provides access to renewable energy and general information which has been provided by various third party data custodians. As a condition of using this website, users should understand and must accept, that the information and data provided on the AREMI website:

 * is entirely dependant on the accuracy of the information and data which has been provided by the third party data custodians;
 * is not necessarily complete;
 * sometimes is general in nature; and
 * should not be relied on in place of independent research and professional advice, including in later stages of investment and planning processes.

ARENA, NICTA and GA, do not warrant, and are not liable for the accuracy of the information and data on the AREMI website.

# Data Description
This one-year sample visualisation of the Bureau of Meteorology Australian Hourly Solar Irradiance Gridded Data is provided here for evaluation only and is not intended to be a finished product. We are actively researching, experimenting and developing prototypes of future products using this data, and welcome feedback - in particular we are looking for ideas from the Renewable Energy Industry of how this data can be used to further the development of Renewable Energy in Australia. Please send your feedback to aremi@nicta.com.au or use the Give Feedback link on the AREMI website.

Solar direct normal irradiance (DNI) is the instantaneous intensity of solar direct beam energy falling on a surface normal to the beam. This product is derived from global horizontal irradiance that is in turn derived from hourly satellite data. It gives a single instantaneous DNI value for every hour, at the time of the satellite observation. Typical values for DNI are up to around 1000 W/m2 (watts per square metre). The values are usually highest in cloud-free conditions away from the ends of the day. Localised variations are caused mainly by variations in atmospheric conditions, primarily cloudiness.

The Bureau of Meteorology's computer radiation model uses hourly visible images from geostationary meteorological satellites to estimate hourly instantaneous solar global horizontal irradiance (GHI) at ground level. Each GHI value is converted to a direct normal irradiance (DNI) value by applying a conversion algorithm which depends on the GHI values and the sun position. The following paragraphs describe these steps in detail.

At each location in each satellite acquired image, the brightnesses are averaged over each grid cell and used to estimate GHI at the ground. Essentially, the GHI at the ground can be calculated from the GHI at the top of the earth's atmosphere, the amount absorbed in the atmosphere (dependant on the amount of water vapour present), the amount reflected from the surface (surface albedo) and the amount reflected from clouds (cloud albedo). These GHI values were produced by reprocessing archived raw satellite data using software that was extensively rewritten in 2006, but based on the two-band physical model (Weymouth and Le Marshall, 2001) that has been the basis of the Bureau of Meteorology's satellite solar radiation system since 2000. Thumbnail images of all GHI grids were inspected and anomalous grids, due to satellite images that were noisy or otherwise anomalous, were rejected.

The August 2012 release of this dataset introduced a bias correction of the GHI values. The GHI bias is corrected by a model derived from comparisons of the satellite estimates with ground-based radiation observations from the Bureau of Meteorology's radiation monitoring network. The model is fitted on an annual basis as a function of the cosine of the solar zenith angle and the atmospheric transmittance (also known as clearness index). The bias correction is imperfect.

The bias corrected GHI is converted to DNI via the diffuse fraction estimated by applying a modified form of the Ridley et al. (2010) model. The model estimates the diffuse fraction (ratio of diffuse irradiance to GHI) from the instantaneous clearness index (ratio of GHI to extraterrestrial irradiance), daily mean clearness index, solar elevation, apparent solar time, and a measure of temporal variability that is the root mean squared difference between the clearness index for the current hour and those for one hour before and after. This variability parameter is used instead of the "persistence" parameter adopted by Ridley et al. because it gave lower uncertainties (median absolute percentage error of 16%, compared with 20% obtained by Ridley et al. for southern hemisphere stations). The model coefficients were established by fitting the model to observations of GHI and DNI from the Bureau's surface radiation network, which were 1-minute observations taken at 1- hour intervals to simulate the satellite sampling.

The hourly irradiance gridded datasets cover Australia with a resolution of 0.05 degrees in latitude and longitude. For each day there are grids for up to eighteen times, labelled by the UT date and hour (without minutes) of the observation time. The grids for a particular local date start at 18 UT on the preceding UT date and end at 11 UT of the date. The irradiance units are watts per square metre (W/m2).

The satellite data on which the analyses were based have an associated resolution and typical accuracy of 0.01 degrees, although some individual images have errors of several km.

The accuracy of the satellite-based DNI values is estimated by comparison with 1- minute averaged DNI measurements from Bureau of Meteorology surface-based instruments. The mean bias difference (average of the satellite - surface difference), calculated on an annual basis across all surface sites, is -20 to +18 W/m2 depending on the year. This is -4% to +4% of the mean irradiance of around 500 W/m2. The root mean square difference, calculated on a similar basis, is around 210 W/m2, which is 42% of the mean irradiance.

The source of uncertainties associated with calculation of DNI includes uncertainties in:

 * anisotropy of cloud-top reflectance;
 * water vapour in the atmosphere;
 * satellite calibration; and
 * the GHI-to-DNI conversion model.

It should be noted that a particular DNI value may not be representative of a 1-hour period, due to variations in the solar zenith angle during the hour, and most significantly because of variations in atmospheric conditions such as cloudiness.

The temporal coverage is not complete. A grid for a particular time may be missing if no satellite image was available, the image was not processed, or the image was rejected by quality control. Notably:

No values are reported for the first two hours and last two hours of the day for the period up until 1994-06-30, due to the absence of satellite images at these times during the initial period of operation of GMS4.
The values are sparser during the period July 2001 to June 2003, which spans the period of reduced imaging frequency at the end of the life of GMS-5, and the initial few weeks of operation of GOES-9 in the Australian region.
DNI values are missing throughout any day for which the daily global horizontal exposure is missing. This is because the current GHI to DNI conversion requires daily global horizontal exposure.
