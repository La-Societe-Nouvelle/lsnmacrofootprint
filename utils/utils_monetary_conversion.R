#' exports

from_pound_to_euro = function(year,update = F,verbose = T)
{

  if(!update)
  {
    files = list.files(dirname(tempdir()),recursive = T,pattern = paste0('OECD_POUND-',format.Date(Sys.Date(),"%Y-%m")),full.names = T)

    if(length(files) == 1){

      FROM_POUND_TO_EURO = read_parquet(files)

      message("Cached data used")

    }else{
      unlink(files,recursive = T)
    }
  }

  if(!exists('FROM_POUND_TO_EURO',envir = environment(),inherits = F))
  {
    FROM_POUND_TO_EURO = read.csv("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A.GBR+EU27_2020...EXC_A.......?&dimensionAtObservation=AllDimensions&format=csvfilewithlabels")

    write_parquet(FROM_POUND_TO_EURO,
                  tempfile(pattern = paste0('OECD_POUND-',format.Date(Sys.Date(),"%Y-%m"))))

    if(verbose) message("Data cached")
  }

  FROM_POUND_TO_EURO =
    FROM_POUND_TO_EURO %>%
    filter(TIME_PERIOD == year) %>%
    summarise(value = OBS_VALUE[REF_AREA == 'EU27_2020'] / OBS_VALUE[REF_AREA == 'GBR']) %>%
    pull(value)

  return(FROM_POUND_TO_EURO)
}

from_usd_to_euro = function(year, update = F, verbose = F)
{
  if(!update)
  {
    files = list.files(dirname(tempdir()),recursive = T,pattern = paste0('OECD_DOLLAR-',format.Date(Sys.Date(),"%Y-%m")),full.names = T)

    if (length(files) == 1) {
      FROM_DOLLAR_TO_EURO = read_parquet(files)
      message("Cached data used")
    } else {
      unlink(files,recursive = T)
    }
  }

  if (!exists('FROM_DOLLAR_TO_EURO', envir = environment(), inherits = F))
  {
    FROM_DOLLAR_TO_EURO = read.csv("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A.EU27_2020...EXC_A.......?&dimensionAtObservation=AllDimensions&format=csvfilewithlabels")
    write_parquet(FROM_DOLLAR_TO_EURO,
                  tempfile(pattern = paste0('OECD_DOLLAR-',format.Date(Sys.Date(),"%Y-%m"))))
    if (verbose) message("Data cached")
  }

  FROM_DOLLAR_TO_EURO =
    FROM_DOLLAR_TO_EURO %>%
    filter(TIME_PERIOD == year) %>%
    pull(OBS_VALUE)

  return(FROM_DOLLAR_TO_EURO)
}

from_cad_to_euro = function(year,update = F,verbose = T)
{

  if(!update)
  {
    files = list.files(dirname(tempdir()),recursive = T,pattern = paste0('OECD_CDOLLAR-',format.Date(Sys.Date(),"%Y-%m")),full.names = T)

    if(length(files) == 1){

      FROM_CDOLLAR_TO_EURO = read_parquet(files)

      message("Cached data used")

    }else{
      unlink(files,recursive = T)
    }
  }

  if(!exists('FROM_DOLLAR_TO_EURO',envir = environment(),inherits = F))
  {
    FROM_CDOLLAR_TO_EURO = read.csv("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A.EU27_2020+CAN...EXC_A.......?&dimensionAtObservation=AllDimensions&format=csvfilewithlabels")

    write_parquet(FROM_CDOLLAR_TO_EURO,
                  tempfile(pattern = paste0('OECD_CDOLLAR-',format.Date(Sys.Date(),"%Y-%m"))))

    if(verbose) message("Data cached")
  }

  FROM_CDOLLAR_TO_EURO =
    FROM_CDOLLAR_TO_EURO %>%
    filter(TIME_PERIOD == year) %>%
    summarise(OBS_VALUE = OBS_VALUE[REF_AREA == "EU27_2020"] / OBS_VALUE[REF_AREA == "CAN"]) %>%
    pull(OBS_VALUE)

  return(FROM_CDOLLAR_TO_EURO)
}