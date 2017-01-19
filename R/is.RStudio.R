
# used by the 'View_obs' function
is.RStudio <- 
function()
{
  Sys.getenv("RSTUDIO") == "1"
}
