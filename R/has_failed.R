
has_failed <- 
function(x, 
         n)
{
  b <- !x
  if (length(x)==1L && identical(which(b), 1L)) {
    rep(TRUE, n)
  } else {
    b
  }
}
