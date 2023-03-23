################################################################################
### utilities functions
################################################################################

###-----------------------------------------------------------------------------
### checks if all elements of a vector are equal to -9
is_minus9 <- function(x){
  for(i in 1:length(x)){
    if(x[i] == -9){
      test = 0
    } else {
      test = 1
    }
  }
  if(test == 0 ){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

###-----------------------------------------------------------------------------
### drop columns of a data frame if all elements in that column is equal to -9
drop_9_cols_for_loop <- function(df) {
  for (nm in names(df))
    if(all(df[[nm]] == -9))
      df[[nm]] <- NULL
    df
}

###-----------------------------------------------------------------------------
### drop columns of a data frame if all elements in that column is NA
drop_NA_cols_for_loop <- function(df) {
  for (nm in names(df))
    if(all(is.na(df[[nm]])))
      df[[nm]] <- NULL
    df
}

###-----------------------------------------------------------------------------
### unpack objects stored in a list
unpack_list <- function(object) {
  for(.x in names(object)){
    assign(value = object[[.x]], x=.x, envir = parent.frame())
  }
}

###-----------------------------------------------------------------------------
### transfomr factor to numeric
as_numeric_factor <- function(x) {as.numeric(levels(x))[x]}

###-----------------------------------------------------------------------------
###
percentage_diff <- function(x, y){
  return((x - y)/((x + y)/2) * 100)
}
