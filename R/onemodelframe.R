formulasFrame<-function(formulas,
                        data=parent.frame(),
                        na.action=getOption("na.action"),
                        subset=NULL,
                        weights=NULL,
                        check.env=FALSE,
                        one.frame=TRUE){

  pf <- parent.frame()

  ## make all objects formulas, make them a list
  formulas <- lapply(formulas, formula)

  if(check.env){
    envs <- lapply(formulas, environment)
    hasenv <- which(sapply(envs,is.null))
    if (length(hasenv)>1){
      for (i in 2:length(hasenv))
        if ( ! identical(envs[[hasenv[1]]], envs[[hasenv[i]]]))
          warning("Different environments on formulas")
    }
  }

  mfs <- eval(substitute(lapply(formulas,
                                model.frame,
                                data=data,
                                na.action="na.pass",
                                subset=subset,
                                weights=weights),
                       list(subset=subset,
                            formulas=formulas,
                            weights=weights,
                            data=data))
            , pf)

  ## One frame works okay
  if (one.frame){

    mf <- do.call("cbind",mfs)
    rm(mfs)
    mf <- mf[,!duplicated(names(mf)), drop=FALSE]
    mf <- get(na.action)(mf)
    return(mf)

  } else {

    ## finds which observations will be dropped in each formula - returns a list
    naa <- lapply(mfs, function(x) attr(get(na.action)(x), "na.action"))

    ## Reduces the list to the unique observations dropped
    ## implicitly converts the object to a vector
    drop <- unique(do.call("c", naa))

    if (length(drop)){

        ## bug in original code
        ## should be ! is.null
        cnaa <- sapply(naa, class)
        class(drop) <- cnaa[ cnaa != "NULL" ][1]

        mfs<-lapply(mfs,
                    function(x) {
                        nomen <- rownames(x)[drop]
                        x <- x[-drop, , drop=FALSE];
                        attr(x, "na.action") <- drop;
                        ## adding names - but does it work with subset. YES
                        names(attr(x, "na.action")) <- nomen
                        return(x)
                    })
    }
    return(mfs)

  }

}
