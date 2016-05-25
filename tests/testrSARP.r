library(rSARP)
sin <- utils::read.csv(system.file("extdata","SearchInput.csv", package = "rSARP"),header=T,fill=F,blank.lines.skip=T,flush=T,colClasses=c(NA,NA,NA,NA),sep=",",as.is=c(1))
                                        # now have the right input file for the tests that follow
utils::write.table(sin,"SearchInput.csv",row.names=FALSE, sep=",", col.names=TRUE, quote=TRUE)   # put the input file where it belongs
searchme(graphs=2)  # run the program and suppress graphing
tdf <- utils::read.csv("SearchOut.csv", header=T, fill=F, blank.lines.skip=T)   # now read the output from the test
odf <- utils::read.csv(system.file("extdata", "SearchOut.csv", package = "rSARP"), header=T, fill = F, blank.lines.skip=T)  # read the stored answer used to compare

# now compare the two dataframes to make sure nothing has changed
if (!(identical(tdf,odf)))
{  # check of equivalence fails
    print(tdf)
    print(odf)
    message("rSARP package has changed and the output no longer passes validation checks")
}

