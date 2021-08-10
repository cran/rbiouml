### R code from vignette source 'rbiouml.Rnw'

###################################################
### code chunk number 1: rbiouml.Rnw:37-39 (eval = FALSE)
###################################################
## library(rbiouml)
## biouml.login("https://ict.biouml.org")


###################################################
### code chunk number 2: rbiouml.Rnw:60-61 (eval = FALSE)
###################################################
## biouml.ls("databases")


###################################################
### code chunk number 3: rbiouml.Rnw:77-78 (eval = FALSE)
###################################################
## biouml.ls("data/Examples/Optimization/Data/Experiments")


###################################################
### code chunk number 4: rbiouml.Rnw:85-87 (eval = FALSE)
###################################################
## x <- biouml.get("data/Examples/Optimization/Data/Experiments/exp_data_1")
## head(x)


###################################################
### code chunk number 5: rbiouml.Rnw:104-107 (eval = FALSE)
###################################################
## x[,5] <- x[,3] + x[,4]
## biouml.put("data/Collaboration/Demo/tmp/exp_data_1_sum", x)
## biouml.ls("data/Collaboration/Demo/tmp")


###################################################
### code chunk number 6: rbiouml.Rnw:116-117 (eval = FALSE)
###################################################
## summary( biouml.analysis.list() )


###################################################
### code chunk number 7: rbiouml.Rnw:131-132 (eval = FALSE)
###################################################
## biouml.analysis.parameters("Filter table")


###################################################
### code chunk number 8: rbiouml.Rnw:144-149 (eval = FALSE)
###################################################
## biouml.analysis("Filter table", list(
##   inputPath="data/Examples/Optimization/Data/Experiments/exp_data_1",
##   filterExpression="time < 40",
##   outputPath="data/Collaboration/Demo/tmp/exp_data_1 filtered"
## ))


###################################################
### code chunk number 9: rbiouml.Rnw:167-168 (eval = FALSE)
###################################################
## head( biouml.importers() )


###################################################
### code chunk number 10: rbiouml.Rnw:180-183 (eval = FALSE)
###################################################
## hiv.genome <- system.file("extdata","HIV-1.fa", package="rbiouml")
## output.folder <- "data/Collaboration/Demo/tmp"
## biouml.import(hiv.genome, output.folder,  importer="Fasta format (*.fasta)")


###################################################
### code chunk number 11: rbiouml.Rnw:191-192 (eval = FALSE)
###################################################
## biouml.ls(output.folder)


###################################################
### code chunk number 12: rbiouml.Rnw:199-200 (eval = FALSE)
###################################################
## head( biouml.exporters() )


###################################################
### code chunk number 13: rbiouml.Rnw:208-211 (eval = FALSE)
###################################################
## biouml.export("data/Collaboration/Demo/tmp/HIV-1",
##   exporter="Fasta format (*.fasta)", target.file="HIV-1.fa")
## file.exists("HIV-1.fa")


###################################################
### code chunk number 14: rbiouml.Rnw:219-220 (eval = FALSE)
###################################################
## biouml.logout()


