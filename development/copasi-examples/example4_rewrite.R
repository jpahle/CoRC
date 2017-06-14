if (!exists("CRootContainer_getRoot")) devtools::load_all()

MODEL_STRING <- '<?xml version="1.0" encoding="UTF-8"?><!-- Created by COPASI version 4.4.29 (Debug) on 2009-03-05 14:41 with libSBML version 3.3.0. --><sbml xmlns="http://www.sbml.org/sbml/level2/version3" level="2" version="3"><model metaid="COPASI1" id="Model_1" name="New Model"> <listOfUnitDefinitions><unitDefinition id="volume"><listOfUnits><unit kind="litre" scale="-6"/></listOfUnits></unitDefinition><unitDefinition id="substance"><listOfUnits><unit kind="mole" scale="-9"/></listOfUnits></unitDefinition></listOfUnitDefinitions><listOfCompartments><compartment id="compartment_1" name="compartment" size="1"/></listOfCompartments><listOfSpecies><species metaid="COPASI2" id="species_1" name="A" compartment="compartment_1" initialConcentration="1e-10"></species><species metaid="COPASI3" id="species_2" name="B" compartment="compartment_1" initialConcentration="0"></species><species metaid="COPASI4" id="species_3" name="C" compartment="compartment_1" initialConcentration="0"></species></listOfSpecies><listOfReactions><reaction id="reaction_1" name="reaction" reversible="false"><listOfReactants><speciesReference species="species_1"/></listOfReactants><listOfProducts><speciesReference species="species_2"/></listOfProducts><kineticLaw><math xmlns="http://www.w3.org/1998/Math/MathML"><apply><times/><ci> compartment_1 </ci><ci> k1 </ci><ci> species_1 </ci></apply></math><listOfParameters><parameter id="k1" value="0.1"/></listOfParameters></kineticLaw></reaction><reaction id="reaction_2" name="reaction_1" reversible="false"><listOfReactants><speciesReference species="species_2"/></listOfReactants><listOfProducts><speciesReference species="species_3"/></listOfProducts><kineticLaw><math xmlns="http://www.w3.org/1998/Math/MathML"><apply><times/><ci> compartment_1 </ci><ci> k1 </ci><ci> species_2 </ci></apply></math><listOfParameters><parameter id="k1" value="0.1"/></listOfParameters></kineticLaw></reaction></listOfReactions></model></sbml>'


stopifnot(!is.null(CRootContainer_getRoot()))
# create a datamodel
dataModel <- CRootContainer_addDatamodel()
stopifnot(CRootContainer_getDatamodelList()$size() == 1)
# the only argument to the main routine should be the name of an SBML file
tryCatch(dataModel$importSBMLFromString(MODEL_STRING), error = function(e) {
  write("Error while importing the model from given string.", stderr())
  quit(save = "default", status = 1, runLast = TRUE)
} )

model <- dataModel$getModel()
stopifnot(!is.null(model))
# create a report with the correct filename and all the species against
# time.
reports <- dataModel$getReportDefinitionList()
# create a report definition object
report <- reports$createReportDefinition("Report", "Output for timecourse")
# set the task type for the report definition to timecourse
report$setTaskType("timeCourse")
# we don't want a table
report$setIsTable(FALSE)
# the entries in the output should be seperated by a ", "
report$setSeparator(CCopasiReportSeparator(", "))

# we need a handle to the header and the body
# the header will display the ids of the metabolites and "time" for
# the first column
# the body will contain the actual timecourse data
sep <- report$getSeparator()
sep_string <- sep$getCN()$getString()
header <- report$getHeaderAddr()
body <- report$getBodyAddr()
time_string <- CCommonName_getString(CCommonName(paste(model$getCN()$getString(), ",Reference=Time", sep = "")))
body$push_back(CRegisteredCommonName(time_string))
body$push_back(CRegisteredCommonName(sep_string))
time_string <- CCommonName_getString(CDataObject_getCN(CDataString("time")))
header$push_back(CRegisteredCommonName(time_string))
header$push_back(CRegisteredCommonName(sep_string))

iMax <- model$getMetabolites()$size()
i <- 0
while (i < iMax) {
    metab <- model$getMetabolite(i)
    stopifnot(!is.null(metab))
    # we don't want output for FIXED metabolites right now
    if (metab$getStatus() != "FIXED") {
        # we want the concentration in the output
        # alternatively, we could use "Reference=Amount" to get the
        # particle number
        conc <- metab$getObject(CCommonName("Reference=Concentration"))
        conc_string <- conc$getCN()$getString()
        body$push_back(CRegisteredCommonName(conc_string))
        # add the corresponding id to the header
        sbml_id <- metab$getSBMLId()
        sbml_string <- CCommonName_getString(CDataObject_getCN(CDataString(sbml_id)))
        header$push_back(CRegisteredCommonName(sbml_string))
        
        if (i != iMax-1) {
          # after each entry, we need a seperator
          body$push_back(CRegisteredCommonName(sep_string))

          # and a seperator
          header$push_back(CRegisteredCommonName(sep_string))
        }
    }
    i <- i + 1
}

# get the trajectory task object
trajectoryTask <- dataModel$getTask("Time-Course")
stopifnot(!is.null(trajectoryTask))
# if there isn't one
if (is.null(trajectoryTask)) {
    # create a one
    trajectoryTask <- CTrajectoryTask()
    # add the time course task to the task list
    # this method makes sure the object is now owned by the list
    # and that SWIG does not delete it
    dataModel$getTaskList()$addAndOwn(trajectoryTask)
}


# run a stochastic time course
trajectoryTask$setMethodType("stochastic")

# get the problem for the task to set some parameters
problem <- as(trajectoryTask$getProblem(), "_p_CTrajectoryProblem")

# pass a pointer of the model to the problem
problem$setModel(model)

# we don't want the trajectory task to run by itself, but we want to
# run it from a scan, so we deactivate the standalone trajectory task
trajectoryTask$setScheduled(FALSE)

# simulate 100 steps
problem$setStepNumber(100)
# start at time 0
model$setInitialTime(0.0)
# simulate a duration of 10 time units
problem$setDuration(10)
# tell the problem to actually generate time series data
problem$setTimeSeriesRequested(TRUE)

# now we set up the scan
scanTask <- dataModel$getTask("Scan")
stopifnot(!is.null(scanTask))
if (is.null(scanTask)) {
    # create a scan task
    scanTask <- CScanTask()
    # add the scan task
    # this method makes sure the object is now owned by the list
    # and that SWIG does not delete it
    dataModel$getTaskList()$addAndOwn(scanTask)
}

# get the problem
scanProblem <- as(scanTask$getProblem(), "_p_CScanProblem")
stopifnot(!is.null(scanProblem))

# set the model for the problem
scanProblem$setModel(model)

# activate the task so that is is run
# if the model is saved and passed to CopasiSE
scanTask$setScheduled(TRUE)

# set the report for the task
scanTask$getReport()$setReportDefinition(report)

# set the output file for the report
scanTask$getReport()$setTarget("example4.txt")
# don't append to an existing file, but overwrite
scanTask$getReport()$setAppend(FALSE)

# tell the scan that we want to make a scan over a trajectory task
scanProblem$setSubtask("timeCourse")

# we just want to run the timecourse task a number of times, so we
# create a repeat item with 100 repeats
scanProblem <- as(scanProblem, '_p_CScanProblem')
scanProblem$addScanItem("SCAN_REPEAT", 100)

# we want the output from the trajectory task
scanProblem$setOutputInSubtask(TRUE)

# we don't want to set the initial conditions of the model to the end
# state of the last run
scanProblem$setContinueFromCurrentState(FALSE)

tryCatch(scanTask$process(TRUE), error = function(e) {
  write("Error. Running the scan failed.", stderr())
  # check if there are additional error messages
  if (CCopasiMessage_size() > 0) {
      # print the messages in chronological order
      write(CCopasiMessage_getAllMessageText(TRUE), stderr())
  }
  quit(save = "default", status = 1, runLast = TRUE)
} )


