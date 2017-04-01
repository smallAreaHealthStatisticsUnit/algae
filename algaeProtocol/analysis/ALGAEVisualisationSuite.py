
"""
=====================================================================================
ALGAE VISUALISATION SUITE README
================================
by Kevin Garwood



Welcome to the ALGAE Visualisation Suite.  The goal of these scripts is to use
data files already produced by the main ALGAE Protocol and create multiple graphs
to visualise the results.

For the convenience of developers, I've split the code base into modules.  But for
the convenience of users who just want to run it in one file, I've made another
version where all the modules have been combined.

Make sure you have installed Python 2.7.  For now, make sure you have unzipped the 
ALGAE Protocol folder to "C:\algae_protocol".  If you want to change that path, 
change the default input and output directories found in the CommandLineArguments 
class.

INVOCATION
For developers:

This will run the script using default input and output directories that assume you
have the ALGAE protocol installed in C:\algae_protocol.
python ALGAEAnalysis.py

This will run the script if you have installed the ALGAE protocol somewhere else
python ALGAEAnalysis.py -inputDirectory c:\myALGAEInstallation -outputDirectory c:\graph_results.

For anyone else, use the same command line arguments but specify the ALGAEVisualisationSuite.py
file:

python ALGAEVisualisationSuite.py
or 
python ALGAEVisualisationSuite.py -inputDirectory c:\myALGAEInstallation -outputDirectory c:\graph_results.

=====================================================================================
"""

import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
import matplotlib
import os
import argparse
from datetime import datetime
import time






"""
=====================================================================================
MODULE: ALGAEAnalysis

PURPOSE:
This is the main starting point for the analysis script and it generates the graphs.

DESCRIPTION:
The ALGAE Analysis uses command line arguments to determine where it should try to
read the results of the ALGAE protocol and where it should generate the graph results.
The -inputDirectory command line parameter expects a directory that has the following
skeleton structure:
[specified directory]
	early_life
		results
			exposure_data
				mobility_clean
				mobility_life_stage
				mobility_unclean
				no_mobility_birth_address			
	later_life
			exposure_data
				mobility_clean
				mobility_life_stage
				mobility_unclean

Either or both early_life and early_life directories may exist.

The script will then create a data frame for each CSV file that appears in the 
exposure directories.  Subset copies of each data frame will be created, and
columns will be renamed so they may work with generic graphing functions that
expect certain fields to be present.  The graphing functions are designed so
they do not care whether the data refer to an early or a later life analysis.

The graphs will all be generated in a target directory specified by the
-outputDirectory command line argument.  Be aware that if no inputDirectory or
outputDirectory is specified, the ALGAE Analysis will use default values that 
assume that the installation for ALGAE appears in c:\algae_protcol.  To change
the defaults, please make changes in the initialisation routine of the 
CommandLineArguments class.

ADAPTING THE CODE:
The ALGAE Analysis scripts are currently applied to only one type of pollutant:
pm10 total.  For the exposure box plot graphs, only the pm10 total cumulative
values are considered.  To make it work for another pollutant, peforming a simple
search and replace should suffice.  To make the protocol accommodate more than one
kind of pollutant, you may want to consider making copies of this class for each
one you want (eg: ALGAEAnalysisNOx.py, ALGAEAnalysisPM10_TOT.py, ...) If you do
this, then you should also consider renaming the output files that are used to 
write graphs to file.  Look at generateLaterLifeBoxPlotGraphs() to observe how
information about the pollutant is passed to the graph.  You might also look
at altering GraphConfigurationSettings.createGraphResultFilePath(...) found in
the GlobalSettings module so that the script can create more descriptive file names.


OPEN SOURCE LICENSING
Copyright 2017 Imperial College London, developed by the Small Area
Health Statistics Unit in collaboration with the Avon Longitudinal Study of Parents
and Children (ALSPAC).
 
The code was originally authored by Kevin Garwood and reviewed by Olly Butters
and Iain Bickerstaffe.

This file is part of the ALGAE (Algorithms for Assessing Early life exposures) project.
ALGAE is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RIF is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with RIF.  If not, see <http://www.gnu.org/licenses/>.

Author: Kevin Garwood
=====================================================================================
"""

#from ALGAEDataSets import ALGAEDataSets
#from GlobalSettings import CommandLineArguments
#from ExposureDataQualityTypeGraph import ExposureDataQualityTypeGraph
#from LifeStageNullExposuresGraph import LifeStageNullExposuresGraph
#from ExposureBoxPlotGraph import ExposureBoxPlotGraph

class ALGAEAnalysis():
	"""

	"""	
	def __init__(self):

		self.algaeDataSets = ALGAEDataSets()		
		self.args = CommandLineArguments()

	def runAnalysis(self):
		self.args.parseCommandLine()
		print "Detecting data sets..."
		self.algaeDataSets.detectDataSets(self.args.inputDirectory)
		
		print "Generating stacked bar graphs showing how many days for each life stage were "
		print "spent at a location that was considered:"
		print "(1) a poor address match exposure day"
		print "(2) a missing exposure day"
		print "(3) an out-of-bounds exposure day"
		print "(4) an invalid exposure day"
		print "(5) a good match exposure day"
		print "All of these graphs will have file names like \"dq_types_bar_earlyCln_[date]\""
		print "..."
		self.generateDataQualityTypeGraphs()
		print "Generating bar graphs indicating how many life stage exposures had null values."
		print "The graphs will have file names like \"null_bar_[life stage]-date.png\"..."
		self.generateLifeStageNullValueGraphs()
		print "Generating box plot graphs show spread of exposure values for each life stage and for"
		print "each exposure assignment method (ie: cleaned, uncleaned, life stage birth address)."
		print "These graphs will have file names like \"exp_box_[life stage]-[date].png\ ..."
		self.generateBoxPlotGraphs()
		print "Analysis FINISHED!"
		print "Please look in the directory " + self.args.outputDirectory + " for your graph images."
	def generateDataQualityTypeGraphs(self):
		exposureDataQualityGraph = ExposureDataQualityTypeGraph()
		exposureDataQualityGraph.setOutputDirectory(self.args.outputDirectory)

		if self.algaeDataSets.earlyMobClnExp is not None:

			#Early Life Cleaned Mobility
			earlyClnData = self.algaeDataSets.earlyMobClnExp[[
				'algae3100_person_id', 
				'algae3101_life_stage', 
				'algae3102_life_stage_duration',
				'algae3127_pm10_tot_good_addr_days',
				'algae3123_pm10_tot_inv_addr_days',
				'algae3124_pm10_tot_oob_days',
				'algae3126_pm10_tot_missing_exp_days',
				'algae3125_pm10_tot_poor_addr_days']]

			earlyClnData.columns = [['person_id', 
				'life_stage', 
				'life_stage_duration',
				'good_match_days',
				'invalid_addr_days',
				'oob_days',
				'missing_exp_days',
				'poor_addr_days']]
		
			exposureDataQualityGraph.setTitle("Cleaned Mobility Assignment")
			exposureDataQualityGraph.generateGraphs(
				earlyClnData,
				"earlyCln")

			#Early Life Uncleaned Mobility
			earlyUnclnData = self.algaeDataSets.earlyMobUnclnExp[[
				'algae3200_person_id', 
				'algae3201_life_stage', 
				'algae3202_life_stage_duration',
				'algae3227_pm10_tot_good_addr_days',
				'algae3223_pm10_tot_inv_addr_days',
				'algae3224_pm10_tot_oob_days',
				'algae3226_pm10_tot_missing_exp_days',
				'algae3225_pm10_tot_poor_addr_days']]

			earlyUnclnData.columns = [['person_id', 
				'life_stage', 
				'life_stage_duration',
				'good_match_days',
				'invalid_addr_days',
				'oob_days',
				'missing_exp_days',
				'poor_addr_days']]
		
			exposureDataQualityGraph.setTitle("Uncleaned Mobility Assignment")
			exposureDataQualityGraph.generateGraphs(
				earlyUnclnData,
				"earlyUncln")

			#Early Life Stage Mobility

			earlyStgData = self.algaeDataSets.earlyStgMobExp[[
				'algae3300_person_id', 
				'algae3301_life_stage', 
				'algae3302_life_stage_duration',
				'algae3327_pm10_tot_good_addr_days',
				'algae3323_pm10_tot_inv_addr_days',
				'algae3324_pm10_tot_oob_days',
				'algae3326_pm10_tot_missing_exp_days',
				'algae3325_pm10_tot_poor_addr_days']]

			earlyStgData.columns = [['person_id', 
				'life_stage', 
				'life_stage_duration',
				'good_match_days',
				'invalid_addr_days',
				'oob_days',
				'missing_exp_days',
				'poor_addr_days']]
		
			exposureDataQualityGraph.setTitle("Life Stage Assignment")
			exposureDataQualityGraph.generateGraphs(
				earlyStgData, 
				"earlyStg")

			#Birth Address Assessment
			earlyBirthAddrData = self.algaeDataSets.earlyNoMobBirthExp[[
				'algae3400_person_id', 
				'algae3401_life_stage', 
				'algae3402_life_stage_duration',
				'algae3427_pm10_tot_good_addr_days',
				'algae3423_pm10_tot_inv_addr_days',
				'algae3424_pm10_tot_oob_days',
				'algae3426_pm10_tot_missing_exp_days',
				'algae3425_pm10_tot_poor_addr_days']]

			earlyBirthAddrData.columns = [['person_id', 
				'life_stage', 
				'life_stage_duration',
				'good_match_days',
				'invalid_addr_days',
				'oob_days',
				'missing_exp_days',
				'poor_addr_days']]
		
			exposureDataQualityGraph.setTitle("Birth Address Assignment")
			exposureDataQualityGraph.generateGraphs(
				earlyBirthAddrData,
				"earlyBirthAddr")


		if self.algaeDataSets.laterMobClnExp is not None:

			#Early Life Cleaned Mobility
			laterClnData = self.algaeDataSets.laterMobClnExp[[
				'algae3500_person_id', 
				'algae3501_life_stage', 
				'algae3502_life_stage_duration',
				'algae3527_pm10_tot_good_addr_days',
				'algae3523_pm10_tot_inv_addr_days',
				'algae3524_pm10_tot_oob_days',
				'algae3526_pm10_tot_missing_exp_days',
				'algae3525_pm10_tot_poor_addr_days']]


			laterClnData.columns = [['person_id', 
				'life_stage', 
				'life_stage_duration',
				'good_match_days',
				'invalid_addr_days',
				'oob_days',
				'missing_exp_days',
				'poor_addr_days']]
		
			exposureDataQualityGraph.setTitle("Cleaned Mobility Assignment")
			exposureDataQualityGraph.generateGraphs(
				laterClnData,
				"laterCln")

			#Later Life Uncleaned Mobility
			laterUnclnData = self.algaeDataSets.laterMobUnclnExp[[
				'algae3600_person_id', 
				'algae3601_life_stage', 
				'algae3602_life_stage_duration',
				'algae3627_pm10_tot_good_addr_days',
				'algae3623_pm10_tot_inv_addr_days',
				'algae3624_pm10_tot_oob_days',
				'algae3626_pm10_tot_missing_exp_days',
				'algae3625_pm10_tot_poor_addr_days']]

			laterUnclnData.columns = [['person_id', 
				'life_stage', 
				'life_stage_duration',
				'good_match_days',
				'invalid_addr_days',
				'oob_days',
				'missing_exp_days',
				'poor_addr_days']]
		
			exposureDataQualityGraph.setTitle("Uncleaned Mobility Assignment")
			exposureDataQualityGraph.generateGraphs(
				laterUnclnData, 
				"laterUncln")

			#Later Life Stage Mobility

			laterStgData = self.algaeDataSets.laterStgMobExp[[
				'algae3700_person_id', 
				'algae3701_life_stage', 
				'algae3702_life_stage_duration',
				'algae3727_pm10_tot_good_addr_days',
				'algae3723_pm10_tot_inv_addr_days',
				'algae3724_pm10_tot_oob_days',
				'algae3726_pm10_tot_missing_exp_days',
				'algae3725_pm10_tot_poor_addr_days']]

			laterStgData.columns = [['person_id', 
				'life_stage', 
				'life_stage_duration',
				'good_match_days',
				'invalid_addr_days',
				'oob_days',
				'missing_exp_days',
				'poor_addr_days']]
		
			exposureDataQualityGraph.setTitle("Life Stage Assignment")
			exposureDataQualityGraph.generateGraphs(
				laterStgData,
				"laterStg")


	"""
	===============================================================================
	FUNCTION DESCRIPTION:
	Generates the null value graphs but also checks to see whether early life
	and or later life data sets exist (some runs of the ALGAE protocol may have
	input data for early life but not later life).  Here, the presence of the
	early cleaned mobility exposure data set is used as an indicator for whether
	any early life exposure data were generated.  A similar indicator is used for
	detecting the presence of later life exposure data.
	===============================================================================
	"""
	def generateLifeStageNullValueGraphs(self):
		lifeStageNullExposuresGraph = LifeStageNullExposuresGraph()
		lifeStageNullExposuresGraph.setOutputDirectory(self.args.outputDirectory)

		if self.algaeDataSets.earlyMobClnExp is not None:
			self.generateEarlyLifeStageNullValueGraphs(lifeStageNullExposuresGraph)
		if self.algaeDataSets.laterMobClnExp is not None:
			self.generateLaterLifeStageNullValueGraphs(lifeStageNullExposuresGraph)			

	"""
	===============================================================================
	FUNCTION DESCRIPTION:
	Most of the work done by this function is copying subsets of original data
	sets and renaming the column values so that the class for generating null
	value graphs does not have to care whether the data refer to early or later 
	life exposure assignments.
	===============================================================================
	"""
	def generateEarlyLifeStageNullValueGraphs(
		self, 
		lifeStageNullExposuresGraph):

		numberOfStudyMembers = len(self.algaeDataSets.earlyMobClnExp['algae3100_person_id'].unique())

		#Early Life Cleaned Mobility
		earlyClnData = self.algaeDataSets.earlyMobClnExp[[
			'algae3100_person_id', 
			'algae3101_life_stage', 
			'algae3152_pm10_tot_sum']]
		earlyClnData.columns = [['person_id', 
			'life_stage', 
			'mob_exp_cln']]

		#Early Life Uncleaned Mobility
		earlyUnclnData = self.algaeDataSets.earlyMobUnclnExp[[
			'algae3200_person_id', 
			'algae3201_life_stage', 
			'algae3240_pm10_tot_sum']]
		earlyUnclnData.columns = [['person_id', 
			'life_stage', 
			'mob_exp_uncln']]

		#Early Life Stage Mobility
		earlyStgData = self.algaeDataSets.earlyStgMobExp[[
			'algae3300_person_id', 
			'algae3301_life_stage', 
			'algae3340_pm10_tot_sum']]
		earlyStgData.columns = [['person_id', 
			'life_stage', 
			'stg_mob_exp']]

		#Birth Address Assessment
		earlyBirthAddrData = self.algaeDataSets.earlyNoMobBirthExp[[
			'algae3400_person_id', 
			'algae3401_life_stage', 
			'algae3440_pm10_tot_sum']]
		earlyBirthAddrData.columns = [['person_id', 
			'life_stage', 
			'no_mob_birth_addr_exp']]

		totalStudyMembers = earlyClnData['life_stage'].count()
		lifeStageNullExposuresGraph.generateGraphs(
			totalStudyMembers,
			earlyClnData,
			earlyUnclnData,
			earlyStgData,
			earlyBirthAddrData)		
			

	def generateLaterLifeStageNullValueGraphs(
		self, 
		lifeStageNullExposuresGraph):

		numberOfStudyMembers = len(self.algaeDataSets.laterMobClnExp['algae3500_person_id'].unique())

		#Later Life Cleaned Mobility
		laterClnData = self.algaeDataSets.laterMobClnExp[[
			'algae3500_person_id', 
			'algae3501_life_stage', 
			'algae3552_pm10_tot_sum']]
		laterClnData.columns = [['person_id', 
			'life_stage', 
			'mob_exp_cln']]

		#Later Life Uncleaned Mobility
		laterUnclnData = self.algaeDataSets.laterMobUnclnExp[[
			'algae3600_person_id', 
			'algae3601_life_stage', 
			'algae3640_pm10_tot_sum']]
		laterUnclnData.columns = [['person_id', 
			'life_stage', 
			'mob_exp_uncln']]

		#Later Life Stage Mobility
		laterStgData = self.algaeDataSets.laterStgMobExp[[
			'algae3700_person_id', 
			'algae3701_life_stage', 
			'algae3740_pm10_tot_sum']]
		laterStgData.columns = [['person_id', 
			'life_stage', 
			'stg_mob_exp']]

		totalStudyMembers = laterClnData['life_stage'].count()
		lifeStageNullExposuresGraph.generateGraphs(
			numberOfStudyMembers,
			laterClnData,
			laterUnclnData,
			laterStgData,
			None)

	def generateBoxPlotGraphs(self):
		exposureBoxPlotGraph = ExposureBoxPlotGraph()
		exposureBoxPlotGraph.setOutputDirectory(self.args.outputDirectory)

		if self.algaeDataSets.earlyMobClnExp is not None:
			self.generateEarlyLifeBoxPlotGraphs(
				exposureBoxPlotGraph)
		if self.algaeDataSets.laterMobClnExp is not None:
			self.generateLaterLifeBoxPlotGraphs(
				exposureBoxPlotGraph)

	def generateEarlyLifeBoxPlotGraphs(
		self,
		exposureBoxPlotGraph):
		
		#Early Life Cleaned Mobility
		cln = self.algaeDataSets.earlyMobClnExp[[
			'algae3100_person_id', 
			'algae3101_life_stage', 
			'algae3152_pm10_tot_sum']]
		cln.columns = [['person_id', 
			'life_stage', 
			'mob_exp_cln']]

		#Early Life Uncleaned Mobility
		uncln = self.algaeDataSets.earlyMobUnclnExp[[
			'algae3200_person_id', 
			'algae3201_life_stage', 
			'algae3240_pm10_tot_sum']]
		uncln.columns = [['person_id', 
			'life_stage', 
			'mob_exp_uncln']]

		#Early Life Stage Mobility
		stg = self.algaeDataSets.earlyStgMobExp[[
			'algae3300_person_id', 
			'algae3301_life_stage', 
			'algae3340_pm10_tot_sum']]
		stg.columns = [['person_id', 
			'life_stage', 
			'stg_mob_exp']]

		#Birth Address Assessment
		birth_addr = self.algaeDataSets.earlyNoMobBirthExp[[
			'algae3400_person_id', 
			'algae3401_life_stage', 
			'algae3440_pm10_tot_sum']]
		birth_addr.columns = [['person_id', 
			'life_stage', 
			'no_mob_birth_addr_exp']]

		exposureBoxPlotGraph.generateGraphs(
			cln, 
			uncln, 
			stg, 
			birth_addr,
			'early',
			'PM10 Total')

	def generateLaterLifeBoxPlotGraphs(
		self,
		exposureBoxPlotGraph):
		
		#Later Life Cleaned Mobility
		cln = self.algaeDataSets.laterMobClnExp[[
			'algae3500_person_id', 
			'algae3501_life_stage', 
			'algae3552_pm10_tot_sum']]
		cln.columns = [['person_id', 
			'life_stage', 
			'mob_exp_cln']]

		#Later Life Uncleaned Mobility
		uncln = self.algaeDataSets.laterMobUnclnExp[[
			'algae3600_person_id', 
			'algae3601_life_stage', 
			'algae3640_pm10_tot_sum']]
		uncln.columns = [['person_id', 
			'life_stage', 
			'mob_exp_uncln']]

		#Later Life Stage Mobility
		stg = self.algaeDataSets.laterStgMobExp[[
			'algae3700_person_id', 
			'algae3701_life_stage', 
			'algae3740_pm10_tot_sum']]
		stg.columns = [['person_id', 
			'life_stage', 
			'stg_mob_exp']]

		exposureBoxPlotGraph.generateGraphs(
			cln, 
			uncln, 
			stg, 
			None,
			'early',
			'PM10 Total')


"""
=====================================================================================
MODULE: ALGAEDataSets

PURPOSE:
This module contains the ALGAE analysis code which tries to initially read
exposure data sets from the files that are generated by the main ALGAE protocol.



DESCRIPTION:
The module is meant to hide code that is responsible for scanning a directory
and detecting the most recent batch of exposure data set files that are 
generated by ALGAE.

Code used to read all the exposure data sets will look like this:

commandLineArguments = CommandLineArguments()
algaeDataSets = ALGAEDataSets()
algaeDataSets.detectDataSets(commandLineArguments)
algaeDataSets.printAvailableDataSetFilePaths()

The data frames would then be accessed like this:

algaeDataSets.earlyMobClnExp

Note that this code is brittle and will break if changes are made to the names of
files that are generated by the main ALGAE Protocol.

OPEN SOURCE LICENSING
Copyright 2017 Imperial College London, developed by the Small Area
Health Statistics Unit in collaboration with the Avon Longitudinal Study of Parents
and Children (ALSPAC).
 
The code was originally authored by Kevin Garwood and reviewed by Olly Butters
and Iain Bickerstaffe.

This file is part of the ALGAE (Algorithms for Assessing Early life exposures) project.
ALGAE is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with RIF.  If not, see <http://www.gnu.org/licenses/>.

Author: Kevin Garwood
=====================================================================================
"""

from GlobalSettings import CommandLineArguments
import re #Library for handling regular expressions
import os
import fnmatch
from datetime import datetime
import pandas as pd


"""
===============================================================================
CLASS DESCRIPTION:
This class uses the inputDirectory parameter found in the CommandLineArguments
class and tries to identify the file paths of all exposure data sets that will
be used to generate graphs. 
===============================================================================
"""
class ALGAEDataSetFileLocations():

	def __init__(self):
		self.earlyCleanedAddrFile = None

		self.earlyCovFile = None  #DONE
		self.earlyMovesCovFile = None  #DONE		
		self.earlyMobClnExpFile = None  #DONE
		self.earlyMobUnclnExpFile = None  #DONE
		self.earlyStgMobExpFile = None  #DONE
		self.earlyNoMobBirthExpFile = None  #DONE		
		self.earlySensVariablesFile = None  #DONE		
		self.earlyStageSensVariablesFile = None  #DONE
			
		self.laterCleanedAddrFile = None
		self.laterCovFile = None  #DONE
		self.laterMovesCovFile = None  #DONE		
		self.laterMobClnExpFile = None  #DONE
		self.laterMobUnclnExpFile = None  #DONE
		self.laterStgMobExpFile = None  #DONE
		self.laterNoMobBirthExpFile = None  #DONE		
		self.laterSensVariablesFile = None  #DONE
		self.laterStageSensVariablesFile = None  #DONE


	"""
	===============================================================================
	FUNCTION DESCRIPTION:
	Uses the "inputDirectory" command line argument to begin identifying 
	available data sets
	===============================================================================
	"""
	def detectAvailableDataSetFiles(self, inputDirectory):

		#first, scan directory for a sample file that should appear in 
		#the results directory.  We want to identify
		latestTimeStamp = self.identifyLatestRunTimeStamp(inputDirectory)

		#Now filter root directory for files that contain the latest time stamp and
		#end with CSV.  All of these files will belong to the latest run
		currentRunFilter = '*' + latestTimeStamp + '*.csv'					
		for path, subdirs, files in os.walk(inputDirectory):
			for filename in fnmatch.filter(files, currentRunFilter):
				self.identifyALGAEInputFile(path, filename)

	"""
	===============================================================================
	FUNCTION DESCRIPTION:
	Looks for the latest version of an example data set to determine the most
	recent version.  Whenever ALGAE runs, it generates CSV files that contains
	the time stamp for when they were created.  Over a period of several runs
	a directory can fill up with various versions of the same exposure file.
	The analysis component is designed to only be applied to the most recent
	batch of generated files and this method identifies which time stamp to look
	for.
	===============================================================================
	"""
	def identifyLatestRunTimeStamp(self, inputDirectory):
		
		#Look in the results for any CSV files that contain the phrase
		#cleaned_addr.  If there are more than one of them, they probably
		#represent different runs and it probably means the user has
		#accumulated old files.  We want to try to extract the latest
		#date from these files and use that to determine which run we
		#want to use for analysis
		
		#Find something like: res_early_cleaned_addr2016-Nov-18-22-57.csv
		#We want to extract phrases like Nov-18-22-57 and then determine which one is most
		#recent
		
		willContainDate = re.compile('._addr(.+?).csv')
		candidates = []
		for path, subdirs, files in os.walk(inputDirectory):
			for fileName in fnmatch.filter(files, '*cleaned_addr*.csv'):
				searchResult = re.search(willContainDate, fileName)
				datePhrase = searchResult.group(1)
				if searchResult:
					#see: http://strftime.org/ for options
					candidates.append(datetime.strptime(datePhrase, '%Y-%b-%d-%H-%M'))
					
		maximumTimeStamp = max(candidates)
					
		return maximumTimeStamp.strftime('%Y-%b-%d-%H-%M')


	"""
	===============================================================================
	FUNCTION DESCRIPTION:
	Takes a given file name and attempts to match it against patterns that would
	indicate it is the file name of one of the exposure data sets that will be
	used in analysis.
	===============================================================================
	"""
	def identifyALGAEInputFile(self, path, fileName):

		if 'early_cleaned_addr' in fileName:
			self.earlyCleanedAddrFile = os.path.join(path, fileName)
		elif 'early_cov' in fileName:
			self.earlyCovFile = os.path.join(path, fileName)
		elif 'early_moves_cov' in fileName:
			self.earlyMovesCovFile = os.path.join(path, fileName)
		elif 'early_mob_cln_exp' in fileName:
			self.earlyMobClnExpFile = os.path.join(path, fileName)	
		elif 'early_mob_uncln_exp' in fileName:
			self.earlyMobUnclnExpFile = os.path.join(path, fileName)		
		elif 'early_stg_mob_exp' in fileName:
			self.earlyStgMobExpFile = os.path.join(path, fileName)
		elif 'early_no_mob_birth_addr' in fileName:
			self.earlyNoMobBirthExpFile = os.path.join(path, fileName)
		elif 'early_sens_variables' in fileName:
			self.earlySensVariablesFile = os.path.join(path, fileName)
		elif 'early_stage_sens_variables' in fileName:
			self.earlyStageSensVariablesFile = os.path.join(path, fileName)
		elif 'later_cleaned_addr' in fileName:
			self.laterCleanedAddrFile = os.path.join(path, fileName)
		elif 'later_cov' in fileName:
			self.laterCovFile = os.path.join(path, fileName)
		elif 'later_moves_cov' in fileName:
			self.laterMovesCovFile = os.path.join(path, fileName)
		elif 'later_mob_cln_exp' in fileName:
			self.laterMobClnExpFile = os.path.join(path, fileName)		
		elif 'later_mob_uncln_exp' in fileName:
			self.laterMobUnclnExpFile = os.path.join(path, fileName)	
		elif 'later_stg_mob_exp' in fileName:
			self.laterStgMobExpFile = os.path.join(path, fileName)
		elif 'later_sens_variables' in fileName:
			self.laterSensVariablesFile = os.path.join(path, fileName)
		elif 'later_stage_sens_variables' in fileName:
			self.laterStageSensVariablesFile = os.path.join(path, fileName)		
		

	"""
	===============================================================================
	FUNCTION DESCRIPTION:
	A convenience function that prints out the file names of all available 
	exposure data sets that would be relevant to the ALGAE analysis.
	===============================================================================
	"""
	def printALGAEFilePaths(self):

		print "1 Early Life Analysis Files"

		print "1.1 Address Period Files"
		print "Address Periods==" + str(self.earlyCleanedAddrFile) + "=="	

		print "1.2 Covariate Files"
		print "Admin Areas at Life Stage Addresses" + str(self.earlyCovFile) + "=="	
		print "Admin Areas for Each Move" + str(self.earlyMovesCovFile) + "=="	

		print "1.3 Exposure Files"
		print "Cleaned Mobility Exposure==" + str(self.earlyMobClnExpFile) + "=="	
		print "Uncleaned Mobility Exposure==" + str(self.earlyMobUnclnExpFile) + "=="	
		print "Life Stage Mobility Exposure==" + str(self.earlyStgMobExpFile) + "=="	
		print "No Mobility Birth Address Exposure==" + str(self.earlyNoMobBirthExpFile) + "=="	

		print "1.4 Sensitivity Variable Files"
		print "Sensitivity Variables across Whole Exposure Time Frame==" + str(self.earlySensVariablesFile)
		print "Sensitivity Variables across each Life Stage==" + str(self.earlyStageSensVariablesFile)

		print "  "
	
		print "2 Later Life Analysis Files"

		print "2.1 Address Period Files"
		print "Address Periods==" + str(self.laterCleanedAddrFile) + "=="	

		print "2.2 Covariate Files"
		print "Admin Areas at Life Stage Addresses" + str(self.laterCovFile) + "=="	
		print "Admin Areas for Each Move" + str(self.laterMovesCovFile) + "=="	

		print "2.3 Exposure Files"
		print "Cleaned Mobility Exposure==" + str(self.laterMobClnExpFile) + "=="	
		print "Uncleaned Mobility Exposure==" + str(self.laterMobUnclnExpFile) + "=="	
		print "Life Stage Mobility Exposure==" + str(self.laterStgMobExpFile) + "=="	

		print "2.4 Sensitivity Variable Files"
		print "Sensitivity Variables across Whole Exposure Time Frame==" + str(self.laterSensVariablesFile)
		print "Sensitivity Variables across each Life Stage==" + str(self.laterStageSensVariablesFile)

"""
===============================================================================
CLASS DESCRIPTION:
This class contains references to the exposure data sets.  It reads 
command line arguments in order to obtain the inputDirectory, which is
then used by an instance of ALGAEDataSetFileLocations to identify the file
paths of all available exposure data sets that could be used in analysis.
This class then uses each of the available exposure data file paths to read
a data set.  These are then available for the visualisation graphs that
will try to express their data in graphs.
===============================================================================
"""
class ALGAEDataSets():

	def __init__(self):
		self.commandLineArgs = CommandLineArguments()
		self.expFilePaths = ALGAEDataSetFileLocations()
	
		self.earlyCov = None  #DONE
		self.earlyMovesCov = None  #DONE		
		self.earlyMobClnExp = None  #DONE
		self.earlyMobUnclnExp = None  #DONE
		self.earlyStgMobExp = None  #DONE
		self.earlyNoMobBirthExp = None  #DONE				
		self.earlySensVariables = None  #DONE		
		self.earlyStageSensVariables = None  #DONE			
		self.laterCleanedAddr = None
		self.laterCov = None  #DONE
		self.laterMovesCov = None  #DONE		
		self.laterMobClnExp = None  #DONE
		self.laterMobUnclnExp = None  #DONE
		self.laterStgMobExp = None  #DONE
		self.laterNoMobBirthExp = None  #DONE				
		self.laterSensVariables = None  #DONE		
		self.laterStageSensVariables = None  #DONE

	def detectDataSets(self, inputDirectory):
		self.expFilePaths.detectAvailableDataSetFiles(inputDirectory)

		if self.expFilePaths.earlyMobClnExpFile is not None:
			self.earlyMobClnExp = pd.read_csv(self.expFilePaths.earlyMobClnExpFile)
		if self.expFilePaths.earlyMobUnclnExpFile is not None:
			self.earlyMobUnclnExp = pd.read_csv(self.expFilePaths.earlyMobUnclnExpFile)
		if self.expFilePaths.earlyStgMobExpFile is not None:
			self.earlyStgMobExp = pd.read_csv(self.expFilePaths.earlyStgMobExpFile)
		if self.expFilePaths.earlyNoMobBirthExpFile is not None:
			self.earlyNoMobBirthExp = pd.read_csv(self.expFilePaths.earlyNoMobBirthExpFile)
		if self.expFilePaths.laterMobClnExpFile is not None:
			self.laterMobClnExp = pd.read_csv(self.expFilePaths.laterMobClnExpFile)	
		if self.expFilePaths.laterMobUnclnExpFile is not None:
			self.laterMobUnclnExp = pd.read_csv(self.expFilePaths.laterMobUnclnExpFile)
		if self.expFilePaths.laterStgMobExpFile is not None:
			self.laterStgMobExp = pd.read_csv(self.expFilePaths.laterStgMobExpFile)
		if self.expFilePaths.earlyCovFile is not None:
			self.earlyCov = pd.read_csv(self.expFilePaths.earlyCovFile)
		if self.expFilePaths.earlyMovesCovFile is not None:
			self.earlyMovesCov = pd.read_csv(self.expFilePaths.earlyMovesCovFile)
		if self.expFilePaths.laterCovFile is not None:
			self.laterCov = pd.read_csv(self.expFilePaths.laterCovFile)
		if self.expFilePaths.laterMovesCovFile is not None:
			self.laterMovesCov = pd.read_csv(self.expFilePaths.laterMovesCovFile)
		if self.expFilePaths.earlySensVariablesFile is not None:
			self.earlySens = pd.read_csv(self.expFilePaths.earlySensVariablesFile)
		if self.expFilePaths.earlyStageSensVariablesFile is not None:
			self.earlyStgSens = pd.read_csv(self.expFilePaths.earlyStageSensVariablesFile)
		if self.expFilePaths.laterSensVariablesFile is not None:
			self.laterSens = pd.read_csv(self.expFilePaths.laterSensVariablesFile)
		if self.expFilePaths.laterStageSensVariablesFile is not None:
			self.laterStgSens = pd.read_csv(self.expFilePaths.laterStageSensVariablesFile)

	def printAvailableDataSetFilePaths(self):
		self.expFilePaths.printALGAEFilePaths()
		
	def getNumberOfEarlyLifeStages(self):
		numberOfLifeStages = 0
		if self.earlyMobClnExp is not None:
			numberOfLifeStages = len(self.earlyMobClnExp['algae3101_life_stage'].unique())
		return numberOfLifeStages

	def getNumberOfLaterLifeStages(self):
		numberOfLifeStages = 0
		if self.laterMobClnExp is not None:
			#print self.laterMobClnExp
			numberOfLifeStages = len(self.laterMobClnExp['algae3501_life_stage'].unique())
		return numberOfLifeStages



#from GlobalSettings import GraphConfigurationSettings
#import pandas as pd
#import matplotlib.pyplot as plt


"""
=====================================================================================
MODULE: ExposureBoxPlotGraph

PURPOSE:
To generate box plot graphs that show how exposure values are distributed for a 
pollutant and life stage.  Each graph will show a box plot for each type of exposure
assignment method (eg: cleaned, uncleaned, life stage). 

DESCRIPTION:
The module is meant to hide details of how the box plots are rendered.  The main
function that should be called from other other modules is
ExposureBoxPlotGraph.generateGraphs(...)


OPEN SOURCE LICENSING
Copyright 2017 Imperial College London, developed by the Small Area
Health Statistics Unit in collaboration with the Avon Longitudinal Study of Parents
and Children (ALSPAC).
 
The code was originally authored by Kevin Garwood and reviewed by Olly Butters
and Iain Bickerstaffe.

This file is part of the ALGAE (Algorithms for Assessing Early life exposures) project.
ALGAE is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RIF is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with RIF.  If not, see <http://www.gnu.org/licenses/>.

Author: Kevin Garwood
=====================================================================================
"""

class ExposureBoxPlotGraph():

	def __init__(self):
		
		#Graph Data properties
		self.maximumValues = []		
		self.barGraphThemes = []
		self.numThemes = 0
		
		#Graph properties
		self.title = None		
		self.xLabel = 'Life Stage'
		self.yLabel = 'Study Members'
		self.configSettings = GraphConfigurationSettings()
		self.barWidth = 0.30
		self.boxFaceColour = 'Wheat'
		self.boxWhiskerColour = 'Black'
		self.boxMedianColour = 'Red'
		self.boxCapsColour = 'Black'

		#File directory variables
		self.outputDirectory = None

	def setOutputDirectory(
		self, 
		outputDirectory):
	
		self.outputDirectory = outputDirectory

	def setTitle(
		self, 
		exposureType, 
		lifeStage):

		self.title = 'Distribution of ' + exposureType + ' values for life stage ' + lifeStage
	
	def createDataSetToVisualise(
		self, 
		cln, 
		uncln, 
		stg, 
		birth,
		lifeStage):

		#Merge the data sets together
		results = pd.merge(cln, uncln, how='left', on=['person_id', 'life_stage'])
		results = pd.merge(results, stg, how='left', on=['person_id', 'life_stage'])
		if birth is not None:
			results = pd.merge(results, birth, how='left', on=['person_id', 'life_stage'])

		return results

	#Now visualise the data
	def visualiseDataSet(
		self,
		graphData,
		ithLifeStage,
		lifeStage,
		pollutantType,
		birth):

		lifeStageSubSet = graphData[graphData.life_stage == lifeStage]

		fig, axis1 = plt.subplots(1, figsize = (10,5))

		if birth is None:
			lifeStageSubSet = lifeStageSubSet[['mob_exp_cln', 'mob_exp_uncln', 'stg_mob_exp']]
		else:
			lifeStageSubSet = lifeStageSubSet[['mob_exp_cln', 'mob_exp_uncln', 'stg_mob_exp', 'no_mob_birth_addr_exp']]
		
		maximumExposureValue = self.determineMaximumExposureValue(lifeStageSubSet, birth)
		lifeStageSubSet.columns = self.barGraphThemes

		colourScheme = dict(
			boxes = self.boxFaceColour,
			whiskers=self.boxWhiskerColour,
			medians=self.boxMedianColour,
			caps="Gray")

		exposureBoxPlotAxes = lifeStageSubSet.plot.box(
			lifeStageSubSet, 
			color = colourScheme, 
			patch_artist=True, 
			title=self.title)
		exposureBoxPlotAxes.grid(True)

		plt.minorticks_on()	
		barChartXValues = [i for i in range(self.numThemes)]
		tickPositions = [barChartXValues[i] + (self.barWidth / 2) for i in range(self.numThemes)] 			
		plt.xlim([min(tickPositions) - self.barWidth, max(tickPositions)+ 2.00])
		plt.ylim([0, maximumExposureValue * 1.20])		
		plt.xlabel('Analysis Type')
		plt.ylabel(pollutantType)

		basePath = self.outputDirectory
		completeFilePath = self.configSettings.createGraphResultFilePath(
			basePath,
			"exp_box", 
			lifeStage)	
		plt.savefig(completeFilePath)
		plt.close("all")

	def generateGraphs(
		self, 
		cln, 
		uncln, 
		stg, 
		birth,
		exposureType,
		pollutantType):
		
		if birth is None:
			self.barGraphThemes = ['Cleaned', 'Uncleaned', 'Life Stage']
		else:
			self.barGraphThemes = ['Cleaned', 'Uncleaned', 'Life Stage', 'Birth Address']
		self.numThemes = len(self.barGraphThemes)
			
		#iterate through life stages and produce a graph
		lifeStages = cln['life_stage'].unique()
		for ithLifeStage, lifeStage in enumerate(lifeStages):
			self.setTitle(
				exposureType, 
				lifeStage)
			graphData = self.createDataSetToVisualise(
				cln, 
				uncln, 
				stg, 
				birth,
				lifeStage)
			self.visualiseDataSet(
				graphData, 
				ithLifeStage, 
				lifeStage,
				pollutantType,
				birth)

	def determineMaximumExposureValue(
		self, 
		lifeStageSubSet,
		birth):
		
		maxCln = max(lifeStageSubSet['mob_exp_cln'])
		maxUncln = max(lifeStageSubSet['mob_exp_uncln'])
		maxStg = max(lifeStageSubSet['stg_mob_exp'])
		
		if birth is not None:
			maxBirth = max(lifeStageSubSet['no_mob_birth_addr_exp'])
			self.maximumValues = [maxCln, maxUncln, maxStg, maxBirth]
		else:
			self.maximumValues = [maxCln, maxUncln, maxStg]
		return max(self.maximumValues)




"""
=====================================================================================
MODULE: ExposureDataQualityTypeGraph

PURPOSE:
Contains all the code used to generate stacked bar graphs that show the number of 
days in a given life stage that had one of the following quality indicators:
(1) Poor matching address exposure day
(2) Missing exposure day
(3) Out-of-bounds exposure day
(4) Invalid address day
(5) Good address match day

DESCRIPTION:
The module is meant to hide details of how the box plots are rendered.  The main
function that should be called from other other modules is
ExposureDataQualityTypeGraph.generateGraphs(...)

OPEN SOURCE LICENSING
Copyright 2017 Imperial College London, developed by the Small Area
Health Statistics Unit in collaboration with the Avon Longitudinal Study of Parents
and Children (ALSPAC).
 
The code was originally authored by Kevin Garwood and reviewed by Olly Butters
and Iain Bickerstaffe.

This file is part of the ALGAE (Algorithms for Assessing Early life exposures) project.
ALGAE is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RIF is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with RIF.  If not, see <http://www.gnu.org/licenses/>.

Author: Kevin Garwood
=====================================================================================
"""

#from GlobalSettings import GraphConfigurationSettings
#import pandas as pd
#import matplotlib.pyplot as plt


class ExposureDataQualityTypeGraph():

	def __init__(self):

		#Graph properties
		self.title = None		
		self.xLabel = 'Life Stage'
		self.yLabel = 'Total Days'			
		self.configSettings = GraphConfigurationSettings()

		self.goodMatchDayLabel = 'Good address match days'
		self.invalidAddressDayLabel = 'Invalid address days'
		self.outOfBoundsDayLabel = 'Out of bounds address days'
		self.missingExposureDaysLabel = 'Missing Exposure days'
		self.poorMatchDayLabel = 'Poor address match days'

		self.goodMatchDayColour = '#F4561D'	
		self.invalidAddressDayColour = '#F1911E'
		self.outOfBoundsDayColour = 'red'
		self.missingExposureDaysColour = 'green'
		self.poorMatchDayColour = 'blue'
		self.barLength = None
		
		#File directory variables
		self.outputDirectory = None	

	def setOutputDirectory(
		self,
		outputDirectory):
		self.outputDirectory = outputDirectory
	
	def setTitle(
		self, 
		exposureAssignmentMethod):

		self.title = 'Data Quality Types for Each Life Stage using ' + exposureAssignmentMethod
	
	
	def createDataSetToVisualise(
		self, 
		originalExpData):

		#Part I: Create a unique list of ordered life stages, preserving the order in which the original 
		#data set recorded them.  Then make an explicit column for the index called 'ith_life_stage',
		#which will be used later on when we try to order the bars that are placed on the graph.
		uniqueLifeStageValues = originalExpData['life_stage'].unique()
		orderedLifeStages = pd.DataFrame(data=uniqueLifeStageValues)
		orderedLifeStages.reset_index(inplace=True)
		orderedLifeStages.columns = ['ith_life_stage', 'life_stage']

		#Part II: Aggregated all the data quality counters.  We will get large values which represent 
		#the total number of days for each data quality type, measured across all of the cohort
		aggregatedExpData = originalExpData.groupby('life_stage').agg({ \
			'life_stage_duration': lambda x: x.sum(),
			'good_match_days': lambda x: x.sum(),
			'invalid_addr_days': lambda x: x.sum(),
			'oob_days': lambda x: x.sum(),
			'missing_exp_days': lambda x: x.sum(),
			'poor_addr_days': lambda x: x.sum()})

		#Ensure that the groupby field is actually saved as an explicit field in the results
		aggregatedExpData['life_stage'] = aggregatedExpData.index

		#Calculate percentages
		aggregatedExpData['percent_good_match'] \
			= (aggregatedExpData['good_match_days'] / aggregatedExpData['life_stage_duration']) * 100
		aggregatedExpData['percent_invalid'] \
			= (aggregatedExpData['invalid_addr_days'] / aggregatedExpData['life_stage_duration']) * 100
		aggregatedExpData['percent_oob'] \
			= (aggregatedExpData['oob_days'] / aggregatedExpData['life_stage_duration']) * 100	
		aggregatedExpData['percent_missing_exp'] \
			= (aggregatedExpData['missing_exp_days'] / aggregatedExpData['life_stage_duration']) * 100	
		aggregatedExpData['percent_poor_addr'] \
			= (aggregatedExpData['poor_addr_days'] / aggregatedExpData['life_stage_duration']) * 100
	
	
		#Part III: Merge the data sets together.  We're trying to do:
		# Table: orderedLifeStages			Table: aggregatedExpData 
		# ith_life_stage    life_stage		life_stage, life_stage_duration, good, invalid, oob, missing, poor, percentages etc
		# 1					T1			X
		# 2					T2
		# 3					T3
		# 4					EL0_to_6
		# 5					EL7_to_12
		#	
		results = pd.merge(orderedLifeStages, aggregatedExpData, how='left', on=['life_stage'])
		return results

	def visualiseDataSet(
		self, 
		exposureDQDataSet,
		fileName):

		#Initialise graph
		# Create the general blog and the "subplots" i.e. the bars
		fig, axis1 = plt.subplots(1, figsize = (10,5))

		self.configSettings.configureAxis(
			axis1, 
			self.xLabel, 
			self.yLabel)
		
		# positions of the left bar-boundaries
		self.barLengths = [i + 1 for i in range(len(exposureDQDataSet['good_match_days']))] 

		# Create bar for Poor Matching Address Days
		axis1.bar(
			self.barLengths, 
			exposureDQDataSet['missing_exp_days'], 
			width = self.configSettings.barWidth,
			label = self.poorMatchDayLabel, 
			alpha = 0.5, 
			bottom = exposureDQDataSet['good_match_days'] + exposureDQDataSet['invalid_addr_days'] + exposureDQDataSet['oob_days'] + exposureDQDataSet['missing_exp_days'],		
			color = self.poorMatchDayColour)

		# Create bar for Missing Exposure Days
		axis1.bar(
			self.barLengths, 
			exposureDQDataSet['missing_exp_days'], 
			width = self.configSettings.barWidth,
			label = self.missingExposureDaysLabel, 
			alpha = 0.5, 
			bottom = exposureDQDataSet['good_match_days'] + exposureDQDataSet['invalid_addr_days'] + exposureDQDataSet['oob_days'],		
			color = self.missingExposureDaysColour)

		# Create bar for Out of Bounds Days
		axis1.bar(
			self.barLengths, 
			exposureDQDataSet['oob_days'], 
			width = self.configSettings.barWidth,
			label = self.outOfBoundsDayLabel, 
			alpha = 0.5, 
			bottom = exposureDQDataSet['good_match_days'] + exposureDQDataSet['invalid_addr_days'],		
			color = self.outOfBoundsDayColour)

		# Create bar for Invalid Address Days
		axis1.bar(
			self.barLengths, 
			exposureDQDataSet['invalid_addr_days'], 
			width = self.configSettings.barWidth,
			label = self.invalidAddressDayLabel, 
			alpha = 0.5, 
			bottom = exposureDQDataSet['good_match_days'],		
			color = self.invalidAddressDayColour)

		# Create bar for Good Address Match Days
		axis1.bar(
			self.barLengths, 
			exposureDQDataSet['good_match_days'], 
			width = self.configSettings.barWidth,
			label = self.goodMatchDayLabel, 
			alpha = 0.5, 
			color = self.goodMatchDayColour)

		legend = self.configSettings.createLegend(plt)

		# Set a buffer around the edge
		# positions of the x-axis ticks (center of the bars as bar labels)
		tickPositions = [i + (self.configSettings.barWidth / 2) for i in self.barLengths] 
		plt.xlim([min(tickPositions)-self.configSettings.barWidth, max(tickPositions)+ (self.configSettings.barWidth * self.configSettings.legendSpacingFactor)])		

		# set the x ticks with names
		plt.xticks(
			tickPositions, 
			exposureDQDataSet['life_stage'])

		plt.title(self.title)
		plt.minorticks_on()
		
		basePath = self.outputDirectory
		completeFilePath = self.configSettings.createGraphResultFilePath(
			basePath, 
			"dq_types_bar_",
			fileName)	
		plt.savefig(completeFilePath)
		plt.close(fig)

	def generateGraphs(
		self, 
		originalExposureData,
		fileName):

		exp = self.createDataSetToVisualise(originalExposureData)
		numberOfLifeStages = exp['life_stage'].count()
		self.configSettings.setNumberOfLifeStages(numberOfLifeStages);
		
		self.visualiseDataSet(
			exp,
			fileName)


#import os
#import argparse
#from datetime import datetime
#import time
#import matplotlib

#===============================================================================
# Module: CommandLineArguments
# ============================
# Purpose:
# --------
# This class is responsible for reading command line arguments that users
# specify when they run the main visualisation program.  There are four
# main command line arguments that are used throughout the analysis code base:
# (1) inputDirectory: this is the directory where where ALGAEAnalysis will
#     try to find all of the exposure data sets.  It assumes that it will find
#     it in [inputDirectory]/early_life/results/exposure_data and
#     [inputDirectory]/later_life/results/exposure_data
# (2) outputDirectory: the directory where all the generated graphs will appear.
# (3) imageWidth: the width of each chart
# (4) imageHeight: the height of each chart 
#===============================================================================
class CommandLineArguments():		

	def __init__(self):
		#Establish defaults in case command line options are not specified.
		self.inputDirectory = 'C:\\algae_protocol\\test_environment\\exposure_data'
		self.outputDirectory = 'C:\\algae_protocol\\analysis\\analysis_results' 
		self.imageWidth = 300
		self.imageHeight = 500
		
	def parseCommandLine(self):
		parser = argparse.ArgumentParser(description="Produces graphs for ALGAE analyses")
		parser.add_argument('-inputDirectory', help='location of results directory', dest='inputDirectory', required = False)
		parser.add_argument('-outputDirectory', help='destination directory for graphs', dest='outputDirectory', required = False)
		parser.add_argument('-imageWidth', help='width of all generated images', dest='imageWidth', required = False)
		parser.add_argument('-imageHeight', help='height of all generated images', dest='imageHeight', required = False)

		arguments = vars(parser.parse_args())

		if arguments['inputDirectory'] is not None:
			self.inputDirectory = arguments['inputDirectory']
		if arguments['outputDirectory'] is not None:
			self.outputDirectory = arguments['outputDirectory']
		if arguments['imageWidth'] is not None:
			self.imageWidth = arguments['imageWidth']
		if arguments['imageHeight'] is not None:
			self.imageHeight = arguments['imageHeight']



class GraphConfigurationSettings():

	def __init__(self):
		self.textFontSize = None
		self.legendSpacingFactor = None
		self.numberOfGraphs = None
		self.barWidth = None

	def setNumberOfLifeStages(self, numberOfLifeStages):
		self.numberOfGraphs = numberOfLifeStages
		if (numberOfLifeStages < 7):
			self.legendSpacingFactor = 8.0;
			#pylab.rcParams.update(self.largeFontOptions)
			#plt.legend(loc = 'best', prop={'size':self.largeFontSize})
			self.textFontSize = '12'
		else:
			self.legendSpacingFactor = 20.0;
			self.textFontSize = '6'

		matplotlib.rc('xtick', labelsize=self.textFontSize) 
		matplotlib.rc('ytick', labelsize=self.textFontSize)	
		self.barWidth = 1.0 / numberOfLifeStages + 0.20

	def configureAxis(self, axis, xLabel, yLabel):
		axis.tick_params(axis = 'y', which = 'minor', bottom = 'on')
		axis.set_ylabel(yLabel)
		axis.set_xlabel(xLabel)
		axis.grid(True)
		axis.tick_params(axis = 'x', which = 'minor', bottom = 'off')
		
	def createLegend(self, plt):
		legend = plt.legend(loc = 'best', borderpad = 2, prop={'size':self.textFontSize})
		return legend


	def createGraphResultFilePath(
		self, 
		basePath,
		prefix,
		fileName):	

		timeStamp = time.time()
		timeStampPhrase = datetime.fromtimestamp(timeStamp).strftime('%Y-%b-%d-%H-%M')		
		timeStampedFileName = prefix + "_" + fileName + "_" + timeStampPhrase + ".png"
		
		completeFilePath = os.path.join(basePath, timeStampedFileName)
		return completeFilePath


"""
=====================================================================================
MODULE: ExposureDataQualityTypeGraph

PURPOSE:
Contains all the code used to generate stacked bar graphs that show the number of 
study members who showed a null or non-null exposure result for a given life stage.
Each image will feature a stacked bar graph for each method used to assign exposures
(eg: cleaned, uncleaned, life stage ...)

DESCRIPTION:
The module is meant to hide details of how the box plots are rendered.  The main
function that should be called from other other modules is
LifeStageNullExposuresGraph.generateGraphs(...)

OPEN SOURCE LICENSING
Copyright 2017 Imperial College London, developed by the Small Area
Health Statistics Unit in collaboration with the Avon Longitudinal Study of Parents
and Children (ALSPAC).
 
The code was originally authored by Kevin Garwood and reviewed by Olly Butters
and Iain Bickerstaffe.

This file is part of the ALGAE (Algorithms for Assessing Early life exposures) project.
ALGAE is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RIF is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with RIF.  If not, see <http://www.gnu.org/licenses/>.

Author: Kevin Garwood
=====================================================================================
"""

#from GlobalSettings import GraphConfigurationSettings
#import pandas as pd
#import matplotlib.pyplot as plt
#from matplotlib.ticker import MaxNLocator

class LifeStageNullExposuresGraph():

	def __init__(self):

		#Graph Data properties
		self.graphData = None

		#Graph properties
		self.title = None
		self.xLabel = 'Analysis Type'
		self.yLabel = 'Study Members'					
		self.configSettings = GraphConfigurationSettings()
		
		self.barWidth = 0.30
		self.barLength = None
		self.nullValueExposureColour = '#F4561D'	
		self.nonNullValueExposureColour = '#F1911E'				
		
		#Legend properties
		self.nullValueExposureLabel = 'Study members\nwith null\nexposure values'
		self.nonNullValueExposureLabel = 'Study members\nwith non-null\nexposure values'
		self.legendBackgroundColour = '#FFF380'
		
		#File directory variables
		self.outputDirectory = None		
		

	def setOutputDirectory(
		self,
		outputDirectory):
		self.outputDirectory = outputDirectory
	
	def setTitle(
		self, 
		lifeStage):

		self.title = 'Null Values for Life Stage Exposure Results in ' + lifeStage	
	
	
	def createDataSetToVisualise(
		self, 
		totalStudyMembers, 
		mobClnSlice, 
		mobUnclnSlice, 
		stgMobSlice, 
		noMobBirthSlice):
	
		#Part I: Count the number of nulls for the exposure column
		#Then count the total number of non-nulls based on subtracting nulls 
		#from the total number of study members
		cln = mobClnSlice.groupby('life_stage').agg({ \
			'mob_exp_cln': lambda x: x.isnull().sum()})
		cln.rename(columns={'mob_exp_cln' : 'mob_exp_cln_nulls'}, inplace = True)
		cln['mob_exp_cln_non_nulls'] = totalStudyMembers - cln['mob_exp_cln_nulls']
		cln.insert(0, 'life_stage', cln.index)

		uncln = mobUnclnSlice.groupby('life_stage').agg({ \
			'mob_exp_uncln': lambda x: x.isnull().sum()})
		uncln.rename(columns={'mob_exp_uncln' : 'mob_exp_uncln_nulls'}, inplace = True)
		uncln['mob_exp_uncln_non_nulls'] = totalStudyMembers - uncln['mob_exp_uncln_nulls']
		uncln.insert(0, 'life_stage', uncln.index)

		stg = stgMobSlice.groupby('life_stage').agg({ \
			'stg_mob_exp': lambda x: x.isnull().sum()})
		stg.rename(columns={'stg_mob_exp' : 'stg_mob_exp_nulls'}, inplace = True)
		stg['stg_mob_exp_non_nulls'] = totalStudyMembers - stg['stg_mob_exp_nulls']
		stg.insert(0, 'life_stage', stg.index)

		if noMobBirthSlice is not None:
			self.barGraphThemes = ['Cleaned', 'Uncleaned', 'Life Stage', 'Birth Address']
			self.numThemes = len(self.barGraphThemes)

			birth = noMobBirthSlice.groupby('life_stage').agg({ \
				'no_mob_birth_addr_exp': lambda x: x.isnull().sum()})
			birth.rename(columns={'no_mob_birth_addr_exp' : 'no_mob_birth_addr_exp_nulls'}, inplace = True)
			birth['no_mob_birth_addr_exp_non_nulls'] = totalStudyMembers - birth['no_mob_birth_addr_exp_nulls']
			birth.insert(0, 'life_stage', birth.index)

			#Merge the data sets together
			results = pd.merge(cln, uncln, how='left', on=['life_stage'])
			results = pd.merge(results, stg, how='left', on=['life_stage'])
			results = pd.merge(results, birth, how='left', on=['life_stage'])

		else:
			self.barGraphThemes = ['Cleaned', 'Uncleaned', 'Life Stage']
			self.numThemes = len(self.barGraphThemes)
			#Merge the data sets together
			results = pd.merge(cln, uncln, how='left', on=['life_stage'])
			results = pd.merge(results, stg, how='left', on=['life_stage'])

		self.graphData = results		

	def visualiseDataSet(
		self, 
		analysisType,
		totalStudyMembers,
		ithLifeStage, 
		lifeStage):

		#Retrieve a data frame that contains only the graph data which relates to
		#the given life stage.  From that data frame, isolate the first row of 
		#results
		lifeStageSubSet = self.graphData[self.graphData.life_stage == lifeStage].iloc[0]

		#Initialise graph
		# Create the general blog and the "subplots" i.e. the bars
		fig, axis1 = plt.subplots(1, figsize = (10,5))

		self.configSettings.configureAxis(
			axis1, 
			self.xLabel, 
			self.yLabel)

		#define the x and y coordinates that will define the locations of bars
		barChartXValues = [i for i in range(self.numThemes)]
		
		nullValues = []
		nonNullValues = []
		if analysisType is 'early':
			nullValues = [lifeStageSubSet.mob_exp_cln_nulls, lifeStageSubSet.mob_exp_uncln_nulls, lifeStageSubSet.stg_mob_exp_nulls, lifeStageSubSet.no_mob_birth_addr_exp_nulls]
			nonNullValues = [lifeStageSubSet.mob_exp_cln_non_nulls, lifeStageSubSet.mob_exp_uncln_non_nulls, lifeStageSubSet.stg_mob_exp_non_nulls, lifeStageSubSet.no_mob_birth_addr_exp_non_nulls]
		else:
			nullValues = [lifeStageSubSet.mob_exp_cln_nulls, lifeStageSubSet.mob_exp_uncln_nulls, lifeStageSubSet.stg_mob_exp_nulls]
			nonNullValues = [lifeStageSubSet.mob_exp_cln_non_nulls, lifeStageSubSet.mob_exp_uncln_non_nulls, lifeStageSubSet.stg_mob_exp_non_nulls]

		# positions of the x-axis ticks (center of the bars as bar labels)
		tickPositions = [barChartXValues[i] + (self.barWidth / 2) for i in range(self.numThemes)] 

		#Create the bar graphs.
		#Here we will define four vertical stacked bars that correspond
		#to the Cleaned, Uncleaned, Life Stage Mobility and Birth Address
		#exposure assessment methods.  
		#
		#Each graph is made from three arrays:
		# * barChartXValues, which are just places to put the names of 
		#   exposure assessment methods
		# * nonNullValues, contains counts of the number of study members who
		#   had non-null values for a given exposure for a given life stage
		#   These bars will be rendered on the bottom of each stacked bar.
		# * nullValues, contains the counts of the number of study members who
		#   had null values for a given exposure for a given life stage
		#   These bars will be placed on top of corresponding nonNullValues bars.

		axis1.bar(
			barChartXValues, 
			nullValues,
			width = self.barWidth,
			label = self.nullValueExposureLabel, 
			alpha = 0.5, 
			bottom = nonNullValues,		
			color = self.nullValueExposureColour)

		# Create bar for Non-Null Values
		axis1.bar(
			barChartXValues, 
			nonNullValues,
			width = self.barWidth,
			label = self.nonNullValueExposureLabel, 
			alpha = 0.5, 
			color = self.nonNullValueExposureColour)	
				
		# Set the graph title and the axis labels
		self.setTitle(lifeStage)
		plt.title(self.title)
	
		#Define grid tick properties and turn on the dashed lines
		#to help guage the values of each bar	
		plt.minorticks_on()

		# Set the label and legends
		#axis1.set_ylabel(self.yLabel, fontsize = 18)
		#axis1.set_xlabel(self.xLabel, fontsize = 18)

		legend = self.configSettings.createLegend(plt)

		#legend = plt.legend(loc = 'center right', shadow = True)
		legend.get_frame().set_facecolor(self.legendBackgroundColour)
		
		# Defining space around the axes
		plt.xlim([min(tickPositions) - self.barWidth, max(tickPositions)+ 2.00])
		plt.ylim([0, totalStudyMembers + 1])
		# Write the words for each bar label at the location described by tickPositions
		plt.xticks(
			tickPositions, 
			self.barGraphThemes)
		
		axis1.yaxis.set_major_locator(MaxNLocator(integer=True))	

		fig.tight_layout()

		self.writeGraphToFile(fig, lifeStage)


	def writeGraphToFile(
		self, 
		fig,
		lifeStage):

		completeFilePath = self.configSettings.createGraphResultFilePath(
			self.outputDirectory,
			"null_bar", 
			lifeStage)	
		plt.savefig(completeFilePath)
		plt.close(fig)
	

	def getOrderedLifeStageNames(self, allLifeStageNameValues):
		uniqueLifeStages = []
		for ithLifeStage, lifeStage in enumerate(allLifeStageNameValues):
			if (lifeStage in uniqueLifeStages):
				#we have identified the first duplicate.  Assume we'll encounter
				#the same life stages for another record
				return uniqueLifeStages
			else:
				uniqueLifeStages.append(lifeStage)
		return uniqueLifeStages		

	def generateGraphs(
		self, 
		totalStudyMembers, 
		mobClnSlice, 
		mobUnclnSlice, 
		stgMobSlice, 
		noMobBirthSlice):

		self.createDataSetToVisualise(
			totalStudyMembers, 
			mobClnSlice, 
			mobUnclnSlice, 
			stgMobSlice, 
			noMobBirthSlice)

		#We want to obtain a list of unique life stages.
		#But the problem with earlyMobClnSlice['life_stage'].unique()
		#is that the ordering of the life stages may not be preserved.

		allLifeStageNameValues = mobClnSlice['life_stage']	

		orderedLifeStages = self.getOrderedLifeStageNames(mobClnSlice['life_stage'])			
		
		analysisType = ''
		if noMobBirthSlice is None:
			analysisType = 'later'
		else:
			analysisType = 'early'
		
		for ithLifeStage in range(len(orderedLifeStages)):
			self.visualiseDataSet(
				analysisType,
				totalStudyMembers, 
				ithLifeStage, 
				orderedLifeStages[ithLifeStage])

# This is where the program starts
algaeAnalysis = ALGAEAnalysis()
algaeAnalysis.runAnalysis()