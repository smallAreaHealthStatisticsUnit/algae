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

from ALGAEDataSets import ALGAEDataSets
from GlobalSettings import CommandLineArguments
from ExposureDataQualityTypeGraph import ExposureDataQualityTypeGraph
from LifeStageNullExposuresGraph import LifeStageNullExposuresGraph
from ExposureBoxPlotGraph import ExposureBoxPlotGraph

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

# This is where the program starts
algaeAnalysis = ALGAEAnalysis()
algaeAnalysis.runAnalysis()