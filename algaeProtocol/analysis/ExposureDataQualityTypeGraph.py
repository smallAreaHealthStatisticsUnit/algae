
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

from GlobalSettings import GraphConfigurationSettings
import pandas as pd
import matplotlib.pyplot as plt
from GlobalSettings import GraphConfigurationSettings

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