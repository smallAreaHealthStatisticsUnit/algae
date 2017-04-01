
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

from GlobalSettings import GraphConfigurationSettings
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator

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
