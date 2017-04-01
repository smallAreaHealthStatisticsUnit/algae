
from GlobalSettings import GraphConfigurationSettings
import pandas as pd
import matplotlib.pyplot as plt


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


