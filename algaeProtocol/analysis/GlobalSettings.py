import os
import argparse
from datetime import datetime
import time
import matplotlib

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
		