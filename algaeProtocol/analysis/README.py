
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

