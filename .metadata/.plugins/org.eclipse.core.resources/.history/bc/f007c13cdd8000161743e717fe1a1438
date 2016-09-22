/*
 *
 * ------------------------------------------------------------------------------------------------
 * Copyright 2016 Imperial College London, developed by the Small Area
 * Health Statistics Unit in collaboration with the Avon Longitudinal Study of Parents
 * and Children (ALSPAC).
 * 
 * The code was originally authored by Kevin Garwood and reviewed by Olly Butters
 * and Iain Bickerstaffe.
 *
 * This file is part of the ALGAE (Algorithms for Assessing Early life exposures) project.
 * ALGAE is free software: you can redistribute it and/or modify it under the terms of the GNU 
 * Lesser General Public License as published by the Free Software Foundation, either version 3 
 * of the License, or (at your option) any later version.
 *
 * RIF is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even 
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU 
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with RIF.  
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Kevin Garwood
 * ================================================================================================= 
 */

CREATE OR REPLACE FUNCTION test_early_results(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;
	number_of_mismatches INT;

BEGIN

	test_suite_directory := test_directory || '\' || 'exposures' || '\' || 'early_life' || '\' || 'input_data';
	output_directory := test_directory || '\' || 'exposures' || '\' || 'early_life' || '\' || 'results';

	PERFORM load_original_test_data(test_suite_directory);
	PERFORM run_early_analysis(output_directory);
		
END;

$$   LANGUAGE plpgsql;

--SELECT "test_early_results"('C:\algae_protocol\test_environment');


