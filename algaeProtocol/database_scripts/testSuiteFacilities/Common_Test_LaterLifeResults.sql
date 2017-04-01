/*
 *
 * ------------------------------------------------------------------------------------------------
 * Copyright 2017 Imperial College London, developed by the Small Area
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
 * This code is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with the code.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Kevin Garwood
 * ================================================================================================= 
 */

CREATE OR REPLACE FUNCTION test_later_results(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	test_suite_directory TEXT;
	output_directory TEXT;

BEGIN

	test_suite_directory 
		:= test_directory || '\' || 'exposures' || '\' || 'later_life' || '\' || 'input_data';
	output_directory :=
		test_directory || '\' || 'exposures' || '\' || 'later_life' || '\' || 'results';


	PERFORM load_original_test_data(test_suite_directory);
	PERFORM run_later_analysis(output_directory);
	
	--PERFORM later_life_create_result_tables_and_backup();	
	--PERFORM later_life_create_reports(output_directory);
END;
$$   LANGUAGE plpgsql;

--SELECT "test_later_results"('C:\algae_protocol\test_environment');

