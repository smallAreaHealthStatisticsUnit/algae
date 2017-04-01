/**
 * ================================================================================================= 
 * MODULE TEST_UTILITIES: Test Suites
 * =================================================================================================
 * Description
 * -----------
 * Contains code used to help run test cases in the ALGAE automated test suite (See: "Test Suites"
 * module.
 *
 * Main Functions
 * -------------
 * This module is a small mini-library of functions that are used to support automated test cases.
 * load_original_test_data is the most important method because it helps load test data into
 * original data tables that form the starting point of ALGAE's main processing operations.  The
 * other two functions "generate_early_exp_test_data" and "generate_later_exp_test_data" are used
 * to generate the file "original_exp_data.csv" using daily exposure records for the early analysis
 * and annual exposure records for the late analysis.
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
CREATE OR REPLACE FUNCTION load_original_test_data(
	test_case_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	original_geocode_data_file TEXT;
	original_study_member_data_file TEXT;
	original_addr_history_data_file TEXT;
	original_exp_data_file TEXT;
	
BEGIN

	original_geocode_data_file := test_case_directory || '\' || 'original_geocode_data.csv';
	original_study_member_data_file := test_case_directory || '\' || 'original_study_member_data.csv';
	original_addr_history_data_file := test_case_directory || '\' || 'original_address_history_data.csv';
	original_exp_data_file := test_case_directory || '\' || 'original_exposure_data.csv';
	
	PERFORM setup_scripts(null, null, null, null);


	/*
	* initialise the staging tables.  These are used by both the code used to load imperial data
	* and the code used to provide test data.  The staging tables serve as the main starting point
	* for the generic code that is responsible for most of the results
	*/
	DROP TABLE IF EXISTS original_geocode_data;
	CREATE TABLE original_geocode_data (
		geocode TEXT, --(x,y) coordinates. Typically "x-y" will be the actual geocode value
		version TEXT, -- version of software eg: institution or software, version number
		comments TEXT, -- comment field, often used in testing
		ed91 TEXT, -- covariate
		oa2001 TEXT, -- UK output areas for 2001 
		coa2011 TEXT, -- UK super output areas for 2011
		has_valid_geocode TEXT -- 'Y' or 'N' indicates if geocode can be used or not
	);

	EXECUTE format ('
	COPY original_geocode_data (	
		geocode,
		version,
		comments,
		ed91,
		oa2001,
		coa2011,
		has_valid_geocode) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', original_geocode_data_file);


	DROP TABLE IF EXISTS original_study_member_data;
	CREATE TABLE original_study_member_data (
		person_id TEXT, -- unique anonymised identifier for case study member
		comments TEXT, -- comment field, often used in testing
		birth_date DATE, -- birth date of case study member
		estimated_gestation_age INT, -- best guess at gestational age at birth, sometimes scans, sometimes LMP
		absent_during_exp_period TEXT, -- absent from address history locations during exposure period
		at_1st_addr_conception TEXT -- at first address on day of conception
	);


	EXECUTE format ('
	COPY original_study_member_data (	
		person_id,
		comments,
		birth_date,
		estimated_gestation_age,
		absent_during_exp_period,
		at_1st_addr_conception) 
	FROM 
			%L
	(FORMAT CSV, HEADER)', original_study_member_data_file);

		
	DROP TABLE IF EXISTS original_addr_history_data;
	CREATE TABLE original_addr_history_data (
		person_id TEXT, -- unique anonymised identifier for case study member
		comments TEXT, -- comment field, often used in testing
		geocode TEXT, --(x,y) coordinates. Typically "x-y" will be the actual geocode value
		start_date DATE, -- date address period begins
		end_date DATE -- date address period ends
	);	
	

	EXECUTE format ('
	COPY original_addr_history_data (	
		person_id,
		comments,
		geocode,
		start_date,
		end_date)
	FROM 
			%L
	(FORMAT CSV, HEADER)', original_addr_history_data_file);
	DROP TABLE IF EXISTS original_exp_data;
	CREATE TABLE original_exp_data (
		geocode TEXT, --(x, y) coordinates. Typically "x-y" will be the actual geocode value
		comments TEXT, -- comment field, often used in testing
		date_of_year DATE, --date of year associated with pollution values
		pm10_rd DOUBLE PRECISION,
		nox_rd DOUBLE PRECISION,
		pm10_gr DOUBLE PRECISION,
		name DOUBLE PRECISION,
		pm10_tot DOUBLE PRECISION
	);	
	
	EXECUTE format ('
	COPY original_exp_data (	
		geocode,
		comments,
		date_of_year,
		pm10_rd,
		nox_rd,
		pm10_gr,
		name,
		pm10_tot) 
	FROM 
			%L
	(FORMAT CSV, HEADER)', original_exp_data_file);

/*
	-- Create Staging Tables
	DROP TABLE IF EXISTS staging_geocode_data;
	CREATE TABLE staging_geocode_data AS
	SELECT
		geocode,
		ed91,
		oa2001,
		coa2011,
		has_valid_geocode
	FROM
		original_geocode_data;

	DROP TABLE IF EXISTS staging_study_member_data;
	CREATE TABLE staging_study_member_data AS
	SELECT
		person_id,
		birth_date,
		estimated_gestation_age,
		absent_during_exp_period,
		at_1st_addr_conception
	FROM
		original_study_member_data;

	DROP TABLE IF EXISTS staging_addr_history_data;
	CREATE TABLE staging_addr_history_data AS
	SELECT
		person_id,
		standardise_geocode_value(geocode, TRUE) AS geocode,
		start_date,
		end_date
	FROM
		original_addr_history_data;
	
	DROP TABLE IF EXISTS staging_exp_data;
	CREATE TABLE staging_exp_data AS
	SELECT
		geocode,
		date_of_year,
		pm10_rd,
		nox_rd,
		pm10_gr,
		name,
		pm10_tot
	FROM
		original_exp_data;
*/		
END;
$$   LANGUAGE plpgsql;
--SELECT "load_original_test_data"('C:\algae_protocol\test_environment\addresses');


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION generate_early_exp_test_data
 * ------------------------------------
 * Description
 * -----------
 * Generates fake exposure data to support tests of the early life analysis.  The function 
 * creates a bunch of daily exposure records.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION generate_early_exp_test_data(test_case_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	test_suite_directory TEXT;

	original_exp_data_file TEXT;

BEGIN


	test_suite_directory := test_case_directory || '\' || 'exposure_data';


	original_exp_data_file := 
		test_case_directory || 
		'\exposure_data\early_life\input_data\' 
		|| 
		'original_exposure_data.csv';

	/*
	* Because we know what the test data looks like, we can assume that
	* the minimum conception date is 1992-01-14 and the maximum
	* first year end date is 1993-11-18
    */

	/*
	* Let a1 have the pollution profile of a straight line function that decreases
	* pollution values over time.	
	* 
	*/
	
	DROP TABLE IF EXISTS test_exp_data;
	CREATE TABLE test_exp_data AS
	WITH normal_date_series AS
		(SELECT  
			extract(doy FROM dd) AS day,
			extract(year FROM dd) AS year,
			dd AS date_of_year
		 FROM  
			generate_series('14-01-1992'::timestamp, '18-11-1993'::timestamp, '1 day'::interval) dd),			
	incomplete_date_series AS -- intentionally missing exposures to test missing_exposure_days
		(SELECT  
			extract(doy FROM dd) AS day,
			extract(year FROM dd) AS year,
			dd AS date_of_year
		 FROM  
			generate_series('13-03-1992'::timestamp, '18-11-1993'::timestamp, '1 day'::interval) dd)				 	      
	SELECT
		'a1' AS geocode,
		'' AS comments,
		normal_date_series.date_of_year,
		1.0 AS pm10_rd,
		2.0 AS nox_rd,
		3.0 AS pm10_gr,		
		4.0 AS name,
		5.0 AS pm10_tot
	FROM
		normal_date_series
	UNION
	SELECT
		'a2' AS geocode,
		'' AS comments,
		normal_date_series.date_of_year,
		3.0 AS pm10_rd,
		4.0 AS nox_rd,
		5.0 AS pm10_gr,		
		6.0 AS name,
		7.0 AS pm10_tot      
	FROM
		normal_date_series
	UNION
	SELECT
		'a3' AS geocode,
		'' AS comments,
		normal_date_series.date_of_year,
		6.0 AS pm10_rd,
		7.0 AS nox_rd,
		8.0 AS pm10_gr,		
		9.0 AS name,
		10.0 AS pm10_tot
	FROM
		normal_date_series
	UNION
	SELECT
		'a4' AS geocode,
		'' AS comments,
		normal_date_series.date_of_year,
		10.0 AS pm10_rd,
		11.0 AS nox_rd,
		12.0 AS pm10_gr,		
		13.0 AS name,
		14.0 AS pm10_tot
	FROM
		normal_date_series
	UNION
	SELECT
		'a5' AS geocode,
		'' AS comments,
		normal_date_series.date_of_year,
		15.0 AS pm10_rd,
		16.0 AS nox_rd,
		17.0 AS pm10_gr,		
		18.0 AS name,
		19.0 AS pm10_tot
	FROM
		normal_date_series
	UNION
	SELECT
		'a6' AS geocode,
		'' AS comments,
		normal_date_series.date_of_year,
		21.0 AS pm10_rd,
		22.0 AS nox_rd,
		23.0 AS pm10_gr,		
		24.0 AS name,
		25.0 AS pm10_tot
	FROM
		normal_date_series
	UNION
	SELECT
		'a7' AS geocode,
		'' AS comments,
		normal_date_series.date_of_year,
		28.0 AS pm10_rd,
		29.0 AS nox_rd,
		30.0 AS pm10_gr,		
		31.0 AS name,
		32.0 AS pm10_tot
	FROM
		normal_date_series
	UNION
	SELECT
		'a8' AS geocode,
		'' AS comments,
		incomplete_date_series.date_of_year,
		10.0 AS pm10_rd,
		10.0 AS nox_rd,
		10.0 AS pm10_gr,		
		10.0 AS name,
		10.0 AS pm10_tot
	FROM
		incomplete_date_series
	UNION
	SELECT
		'a9' AS geocode,
		'' AS comments,
		incomplete_date_series.date_of_year,
		NULL AS pm10_rd,
		NULL AS nox_rd,
		NULL AS pm10_gr,		
		NULL AS name,
		NULL AS pm10_tot
	FROM
		incomplete_date_series;

	EXECUTE format ('COPY test_exp_data TO %L CSV HEADER', original_exp_data_file);
		
END;
$$   LANGUAGE plpgsql;
--SELECT "generate_early_exp_test_data"('C:\algae_protocol\test_environment');

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION generate_later_exp_test_data
 * ------------------------------------
 * Description
 * -----------
 * Generates fake exposure data to support tests of the late life analysis.  The function creates
 * a bunch of annual exposure records.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION generate_later_exp_test_data(test_case_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	original_later_exp_data_file TEXT;

BEGIN

	original_later_exp_data_file := 
		test_case_directory || 
		'\exposure_data\later_life\input_data\' 
		|| 
		'original_exposure_data.csv';

	RAISE NOTICE 'exposure file is ==%==', original_later_exp_data_file;
	
	DROP TABLE IF EXISTS test_later_exp_data;
	CREATE TABLE test_later_exp_data AS
	WITH date_series AS
		(SELECT  
			extract(doy FROM dd) AS day,
			extract(year FROM dd) AS year,
			dd AS date_of_year
		 FROM  
			generate_series('01-01-1992'::timestamp, '01-01-2008'::timestamp, '1 year'::interval) dd)
	SELECT
		'a1' AS geocode,
		'' AS comments,
		date_series.date_of_year,
		1.0 AS pm10_rd,
		2.0 AS nox_rd,
		3.0 AS pm10_gr,		
		4.0 AS name,
		5.0 AS pm10_tot
	FROM
		date_series
	UNION
	SELECT
		'a2' AS geocode,
		'' AS comments,
		date_series.date_of_year,
		3.0 AS pm10_rd,
		4.0 AS nox_rd,
		5.0 AS pm10_gr,		
		6.0 AS name,
		7.0 AS pm10_tot      
	FROM
		date_series
	UNION
	SELECT
		'a3' AS geocode,
		'' AS comments,
		date_series.date_of_year,
		6.0 AS pm10_rd,
		7.0 AS nox_rd,
		8.0 AS pm10_gr,		
		9.0 AS name,
		10.0 AS pm10_tot
	FROM
		date_series
	UNION
	SELECT
		'a4' AS geocode,
		'' AS comments,
		date_series.date_of_year,
		10.0 AS pm10_rd,
		11.0 AS nox_rd,
		12.0 AS pm10_gr,		
		13.0 AS name,
		14.0 AS pm10_tot
	FROM
		date_series
	UNION
	SELECT
		'a5' AS geocode,
		'' AS comments,
		date_series.date_of_year,
		15.0 AS pm10_rd,
		16.0 AS nox_rd,
		17.0 AS pm10_gr,		
		18.0 AS name,
		19.0 AS pm10_tot
	FROM
		date_series
	UNION
	SELECT
		'a6' AS geocode,
		'' AS comments,
		date_series.date_of_year,
		21.0 AS pm10_rd,
		22.0 AS nox_rd,
		23.0 AS pm10_gr,		
		24.0 AS name,
		25.0 AS pm10_tot
	FROM
		date_series
	UNION
	SELECT
		'a7' AS geocode,
		'' AS comments,
		date_series.date_of_year,
		28.0 AS pm10_rd,
		29.0 AS nox_rd,
		30.0 AS pm10_gr,		
		31.0 AS name,
		32.0 AS pm10_tot
	FROM
		date_series;

   EXECUTE format ('COPY test_later_exp_data TO %L CSV HEADER', original_later_exp_data_file);

END;
$$   LANGUAGE plpgsql;
--SELECT "generate_later_exp_test_data"('C:\algae_protocol\test_environment');

