/**
 * ================================================================================================= 
 * MODULE COMM_PREPROCESS: Preliminary System Checks
 * =================================================================================================
 * Description
 * -----------
 * This module creates a staging table for each "original" data table that is provided by an 
 * individual cohort.  In the process it tries to standardise text field values that may have 
 * values which are correct, but are different from standard field values that are used in this 
 * program.
 *
 * Although programmers need to massage their data into staging tables, their data may contain 
 * field values that could cause computational problems.  For example, every address period requires 
 * a non-null geocode, so that it may be used to link with other tables.
 * 
 * If geocode were null, it may be ignored in some database joins.  Therefore, as a precaution, any 
 * geocode that has a NULL value is replaced with a String value "empty_geocode_value".  This value 
 * is defined in the global_script_constants table and is by default "empty_geocode".  We can
 * be guaranteed that it is not a legitimate geocode, but we can also guarantee it is represented 
 * in database joins as well.
 *
 * Main Function
 * -------------
 *    common_preprocess_staging_tables
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

/**
 * ------------------------------------------------------------------------------------------------ 
 * MAIN FUNCTION common_preprocess_staging_tables
 * ----------------------------------------------
 * Description
 * -----------
 * (1) Ensures staging tables exist
 * (2) standardise geocode values (mainly ensuring that null geocodes are expressed as
 * "empty_geocode" so they can participate in joins.  Note that if a geocode were null then
 * links that used it would be dropped from queries.
 * (3) standardise yes/no values
 * ------------------------------------------------------------------------------------------------ 
*/

CREATE OR REPLACE FUNCTION comm_preprocess_staging_tables()
	RETURNS void AS 
$$
DECLARE

BEGIN	

	-- ====================================================================
	-- Pre-processing 1: Verify original data tables exist
	-- ---------------------------------------------------
	-- The first step in pre-processing is to ensure that the original data 
	-- tables exist.
	-- ====================================================================
	
	PERFORM check_table_exists('original_study_member_data');
	PERFORM check_table_exists('original_addr_history_data');
	PERFORM check_table_exists('original_exp_data');
	PERFORM check_table_exists('original_geocode_data');


	-- =====================================================================
	-- Pre-processing 2: Standardise correct field values
	-- --------------------------------------------------
	-- Next, we create a staging table for each original data table.  During
	-- this process, we may alter some values that are correct, but which
	-- are not expressed in a standard way.  There are two main examples of this,
	-- both of which relate to text-based field values:
	--    * standardising the way empty geocode values are represented
	--    * standardising the way 'yes' and 'no' values are represented 
	-- 
	-- Geocode data can be generated or created by various applications that
	-- may represent an empty value differently (eg: null, #NULLIF, '').  They
	-- have the same meaning of 'no value' but they need to be standardised.
	-- In this protocol, we use the phrase 'empty_geocode' to specify an empty
	-- geocode so that database joins are easier to do.
	-- 
	-- The other main example relates to how yes and no can be represented.
	-- 'Y', 'Yes', 'YES', 'true', 'TRUE', '1' can all carry the same meaning of
	-- a 'yes' answer.  All of these values may be regarded as equally correct,
	-- but standardising them to be 'Y' means the rest of the application code 
	-- can make stronger assumptions about the data it processes.
	--
	-- In both cases, when values are altered, the changes are not audited.
	-- We audit changes that change the meaning of a field value such as
	-- imputing blank values or altering dates.  However, we don't audit
	-- changes made to standardise correct data because we are only changing
	-- the representation and not the meaning of values.
	-- =====================================================================
	

	-- ====================================================================
	-- Pre-processing 2.1: Standardise the original_study_member_data table
	-- --------------------------------------------------------------------
	-- standardise variations of 'yes' and 'no' answers that could appear
	-- in the yes no fields
	-- ====================================================================		
	DROP TABLE IF EXISTS staging_study_member_data;
	CREATE TABLE staging_study_member_data AS
	SELECT
		row_number() OVER() AS original_row_number,
		person_id,
		birth_date,
		estimated_gestation_age,
		standardise_yes_no_value(absent_during_exp_period) AS absent_during_exp_period,
		standardise_yes_no_value(at_1st_addr_conception) AS at_1st_addr_conception
	FROM
		original_study_member_data;

	-- ======================================================================
	-- Pre-processing 2.2 Standardise the original_addr_history_data table
	-- ----------------------------------------------------------------------
	-- standardise the geocode values.  If they are empty, we will use
	-- a standard value to indicate an empty value (eg: empty_geocode)
	-- ====================================================================
	DROP TABLE IF EXISTS staging_addr_history_data;
	CREATE TABLE staging_addr_history_data AS
	SELECT
		row_number() OVER() AS original_row_number,
		person_id,
		standardise_geocode_value(geocode, TRUE) AS geocode,
		start_date,
		end_date
	FROM
		original_addr_history_data;

	-- =======================================================================
	-- Pre-processing 2.3: Standardise the original_exp_data table
	-- -----------------------------------------------------------------------
	-- standardise the geocode values.  If they are empty, we will use
	-- a standard value to indicate an empty value (eg: empty_geocode)
	-- ====================================================================	
	DROP TABLE IF EXISTS staging_exp_data;
	CREATE TABLE staging_exp_data AS
	SELECT
		row_number() OVER() AS original_row_number,
		standardise_geocode_value(geocode, FALSE) AS geocode,
		date_of_year,
		name,
		nox_rd,
		pm10_rd,
		pm10_gr,
		pm10_tot				
	FROM
		original_exp_data;

	-- =====================================================================
	-- Pre-processing 2.4: Standardise the original_geocode_data table
	-- ---------------------------------------------------------------------
	DROP TABLE IF EXISTS staging_geocode_data;
	CREATE TABLE staging_geocode_data AS
	SELECT
		row_number() OVER() AS original_row_number,
		standardise_geocode_value(geocode, FALSE) AS geocode,
		version,
		ed91,
		oa2001,
		coa2011,
		standardise_yes_no_value(has_valid_geocode) AS has_valid_geocode
	FROM
		original_geocode_data;


	-- =====================================================================
	-- Pre-processing 3: Validate table properties by applying constraints
	-- -------------------------------------------------------------------
	-- Here we make use of database features to verify two table properties
	--    * that a required field column for a table has no null field values
	--    * that a required field column has a unique set of field values
	-- =====================================================================
    
    UPDATE global_script_constants
    SET total_study_members =
		(SELECT
	   		COUNT(DISTINCT person_id) total
		FROM
	   		staging_study_member_data);
   
END;
$$   LANGUAGE plpgsql;	

--SELECT "common_preprocess_staging_tables"();