/**
 * ================================================================================================= 
 * MODULE COMM_LIFE_STAGE: Common life stage calculation methods
 * =================================================================================================
 * Description
 * -----------
 * Contains code that is used to calculate exposure time frames for both early and later life 
 * analyses.  In both analyses, the starting point for the exposure time is the conception date.
 * Conception date is calculated using the formula:
 *    
 *    conception_date = birth_date - (7 x gestation age at birth in weeks) - 1 Day.
 *
 * The code also imputes blank gestation age at birth values and audits the imputation action with
 * a flag "is_gestation_age_imputed".  The calculations are used to produce a table which is then
 * available for either analysis to use. 
 *
 * Blank gestation age at birth values are imputed using a value that is defined in the table field
 * global_script_constants.default_gestation_age.  (See function setup_scripts(...) in module
 * "Initialise global constants".  You can change the default value by changing the first value
 * that appears when you call the method.  For example, a typical call might be:
 *
 *    setup_scripts"(38, 0.002, 0.005, 'M')
 *
 * To change the default gestation age from 38 to 37, simply replace the first parameter value.
 *
 * Main Function
 * -------------
 *    comm_cln_gest_age_at_birth
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
 * ALGAE is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * RIF is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with RIF.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Kevin Garwood
 * ------------------------------------------------------------------------------------------------ 
 */


/**
 * ------------------------------------------------------------------------------------------------ 
 * MAIN FUNCTION comm_cln_gest_age_at_birth
 * ----------------------------------------------------
 * Description
 * ------------
 * Imputes gestational age at birth with a value stored in the "default_gestation_age" field
 * of the global_constants table (See: "Initialise Global Constants" module)
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION comm_cln_gest_age_at_birth()
	RETURNS void AS 
$$
DECLARE

BEGIN

	/*
	 * #DESIGN_FOR_REUSE: Change conception date calculation.
	 * #CHANGE_CONCEPTION_CALCULATION
	 * If you think the conception date should be calculated differently, edit the 
	 * case...end statement which provides the conception_date variable
	 *
	 */	 
	DROP TABLE IF EXISTS fin_basic_life_stage_data;
	CREATE TABLE fin_basic_life_stage_data AS
	WITH global_constants AS
		(SELECT
			default_gestation_age
		 FROM
		 	global_script_constants)
	SELECT
		person_id,
		birth_date,
		CASE
			WHEN estimated_gestation_age IS NULL THEN 
		   		global_constants.default_gestation_age
			ELSE
				estimated_gestation_age
		END AS gestation_age_at_birth,	
		CASE
			WHEN estimated_gestation_age IS NULL THEN 
		   		(birth_date - INTERVAL '1 day' - 
		   		(INTERVAL '1 week' * global_constants.default_gestation_age))::date
			ELSE 
		   		(birth_date - INTERVAL '1 day' - 
		   		(INTERVAL '1 week' * estimated_gestation_age))::date
		END AS conception_date,
		CASE
			WHEN estimated_gestation_age IS NULL THEN
				'Y'
			ELSE 'N'
		END AS is_gestation_age_imputed
	FROM
		staging_study_member_data,
		global_constants;
	
	-- Adding a primary key helps improve performance and provides an integrity 
	-- check that can help identify errors.  If adding the person_id field as a primary
	-- key failed, it would indicate that there were duplicate records in the
	-- table staging_study_member_data.
	ALTER TABLE fin_basic_life_stage_data ADD PRIMARY KEY (person_id);

END;
$$   LANGUAGE plpgsql;

