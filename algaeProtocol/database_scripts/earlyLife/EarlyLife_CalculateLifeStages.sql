/**
 * ================================================================================================= 
 * MODULE COMM_EARLY_LIFE_STAGES: Establish Early Life Stages
 * =================================================================================================

 * Description
 * -----------
 * This module contains code that is used to calculate the temporal boundaries of early life stages.
 * Most of them rely on conception date, which is calculated as:
 *
 *    conception date = birth date - (7 x gestation age at birth in weeks) - 1 day.
 *
 * Life Stage           Start Date                            End Date
 * ----------           ----------                            --------
 * Trimester 1 (T1)     conception date                       conception date + 92 days
 * Trimester 2 (T2)     conception date + 93 days             conception date + 183 days
 * Trimester 3 (T3)     conception date + 184 days            birth date - 1 day
 * Early Life (EL)      birth date                            birth date + 1 year - 1 day
 *
 * Main Function
 * -------------
 *   early_calc_life_stages 
 *
 * Assumptions 
 * -----------
 * Any missing gestation age at birth values have been imputed.
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
 
/**
 * ------------------------------------------------------------------------------------------------ 
 * MAIN FUNCTION early_calc_life_stages
 * -----------------------------------------
 * Description
 * -----------
 * Establishes the temporal boundaries of individual early life stages and the boundaries of the
 * overall exposure time frame.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION early_calc_life_stages()
	RETURNS void AS 
$$
DECLARE

BEGIN

	PERFORM comm_cln_gest_age_at_birth();
	
	-- This table is only meant to describe the beginning and end of the overall exposure
	-- time frame for each study member
	DROP TABLE IF EXISTS tmp_overall_exp_time_frames;
	CREATE TABLE tmp_overall_exp_time_frames AS
	SELECT
		person_id,
		birth_date,
		gestation_age_at_birth,
		conception_date,
		is_gestation_age_imputed,
		conception_date AS time_frame_start_date,
		(birth_date + INTERVAL '1 year' - INTERVAL '1 day')::date AS time_frame_end_date, -- used to input end date for incomplete address period		
		(birth_date + INTERVAL '1 year' - INTERVAL '1 day')::date - conception_date + 1 AS time_frame_duration
	FROM
		fin_basic_life_stage_data;

	ALTER TABLE tmp_overall_exp_time_frames ADD PRIMARY KEY (person_id);

	DROP INDEX IF EXISTS ind_tmp_overall_exp_time_frames1;
	CREATE INDEX  ind_tmp_overall_exp_time_frames1 ON tmp_overall_exp_time_frames(time_frame_start_date);

	DROP INDEX IF EXISTS ind_tmp_overall_exp_time_frames2;
	CREATE INDEX  ind_tmp_overall_exp_time_frames2 ON tmp_overall_exp_time_frames(time_frame_end_date);

	-- #DESIGN_FOR_REUSE: Change the life stage calculations
	-- This is the part of the code you would change if either you wanted to change the existing
	-- boundaries of trimesters or you wanted to add in your own life stages. For example, you
	-- might try adding a stage for 'lung development', when you might think a foetus is first 
	-- developing lungs.  In this case, the life stages would not refer to chronological divisions
	-- but phases that corresponded to the development of some physiological system.
	--
	-- The rest of the program does not attach meaning to any one life stage, so long as they
	-- are mutually exclusive - a person is assumed to belong to at most one stage on any given day


	/*
	  #CHANGE_LIFE_STAGES
	  #DESIGN_FOR_REUSE: I'll outline here how you'd split the early life stage EL
	  into two smaller stages: one that measures 0 to 6 months after birth and the
	  other that includes months 7 to 12.  To make this work, 

		substitute: 
		UNION
		SELECT
			person_id,
			'EL' AS life_stage,
			birth_date,
			birth_date AS start_date,
			(birth_date + INTERVAL '1 year' - INTERVAL '1 day')::date AS end_date
		FROM
			fin_basic_life_stage_data)
		
		with:
		
		SELECT
			person_id,
			'EL_0to6' AS life_stage,
			birth_date,
			birth_date AS start_date,
			(birth_date + INTERVAL '6 months')::date AS end_date
		FROM
			fin_basic_life_stage_data
		UNION
		SELECT
			person_id,
			'EL_7to12' AS life_stage,
			birth_date,
			(birth_date + INTERVAL '6 months' + '1 day')::date AS start_date,
			(birth_date + INTERVAL '1 year' - INTERVAL '1 day')::date AS end_date
		FROM
			fin_basic_life_stage_data)



	*/
	DROP TABLE IF EXISTS tmp_early_life_stages1;
	CREATE TABLE tmp_early_life_stages1 AS
	WITH a AS
		(SELECT
			person_id,
			'T1' AS life_stage,
			birth_date,
			conception_date AS start_date,
			(conception_date + INTERVAL '91 days')::date AS end_date
		 FROM
			fin_basic_life_stage_data
		 UNION
		 SELECT
		 	person_id,
			'T2' AS life_stage,
			birth_date,
			(conception_date + INTERVAL '92 days')::date AS start_date,
			(conception_date + INTERVAL '183 days')::date AS end_date
		FROM
			fin_basic_life_stage_data
		UNION
		SELECT
			person_id,
			'T3' AS life_stage,
			birth_date,
			(conception_date + INTERVAL '184 days')::date AS start_date,
			(birth_date - INTERVAL '1 day')::date AS end_date
		FROM
			fin_basic_life_stage_data
		UNION
		SELECT
			person_id,
			'EL' AS life_stage,
			birth_date,
			birth_date AS start_date,
			(birth_date + INTERVAL '1 year' - INTERVAL '1 day')::date AS end_date
		FROM
			fin_basic_life_stage_data)
	SELECT
		person_id,
		life_stage,
		birth_date::date,
	    extract(year from start_date)::int AS life_stage_year,
		start_date::date,		
		end_date::date
	FROM
		a
	ORDER BY
		person_id,
		start_date;

	/*
	  #CHANGE_LIFE_STAGES
	  #DESIGN_FOR_REUSE: I'll outline here how you'd split the early life stage EL
	  into two smaller stages: one that measures 0 to 6 months after birth and the
	  other that includes months 7 to 12.  The main trouble in early life stage
	  calculations is that we want to ensure that if any of the life stages that
	  happen before birth are left ovelapping with birth date, that needs to be corrected.
	  And if there are any pre-birth life stages which start *after* the birth date,
	  they need to be set to null (eg: T3 calculations that happen from premature
	  births.  To change the life stages to include 'EL_0to6' and 'EL_7to12'
	  substitute code in the query below the commented section with the case statements
	  which follow here:
	
			CASE
				WHEN a.life_stage = 'EL_0to6' OR a.life_stage = 'EL_7to12' THEN
					a.start_date --ignore 
				WHEN a.start_date >= b.birth_date THEN
					NULL
				ELSE
					a.start_date
			END AS start_date,
			CASE
				WHEN a.life_stage = 'EL_0to6' OR a.life_stage = 'EL_7to12' THEN
					a.end_date -- ignore 
				WHEN a.start_date >= b.birth_date THEN
					NULL -- this conditions designates the entire stage as not applicable
			WHEN a.end_date >= b.birth_date THEN
				(b.birth_date - INTERVAL '1 day')::date -- start date is OK but end date would overlap with EL
			ELSE
				a.end_date
			END AS end_date
	*/

	

	DROP TABLE IF EXISTS fin_general_life_stage_data;
	CREATE TABLE fin_general_life_stage_data AS
	WITH cleaned_life_stages AS
		(SELECT
			a.person_id,
			a.life_stage,
			a.life_stage_year,
			CASE
				WHEN a.life_stage = 'EL' THEN
					a.start_date --ignore EL period because it needs to start with birth date
				WHEN a.start_date >= b.birth_date THEN
					NULL
				ELSE
					a.start_date
			END AS start_date,
			CASE
				WHEN a.life_stage = 'EL' THEN
					a.end_date -- ignore because the algorithm is only meant to deal with T1, T2, T3
				WHEN a.start_date >= b.birth_date THEN
					NULL -- this conditions designates the entire stage as not applicable
			WHEN a.end_date >= b.birth_date THEN
				(b.birth_date - INTERVAL '1 day')::date -- start date is OK but end date would overlap with EL
			ELSE
				a.end_date
			END AS end_date
		FROM
			tmp_early_life_stages1 a,
			fin_basic_life_stage_data b
		WHERE
			a.person_id = b.person_id)
	SELECT 
		person_id,
		row_number() OVER 
			(PARTITION BY 
				person_id 
		 	ORDER BY 
				start_date) AS ith_life_stage,
		life_stage,
		life_stage_year,
		start_date,
		end_date,
		(end_date::date - start_date::date) + 1 AS life_stage_duration       
	FROM
		cleaned_life_stages;

	ALTER TABLE fin_general_life_stage_data ADD PRIMARY KEY(person_id, ith_life_stage);

/*
	DROP TABLE IF EXISTS results_early_life_stages;
	CREATE TABLE results_early_life_stages AS
	SELECT
		*
	FROM
		fin_general_life_stage_data;
	
	ALTER TABLE results_early_life_stages ADD PRIMARY KEY(person_id, ith_life_stage);

	CREATE INDEX  results_early_life_stages_1 ON results_early_life_stages(start_date);
	CREATE INDEX  results_early_life_stages_2 ON results_early_life_stages(end_date);
*/

	/*
	* Update the global constants list so that it can use the minimum conception date
	* and the maximum first year end date to handle exposure values used in the test data set
	*/
	UPDATE global_script_constants
		SET minimum_conception_date =
			(SELECT 
			    min(time_frame_start_date)
			 FROM
			    tmp_overall_exp_time_frames);
			    
	UPDATE global_script_constants
		SET maximum_el_end_date =
			(SELECT 
			    max(time_frame_end_date)
			 FROM
			    tmp_overall_exp_time_frames);

	-- Cleanup
	DROP TABLE tmp_early_life_stages1;

END;
$$   LANGUAGE plpgsql;

--SELECT "calculate_early_stages"();

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION validate_early_stage_data
 * ----------------------------------
 * Description
 * -----------
 * Checks that the number of people in the early life stage calculations table matches the 
 * number of study members we expect to have results 
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION validate_early_stage_data()
	RETURNS void AS 
$$
DECLARE
	current_total_study_members INT;

BEGIN

	SELECT
		COUNT(DISTINCT person_id)
	FROM
		results_early_life_stages
	INTO 
		current_total_study_members;

	PERFORM validate_total_study_members(
		'validate_year1_life_stage_data',
		current_total_study_members);

END;
$$   LANGUAGE plpgsql;
