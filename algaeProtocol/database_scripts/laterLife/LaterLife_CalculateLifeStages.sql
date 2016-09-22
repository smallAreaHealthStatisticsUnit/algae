/**
 * ================================================================================================= 
 * MODULE COMM_later_LIFE_STAGES: Establish Late Life Stages
 * =================================================================================================
 * Description
 * -----------
 * This module contains code that is used to calculate the temporal boundaries of late life stages.
 * The start of the late life analysis begins at conception date, which is calculated as:
 *
 *    conception date = birth date - (7 x gestation age at birth in weeks) - 1 day.
 * 
 * The life stages in the late analysis are all years of life, each of which has the boundaries
 * resembling:
 *
 * Life Stage           Start Date                            End Date
 * ----------           ----------                            --------
 * Yn                   birth date + n years                  birth date + (n + 1) years - 1 day
 *
 * Main Function
 * -------------
 *   late_calc_life_stages 
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
 * MAIN FUNCTION late_calc_life_stages
 * -----------------------------------------
 * Description
 * -----------
 * Establishes the temporal boundaries of individual later life stages and the boundaries of the
 * overall exposure time frame.
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION late_calc_life_stages()
	RETURNS void AS 
$$
DECLARE

BEGIN

	PERFORM comm_cln_gest_age_at_birth();

	DROP TABLE IF EXISTS tmp_overall_exp_time_frames;
	CREATE TABLE tmp_overall_exp_time_frames AS
	SELECT
		person_id,
		birth_date,
		gestation_age_at_birth,
		conception_date,
		conception_date AS time_frame_start_date,
		is_gestation_age_imputed,
		(birth_date + INTERVAL '16 years')::date AS time_frame_end_date, -- used to input end date for incomplete address period		
		(birth_date + INTERVAL '16 years')::date - conception_date + 1 AS time_frame_duration		
	FROM
		fin_basic_life_stage_data;
		
	ALTER TABLE tmp_overall_exp_time_frames ADD PRIMARY KEY (person_id);
	DROP INDEX IF EXISTS ind_tmp_overall_exp_time_frames1;
	CREATE INDEX  ind_tmp_overall_exp_time_frames1 ON tmp_overall_exp_time_frames(time_frame_start_date);
	DROP INDEX IF EXISTS ind_tmp_overall_exp_time_frames2;
	CREATE INDEX  ind_tmp_overall_exp_time_frames2 ON tmp_overall_exp_time_frames(time_frame_end_date);


	DROP TABLE IF EXISTS fin_general_life_stage_data;
	CREATE TABLE fin_general_life_stage_data AS
	WITH study_member_birthdays1 AS 
		(SELECT
			a.person_id,
			a.birth_date,
			generate_series(a.birth_date, a.birth_date + INTERVAL '15 years', '1 year'::INTERVAL)::date AS start_date
	 	FROM
			fin_basic_life_stage_data a),
	study_member_birthdays2 AS
		(SELECT
			person_id,
			birth_date,
			row_number() OVER 
				(PARTITION BY 
			    	person_id 
			 	 ORDER BY 
			     	start_date) AS ith_life_stage,
			'YR' || (extract(year from start_date) - extract(year from birth_date) + 1) AS life_stage,
			start_date::date,
			(start_date + INTERVAL '1 year' - INTERVAL '1 day')::date AS end_date
		FROM
			study_member_birthdays1)	
	SELECT
		person_id,
		ith_life_stage,
		life_stage,
	    extract(year from start_date)::int AS life_stage_year,
		start_date,
		end_date,
		(end_date - start_date) + 1 AS life_stage_duration	
	FROM
		study_member_birthdays2
	ORDER BY
		person_id,
		ith_life_stage;
	ALTER TABLE fin_general_life_stage_data ADD PRIMARY KEY (person_id, ith_life_stage);

	DROP TABLE IF EXISTS results_later_life_stages;
	CREATE TABLE results_later_life_stages AS
	SELECT
		*
	FROM
		fin_general_life_stage_data;
	ALTER TABLE results_later_life_stages ADD PRIMARY KEY (person_id, ith_life_stage);

END;
$$   LANGUAGE plpgsql;
--SELECT "late_calc_life_stages"();

