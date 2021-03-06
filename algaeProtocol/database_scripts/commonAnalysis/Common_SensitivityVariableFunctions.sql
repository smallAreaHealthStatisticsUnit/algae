/**
 * ================================================================================================= 
 * MODULE COMM_SENSITIVITY: General Sensitivity Variables Functions
 * =================================================================================================
 * Description
 * -----------
 * This module contains methods that gather meta data relating to the data quality of results
 * that are produced by the protocol.  The sensitivity variables may be used by epidemiologists
 * to help assess how much data cleaning influenced results.  For example, they could isolate
 * study members whose address histories did not have to be adjusted for gaps and overlaps.  
 * They could then compare the trends they observe in the results with the results generated
 * by all study members and help guage the effects of data cleaning on overall trends they
 * notice.
 *
 * Main Function
 * -------------
 *      comm_set_exp_sensitivity_data();
 *      comm_set_stage_sensitivity_data(); 
 *
 * Assumptions
 * -----------
 * The routines in this module assume that the address periods have already been completely 
 * processed.
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



/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION impute_addr_period_end_dates
 * -----------------------------------------
 * Description
 * -----------
 * The first sensitivity variables we produce come from the study member health data and from
 * other sources of cohort variables.  We borrow "gestation_age_at_birth" and 
 * "is_gestation_age_imputed" from the staging_study_member_data table.  gestation_age_at_birth
 * is important because premature babies tend to produce short or non-existent trimester 3 exposures.
 * Premature births also cause the trimester 2 periods to change as well.
 *
 * Cohort groups will write their own code which produces values for these two variables:
 * (1) at_1st_addr_conception: yes or no, whether study members were definitely at their 
 *     first address on the day of their conception dates.
 * (2) absent_during_exp_period: yes or no, whether study members spent a significant amount of 
 *     their exposure time frame living at a location that isn't listed in the residential history.
 *
 * The variables will typically be the result of combining the responses for several questionnaire
 * variables.
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION comm_set_study_member_sensitivity_data()
	RETURNS void AS 
$$
DECLARE

BEGIN

	--Here we expect that there will be no dropped rows in a join.
	--fin_basic_life_stage_data should have the same person IDs 
	--as staging_study_member_data
	DROP TABLE IF EXISTS tmp_sensitivity_variables1;
	CREATE TABLE tmp_sensitivity_variables1 AS
	SELECT
		a.person_id,
		a.at_1st_addr_conception,
		a.absent_during_exp_period,
		b.gestation_age_at_birth,
		b.is_gestation_age_imputed
	FROM
		staging_study_member_data a,
		fin_basic_life_stage_data b
	WHERE
		a.person_id = b.person_id;

	ALTER TABLE tmp_sensitivity_variables1 ADD PRIMARY KEY (person_id);
			
END;
$$   LANGUAGE plpgsql;
--SELECT "comm_set_study_member_sensitivity_data"();

CREATE OR REPLACE FUNCTION comm_set_geocode_sensitivity_data()
	RETURNS void AS 
$$
DECLARE

BEGIN

 	-- The program cannot generate exposure results for study members if, during their 
 	-- exposure time of the study, they have at least one address period which has a 
 	-- bad geocode.
	-- A bad geocode is an (x,y) coordinate that the program cannot process.  There are 
	-- three causes for a bad geocode, each warranting its own sensitivity variable.
	--
	-- (1)invalid geocodes.  These will occur in two situations.  The address period may 
	-- have a blank geocode which almost always because the residential address didn't 
	-- have enough information to make a match.  Another scenario is where the geocoding 
	-- software uses a partial address to produce a match, but the quality of the match is 
	-- so low that the 'is_valid' flag is set to indicate a failed match anyway.
	-- 
	-- (2)out-of-bounds exposures.  These refer to address periods which have a geocode 
	-- which is valid but which is not associated with any exposure values.  We assume that 
	-- in this scenario, the geocode refers to a location which is outside the exposure 
	-- area. 

	-- In a lot of the temporary address period tables, we use flags that have
	-- a 'Y' or 'N' value because they are more readable than a '0' or a '1'.  
	-- When we count them up, it is convenient to convert the yes/no values to a 
	-- score and then add them up.  The counts we create here only consider address 
	-- periods that are within a study member's exposure time frame
	DROP TABLE IF EXISTS tmp_sensitivity_variables2;
	CREATE TABLE tmp_sensitivity_variables2 AS
	WITH relevant_addr_period_geocode_scores1 AS
		(SELECT
			person_id,
			ith_residence,
			CASE
				WHEN has_valid_geocode = 'N' THEN
					1
				ELSE
					0
			END AS invalid_geocode_score,
			CASE		
				WHEN is_fixed_invalid_geocode = 'Y' THEN
					1
				ELSE
					0
			END AS fixed_invalid_geocode_score
		 FROM
		 	fin_cleaned_addr_periods
		 WHERE
		 	is_within_exposure_time_frame = 'Y'),
	relevant_addr_period_geocode_scores2	AS
		 (SELECT
		 	person_id,
		 	SUM(invalid_geocode_score) AS invalid_geocodes,
		 	SUM(fixed_invalid_geocode_score) AS fixed_geocodes
		 FROM
		 	relevant_addr_period_geocode_scores1
		 GROUP BY
		 	person_id)
	SELECT
		a.person_id,
		a.at_1st_addr_conception,
		a.absent_during_exp_period,
		a.gestation_age_at_birth,		
		a.is_gestation_age_imputed,
		COALESCE(b.invalid_geocodes, 0) AS invalid_geocodes,		
		COALESCE(b.fixed_geocodes, 0) AS fixed_geocodes
	FROM
		tmp_sensitivity_variables1 a
	LEFT JOIN 
		relevant_addr_period_geocode_scores2 b
	ON
		a.person_id = b.person_id
	ORDER BY
		a.person_id;

	ALTER TABLE tmp_sensitivity_variables2 ADD PRIMARY KEY (person_id);

END;
$$   LANGUAGE plpgsql;
--SELECT "comm_set_geocode_sensitivity_data"();

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION common_set_sensitivity_total_addr_periods
 * -----------------------------------------------------
 * Description
 * -----------
 * This function establishes the total number of address periods that exist for a study member.  
 * We exclude address periods in two cases:
 * (1) it is not within the overall exposure time frame
 * (2) it is an address period with an invalid geocode which was subsequently fixed.
 *     'Fixing' means essentially deleting the address period from consideration and
 *     its period is subsumed by the very next address period that follows it.
 * The method takes into account study members who may not have any address periods.  In this 
 * scenario, we convert a null value for total_addr_periods to "0".
 
 * The first sensitivity variables which are created in this function give the frequency of different 
 * fit type problems that appear in the address periods of a study member.  There are two types of 
 * sensitivity variables that are developed here:
 *  (1) those that describe the way the dates of an address period are imputed and
 *  (2) those that describe how two successive address periods fit together.
 * 
 * Each of these types of variables are identified if the address period overlaps with the exposure 
 * time frame of interest (eg: from conception date to the last day of the first year of life).
 * 
 * The program requires that all address periods have a non-blank value for both the start date and 
 * the end date.  Often, incomplete address periods are imputed to ensure this condition is true.  
 * The sensitivity variables are:
 *    (1) total_cln_blank_start_dates
 *    (2) total_cln_blank_end_dates
 *    (3) total_cln_blank_both_dates
 *    (4) total_cln_last_dates
 *
 * Once address periods are guaranteed to be complete, they are ordered for each study member, first 
 * by ascending order of start date and second by their duration.  When two address periods are put 
 * together, they can exhibit various types of fit.
 *
 * A 'fit type' describes the way successive address periods fit together temporally.  Fit type has 
 * the following types:
 *    (1) Gap ("G"). At least one day exists between the end date of one address and the start date 
 *        of the next.
 *    (2) Overlap ("O").  At least one day in one address period is also a day that is part of the 
 *        successive address period.
 *    (3) Both ("B").  Describes the frequency of an event where the start date of an address period 
 *        is adjusted to cover a gap and the end date is pushed back by an overlap with another
 *        period.
 *    (4) Deleted ("D"). Describes a special kind of overlap where the time frame of one address 
 *        period is completely subsumed by another.
 *
 * This function counts the frequency of each of these states in address periods and produces the 
 * following sensitivity variables:
 *    (1) total gaps
 *    (2) total overlaps
 *    (3) total_gap_and_overlap_same_period
 *    (4) total deletions 
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION comm_set_address_history_sensitivity_data()
	RETURNS void AS 
$$
DECLARE

BEGIN

	--Identify study members who have no address periods
	--In "a", we try to match address periods with study members
	--who have demographic data available.  For study members who
	--have no address periods, they will have a NULL value for 
	--geocode.  In "b", we give each geocode record a score.
	--The scoring is intended to treat NULL values as contributing
	--zero to the total address periods	
	DROP TABLE IF EXISTS tmp_sensitivity_variables3;
	CREATE TABLE tmp_sensitivity_variables3 AS
	WITH addresses_within_time_frame1 AS 
		(SELECT
			person_id,
			COUNT(person_id) AS total_addr_periods
		FROM
			fin_cleaned_addr_periods
		WHERE
			is_within_exposure_time_frame = 'Y' AND
			is_fixed_invalid_geocode = 'N'
		GROUP BY
			person_id)
	SELECT
		a.person_id,
		a.at_1st_addr_conception,
		a.absent_during_exp_period,
		a.gestation_age_at_birth,		
		a.is_gestation_age_imputed,	
		a.invalid_geocodes,	
		a.fixed_geocodes,
		COALESCE(b.total_addr_periods, 0) AS total_addr_periods	--some may be null
	FROM
		tmp_sensitivity_variables2 a
	LEFT JOIN 
		addresses_within_time_frame1 b
	ON
		a.person_id = b.person_id
	ORDER BY
		a.person_id;

	ALTER TABLE tmp_sensitivity_variables3 ADD PRIMARY KEY (person_id);


	-- #DATABASE_TECHNIQUE
	-- The following query shows a recurring pattern in some of the tables.
	-- Here, we preserve 'Y' and 'N' flags in temporary tables to make tables
	-- easier to read.  However, when we want to do counts, it is convenient to
	-- convert them to 0 and 1 values and then use them in summation calculations.
	--
	-- Further down in the query has lines such as:
	-- 		COALESCE(b.deletions, 0) AS deletions
	-- which are used in left join operations that may mean a field value is null.
	-- In final exposure results, we preserve nulls because they help indicate there
	-- are no results rather than the exposure aggregates yielded a zero value.
	-- In the context of sensitivity variables, we use coalesce to guarantee that
	-- null totals will be replaced with 0.
	-- 
	-- If a person has no address periods, then they will presumably result in a null
	-- total for some of the variables.  However, in the example above, if a person has
	-- no address periods then they will have zero gaps or overlaps
	DROP TABLE IF EXISTS tmp_sensitivity_variables4;
	CREATE TABLE tmp_sensitivity_variables4 AS
	WITH addr_change_summaries1 AS
		(SELECT
			person_id,
			CASE
				WHEN fit_type = 'O' THEN
					1
				ELSE
					0
			END AS overlap_period_score,
			CASE
				WHEN fit_type = 'G' THEN
					1
				ELSE
					0
			END AS gap_period_score,
			CASE
				WHEN fit_type = 'B' THEN
					1
				ELSE
					0
			END AS gap_and_overlap_period_score,
			CASE
				WHEN fit_type = 'D' THEN
					1
				ELSE
					0
			END AS deleted_period_score,
			CASE
				WHEN date_state = 'imputed_start_date' THEN
					1
				ELSE
					0
			END AS imputed_start_date_score,
			CASE
				WHEN date_state = 'imputed_end_date' THEN
					1
				ELSE
					0
			END AS imputed_end_date_score,
			CASE
				WHEN date_state = 'imputed_both_dates' THEN
					1
				ELSE
					0
			END AS imputed_both_dates_score,
			CASE
				WHEN imputed_last_end = 'Y' THEN 
					1
				ELSE
					0
			END AS imputed_last_end_date_score,
			days_changed
		FROM
			fin_cleaned_addr_periods
		WHERE
			is_within_exposure_time_frame = 'Y' AND
			is_fixed_invalid_geocode = 'N'),
	addr_change_summaries2 AS
		(SELECT
			person_id,
			SUM(overlap_period_score)::int AS over_laps, --we use "over_laps" because "overlaps" is a function
			SUM(gap_period_score)::int AS gaps,
			SUM(gap_and_overlap_period_score)::int AS gap_and_overlap_same_period,
			SUM(deleted_period_score)::int AS deletions,
			SUM(imputed_start_date_score)::int AS imp_blank_start_dates,
			SUM(imputed_end_date_score)::int AS imp_blank_end_dates,
			SUM(imputed_both_dates_score)::int AS imp_blank_both_dates,
			SUM(imputed_last_end_date_score)::int AS imp_last_dates,
			SUM(days_changed)::int AS days_changed
		 FROM
		 	addr_change_summaries1
		 GROUP BY
		 	person_id)
	SELECT
		a.person_id,
		a.at_1st_addr_conception,
		a.absent_during_exp_period,
		a.gestation_age_at_birth,		
		a.is_gestation_age_imputed,		
		a.invalid_geocodes,					
		a.fixed_geocodes,
		a.total_addr_periods,
		COALESCE(b.over_laps, 0) AS over_laps,
		COALESCE(b.gaps, 0) AS gaps,
		COALESCE(b.gap_and_overlap_same_period, 0) AS gap_and_overlap_same_period,
		COALESCE(b.deletions, 0) AS deletions,
		COALESCE(b.imp_blank_start_dates, 0) AS imp_blank_start_dates,
		COALESCE(b.imp_blank_end_dates, 0) AS imp_blank_end_dates,
		COALESCE(b.imp_blank_both_dates, 0) AS imp_blank_both_dates,
		COALESCE(b.imp_last_dates, 0) AS imp_last_dates,
		COALESCE(b.days_changed, 0) AS days_changed
	FROM
		tmp_sensitivity_variables3 a	
	LEFT JOIN
		addr_change_summaries2 b
	ON
		a.person_id = b.person_id
	ORDER BY
		a.person_id;

	ALTER TABLE tmp_sensitivity_variables4 ADD PRIMARY KEY (person_id);

	-- Check if study member has bad geocode within their exposure time frame
	DROP TABLE IF EXISTS tmp_sensitivity_variables5;
	CREATE TABLE tmp_sensitivity_variables5 AS
	SELECT
		a.person_id,
		a.at_1st_addr_conception,
		a.absent_during_exp_period,
		a.gestation_age_at_birth,		
		a.is_gestation_age_imputed,
		a.invalid_geocodes,					
		a.fixed_geocodes,
		a.total_addr_periods,
		a.over_laps,
		a.gaps,
		a.gap_and_overlap_same_period,
		a.deletions,
		a.imp_blank_start_dates,
		a.imp_blank_end_dates,
		a.imp_blank_both_dates,
		a.imp_last_dates,
		a.days_changed
	FROM		 	
		tmp_sensitivity_variables4 a;
	ALTER TABLE tmp_sensitivity_variables5 ADD PRIMARY KEY (person_id);

   	DROP INDEX IF EXISTS ind_tmp_sensitivity_variables5;
	
END;
$$   LANGUAGE plpgsql;
--SELECT "comm_set_address_history_sensitivity_data"();			

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION comm_set_exp_sensitivity_data
 * ---------------------------------------------
 * Description
 * -----------
 * Creates two sensitivity variables that relate to exposures:
 * (1) The number of days of contention and
 * (2) The number of missing exposure days
 *
 * Assumes
 * -------
 * common_calc_exposures() has been run.  These routines gathe
 * ------------------------------------------------------------------------------------------------ 
*/

CREATE OR REPLACE FUNCTION comm_set_exp_sensitivity_data()
	RETURNS void AS 
$$
DECLARE

BEGIN

	-- Determine how many days of contention there are in a person's exposure time frame
	DROP TABLE IF EXISTS tmp_sensitivity_variables6;
	CREATE TABLE tmp_sensitivity_variables6 AS
	WITH all_contention_days1 AS
		(SELECT
			person_id,
			date_of_year,
			CASE
				WHEN has_contention = 'Y' THEN
					1
				ELSE
					0
			END AS contention_score
		 FROM
			tmp_all_error_contributions_per_day),
	all_contention_days2 AS
		(SELECT
			person_id,
			SUM(contention_score) AS total_contention_days
		 FROM
		 	all_contention_days1
		 GROUP BY
		 	person_id)
	SELECT
		a.person_id,
		a.at_1st_addr_conception,
		a.absent_during_exp_period,
		a.gestation_age_at_birth,		
		a.is_gestation_age_imputed,	
		a.invalid_geocodes,							
		a.fixed_geocodes,
		a.total_addr_periods,
		a.over_laps,
		a.gaps,
		a.gap_and_overlap_same_period,
		a.deletions,
		a.imp_blank_start_dates,
		a.imp_blank_end_dates,
		a.imp_blank_both_dates,
		a.imp_last_dates,
		a.days_changed,
		b.total_contention_days
	FROM
		tmp_sensitivity_variables5 a
	LEFT JOIN
		all_contention_days2 b
	ON
		a.person_id = b.person_id
	ORDER BY
		person_id;
	ALTER TABLE tmp_sensitivity_variables6 ADD PRIMARY KEY (person_id);
		
	-- determine missing exposure days


	-- We're mainly trying to calculate the number of missing exposure days,
	-- but we're also using the opportunity to create a final sensitivity
	-- variables table to set most of the sensitivity variables to null
	-- for study members who have no address periods.  
	-- We touch on a philosophical issue relating to the treatment of 
	-- nulls: if a person had no address periods, does it make more sense
	-- to say they had 0 overlaps or that it is unknown? On one hand
	-- the program could count the number of overlaps it encountered in an 
	-- empty address history and claim that it found 0 overlaps.  On the
	-- other hand, perhaps it makes more sense to assert 
	
	DROP TABLE IF EXISTS fin_sens_variables;
	CREATE TABLE fin_sens_variables AS
	WITH total_exposure_days AS
		(SELECT
			a.person_id,
			COUNT(a.date_of_year) AS total_exposure_days
		 FROM
		 	tmp_common_exp_periods1 a,
		 	fin_daily_exposures b
		 WHERE
		 	a.geocode = b.geocode AND
		 	a.date_of_year = b.date_of_year
		 GROUP BY
		 	a.person_id),
	total_days_in_time_frame AS
		(SELECT
			person_id,
			time_frame_duration
		 FROM
		 	tmp_overall_exp_time_frames),
	no_exposure_data_days AS
		(SELECT
			a.person_id,
		 	(a.time_frame_duration - COALESCE(b.total_exposure_days, 0)) AS missing_days
		 FROM
		 	total_days_in_time_frame a
		 LEFT JOIN
		 	total_exposure_days b		 
		 ON
		 	a.person_id = b.person_id)
	SELECT
		a.person_id,
		a.at_1st_addr_conception,
		a.absent_during_exp_period,
		a.gestation_age_at_birth,		
		a.is_gestation_age_imputed,
		a.invalid_geocodes,								
		a.fixed_geocodes,
		a.total_addr_periods,
		a.over_laps,
		a.gaps,
		a.gap_and_overlap_same_period,
		a.deletions,
		a.imp_blank_start_dates,
		a.imp_blank_end_dates,
		a.imp_blank_both_dates,
		a.imp_last_dates,
		a.days_changed,
		a.total_contention_days,
		b.missing_days AS no_exposure_data_days
	FROM
		tmp_sensitivity_variables6 a
	LEFT JOIN
		no_exposure_data_days b
	ON
		a.person_id = b.person_id
	ORDER BY 
		a.person_id;
	ALTER TABLE fin_sens_variables ADD PRIMARY KEY (person_id);

END;
$$   LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION comm_set_stage_sensitivity_data()
	RETURNS void AS 
$$
DECLARE

BEGIN

	-- number of moves, number of addresses, number of contention days
	-- per life stage

	DROP TABLE IF EXISTS fin_life_stage_sensitivity_variables;
	CREATE TABLE fin_life_stage_sensitivity_variables AS 
	WITH life_stages AS
		(SELECT
			person_id,
			ith_life_stage,
			life_stage
		FROM
			fin_general_life_stage_data),
	move_data AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,		
			COUNT(DISTINCT ith_residence) - 1 AS moves,
			COUNT(DISTINCT geocode) AS distinct_addresses
		FROM
			tmp_common_exp_periods1
		GROUP BY
			person_id,
			ith_life_stage,
			life_stage),
	contention_days1 AS
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			CASE
				WHEN has_contention = 'Y' THEN
					1
				WHEN has_contention = 'N' THEN
					0
				ELSE
					NULL
			END AS contention_score
		 FROM
			tmp_all_error_contributions_per_day),
	contention_days2 AS
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			SUM(contention_score) AS contention_days
		 FROM
		 	contention_days1
		 GROUP BY
		 	person_id,
		 	ith_life_stage,
		 	life_stage)
	SELECT
		a.person_id,
		a.life_stage,
		COALESCE(b.moves, 0) AS moves,
		COALESCE(b.distinct_addresses, 0) AS distinct_addresses,
		COALESCE(c.contention_days, 0) AS contention_days
	FROM
		life_stages a
	LEFT JOIN
		move_data b
	ON
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	LEFT JOIN
		contention_days2 c
	ON
		a.person_id = c.person_id AND
		a.ith_life_stage = c.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage;

	ALTER TABLE fin_life_stage_sensitivity_variables ADD PRIMARY KEY (person_id, life_stage);

END;
$$   LANGUAGE plpgsql;
--SELECT comm_set_stage_sensitivity_data();

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION validate_sensitivity_variables
 * ----------------------------------------
 * Description
 * -----------
 * The program accumulates new sensitivity variables through the use of temporary tables.  In order 
 * to ensure that the process retains the same set of study members, we do three checks:
 * (1) ensure that the first temporary table and the final sensitivity variable table have the 
 *     same number of rows
 * (2) ensure that there are no study members in the first temporary table that do not appear in the 
 *     last table (ie: we haven't lost study members)
 * (3) ensure that there are no study members in the final sensitivity variable table that do not 
 *     appear in the first (ie: we haven't gained study members)
 * If any of these checks fail, the program is halted.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION comm_val_sensitivity_variables()
	RETURNS void AS 
$$
DECLARE
	current_total_study_members INT;	
	study_members_in_one_not_the_other INT;
	validation_failed BOOLEAN;
	
BEGIN

	validation_failed := FALSE;
	SELECT
		COUNT(DISTINCT person_id)
	FROM
		fin_sens_variables
	INTO
		current_total_study_members;

	PERFORM validate_total_study_members(
		'validate_sensitivity_variables 1',
		current_total_study_members);

	SELECT
		COUNT(DISTINCT person_id)
	FROM
		fin_life_stage_sensitivity_variables
	INTO
		current_total_study_members;

	PERFORM validate_total_study_members(
		'validate_sensitivity_variables 2',
		current_total_study_members);
	
	SELECT
		COUNT(DISTINCT person_id)
	FROM
		fin_sens_variables
	INTO
		study_members_in_one_not_the_other
	WHERE
		person_id NOT IN
			(SELECT
				person_id
			 FROM
			 	tmp_sensitivity_variables1);
	
	IF study_members_in_one_not_the_other != 0 THEN
		RAISE NOTICE 'ERROR. There are % study members were lost in the final sensitivity variable table ',
		study_members_in_one_not_the_other;
		validation_failed := TRUE;
	ELSE
		RAISE NOTICE 'VALIDATION PASSED.  There are no study members in the final sensitivity variables'; 
		RAISE NOTICE ' table that are not in the first sensitivity variable table.';
	END IF;
	
	IF validation_failed = TRUE THEN
		RAISE EXCEPTION 'Unacceptable errors occurred.';
	END IF;

END;
$$   LANGUAGE plpgsql;


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION common_cleanup_sensitivity_variable_data
 * -------------------------------------------------
 * Description
 * -----------
 * Used to clean up temporary tables that are created to manage sensitivity variables.  Note that
 * during debugging it is probably best not to call the function because the succession of 
 * temporary tables can provide clues to isolate errors.
 * ------------------------------------------------------------------------------------------------  
 */

CREATE OR REPLACE FUNCTION comm_cleanup_sensitivity_variable_data()
	RETURNS void AS 
$$
DECLARE

BEGIN

	DROP TABLE IF EXISTS tmp_sensitivity_variables1;
	DROP TABLE IF EXISTS tmp_sensitivity_variables2;
	DROP TABLE IF EXISTS tmp_sensitivity_variables3;
	DROP TABLE IF EXISTS tmp_sensitivity_variables4;
	DROP TABLE IF EXISTS tmp_sensitivity_variables5;
	DROP TABLE IF EXISTS tmp_sensitivity_variables6;

END;
$$   LANGUAGE plpgsql;

