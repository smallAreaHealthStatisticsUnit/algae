

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
 * Copyright 2017 Imperial College London, developed by the Small Area
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

/**
 * ================================================================================================= 
 * MODULE COMM_CLEAN_ADDR: Common life stage calculation methods
 * =================================================================================================
 * Description
 * -----------
 * Contains all the functions used to clean the address periods.  The goal of data cleaning in this
 * module is to ensure that study members are associated with exactly one location for each day of
 * their exposure time frames.  In order to re-purpose the address periods from an administrative
 * system to that they may support research questions, we have developed code to anticipate the
 * following kinds of problems:
 * 
 * Cleaning the address periods follows these steps:
 * (1) Ensure that address periods have non-blank values for their start and end dates.  Impute 
 *     missing start dates with the date of conception and impute missing end dates with the 
 *     current date.
 * (2) Calculate the duration of each address period.  The formula is end_date - start_date + 1 day
 *     to ensure that the end days are counted.  Duration is an important attribute used to sort
 *     the address periods.
 * (3) Order address periods first by person_id, second by start_date, third by duration and fourth by 
 *     geocode.  The ordering will be used throughout the rest of the address cleaning methods to
 *     establish an ascending order of address periods.  The ordering is what will be used to fix
 *     anomalies that exist between successive address periods.
 * (4) Before we fix temporal errors that may exist between address periods, we will attempt to 
 *     fix certain address periods having certain types of 'bad geocodes', which would result in 
 *     some study members having holes in their exposure assessment.  The holes would mean they spent
 *     significant amount of time during their exposure time frame at an unknown address or one which
 *     was not associated with exposure values.  For each address period, identify whether its geocode
 *     is valid, whether it is associated with exposure values.
 * (5) fix a specific type of bad geocode scenario where the address period is assumed to be an 
 *     incorrectly entered address that was later corrected by cohort staff in the next address.  
 *     An address period can be fixed if it has an invalid geocode, if it is immediately followed
 *     by an address period with a valid geocode and if the duration of the address period does 
 *     not overlap with any life stage by more than 25%.  "Fixing" means the address period having
 *     the bad geocode will be subsumed by the address period that follows it.  The bad address
 *     period will be marked and effectively ignored from further cleaning as if it never existed
 *     in the residential address history.  Later on in the protocol, any study member who has an
 *     address period within their exposure time frame which either has an invalid geocode or one
 *     that has no exposure values - will be excluded from exposure results.
 * (6) Use the ordering established in step (3) to identify gaps and overlaps between successive
 *     address periods a(n) and a(n+1). The numeric sign of a(n+1).start_date - a(n).end_date is
 *     used to determine whether periods are contiguous (0 difference), have a gap (positive 
 *     difference), or an overlap (a negative difference).
 * (7) Whereas in step 6 we identify gaps and overlaps, in this step we actually fix the problems.
 *     Gaps are fixed with the formula: a(n+1).start_date = a(n).end_date + 1 day.  Overlaps are
 *     fixed by the formula: a(n).end_date = a(n+1).start_date - 1 day.  When a(n) has an end date
 *     that exceeds its start date, it is an indicator that a(n) was completely subsumed by a(n + 1).
 *     In this scenario, a(n) is marked as a duplicate and ignored from further data cleaning.
 * (8) Audit adjusted boundaries for each address period.  [start_date_delta1, start_date_delta2]
 *     measures changes in the start date and [end_date_delta1, end_date_delta2] measures the 
 *     changes in an end date.  Each of these ranges identifies days of contention, where a lack
 *     of cleaning would have resulted in a person being able to occupy either a(n) or a(n + 1) on
 *     a given day.  This is referred to as a day of contention and forms the basis of the 
 *     approach for calculating exposure error.  The exposure measurement error for a given day
 *     of contention is the absolute difference between the pollution value at the cleaned assigned
 *     geocode and the pollution value at the other location.  If a(n).end_date changes, then the
 *     pollution value for [end_date_delta1, end_date_delta2] uses the geocode for a(n) as the 
 *     opportunity cost because cleaning assigned the days of contention to a(n+1).  Likewise, if
 *     a(n+1).start_date changes, the days in [start_date_delta1, start_date_delta2] are use the
 *     opportunity cost for a(n) because the extra days in the gap were assigned to a(n + 1) but
 *     could have belonged to a(n).
 * (9) Ensure that the cleaned address periods cover every day in the exposure time frame.  
 *     In most cases, the start date will be changed to the date of conception and, if necessary,
 *     the end date of the last address period will be changed to the last day in the time frame.
 *(10) Determine whether a cleaned address period is relevant.  For example, if the time frame is
 *     from conception until the last day of the first year of life, then an address period that 
 *     starts in year 4 of life is not relevant.  The "is relevant" flag is important because
 *     it helps ALGAE ignore bad geocode errors which will never occur within the exposure time 
 *     frame.
 *(11) Calculate days of contention for each address period.
 *(12) Validate the final cleaned collection of address periods.  Ensure that no records have been
 *     lost in the sequence of temporary tables. 
 *
 * ALGAE audits any changes it makes in flags that are later aggregated to provide variables for 
 * sensitivity analysis.  For example, the total number of days that need to be adjusted to fix 
 * gaps and overlaps within the exposure time frame are used to help scientists judge how much
 * cleaning they will allow before a person's address periods are not fit for their study.
 * 
 * Main Function
 * -------------
 * comm_process_addr_histories()
 *
 * Assumptions
 * ----------- 
 * (1) staging_addr_history_data table has been created and populated
 * (2) staging_study_member_data table has been created and populated
 * (3) early life stage calculations have been made that ensure that the
 *     conception date of each study member has been calculated.  Ensure
 *     that the function "calculate_early_stage_data" has been run.
 *
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

/* 
 * #DATABASE_TECHNIQUE 
 * In this module, we transform the address periods through a succession of temporary tables, all of 
 * which have the prefix "tmp_".  The use of temporary tables has benefits and drawbacks.  The 
 * drawbacks are that the tables may occupy unnecessary storage space, they can make the code appear 
 * longer than it has to be, and it may result in extra computation time because tables have to be 
 * written in one step and then read back in the next.  The benefits of using a sequence of temporary 
 * tables is that they provide an audit trail which can be useful for isolating bugs.
 *
 * In the code for this module, there are places where we could have combined more steps to produce
 * fewer tables or split merged steps into more tables.  Generally, we have used WITH statements to 
 * create in-memory tables that would not be re-used by other tables.  We've used temporary tables 
 * in order to isolate discrete protocol steps which may need to be isolated for testing.
 */

 /**
  * ------------------------------------------------------------------------------------------------ 
  * FUNCTION impute_addr_period_end_dates
  * -----------------------------------------
  * Description
  * -----------
  * Each address period processed by the scripts must have non-blank values for the start date and 
  * end date.  Where date fields are blank, the following imputations are applied:
  * (1) Blank start date: the date is imputed as the conception date of the
  *     study member.
  * (2) Blank end date: the date is imputed as the current time.
  *  
  * The program uses these imputations to support a relaxed policy of accepting address periods. Had 
  * a more restrictive policy been adopted, then a significant number of periods would have to be 
  * excluded from analysis.  Because the program is not designed to accommodate holes in residential 
  * address histories, the effect of excluding address periods would have been to exclude study 
  * members 
  * as well.
  *
  * Assumptions
  * -----------
  * (1) If a start date is blank, then we may assume that a study member has been living there since he 
  *     or she was conceived.
  * (2) If an end date is blank, then we assume that a study member has
  *     not moved and is still residing at the address.
  */
CREATE OR REPLACE FUNCTION comm_cln_addr_period_end_dates()
	RETURNS void AS 
$$
DECLARE


BEGIN

	-- Impute any blank end dates so that we can establish durations.  We need durations to
	-- help order the address periods.  Note that we are only considering study members who
	-- have no geocodes which are either invalid and have no exposures or are valid but place
	-- them outside the exposure area.
	DROP TABLE IF EXISTS tmp_addr_periods1;
	CREATE TABLE tmp_addr_periods1 AS
	SELECT 
		addr.original_row_number,
		addr.person_id,
		addr.geocode,
		CASE
			WHEN addr.start_date IS NULL AND addr.end_date IS NULL THEN 'imputed_both_dates'
			WHEN addr.start_date IS NULL AND addr.end_date IS NOT NULL THEN 'imputed_start_date'
			WHEN addr.start_date IS NOT NULL AND addr.end_date IS NULL THEN 'imputed_end_date'
			WHEN addr.start_date IS NOT NULL AND addr.end_date IS NOT NULL THEN 'no_imputation'
		END AS date_state, -- totals of each category are later used as sensitivity variables
		CASE
			WHEN addr.start_date IS NULL THEN time_frames.conception_date 
			ELSE addr.start_date
		END AS start_date,
		CASE
			WHEN addr.end_date IS NULL THEN NOW() -- assume they are still living there now 
			ELSE addr.end_date
		END AS end_date
	FROM
		staging_addr_history_data addr,
		tmp_overall_exp_time_frames time_frames
	WHERE
		addr.person_id = time_frames.person_id;

END;
$$   LANGUAGE plpgsql;

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION calculate_addr_period_durations
 * -----------------------------------------
 * Description
 * -----------
 * Once it can be guaranteed that an address period will have non-blank values for start date and end 
 * date, we calculate the duration.  The duration of the address period becomes important when we sort 
 * address periods for each person.  
 *
 * Assumptions
 * -----------
 * (1) The start and end dates of each address period are non-blank.  Ensure
 *     that function "impute_addr_period_end_dates" has been run.
 */
CREATE OR REPLACE FUNCTION comm_calc_addr_period_durations()
	RETURNS void AS 
$$
DECLARE

BEGIN

	-- Note that we add one to the difference to include both days in the
	-- interval.  Consider the interval [2000-05-13, 2000-05-15]. If you issue the 
	-- command:
	--    SELECT '2000-05-15'::date - '2000-05-13'::date
	-- it will return 2.  However, we want to include days 13, 14 and 15 which
	-- is 3 days.  Therefore we use +1 in the duration calculation

	DROP TABLE IF EXISTS tmp_addr_periods2;
	CREATE TABLE tmp_addr_periods2 AS
	SELECT
		original_row_number,
		person_id,
		geocode,
		date_state,
		start_date::date,
		end_date::date,
		end_date::date - start_date::date + 1 AS duration
	FROM
		tmp_addr_periods1;

END;
$$   LANGUAGE plpgsql;



/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION common_order_addresses
 * -------------------------------
 * Description
 * -----------
 * Establishes an order for arranging address periods.  The order is:
 * (1) person_id, ascending
 * (2) start_date, ascending
 * (3) duration, ascending
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION comm_order_addresses()
	RETURNS void AS 
$$
DECLARE

BEGIN


	-- Here we introduce the variable ith_residence, which is meant to help distinguish
	-- address periods as they are transformed by future steps.  Ith_residence is useful
	-- for when we want to help uniquely identify an address period.  
 	DROP TABLE IF EXISTS tmp_addr_periods3;
	CREATE TABLE tmp_addr_periods3 AS
	SELECT 
		original_row_number,
		person_id,
		row_number() OVER 
			(PARTITION BY 
			 	person_id 
			 ORDER BY 
				start_date, 
				duration, 
				geocode) AS ith_residence,
		geocode,
		start_date,
		end_date,
		date_state,
		duration
	FROM
		tmp_addr_periods2;
	ALTER TABLE tmp_addr_periods3 ADD PRIMARY KEY (person_id, ith_residence);
		
	
	-- Introduce the concept of an ith_residence_type, which describes the position 
	-- of an address period in a sequence.  In a chronological ordering of address periods
	-- for a study member, periods marked "first", "last" or "only" have special significance.
	-- First and last are significant because they are used to impute the overall time frame
	-- of a study member.  First is used to cover the exposure time between conception and
	-- the first reported start date.  Last may not have been completed because it is the last
	-- entry in an audit trail and the person has not changed address since. 
	--
	-- Both first and last address periods will usually be adjusted, but those adjustments do 
	-- not count as days of contention.  For example, when the start date of the first period 
	-- is pushed back so it covers the conception date, the start date delta variables which 
	-- mark a change in start date will be marked NULL.  However, if in two successive address
	-- periods the start date of a(n+1) has to be moved back so it equals the end date of a(n),
	-- the start date delta will reflect the change in start dates.
	--
	-- "only" address periods are also special.  Like "first" and "last", they may be adjusted
	-- to cover an overall exposure time frame for a study member and the adjustments will not 
	-- count as days of contention.  However, it is also impossible for "only" address periods
	-- to be part of gap or overlap calculations because that requires a minimum of two address
	-- periods. 	
 	DROP TABLE IF EXISTS tmp_addr_periods4;
	CREATE TABLE tmp_addr_periods4 AS		
	WITH maximum_ith_residence AS
		(SELECT
		 	person_id,
		 	COUNT(person_id) AS total_addr_periods,
		 	MAX(ith_residence) AS max_ith_residence -- establish the last address period
		 FROM
			tmp_addr_periods3
		 GROUP BY
			person_id)
	SELECT
		a.original_row_number,
		a.person_id,
		a.ith_residence,
		a.geocode,
		a.date_state,
		a.start_date,
		a.end_date,
		a.duration,
		CASE
			WHEN b.total_addr_periods = 1 THEN
				'only'
			WHEN ith_residence = 1 THEN 
				'first'
			WHEN ith_residence = b.max_ith_residence THEN 
				'last'
			ELSE 
				'middle'
		END as ith_residence_type
	FROM
		tmp_addr_periods3 a,
		maximum_ith_residence b
	WHERE
		a.person_id = b.person_id;
	
	ALTER TABLE tmp_addr_periods4 ADD PRIMARY KEY (person_id, ith_residence);

   	--DROP INDEX IF EXISTS ind_tmp_addr_periods4;
	--CREATE INDEX  ind_tmp_addr_periods4 ON tmp_addr_periods4(geocode);

END;
$$   LANGUAGE plpgsql;




/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION comm_assess_geocode_quality
 * --------------------------------------
 * Description
 * -----------
 * ALGAE attempts to assess study members' exposure for each day of their exposure time frames.  
 * It is critically important that the geocodes are good enough quality to use in the study because
 * they are what help link a study member's address periods with exposure values.  If the geocode
 * is poor, it may not be appropriate to link the two and as a result, it may not be feasible to
 * produce exposure results.
 *
 * The protocol tries to "fix" certain scenarios where an address period appear to have a bad 
 * geocode.  A bad geocode is one satisfies one or more of the following categories:
 * (1) invalid.  An invalid geocode could be one where geocoding software generated a blank
 *     geocode because it could not generate any match.  An invalid geocode could also be one
 *     where it has a non-blank geocode, but the geocoding effort still flags it as being of
 *     such poor quality that it shouldn't be used in analysis.
 * (2) has no exposure values.  The geocode may or may not be inside the exposure area, but it
 *     is not associated with any exposure values.
 * (3) is an out-of-bounds geocode.  This could be an address which lies outside the exposure area.
 * 
 * For now, we simply try to add a few more data quality flags to each record.
 *
 * Assumptions
 * -----------
 *
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION comm_assess_geocode_quality()
	RETURNS void AS 
$$
DECLARE

BEGIN

	--For each address period, we want to determine whether the geocode is invalid.
	--A geocode is invalid if at least one of these conditions is true:
	--   (1) it is blank or marked with an 'empty geocode' code
	--   (2) it is marked as 'N' in the staging_geocode_data table
	--   (3) the geocode found in the address periods table isn't in the staging_geocode_data table	
 	DROP TABLE IF EXISTS tmp_addr_period_geocode_validity;
 	CREATE TABLE tmp_addr_period_geocode_validity AS
	WITH valid_addr_geocodes1 AS
		(SELECT
		 	a.person_id,
			a.ith_residence,
			b.has_valid_geocode
		 FROM
			tmp_addr_periods4 a
		 LEFT JOIN 
		 	staging_geocode_data b
		 ON
			a.geocode = b.geocode)
	SELECT
		c.person_id,
		c.ith_residence,
		CASE
			WHEN c.has_valid_geocode IS NULL THEN
				'N' -- if there is no data quality value for a geocode, impute it with 'No'.
			ELSE
				c.has_valid_geocode
		END AS has_valid_geocode
	FROM
		valid_addr_geocodes1 c
	ORDER BY
		person_id,
		ith_residence;


	-- Create a table that can tell for each address and each
	-- pollutant, whether exposures exist
	DROP TABLE IF EXISTS geocode_exposure_exposures;
	CREATE TABLE geocode_exposure_exposures AS
	WITH unique_geocodes AS
		(SELECT DISTINCT
			geocode
		 FROM 
		 	staging_geocode_data),	
	name_exposures AS
		(SELECT DISTINCT
			geocode
		 FROM 
		 	staging_exp_data
		 WHERE 
		 	name IS NOT NULL),
	nox_rd_exposures AS
		(SELECT DISTINCT
			geocode
		 FROM 
		 	staging_exp_data
		 WHERE 
		 	nox_rd IS NOT NULL),
	pm10_gr_exposures AS
		(SELECT DISTINCT
			geocode
		 FROM 
		 	staging_exp_data
		 WHERE 
		 	pm10_gr IS NOT NULL),
	pm10_rd_exposures AS
		(SELECT DISTINCT
			geocode
		 FROM 
		 	staging_exp_data
		 WHERE 
		 	pm10_rd IS NOT NULL),
	pm10_tot_exposures AS
		(SELECT DISTINCT
			geocode
		 FROM 
		 	staging_exp_data
		 WHERE 
		 	pm10_tot IS NOT NULL)
	SELECT
		unique_geocodes.geocode,
		CASE
			WHEN name_exposures.geocode IS NOT NULL THEN 
				'Y'
			ELSE
				'N'
		END has_name_exposures,
		CASE
			WHEN nox_rd_exposures.geocode IS NOT NULL THEN 
				'Y'
			ELSE
				'N'
		END has_nox_rd_exposures,
		CASE
			WHEN pm10_gr_exposures.geocode IS NOT NULL THEN 
				'Y'
			ELSE
				'N'
		END has_pm10_gr_exposures,		
		CASE
			WHEN pm10_rd_exposures.geocode IS NOT NULL THEN 
				'Y'
			ELSE
				'N'
		END has_pm10_rd_exposures,
		CASE
			WHEN pm10_tot_exposures.geocode IS NOT NULL THEN 
				'Y'
			ELSE
				'N'
		END has_pm10_tot_exposures
	FROM
		unique_geocodes
	LEFT JOIN name_exposures
		ON unique_geocodes.geocode = name_exposures.geocode
	LEFT JOIN nox_rd_exposures
		ON unique_geocodes.geocode = nox_rd_exposures.geocode 	
	LEFT JOIN pm10_gr_exposures
		ON unique_geocodes.geocode = pm10_gr_exposures.geocode 	
	LEFT JOIN pm10_rd_exposures
		ON unique_geocodes.geocode = pm10_rd_exposures.geocode 	
	LEFT JOIN pm10_tot_exposures
		ON unique_geocodes.geocode = pm10_tot_exposures.geocode; 	

	ALTER TABLE tmp_addr_period_geocode_validity ADD PRIMARY KEY (person_id, ith_residence);
			
	--Next, for each address period, we want to identify any geocode which has no associated
	--exposure values.


	DROP TABLE IF EXISTS tmp_addr_period_geocodes_with_exp;
	CREATE TABLE tmp_addr_period_geocodes_with_exp AS
	WITH geocodes_with_exposures AS
		(SELECT DISTINCT
			geocode
		 FROM 
		 	staging_exp_data),
	addr_with_exp AS
		(SELECT
			a.person_id,
			a.ith_residence,
			b.geocode AS exp_geocode -- because of the left join, some of these may be null.
		 FROM
			tmp_addr_periods4 a
		 LEFT JOIN
			geocodes_with_exposures b
		 ON
			a.geocode = b.geocode)
	SELECT
		a.person_id,
		a.ith_residence,
		b.has_name_exposures,
		b.has_nox_rd_exposures,
		b.has_pm10_gr_exposures,
		b.has_pm10_rd_exposures,
		b.has_pm10_tot_exposures
	FROM
		tmp_addr_periods4 a
	LEFT JOIN geocode_exposure_exposures b --KLG 23/10/2016 change
	ON
		a.geocode = b.geocode
	ORDER BY
		person_id,
		ith_residence;
	ALTER TABLE tmp_addr_period_geocodes_with_exp ADD PRIMARY KEY (person_id, ith_residence);
	
 	DROP TABLE IF EXISTS tmp_addr_periods5;
	CREATE TABLE tmp_addr_periods5 AS		
	SELECT
		a.original_row_number,
		a.person_id,
		a.ith_residence,
		a.geocode,
		a.date_state,
		a.start_date::date,
		a.end_date::date,
		a.duration,
		a.ith_residence_type,
		b.has_valid_geocode,
		CASE
			WHEN c.has_name_exposures IS NULL THEN
				'N'
			ELSE
				c.has_name_exposures
		END AS has_name_exposures,
		CASE
			WHEN c.has_nox_rd_exposures IS NULL THEN
				'N'
			ELSE
				c.has_nox_rd_exposures
		END AS has_nox_rd_exposures,
		CASE
			WHEN c.has_pm10_gr_exposures IS NULL THEN
				'N'
			ELSE
				c.has_pm10_gr_exposures
		END AS has_pm10_gr_exposures,
		CASE
			WHEN c.has_pm10_rd_exposures IS NULL THEN
				'N'
			ELSE
				c.has_pm10_rd_exposures
		END AS has_pm10_rd_exposures,
		CASE
			WHEN c.has_pm10_tot_exposures IS NULL THEN
				'N'
			ELSE
				c.has_pm10_tot_exposures
		END AS has_pm10_tot_exposures
	FROM
		tmp_addr_periods4 a,
		tmp_addr_period_geocode_validity b,
		tmp_addr_period_geocodes_with_exp c
	WHERE
		a.person_id = b.person_id AND
		a.ith_residence = b.ith_residence AND
		a.person_id = c.person_id AND
		a.ith_residence = c.ith_residence
	ORDER BY
		a.person_id,
		a.ith_residence;

	ALTER TABLE tmp_addr_periods5 ADD PRIMARY KEY (person_id, ith_residence);


END;
$$   LANGUAGE plpgsql;

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION common_id_and_fix_bad_geocodes
 * ---------------------------------------------
 * Description
 * -----------
 * If study members have a bad geocode within their exposure time frame, it usually means there
 * is nothing we can do about it and we need to exclude them from having exposure results.  This is 
 * because their residential address histories will have a hole in them where we can't tell where 
 * they were on a given exposure day.  
 *
 * We have developed an approach for fixing some kinds of bad geocode scenarios by considering the 
 * original intent of the database system that was used to capture the address histories.  The 
 * residential address histories are assumed to come from a system that maintained an audit trail 
 * of current addresses for cohort members.  We assume that in such a system, each change made to 
 * a current address caused a new address record to be created.  It is therefore possible that an 
 * ordered list of address periods, a(n + 1) could either be a new address or a correction relative 
 * to a(n).  
 *
 * A scenario such as the one shown below could be fixed if we assumed that an address period with 
 * a bad geocode was fixed in the subsequent entry:
 *
 *  person_id  address      geocode                    start date  end date
 *  1234       34 Min St    (blank value)              05-11-2007  10-11-2007 <== FIX
 *  1234       34 Main St.  g2 (valid, has exposures)  12-11-2007  12-11-2008
 *
 * Here the record could have been initially filled in wrong and it was later corrected. In this 
 * case we would assume that there was no change of address and make the following correction:
 *  person_id  address      geocode                    start date  end date
 *  1234       34 Min St    (blank value)              05-11-2007  10-11-2007 
 *  1234       34 Main St.  g2 (valid, has exposures)  05-11-2007  12-11-2008
 *
 * We would then retain the 34 Min St address for provenance but assume in all other data cleaning
 * operations that a person stayed at the same address (the second one) for longer.  The first 
 * address period is effectively marked as being deleted and not used in any further processing 
 * steps.
 * 
 * It may be the case that the person stayed at an invalid geocode for so long it would not be 
 * reasonable for a study group to make the fix.  Therefore, we have come up with three criteria
 * that an address period must satisfy if they can be fixed:
 * (1) The address period must have an invalid geocode
 * (2) The address period must be followed by an address period which has a valid geocode
 * (3) The address period does not have an overlap of 25% or more with any life stage.
 *
 * An address period a(n) which meets all three criteria will be fixed in the following way:
 * (1) a(n+1).start_date = a(n).start_date
 * (2) a(n) will be marked so that future steps can ignore it and treat a(n + 1) as if it covered
 *     the time for a(n) and a(n + 1).
 *
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION comm_id_and_fix_bad_geocodes()
	RETURNS void AS 
$$
DECLARE

BEGIN

	-- for each address period, calculate its maximum overlap with
	-- each of a person's life stages.  We're interested in the greatest overlap
	-- to satisfy fixing criteria 3:
 	DROP TABLE IF EXISTS tmp_max_percent_life_stage_overlap;
	CREATE TABLE tmp_max_percent_life_stage_overlap AS	
	WITH addr_life_stage_overlaps1 AS 
		(SELECT
			a.person_id,
			a.ith_residence,
			calculate_days_overlap(
				a.start_date, 
				a.end_date, 
				b.start_date, 
				b.end_date) AS days_overlap,
			b.life_stage_duration
		 FROM
			tmp_addr_periods5 a,
			fin_general_life_stage_data b
		WHERE
			a.person_id = b.person_id),
	addr_life_stage_overlaps2 AS 
		(SELECT
			person_id,
			ith_residence,
			days_overlap,
			life_stage_duration,
			(days_overlap * 100.0) / life_stage_duration AS percent_overlap 
		 FROM
		 	addr_life_stage_overlaps1),
	max_addr_life_stage_overlaps AS
		(SELECT
			person_id,
			ith_residence,
			MAX(percent_overlap) AS maximum_life_stage_overlap
		 FROM
			addr_life_stage_overlaps2
		 GROUP BY
			person_id,
			ith_residence)			
	SELECT
		person_id,
		ith_residence,
		maximum_life_stage_overlap,
		CASE
			WHEN maximum_life_stage_overlap >= 25.0 THEN 
				'Y' -- if its' more than 25% then we won't try to fix it.
			ELSE
				'N'
		END AS shows_significant_life_stage_overlap
	FROM
		max_addr_life_stage_overlaps
	ORDER BY
		person_id,
		ith_residence;

	ALTER TABLE tmp_max_percent_life_stage_overlap ADD PRIMARY KEY (person_id, ith_residence);

	/**
	  * #DISABLE_FIXING_BAD_ADDRESS_PERIODS
   	  * #DESIGN_FOR_REUSE
   	  * 
   	  * You may want to disable our feature for fixing bad address periods.
   	  * You may want to to this for one of two reasons: (1) to remove our 
   	  * own sense of judgement from your results and (2) to allow you to 
   	  * intentionally make use of injecting bad address periods into address
   	  * histories.  Injecting bad address periods may be useful in scenarios
   	  * where you have gaps between address periods that you want to simply 
   	  * treat with null value contributions in the results.  For example, if
   	  * your addresses describe work place locations, then you may want to 
   	  * preserve gaps between them to indicate you have no data owing to 
   	  * occupational pollution for periods when someone wasn't working.
   	  *
   	  * To disable this, comment out the existing code used to create the 
   	  * tmp_addr_periods6 table.  Then copy it and edit the copy.
   	  * You will want to make sure a field called is_fixable_invalid_geocode 
   	  * exists but is always set to 'N'.
   	  * 
   	  * I think the following might work:
   	  *
   	  * Replace:
   	  *		CASE
	  *			WHEN a.has_valid_geocode = 'N' AND 
	  *				LEAD(a.has_valid_geocode) OVER 
	  *					(PARTITION 
	  *						BY a.person_id 
	  *					 ORDER BY 
	  *						a.ith_residence) = 'Y' AND
	  *				b.shows_significant_life_stage_overlap = 'N' THEN
	  *					'Y'
	  *			ELSE
	  *				'N' --
	  *		END AS is_fixable_invalid_geocode,   	  
   	  *
   	  * with:
   	  *		'N' AS is_fixable_invalid_geocode
   	  *
   	  * The rest of the code used to create tmp_addr_periods6 should then NOT
   	  * be triggered because of the 'N' value.  For the rest of the protocol,
   	  * it should assume that none of the address periods matched criteria for
   	  * being fixed.  The bad address periods will then be preserved, although
   	  * data cleaning may change their temporal boundaries if they have gaps or
   	  * overlaps with neighbouring address periods.
   	  *
   	  */
 

	-- Create a flag to indicate whether it is a fixable_invalid_geocode.  Note that
	-- the flag may be No because the address period is in fact valid.  Also note that
	-- if we have had to subsume a(n) with a(n + 1), then the duration of a(n + 1) would
	-- have to be recalculated.
	DROP TABLE IF EXISTS tmp_addr_periods6;
	CREATE TABLE tmp_addr_periods6 AS	
	WITH fixable_geocodes AS
		(SELECT
			a.original_row_number,
			a.person_id,
			a.ith_residence,
			a.ith_residence_type,
			a.geocode,
			a.has_valid_geocode,
			a.has_name_exposures,
			a.has_nox_rd_exposures,
			a.has_pm10_gr_exposures,
			a.has_pm10_rd_exposures,
			a.has_pm10_tot_exposures,
			b.maximum_life_stage_overlap,
			CASE
				WHEN a.has_valid_geocode = 'N' AND 
					LEAD(a.has_valid_geocode) OVER 
						(PARTITION 
							BY a.person_id 
						 ORDER BY 
							a.ith_residence) = 'Y' AND
					b.shows_significant_life_stage_overlap = 'N' THEN
						'Y'
				ELSE
					'N' --
			END AS is_fixable_invalid_geocode,
			a.start_date,
			a.end_date,
			a.date_state,
			a.duration			
		 FROM
			tmp_addr_periods5 a,
			tmp_max_percent_life_stage_overlap b
		WHERE
			a.person_id=b.person_id AND
			a.ith_residence=b.ith_residence
		ORDER BY 
			a.person_id,
			a.ith_residence)
	SELECT
		original_row_number,
		person_id,
		ith_residence,
		geocode,
		date_state,
		CASE
			WHEN LAG(is_fixable_invalid_geocode) OVER
				(PARTITION BY 
					person_id
				 ORDER BY
					ith_residence) = 'Y' THEN
				 	
				 LAG(start_date) OVER
					(PARTITION BY
						person_id
					 ORDER BY
						ith_residence)
			ELSE
				start_date
		END AS start_date, --recalculate start date
		end_date,
		CASE
			WHEN LAG(is_fixable_invalid_geocode) OVER
				(PARTITION BY 
					person_id
				 ORDER BY
					ith_residence) = 'Y' THEN
				 
				end_date - 	
				LAG(start_date) OVER
					(PARTITION BY
						person_id
					 ORDER BY
					ith_residence) + 1
			ELSE
				duration
		END AS duration, -- recalculate duration
		ith_residence_type,
		has_valid_geocode,
		has_name_exposures,
		has_nox_rd_exposures,
		has_pm10_gr_exposures,
		has_pm10_rd_exposures,
		has_pm10_tot_exposures,		
		maximum_life_stage_overlap,		
		is_fixable_invalid_geocode AS is_fixed_invalid_geocode
	FROM
		fixable_geocodes;

	ALTER TABLE tmp_addr_periods6 ADD PRIMARY KEY (person_id, ith_residence);

   	--DROP INDEX IF EXISTS ind_tmp_addr_periods6;
	--CREATE INDEX  ind_tmp_addr_periods6 ON tmp_addr_periods6(is_fixed_invalid_geocode);

	-- If we decide to ignore an address period and assume that it is subsumed by the 
	-- next address period, we may have to recalculate ith_residence_type, which tells
	-- if an address period is an only, first, middle or last period.
	DROP TABLE IF EXISTS tmp_addr_periods7;
	CREATE TABLE tmp_addr_periods7 AS
	WITH addr_without_fixable_geocodes1 AS	
	(SELECT
		person_id,
		ith_residence,
		ith_residence_type,
		CASE
			WHEN LAG(ith_residence) OVER 
				(PARTITION BY person_id
				 ORDER BY ith_residence) IS NULL THEN
				FALSE
			ELSE
				TRUE
		END has_previous,
		CASE
			WHEN LEAD(ith_residence) OVER 
				(PARTITION BY person_id
				 ORDER BY ith_residence) IS NULL THEN
				FALSE
			ELSE
				TRUE
		END has_next
	 FROM
		tmp_addr_periods6
	WHERE
		is_fixed_invalid_geocode='N'
	ORDER BY
		person_id,
		ith_residence),
	addr_without_fixable_geocodes2 AS	
		(SELECT
			person_id,
			ith_residence,
			CASE
				WHEN has_previous = FALSE AND has_next = FALSE THEN
					'only'
				WHEN has_previous = FALSE AND has_next = TRUE THEN
					'first'
				WHEN has_previous = TRUE AND has_next = FALSE THEN
					'last'  
				WHEN has_previous = TRUE AND has_next = TRUE THEN
					'middle'
				ELSE
					'error'
			END AS ith_residence_type
		FROM
			addr_without_fixable_geocodes1)
	SELECT
		a.original_row_number,
		a.person_id,
		a.ith_residence,
		a.geocode,
		a.date_state,
		a.start_date,
		a.end_date,
		a.duration,
		CASE
			WHEN b.ith_residence_type IS NOT NULL THEN
				b.ith_residence_type
			ELSE
				a.ith_residence_type
		END AS ith_residence_type,
		a.has_valid_geocode,
		a.has_name_exposures,
		a.has_nox_rd_exposures,
		a.has_pm10_gr_exposures,
		a.has_pm10_rd_exposures,
		a.has_pm10_tot_exposures,		
		a.maximum_life_stage_overlap,
		a.is_fixed_invalid_geocode
	FROM
		tmp_addr_periods6 a
	LEFT JOIN
		addr_without_fixable_geocodes2 b
	ON
		a.person_id = b.person_id AND
		a.ith_residence = b.ith_residence;

	ALTER TABLE tmp_addr_periods7 ADD PRIMARY KEY (person_id, ith_residence);

   	--DROP INDEX IF EXISTS ind_tmp_addr_periods7;
	--CREATE INDEX ind_tmp_addr_periods7 ON tmp_addr_periods7(is_fixed_invalid_geocode);


END;
$$   LANGUAGE plpgsql;

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION identify_gaps_and_overlaps
 * -----------------------------------
 * Description
 * -----------
 * In order to ensure that each person has a temporally contiguous address history, we have to fix 
 * any gaps and overlaps that may exist between periods.  Fixing these problems is critically 
 * dependent on how we sort the address periods.
 
 * The address periods are sorted based on the ascending value of the following four factors:
 * (1) person_id -  we partition the rows based on this
 * (2) start_date - within the periods for a given person_id, rank first by chronologically 
 *                  ordered start date
 * (3) duration -   where two starts are the same, order the address periods next by order of 
 *                  increasing duration
 * (4) geocode  -   where two address periods for the same person have the same start date 
 *                  and duration, order next by geocode.  We would only need to use this 
 *                  sorting factor when the administrative records show duplicate entries at 
 *                  different locations.
 *
 * We need to assign an ith address for each period so we can identify the first and last 
 * address periods.  The first address period for each person will later be adjusted so that it 
 * can start at the conception date.
 *
 *  
 * Let a(n), a(n+1) be any two successively address periods using the ordering above, and relating 
 * to the same case study member.  We're using postgresql's analytic functions that use the 
 * notion of lag and lead for any two successive periods.  
 * 
 * We have the notion of current, lag and lead rows.  We're going to go through each row of the 
 * ordered table, and consider the row that is immediately before it.  An expression like 
 * "LAG(end_date, 1)" means "look at the end date field that is 1 row before the current one".  
 * We need this expression so that we can calculate: 
 * 
 *      a(n+1).start_date - a(n).end_date
 * 
 * We need to consider two cases: 
 *   (1) the first address period for a given person, where there isn't a previous row and 
 *   (2) any other address period, which will have a previous row.  The scenarios are shown below:
 *
 * Case 1 : we're currently at the first address period for a study member
 * Partitioned by person_id
 * eg: person 123 address periods
 * 	 
 *                 geocode  start_date              end_date            duration
 *                 -------  ----------              --------            --------
 * (trying to access LAG of 1 wrt current row gives a null)
 * current row --> a(n+1)     a(n+1).start_date     a(n+1).end_date 
 *
 * Case 2 : we're currently at at least the second address period for a 
 * study member 
 * Partitioned by person_id
 * eg: person 123 address periods
 * 	 
 *                 geocode  start_date   end_date duration
 *                 -------  ----------   -------- --------
 * LAG row ------> a(n)       a(n).start_date     a(n).end_date   
 * current row --> a(n+1)     a(n+1).start_date   a(n+1).end_date 
 *
 * Assumptions:
 * (1) person_id, start_date, duration are not null
 * ------------------------------------------------------------------------------------------------ 
 */ 
CREATE OR REPLACE FUNCTION comm_id_gaps_and_overlaps()
	RETURNS void AS 
$$
DECLARE

BEGIN

	-- Introduce the concept of a "fit_extent".  It is used in later steps to determine
	-- what kind of fit exists between two successive address periods.  It is the sign
	-- rather than the magnitude that is important in fit_extent.  The meanings are:
	--
	-- fit_extent = 0: the address periods are temporally contiguous.
	-- fit_extent > 0: it is a gap
	-- fit_extent < 0: it is an overlap
	--
	-- fit_extent is used

 	DROP TABLE IF EXISTS tmp_addr_periods8;
	CREATE TABLE tmp_addr_periods8 AS
	WITH addr_without_fixable_geocodes AS	
		(SELECT 
			person_id,
			ith_residence,
			ith_residence_type,
			CASE
				WHEN ith_residence_type='only' OR
					ith_residence_type='first' THEN
					0 -- it cannot be subject to gaps and overlaps
				ELSE
					start_date::date - 
					LAG(end_date::date, 1) OVER 
						(PARTITION BY 
							person_id 
						 ORDER BY 
							ith_residence) - 1 -- it can only be a gap  
			END AS fit_extent			
		FROM 
			tmp_addr_periods7
		WHERE
			is_fixed_invalid_geocode = 'N')
	SELECT
		a.original_row_number,
		a.person_id,
		a.ith_residence,
		a.geocode,
		a.date_state,
		a.start_date,
		a.end_date,
		a.duration,		
		a.ith_residence_type,
		a.has_valid_geocode,
		a.has_name_exposures,
		a.has_nox_rd_exposures,
		a.has_pm10_gr_exposures,
		a.has_pm10_rd_exposures,
		a.has_pm10_tot_exposures,		
		a.maximum_life_stage_overlap,		   
		a.is_fixed_invalid_geocode,
		b.fit_extent
	FROM
		tmp_addr_periods7 a
	LEFT JOIN
		addr_without_fixable_geocodes b
	ON
		a.person_id = b.person_id AND
		a.ith_residence = b.ith_residence;
		
	ALTER TABLE tmp_addr_periods8 ADD PRIMARY KEY (person_id, ith_residence);

   	--DROP INDEX IF EXISTS ind_tmp_addr_periods8;
	--CREATE INDEX ind_tmp_addr_periods8 ON tmp_addr_periods8(is_fixed_invalid_geocode);


END;
$$   LANGUAGE plpgsql;


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION fix_gaps_and_overlaps
 * ------------------------------
 * Description
 * -----------
 * This function examines each address period and takes one of four actions:
 * (1) adjusts the start date to correct a gap with the previous 
 * (2) adjusts the end date correct an overlap
 * (3) correct both
 * (4) correct nothing.
 *
 * Two factors are used to determine which course of action to take:
 * (1) the sign of the fit extent value of the address period we're interested in.
 * (2) whether that fit extent belongs to the current address period or the one which follows 
 *     it.
 *
 * In any address period, the fit extent will indicate one of three things:
 * (1) negative number: the end date of the previous address period needs to be adjusted in order 
 *     to correct an overlap
 * (2) positive number: the start date of the current address period needs to be adjusted in order 
 *     to correct a gap
 * (3) zero: the address period shows no gaps or overlaps with respect to the address period which 
 *     precedes it.  No correction is necessary.
 *
 * The fit extent value for an address period can indicate a change needs to be made to its start 
 * date or the end date of the previous period.  In order to keep track of current, previous and 
 * next periods, we rely on PostgreSQL's analytic functions "LAG" and "LEAD".  Here we see how fit 
 * extent, current row and lead row are used together to make corrections.
 * 
 * Processing address period 1:
 *
 *            ith_residence  Person ID Geocode  Start Date   End Date     Fit Extent  Adj. Start   Adj. End
 * Current--> 1              111       a1       01-May-2015  15-May-2015  0           01-May-2015  09-May-2015
 * Lead-->    2              111       a2       10-May-2015  20-May-2015  -6          
 *            3              111       a3       25-May-2015  30-May-2015  6
 *            4              111       a4       28-May-2015  31-May-2015  -3
 *
 *  Start date is unchanged because fit extent is 0. The end date is changed to 09-May-2015 because
 *  the fit extent of the following row (the "lead row") is -6.
 *
 * Processing address period 2:
 *
 *            ith_residence  Person ID Geocode  Start Date   End Date     Fit Extent  Adj. Start   Adj. End
 *            1              111       a1       01-May-2015  15-May-2015  0           01-May-2015  09-May-2015
 * Current--> 2              111       a2       10-May-2015  20-May-2015  -6          10-May-2015  20-May-2015  
 * Lead -->   3              111       a3       25-May-2015  30-May-2015  6
 *            4              111       a4       28-May-2015  31-May-2015  -3
 *
 * Start date remains unchanged because fit extent is a negative number. End date remains unchanged because
 * the fit extent of the following period is a positive number.
 *
 * Processing address period 3:
 *
 *            ith_residence  Person ID Geocode  Start Date   End Date     Fit Extent  Adj. Start   Adj. End
 *            1              111       a1       01-May-2015  15-May-2015  0           01-May-2015  09-May-2015
 *            2              111       a2       10-May-2015  20-May-2015  -6          10-May-2015  20-May-2015  
 * Current--> 3              111       a3       25-May-2015  30-May-2015  6           21-May-2015  27-May-2015
 * Lead-->    4              111       a4       28-May-2015  31-May-2015  -3
 *
 * Start date is adjusted to 21-May-2015 because the fit extent of the current period is a positive 
 * number (6).
 * End date is adjusted to 27-May-2015 because the fit extent of the following period is a negative 
 * number (-3).
 *
 * Processing address period 4:
 *
 *            ith_residence  Person ID Geocode  Start Date   End Date     Fit Extent  Adj. Start   Adj. End
 *            1              111       a1       01-May-2015  15-May-2015  0           01-May-2015  09-May-2015
 *            2              111       a2       10-May-2015  20-May-2015  -6          10-May-2015  20-May-2015  
 *            3              111       a3       25-May-2015  30-May-2015  6           21-May-2015  27-May-2015
 * Current--> 4              111       a4       28-May-2015  31-May-2015  -3          28-May-2015  31-May-2015
 * Lead-->NULL (no more periods)
 *
 * Start date remains unchanged because fit extent of the current period is a negative number.
 * End date remains unchanged because there is no lead address period. 
 *
 * Assumptions
 * -----------
 * (1) Function "common_id_gaps_and_overlaps" has been called 
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION comm_fix_gaps_and_overlaps()
	RETURNS void AS 
$$
DECLARE

BEGIN

	/*
	 * #CHANGE_GAP_OVERLAP_FIXING_BEHAVIOUR
	 * #DESIGN_FOR_REUSE
	 *
	 * It seems unlikely, but you may decide that when you encounter a gap or
	 * overlap, you want to preserve the end date as a stronger signal than
	 * the start date.  This would imply that study members would have not yet
	 * left a location by the time of the end date for the address period.  
	 *
	 * This would only seem likely if people in general notified cohorts of their
	 * moves before they actually moved. If you still want to change the behaviour
	 * then you will probably only need to swap CASE WHEN fragments so that
	 * for a gap between a(n) and a(n+1), a(n).end_date = a(n+1).start_date - 1 day
	 * and for an overlap, a(n+1).start_date = a(n).end_date + 1 day.
	 *
	 */
	--Adjust start and end dates as needed
	DROP TABLE IF EXISTS tmp_addr_periods9;
	CREATE TABLE tmp_addr_periods9 AS 
	WITH addr_without_fixable_geocodes AS	
		(SELECT
			person_id,
			ith_residence,
			CASE
				WHEN fit_extent > 0 THEN --focus on current row
					(LAG(end_date) OVER 
					(PARTITION BY 
						person_id 
					 ORDER BY 
					 ith_residence)) + INTERVAL '1 day' -- only circumstance when start date changes. 
				ELSE 
					start_date 
		 	END AS adjusted_start_date,
		 	CASE
		 		WHEN LEAD(fit_extent) OVER --focus on the 
					(PARTITION BY 
						person_id 
					ORDER BY 
						ith_residence) < 0 THEN 
					(LEAD(start_date) OVER 
						(PARTITION BY person_id 
					 	 ORDER BY 
			        		ith_residence)) - INTERVAL '1 day'
			    ELSE 
					end_date
			END AS adjusted_end_date
		FROM 
			tmp_addr_periods8
		WHERE
			is_fixed_invalid_geocode = 'N')
	SELECT
		a.original_row_number,
		a.person_id,
		a.ith_residence,
		a.geocode,
		a.date_state,
		a.start_date,
		a.end_date,
		a.duration,		
		a.ith_residence_type,
		a.has_valid_geocode,
		a.has_name_exposures,
		a.has_nox_rd_exposures,
		a.has_pm10_gr_exposures,
		a.has_pm10_rd_exposures,
		a.has_pm10_tot_exposures,		
		a.maximum_life_stage_overlap,		
		a.is_fixed_invalid_geocode,
		a.fit_extent,
		b.adjusted_start_date,
		b.adjusted_end_date
	FROM
		tmp_addr_periods8 a
	LEFT JOIN
		addr_without_fixable_geocodes b
	ON
		a.person_id = b.person_id	AND 
		a.ith_residence = b.ith_residence;
		
	ALTER TABLE tmp_addr_periods9 ADD PRIMARY KEY (person_id, ith_residence);


   	--DROP INDEX IF EXISTS ind_tmp_addr_periods9;
	--CREATE INDEX ind_tmp_addr_periods9 ON tmp_addr_periods9(is_fixed_invalid_geocode);

END;
$$   LANGUAGE plpgsql;


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION common_audit_adjusted_days
 * -----------------------------------
 * Description
 * -----------
 * This function does three tasks that are related to auditing adjusted days of each period:
 * (1) Associates a description whatever changes were made
 * (2) Establishes the time frame between original and adjusted start dates
 * (3) Establishes the time frame between original and adjusted end dates
 *
 * Each address period is associated with a fit_type, which may be one of the following:
 * (1) C: Contiguous.  No changes were made.
 * (2) D: Address period deleted.  This period was completely subsumed by the following period.
 * (3) O: End date was adjusted because of an overlap with the next period.
 * (4) G: Start date was adjusted because of a gap with the previous period.
 * (5) B: Start date was adjusted because of a gap and end date was adjusted because of an overlap.
 *
 * Differences in start dates are expressed as their own time periods.  The same is done for end 
 * dates.  These change periods are later used to determine whether a given day is a day of 
 * contention.
 * ------------------------------------------------------------------------------------------------  
 */
CREATE OR REPLACE FUNCTION comm_audit_adjusted_days()
	RETURNS void AS 
$$
DECLARE

BEGIN

	-- Here we try to audit changes made to the address periods.  If the start date of an 
	-- address period is adjusted, it will always be to fill a gap and [start_date_delta1, 
	-- start_date_delta2] will describe the duration of that gap.  Similarly, if the
	-- end date is adjusted, it will always be to fix an overlap and [end_date_delta1,
	-- end_date_delta2] will represent the duration of that overlap.  Deleted address
	-- periods are a special kind of gap where the entire period is subsumed by one that
	-- follows it.  In this case, [end_date_delta1, end_date_delta2
	-- 
	DROP TABLE IF EXISTS tmp_addr_periods10;
	CREATE TABLE tmp_addr_periods10 AS		
	WITH addr_without_fixable_geocodes AS	
		(SELECT
			person_id,
			ith_residence,
			ith_residence_type,
			ABS(extract(day FROM start_date - adjusted_start_date)) + 
			ABS(extract(day FROM end_date - adjusted_end_date)) AS days_changed, 
			CASE
				WHEN start_date = adjusted_start_date AND 
					end_date = adjusted_end_date THEN 
		  			'C'
				WHEN adjusted_start_date > adjusted_end_date THEN
					'D'		    	
				WHEN start_date = adjusted_start_date AND
					end_date != adjusted_end_date THEN 
					'O'
				WHEN start_date != adjusted_start_date AND
					end_date = adjusted_end_date THEN
					'G'
				WHEN start_date != adjusted_start_date AND
					end_date != adjusted_end_date THEN
					'B'
				ELSE
					'C'
			END AS fit_type,
			LAG(geocode) OVER
				(PARTITION BY 
					person_id
				 ORDER BY
					ith_residence) AS previous_geocode,
			LEAD(geocode) OVER
				(PARTITION BY 
					person_id
				 ORDER BY
					ith_residence) AS next_geocode,
			CASE
				WHEN adjusted_start_date < start_date THEN 
					adjusted_start_date
				ELSE 
					NULL
			END AS start_date_delta1,
			CASE
				WHEN adjusted_start_date < start_date THEN 
					start_date - INTERVAL '1 day' -- start date = adj_start date or start_date > adjusted_start_date
				ELSE 
					NULL
			END AS start_date_delta2,
			CASE
				WHEN adjusted_start_date > adjusted_end_date THEN
					start_date -- this overlap is a deleted period
				WHEN adjusted_end_date < end_date THEN 
					adjusted_end_date + INTERVAL '1 day'
				ELSE 
					NULL
			END AS end_date_delta1,
			CASE
				WHEN adjusted_start_date > adjusted_end_date THEN
					end_date -- this overlap is a deleted period --KLG: Added 12/11/2015
				WHEN adjusted_end_date < end_date THEN 
					end_date
				ELSE 
					NULL
			END AS end_date_delta2
		FROM
			tmp_addr_periods9
		WHERE
			is_fixed_invalid_geocode = 'N')
	SELECT
		a.original_row_number,
		a.person_id,
		a.ith_residence,
		a.geocode,
		a.date_state,
		a.start_date,
		a.end_date,
		a.duration,	   	
		a.ith_residence_type,
		a.has_valid_geocode,
		a.has_name_exposures,
		a.has_nox_rd_exposures,
		a.has_pm10_gr_exposures,
		a.has_pm10_rd_exposures,
		a.has_pm10_tot_exposures,		
		a.maximum_life_stage_overlap,
		a.is_fixed_invalid_geocode,
		a.fit_extent,
		a.adjusted_start_date,
		a.adjusted_end_date,
		b.days_changed,
		b.fit_type,
		b.previous_geocode,
		b.next_geocode,
		b.start_date_delta1,
		b.start_date_delta2,
		b.end_date_delta1,
		b.end_date_delta2
	FROM
		tmp_addr_periods9 a
	LEFT JOIN
		addr_without_fixable_geocodes b
	ON
		a.person_id = b.person_id AND
		a.ith_residence = b.ith_residence;
    	
    ALTER TABLE tmp_addr_periods10 ADD PRIMARY KEY (person_id, ith_residence);
   	--DROP INDEX IF EXISTS ind_tmp_addr_periods10;
	--CREATE INDEX ind_tmp_addr_periods10 ON tmp_addr_periods10(is_fixed_invalid_geocode);

END;
$$   LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION comm_id_geocode_contenders()
	RETURNS void AS 
$$
DECLARE

BEGIN

	DROP TABLE IF EXISTS tmp_addr_periods11;
	CREATE TABLE tmp_addr_periods11 AS		
	WITH addr_without_fixable_geocodes AS	
		(SELECT
			person_id,
			ith_residence,	   		
			CASE
				WHEN start_date_delta1 IS NOT NULL AND start_date_delta2 IS NOT NULL THEN
					LAG(geocode) OVER 
					(PARTITION BY 
						person_id 
		         	ORDER BY
						ith_residence)
				ELSE
					NULL
			END AS start_date_contender,
			CASE
				WHEN end_date_delta1 IS NOT NULL AND end_date_delta2 IS NOT NULL THEN
					geocode -- if we have adjusted end date, the current geocode IS the opportunity cost
				ELSE
					NULL
			END AS end_date_contender
		FROM
			tmp_addr_periods10
		WHERE
			is_fixed_invalid_geocode = 'N')
	SELECT
		a.original_row_number,
		a.person_id,
		a.ith_residence,
		a.geocode,
		a.date_state,
		a.start_date,
		a.end_date,
		a.duration,
		a.ith_residence_type,
		a.has_valid_geocode,
		a.has_name_exposures,
		a.has_nox_rd_exposures,
		a.has_pm10_gr_exposures,
		a.has_pm10_rd_exposures,
		a.has_pm10_tot_exposures,		
		a.maximum_life_stage_overlap,
		a.is_fixed_invalid_geocode,
		a.fit_extent,
		a.adjusted_start_date,
		a.adjusted_end_date,
		a.days_changed,
		a.fit_type,
		a.previous_geocode,
		a.next_geocode,
		a.start_date_delta1,
		a.start_date_delta2,
		a.end_date_delta1,
		a.end_date_delta2,
		b.start_date_contender,
		b.end_date_contender
	FROM
		tmp_addr_periods10 a
	LEFT JOIN
		addr_without_fixable_geocodes b
	ON
		a.person_id = b.person_id AND
		a.ith_residence = b.ith_residence;

	ALTER TABLE tmp_addr_periods11 ADD PRIMARY KEY (person_id, ith_residence);
   	--DROP INDEX IF EXISTS ind_tmp_addr_periods11;
	--CREATE INDEX ind_tmp_addr_periods11 ON tmp_addr_periods11(is_fixed_invalid_geocode);

END;
$$   LANGUAGE plpgsql;

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION common_expand_periods_to_time_frame
 * --------------------------------------------
 * Description
 * -----------
 * Once the address periods have been cleaned, we need to ensure that they cover every day of
 * the exposure time frame.  We ensure that the earliest days of the exposure time frame are
 * covered by changing the start date of the earliest address period to be the date of conception.
 * 
 * If the end date of the last address period be less than the end of the exposure time frame, we 
 * change the end date to be the end of time frame date.  Neither kind of adjustment is counted as 
 * a day of contention or as a day that was changed.  However, scientists will be able to access 
 * "imputed_first_start" and "imputed_last_end" flags in the final fin_cleaned_addr_periods 
 * table that is produced by the function "process_addr_histories".
 * 
 * Note that in future, these flags will be migrated to the sensitivity variables table.
 * ------------------------------------------------------------------------------------------------  
 */
CREATE OR REPLACE FUNCTION comm_expand_periods_to_time_frame()
	RETURNS void AS 
$$
DECLARE

BEGIN

   /*
   * Adjust start date of the first address period so that it starts on the date of conception.
   * When we do this, we will not mark them as days of contention, because we would not have any 
   * addresses prior to conception geocoded. Also adjust the end date of the last address period
   * so that it matches the last date of the exposure time frame 
   */ 
	DROP TABLE IF EXISTS tmp_addr_periods12;
	CREATE TABLE tmp_addr_periods12 AS
	WITH addr_without_fixable_geocodes1 AS
		(SELECT
			b.person_id,
			a.ith_residence,
			a.ith_residence_type,
			CASE
				WHEN (a.ith_residence_type = 'first' OR a.ith_residence_type = 'only') AND 
					a.adjusted_start_date > b.time_frame_start_date THEN 
					b.time_frame_start_date
				ELSE 
					a.adjusted_start_date
			END AS fin_adjusted_start_date,
			CASE
				WHEN (a.ith_residence_type = 'first' OR a.ith_residence_type = 'only') AND 
					a.adjusted_start_date > b.time_frame_start_date THEN 
					'Y'
				ELSE 
					'N'
			END AS imputed_first_start,	
			CASE
				WHEN (a.ith_residence_type = 'last' OR a.ith_residence_type = 'only') AND 
					a.adjusted_end_date < b.time_frame_end_date THEN 
					b.time_frame_end_date
				ELSE 
					a.adjusted_end_date
			END AS fin_adjusted_end_date,
			CASE
				WHEN (a.ith_residence_type = 'last' OR a.ith_residence_type = 'only') AND  
					a.adjusted_end_date < b.time_frame_end_date THEN 
					'Y'
				ELSE 
					'N'
			END AS imputed_last_end
	FROM
		tmp_addr_periods11 a,
		tmp_overall_exp_time_frames b
	WHERE
		a.person_id = b.person_id AND 
		a.is_fixed_invalid_geocode = 'N'),
	addr_without_fixable_geocodes2 AS
		(SELECT
			c.person_id,
			c.ith_residence,
			c.fin_adjusted_start_date,
			c.imputed_first_start,
			c.fin_adjusted_end_date,
			c.imputed_last_end,
			CASE
				WHEN (c.ith_residence_type = 'middle' OR c.ith_residence_type = 'last') THEN
					c.fin_adjusted_start_date::date - d.conception_date::date + 1
				ELSE
					NULL
			END AS start_date_days_from_conception
		 FROM
			addr_without_fixable_geocodes1 c,
			fin_basic_life_stage_data d
		 WHERE
			c.person_id = d.person_id)
	SELECT
		e.original_row_number,
		e.person_id,
		e.ith_residence,
		e.geocode,
		e.date_state,
		e.start_date,
		e.end_date,
		e.duration,
		e.ith_residence_type,
		e.has_valid_geocode,
		e.has_name_exposures,
		e.has_nox_rd_exposures,
		e.has_pm10_gr_exposures,
		e.has_pm10_rd_exposures,
		e.has_pm10_tot_exposures,		
		e.maximum_life_stage_overlap,
		e.is_fixed_invalid_geocode,
		e.fit_extent,
		e.adjusted_start_date,
		e.adjusted_end_date,
		e.days_changed,
		e.fit_type,
		e.previous_geocode,
		e.next_geocode,
		e.start_date_delta1,
		e.start_date_delta2,
		e.end_date_delta1,
		e.end_date_delta2,
		e.start_date_contender,
		e.end_date_contender,
		f.fin_adjusted_start_date,
		f.imputed_first_start,
		f.fin_adjusted_end_date,
		f.imputed_last_end,
		f.start_date_days_from_conception
	FROM
		tmp_addr_periods11 e
	LEFT JOIN
		addr_without_fixable_geocodes2 f
	ON
		e.person_id = f.person_id AND
		e.ith_residence = f.ith_residence;   
   
   ALTER TABLE tmp_addr_periods12 ADD PRIMARY KEY (person_id, ith_residence);

END;
$$   LANGUAGE plpgsql;


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION common_id_relevant_addr_periods
 * -------------------------------------------------
 * Description
 * -----------
 * The program assumes that some of the address periods may not be relevant to the exposure time
 * frame of interest.  For example, in the early life analysis of one of Imperial College's studies
 * the last day of the exposure time frame is the last day of the first year of 
 * life.  In the later life analysis, the end date is the day before the study members's 16 
 * birthdays.  Other studies may have different exposure time frames.  
 *
 * An address period is considered relevant if it overlaps with the exposure time frame. The flag
 * is used to help define the context of the sensitivity variables. For example, the variable
 * "total_gaps" that is reported in table "results_sensitivity_variables" only refers to gaps that
 * have been corrected in address periods that overlap with the time frame [conception date, last 
 * day of first year of life].  The gaps in address periods which occur years after this time frame 
 * are not considered in the total.
 *  
 * Note that just because an address period is relevant does not necessarily mean an adjustment made
 * to it is.  For example, suppose study member 111 was conceived on 03-Mar-1992, was born on 
 * 18-Nov-1992 and had her first birthday on 18-Nov-1993.   Assume the exposure time frame of interest 
 * starts at conception and ends when study members turn 1. Now consider the following address history 
 * for 111:
 *
 *   ith_residence  Person ID Geocode  Final Adj. Start Date   Final Adj. End Date  Is Relevant   
 *   1              111       a1       03-Mar-1992             01-Sep-1992          Y
 *   2              111       a2       02-Sep-1992             31-Oct-1992          Y
 *   3              111       a3       01-Nov-1992             27-May-2000          Y
 *   4              111       a4       28-May-2000             27-May-2015          N
 *
 * Suppose that originally the end of address period 3 was 30-Aug-2005, but was changed to fix a 
 * large overlap with address period 4.  The address perios is relevant because it overlaps with 
 * [03-Mar-1992, 18-Nov-1993], but the drastic changes in the end date have no bearing on days that 
 * were within that same period.  
 * 
 * In future releases, "days changed within exposure time frame" will give a more meaningful view of 
 * days that were changed within the study time frame.
 * ------------------------------------------------------------------------------------------------  
 */
CREATE OR REPLACE FUNCTION comm_id_relevant_addr_periods()
	RETURNS void AS 
$$
DECLARE

	total_study_members INT;
	
BEGIN

	--Check for geocodes that have no corresponding exposure data
	DROP TABLE IF EXISTS tmp_addr_periods13;
	CREATE TABLE tmp_addr_periods13 AS
	WITH addr_without_fixable_geocodes AS
		(SELECT
			a.person_id,
			a.ith_residence,
			CASE
				WHEN is_fixed_invalid_geocode = 'Y' AND
					(b.time_frame_start_date, b.time_frame_end_date + INTERVAL '1 day') 
					OVERLAPS 
					(start_date, end_date) THEN
					'Y'
				WHEN (b.time_frame_start_date, b.time_frame_end_date + INTERVAL '1 day') 
					OVERLAPS 
					(fin_adjusted_start_date, fin_adjusted_end_date) THEN
					'Y'
				ELSE 
					'N'
			END AS is_within_exposure_time_frame
		 FROM
		 	tmp_addr_periods12 a,
		 	tmp_overall_exp_time_frames b
		 WHERE
		 	a.person_id=b.person_id)
	SELECT
		c.original_row_number,
		c.person_id,
		c.ith_residence,
		c.geocode,
		c.date_state,
		c.start_date,
		c.end_date,
		c.duration,
		c.ith_residence_type,
		c.has_valid_geocode,
		c.has_name_exposures,
		c.has_nox_rd_exposures,
		c.has_pm10_gr_exposures,
		c.has_pm10_rd_exposures,
		c.has_pm10_tot_exposures,
		c.maximum_life_stage_overlap,
		c.is_fixed_invalid_geocode,
		c.fit_extent,
		c.adjusted_start_date,
		c.adjusted_end_date,
		c.days_changed,
		c.fit_type,
		c.previous_geocode,
		c.next_geocode,
		c.start_date_delta1,
		c.start_date_delta2,
		c.end_date_delta1,
		c.end_date_delta2,
		c.start_date_contender,
		c.end_date_contender,
		c.fin_adjusted_start_date,
		c.imputed_first_start,
		c.fin_adjusted_end_date,
		c.imputed_last_end,
		c.start_date_days_from_conception,
		COALESCE(d.is_within_exposure_time_frame, 'N') AS is_within_exposure_time_frame
	FROM
		tmp_addr_periods12 c
	LEFT JOIN
		addr_without_fixable_geocodes d
	ON
		c.person_id = d.person_id AND
		c.ith_residence = d.ith_residence;
      
	ALTER TABLE tmp_addr_periods13 ADD PRIMARY KEY (person_id, ith_residence);
   	--DROP INDEX IF EXISTS ind_tmp_addr_periods13_1;
	--CREATE INDEX ind_tmp_addr_periods13_1 ON tmp_addr_periods13(is_fixed_invalid_geocode);
   	--DROP INDEX IF EXISTS ind_tmp_addr_periods13_2;
	--CREATE INDEX ind_tmp_addr_periods13_4 ON tmp_addr_periods13(has_valid_geocode);

	DROP TABLE IF EXISTS fin_cleaned_addr_periods;
	CREATE TABLE fin_cleaned_addr_periods AS
	SELECT
		original_row_number,
		person_id,
		ith_residence,
		geocode,
		date_state,
		start_date::date,
		end_date::date,
		duration,
		ith_residence_type,
		has_valid_geocode,
		has_name_exposures,		
		has_nox_rd_exposures,
		has_pm10_gr_exposures,
		has_pm10_rd_exposures,
		has_pm10_tot_exposures,		
		maximum_life_stage_overlap,
		is_fixed_invalid_geocode,
		fit_extent,
		adjusted_start_date::date,
		adjusted_end_date::date,
		days_changed,
		fit_type,
		previous_geocode,
		next_geocode,
		start_date_delta1::date,
		start_date_delta2::date,
		end_date_delta1::date,
		end_date_delta2::date,
		start_date_contender,
		end_date_contender,
		fin_adjusted_start_date::date,
		imputed_first_start,
		fin_adjusted_end_date::date,
		imputed_last_end,
		start_date_days_from_conception,
		is_within_exposure_time_frame
	FROM
		tmp_addr_periods13 c
	ORDER BY
		person_id,
		ith_residence;

	ALTER TABLE fin_cleaned_addr_periods ADD PRIMARY KEY (person_id, ith_residence);

	DROP INDEX IF EXISTS ind_fin_cleaned_addr_periods1;
	CREATE INDEX  ind_fin_cleaned_addr_periods1 ON fin_cleaned_addr_periods(geocode);
         
	DROP INDEX IF EXISTS ind_fin_cleaned_addr_periods2;
	CREATE INDEX  ind_fin_cleaned_addr_periods2 ON fin_cleaned_addr_periods(fit_type);
                  
	-- fin_adjusted_start_date and fin_adjusted_end_date are often used together as
	-- part of a BETWEEN statement
	DROP INDEX IF EXISTS ind_fin_cleaned_addr_periods3;
	CREATE INDEX  ind_fin_cleaned_addr_periods3 ON fin_cleaned_addr_periods(fin_adjusted_start_date, fin_adjusted_end_date);

	DROP INDEX IF EXISTS ind_fin_cleaned_addr_periods5;
	CREATE INDEX  ind_fin_cleaned_addr_periods5 ON fin_cleaned_addr_periods(start_date_delta1, start_date_delta2);

	DROP INDEX IF EXISTS ind_fin_cleaned_addr_periods7;
	CREATE INDEX  ind_fin_cleaned_addr_periods7 ON fin_cleaned_addr_periods(end_date_delta1, end_date_delta2);

	DROP INDEX IF EXISTS ind_fin_cleaned_addr_periods9;
	CREATE INDEX  ind_fin_cleaned_addr_periods9 ON fin_cleaned_addr_periods(is_fixed_invalid_geocode);

	DROP INDEX IF EXISTS ind_fin_cleaned_addr_periods10;
	CREATE INDEX  ind_fin_cleaned_addr_periods10 ON fin_cleaned_addr_periods(is_within_exposure_time_frame);

	SELECT
		COUNT(DISTINCT person_id)
	INTO 
		total_study_members
	FROM
		fin_cleaned_addr_periods;
	PERFORM validate_total_study_members_with_addresses(
		'calculate_contention_periods',
		total_study_members);


END;
$$   LANGUAGE plpgsql;


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION common_val_addr_histories
 * -------------------------------------------------
 * Description
 * -----------
 * This function checks whether we have lost or gained study members during the process of creating 
 * temporary tables.
 * ------------------------------------------------------------------------------------------------  
 */
CREATE OR REPLACE FUNCTION comm_val_addr_histories()
	RETURNS void AS 
$$
DECLARE

	study_members_in_first_tmp_addr_table INT;
	study_members_in_last_tmp_addr_table INT;
	study_members_in_one_not_the_other INT;	
	validation_failed BOOLEAN;
	
BEGIN

	validation_failed := FALSE;

	SELECT
		COUNT(DISTINCT person_id)
	FROM
		tmp_addr_periods1
	INTO
		study_members_in_first_tmp_addr_table;
		
	SELECT
		COUNT(DISTINCT person_id)
	FROM
		fin_cleaned_addr_periods
	INTO
		study_members_in_last_tmp_addr_table;
		
	IF study_members_in_first_tmp_addr_table !=
		study_members_in_last_tmp_addr_table THEN
		
		RAISE NOTICE 'ERROR.  The number of study members with address periods changed.';
		RAISE NOTICE 'The total at the beginning was % but is now %',
			study_members_in_first_tmp_addr_table,
			study_members_in_last_tmp_addr_table;
		validation_failed := TRUE;
	ELSE
		RAISE NOTICE 'VALIDATION PASSED.  Study members with addresses remains unchanged.';	
	END IF;
	
	SELECT
		COUNT(DISTINCT person_id)
	FROM
		tmp_addr_periods1
	INTO
		study_members_in_one_not_the_other
	WHERE
		person_id NOT IN 
			(SELECT
				person_id
			 FROM
				fin_cleaned_addr_periods);
	
	IF study_members_in_one_not_the_other != 0 THEN
		RAISE NOTICE 'ERROR.  There are study members in the first temporary address period table ';
		RAISE NOTICE 'that are not in the final address period table.';
		validation_failed := TRUE;
	ELSE
		RAISE NOTICE 'VALIDATION PASSED. All study members in first address period table are in last.';
	END IF;
	
	SELECT
		COUNT(DISTINCT person_id)
	FROM
		fin_cleaned_addr_periods
	INTO
		study_members_in_one_not_the_other
	WHERE
		person_id NOT IN 
			(SELECT
				person_id
			 FROM
			 	tmp_addr_periods1);
	
	IF study_members_in_one_not_the_other != 0 THEN
		RAISE NOTICE 'ERROR.  There are study members in the final address period table ';
		RAISE NOTICE 'that are not in the first address period table.';
		validation_failed := TRUE;
	ELSE
		RAISE NOTICE 'VALIDATION PASSED. All study members in final period table are in first.';
	END IF;
	
	IF validation_failed = TRUE THEN
		RAISE EXCEPTION 'Unacceptable errors occurred.';
	END IF;

END;
$$   LANGUAGE plpgsql;
--SELECT "validate_addr_histories"();



/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION common_cleanup_addr_history_data
 * -------------------------------------------------
 * Description
 * -----------
 * Deletes all the temporary tables that were created by functions in this module. This function 
 * should be commented out during testing and debugging phases of development.  The temporary tables 
 * can provide useful information for identifying where problems may be occuring in the
 * code.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION comm_cleanup_addr_history_data()
	RETURNS void AS 
$$
DECLARE

BEGIN

	DROP TABLE IF EXISTS tmp_addr_periods1;
	DROP TABLE IF EXISTS tmp_addr_periods2;
	DROP TABLE IF EXISTS tmp_addr_periods3;
	DROP TABLE IF EXISTS tmp_addr_periods4;
	DROP TABLE IF EXISTS tmp_addr_periods5;
	DROP TABLE IF EXISTS tmp_addr_periods6;
	DROP TABLE IF EXISTS tmp_addr_periods7;
	DROP TABLE IF EXISTS tmp_addr_periods8;
	DROP TABLE IF EXISTS tmp_addr_periods9;
	DROP TABLE IF EXISTS tmp_addr_periods10;
	DROP TABLE IF EXISTS tmp_addr_periods11;
	DROP TABLE IF EXISTS tmp_addr_periods12;
	DROP TABLE IF EXISTS tmp_addr_periods13;
	DROP TABLE IF EXISTS tmp_max_percent_life_stage_overlap;
	DROP TABLE IF EXISTS tmp_addr_period_geocode_validity;
	DROP TABLE IF EXISTS tmp_addr_period_geocodes_with_exp;
END;
$$   LANGUAGE plpgsql;


/**
 * ------------------------------------------------------------------------------------------------ 
 * MAIN FUNCTION comm_process_addr_histories
 * -------------------------------------------------
 * Description
 * ------------
 * The functions in this module should be used by calling this method.
 *
 * Note that this function uses two functions that are designed in the Sensitivity Variables 
 * Calculations module:
 * (1) common_set_sensitivity_bad_geocodes
 * (2) common_set_sensitivity_addr_period_changes
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION comm_process_addr_histories()
	RETURNS void AS 
$$
DECLARE

BEGIN

	-- Establish the way address periods will be sorted for the cleaning activity.  
	-- Impute any missing dates in the address periods so they are guaranteed to 
	-- have valid start and end dates.  Then calculate the durations of all the
	-- address periods.  The periods will be sorted in ascending order - first by
	-- person_id, then by start_date and finally by duration.  After these three
	-- steps have been done, the sorting order will be fixed for the remainder of 
	-- the data cleaning.
	PERFORM comm_cln_addr_period_end_dates();
	PERFORM comm_calc_addr_period_durations();
	PERFORM comm_order_addresses();


	-- Before we begin fixing gaps and overlaps in the ordered address periods, 
	-- we have the option of trying to "fix" some address periods which have invalid
	-- geocodes.  If an address period does not have a valid geocode, it will not have
	-- a valid exposure values between its start and end date.  This means that someone
	-- will have an unknown exposure for some part of their exposure time frame and no
	-- exposure results will be calculated for them.
	
	-- We only try to fix a certain class of error here.  We assume that the chronology
	-- of addresses comes from an audit trail of current addresses for study members.
	-- To be "fixable", an address period must satisfy all of these critieria
	-- (1) It must have an invalid geocode.  An invalid geocode is either a blank geocode 
	--     value or one which is non-blank but otherwise flagged as invalid with a 'N' 
	--     result in the is_valid flag (see table: original_geocode_data)
	-- (2) It must be immediately followed by an address period which does have a valid
	--     geocode.
	-- (3) The address period must be less than 25% of any life stage with which it overlaps.
	-- 
	-- When address periods satsify these criteria, we assume that cohort staff initially 
	-- entered an error in a residential address and then corrected it in the next current 
	-- address they corrected it. 
	PERFORM comm_assess_geocode_quality();
	PERFORM comm_id_and_fix_bad_geocodes();	

	-- fix gaps and overlaps and audit any changes that have been made.
	PERFORM comm_id_gaps_and_overlaps();
	PERFORM comm_fix_gaps_and_overlaps();
	PERFORM comm_audit_adjusted_days();
   
   --move the first start date back so that the address period
   --begins at date of conception.  As well, if the end date of the last address
   --period doesn't reach the end of the exposure time frame, set that end date
   --so it does.
    PERFORM comm_id_geocode_contenders();
	PERFORM comm_expand_periods_to_time_frame(); 	  
	PERFORM comm_id_relevant_addr_periods();

	PERFORM comm_val_addr_histories();
	PERFORM comm_cleanup_addr_history_data();
END;
$$   LANGUAGE plpgsql;

--SELECT "comm_process_addr_histories"();

/*
 * This is a module to help clean up some of the temporary tables that the protocol
 * produces.  When you're debugging the code or tracing end results back to the
 * original data sets, you may want to not call the comm_cleanup method.
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

CREATE OR REPLACE FUNCTION comm_cleanup()
	RETURNS void AS 
$$
DECLARE


BEGIN 

	DROP TABLE IF EXISTS fin_basic_life_stage_data;
	DROP TABLE IF EXISTS fin_cleaned_addr_periods;
	DROP TABLE IF EXISTS fin_daily_exposures;
	DROP TABLE IF EXISTS fin_general_life_stage_data;
	DROP TABLE IF EXISTS fin_life_stages_cov;
	DROP TABLE IF EXISTS fin_life_stage_sensitivity_variables;
	DROP TABLE IF EXISTS fin_uncln_vs_no_mob;
	DROP TABLE IF EXISTS fin_mob_uncln_exp;
	DROP TABLE IF EXISTS fin_mob_cln_exp;
	DROP TABLE IF EXISTS fin_mob_cln_vs_no_mob;
	DROP TABLE IF EXISTS fin_mob_cln_vs_unimp;
	DROP TABLE IF EXISTS fin_stg_mob_exp;
	DROP TABLE IF EXISTS fin_moves_cov;
	DROP TABLE IF EXISTS fin_sens_variables;

END;
$$   LANGUAGE plpgsql;


/**
 * ================================================================================================= 
 * MODULE COMM_MISC_FUNC: Miscellaneous Functions
 * =================================================================================================
 * Description
 * -----------
 * This module contains functions which either have generic utility or are used
 * in different parts of the code base.  Currently there are three functions
 * (1) median: used to calculate median exposure results.
 * (2) validate_total_study_members:  checks whether a total number of records
 *     in a table is the same as the total number of study members being processed
 *     by the study. 
 * (3) check_table_exists: checks if the name of a given table exists in the data
 *     base.
 * (4) validate_total_study_members_with_addresses: checks whether a total number
 *     of records matches the number of study members who have address periods
 * (5) is_numeric: a simple utility function that determines whether a value is
 *     an integer or not.
 * 
 * Main Function
 * -------------
 * In other modules, we try to discourage people from accessing methods other than a single
 * function that advertises the module's main task.  This module is more of a library of small
 * functions that are used in other parts of the code base.

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
 * FUNCTION validate_total_study_members
 * -------------------------------------
 * Description
 * -----------
 * This is a validation function that is used throughout the program to ensure that a table has the 
 * same number of rows as there are study members being monitored by the study.
 *
 * Inputs
 * ------ 
 * (1) calling_context: is a phrase used to help specify a debugging context in echoed statements
 * (2) actual_total_study_members: the number of study members in the study.  This is the result
 *     of a query that is made to check that a temporary table has not lost any study members that
 *     may have happened because of query joins and filtering conditions.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION validate_total_study_members(
	calling_context VARCHAR(250),
	actual_total_study_members INT)
	RETURNS void AS 
$$
DECLARE
	expected_total_study_members INT;
BEGIN

	SELECT
		total_study_members
	FROM
		global_script_constants	
	INTO
		expected_total_study_members;
	
	IF (expected_total_study_members != actual_total_study_members) THEN
		RAISE EXCEPTION 'ERROR:(%). Total study members is (%) but should be (%)',
		calling_context,
		actual_total_study_members,
		expected_total_study_members;
	ELSE
		RAISE NOTICE '(%) validation check. Total study members (%) is correct.', 
		calling_context,
		actual_total_study_members;
	END IF;

END;
$$   LANGUAGE plpgsql;
--SELECT "validate_total_study_members"('procedure blah', 6);

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION validate_total_study_members_with_addresses
 * ----------------------------------------------------
 * Description
 * -----------
 * Checks whether a total number of records is the same as the number of study members who have 
 * address periods
 *
 * Inputs
 * ------
 * (1) calling_context: Used to describe the context of where the function is being called.
 * (2) actual_total_study_members_with_addresses: a number that is meant to be a COUNT() total of 
 *     some table.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION validate_total_study_members_with_addresses(
	calling_context VARCHAR(5),
	actual_total_study_members_with_addresses INT)
	RETURNS void AS 
$$
DECLARE
	expected_total_study_members_with_addresses INT;
BEGIN

	SELECT
		total_study_members_with_addresses
	FROM
		global_script_constants	
	INTO
		expected_total_study_members_with_addresses;

	IF (expected_total_study_members_with_addresses != actual_total_study_members_with_addresses) THEN
		RAISE EXCEPTION 'ERROR:(%). Total study members is (%) but should be (%)',
		calling_context,
		actual_total_study_members_with_addresses,
		expected_total_study_members_with_addresses;
	ELSE
		RAISE NOTICE '(%) validation check. Total study members (%) is correct.', 
		calling_context,
		actual_total_study_members_with_addresses;
	END IF;

END;
$$   LANGUAGE plpgsql;
--SELECT "validate_total_study_members_with_addresses"('procedure blah', 6);

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION check_table_exist
 * --------------------------
 * Description
 * -----------
 * A simple function that checks whether a table exists.  This function is used to check whether 
 * staging tables have been created before the rest of the program attempts to use them.
 *
 * Inputs
 * -------
 * (1) table_name_to_check: the table that should exist in the database.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION check_table_exists(
	table_name_to_check VARCHAR(100))
	RETURNS void AS 
$$
DECLARE
	is_table_created BOOLEAN;
		
BEGIN

	SELECT EXISTS(
		SELECT 
			1
		FROM 
			information_schema.tables
		WHERE
			table_name = table_name_to_check)
	INTO is_table_created;
	
	IF is_table_created = false THEN
		RAISE EXCEPTION 'The table (%) does not exist in the schema',
		table_name_to_check;
	ELSE
		RAISE NOTICE 'The table (%) exists in the schema', 
		table_name_to_check;
	END IF;
   
END;
$$   LANGUAGE plpgsql;
--SELECT "check_table_exists"('test_exp_data');


/*
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION is_numeric
 * -------------------
 * Description
 * -----------
 * Uses a regular expression to check whether a piece of text is numeric or not.
 *
 * Inputs
 * ------ 
 * (1) a text value
 *
 * Returns
 * -------
 *    true if the text value represents a number or
 *    false if the text value is not a number
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION is_numeric(text) 
	RETURNS BOOLEAN AS '
	
	SELECT $1 ~ ''^[0-9]+$''
' LANGUAGE 'sql';


/*
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION standardise_geocode_value
 * ----------------------------------
 * Description
 * -----------
 * Standardises all the possible ways a field could appear as empty in the staging tables.  This 
 * function is guarantees that the field value either has a non-empty value or that it is marked 
 * "empty_value".  It is parameterised so that we can also check whether a required field value is 
 * null or not.  In some cases the gecode can be empty and in others an empty value should trigger 
 * an exception.
 *
 * Inputs
 * ------
 * (1) original_geocode_value: a text value that represents a geographic location
 * (2) is_null_allowed: if false then when the function encounters a null value for 
 *     original_geocode_value it will raise an exception.
 *
 * Returns
 * -------
 *    the same input value, if it is not an empty field value or 'empty_geocode'
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION standardise_geocode_value(
	original_geocode_value VARCHAR,
	is_null_allowed BOOLEAN)
	RETURNS VARCHAR AS 
$$
DECLARE
	canonical_geocode_value VARCHAR;
	empty_value VARCHAR;
BEGIN
	
	--standardise input value so we do not have to consider mixed case possibilities
	SELECT 
		TRIM(UPPER(original_geocode_value))
	INTO 
		canonical_geocode_value;

	IF (canonical_geocode_value = '' OR 
		canonical_geocode_value = '#NULLIF' OR 
		canonical_geocode_value IS NULL) AND
		is_null_allowed = TRUE THEN

		SELECT
			empty_geocode_value
		FROM
			global_script_constants
		INTO
			empty_value;

		IF is_null_allowed = TRUE THEN
			RETURN empty_value;
		ELSE
			RAISE EXCEPTION 'This geocode field value cannot be NULL';
		END IF;
	ELSE	
		RETURN original_geocode_value;
	END IF;
END;
$$   LANGUAGE plpgsql;

/*
-- Test Cases:
SELECT
	standardise_geocode_value('123456-234543', TRUE), -- Expected: '123456-234543'
	standardise_geocode_value(NULL, TRUE),            -- Expected: 'empty_geocode'
	standardise_geocode_value('#NULLIF', TRUE),       -- Expected: 'empty_geocode'
	standardise_geocode_value('', TRUE),              -- Expected: 'empty_geocode'
	standardise_geocode_value('empty', TRUE);         -- Expected: 'empty'
	
SELECT
	standardise_geocode_value('', FALSE);             -- Expected: Exception
*/

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION standardise_yes_no_value
 * ----------------------------------
 * Description
 * -----------
 * Standardises various forms of input that have the meaning of 'yes' or 'no'.  The function is 
 * used to help constrain the values of the staging tables as much as possible so that the rest 
 * of the protocol can assume that a yes-no field is not null and is either 'Y' or 'N'.
 *
 * Inputs
 * ------
 * (1) anyarray: a list of numbers
 *
 * Returns
 * ------- 
 *    'Y' for an input value meaning yes or true or 'N' for an input value meaning no or false or
 *    throws an exception
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION standardise_yes_no_value(
    original_field_value VARCHAR)
	RETURNS VARCHAR AS 
$$
DECLARE
	canonical_field_value VARCHAR;
BEGIN

	--standardise input value so we do not have to consider mixed case possibilities
	SELECT 
		TRIM(UPPER(original_field_value))
	INTO 
		canonical_field_value;

	IF canonical_field_value = 'YES' OR
		canonical_field_value = 'Y' OR
		canonical_field_value = 'TRUE' OR
		canonical_field_value = '1' THEN
		RETURN 'Y';
	ELSIF canonical_field_value = 'NO' OR
		canonical_field_value = 'N' OR
		canonical_field_value = 'FALSE' OR
		canonical_field_value = '0' THEN
		RETURN 'N';
	ELSIF canonical_field_value IS NULL THEN
		RAISE EXCEPTION 'Unacceptable yes no value of NULL';	
	ELSE
		RAISE EXCEPTION 'Unacceptable yes no value of "%"', original_field_value;
	END IF;	
END;
$$   LANGUAGE plpgsql;

/*
--Test cases: 
SELECT
	standardise_yes_no_value('n') AS test_comm_no1,      -- Expected: 'N'
	standardise_yes_no_value('N') AS test_comm_no2,      -- Expected: 'N'
	standardise_yes_no_value('No') AS test_comm_no3,     -- Expected: 'N'
	standardise_yes_no_value('false') AS test_comm_no4,  -- Expected: 'N'
	standardise_yes_no_value('0') AS test_comm_no4,      -- Expected: 'N'	
	standardise_yes_no_value('y') AS test_comm_yes1,     -- Expected: 'Y'
	standardise_yes_no_value('Y') AS test_comm_yes2,     -- Expected: 'Y'
	standardise_yes_no_value('Yes') AS test_comm_yes3,   -- Expected: 'Y'
	standardise_yes_no_value('true') AS test_comm_yes4,  -- Expected: 'Y'
	standardise_yes_no_value('1') AS test_comm_yes5;     -- Expected: 'Y'

SELECT
	standardise_yes_no_value('blah') AS test_exception1;   -- Expected: exception

SELECT
	standardise_yes_no_value(NULL) AS test_exception2;     -- Expected: exception
*/

/*
 * Many thanks for inspiration from https://wiki.postgresql.org/wiki/Extract_days_from_range_type 
 */

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION calculate_days_overlap
 * -------------------------------
 * Description
 * -----------
 * Calculates the overlap between date ranges [range1_start_date, range1_end_date] and
 * [range2_start_date, range2_end_date].  It is used to help determine the overlap between an
 * address period and a life stage.  The result indicates how much of a given address period
 * contributes to a given life stage.
 *
 * Many thanks for inspiration from https://wiki.postgresql.org/wiki/Extract_days_from_range_type 
 *
 * Inputs
 * ------
 * (1) range1_start_date: a date value
 * (2) range1_end_date: a date value
 * (3) range2_start_date: a date value
 * (4) range2_end_date: a date value
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION calculate_days_overlap(
	range1_start_date DATE,
	range1_end_date DATE,
	range2_start_date DATE,
	range2_end_date DATE)
	RETURNS INT AS 
$$
DECLARE
	total_days_overlap INT;
BEGIN

	total_days_overlap := 0;
	
 	--#DATABASE_TECHNIQUE
 	--Here we're trying to use a guard clause to help catch problems as early as possible.
 	--Using this statement allows the rest of the code to operate with more assumptions that
 	--simplify the code
 	 	
	IF (range1_start_date IS NULL OR
		range1_end_date IS NULL OR
		range2_start_date IS NULL OR
		range2_end_date IS NULL) THEN
		
		RETURN total_days_overlap;
	END IF;

	--Note that if the two ranges don't overlap, the result will be a range that has
	--a null value for upper and lower values.  
	total_days_overlap :=
		(WITH a AS
			(SELECT 
				tsrange(range1_start_date, range1_end_date, '[]') * 
				tsrange(range2_start_date, range2_end_date, '[]') AS overlap_extent)
		 SELECT
		 	CASE
				WHEN upper(overlap_extent) IS NULL 
				THEN 0 -- an empty interval of overlap
			ELSE
				date_trunc('day', upper(overlap_extent))::date - 
				date_trunc('day', lower(overlap_extent))::date + 1
			END 
		 FROM
			a);

	RETURN total_days_overlap;
END;
$$   LANGUAGE plpgsql;

--Test 1: A gap of one day.  an = [mar1, mar 2], an+1 = [mar 4, mar 5].  Expected: 0
--SELECT "calculate_days_overlap"('1992-03-01'::date, '1992-03-02'::date, '1992-03-04'::date, '1992-03-05'::date);	
--Test 2: Temporally Contiguous.  an = [mar 1, mar 2], an+1 = [mar 3, mar 4]. Expected: 0 days.
--SELECT "calculate_days_overlap"('1992-03-01'::date, '1992-03-02'::date, '1992-03-03'::date, '1992-03-04'::date);	
--Test 3: Identical one day intervals. an = [mar 1, mar 1], an+1 = [mar 1, mar 1]. Expected: 1 day
--SELECT "calculate_days_overlap"('1992-03-01'::date, '1992-03-01'::date, '1992-03-01'::date, '1992-03-01'::date);
--Test 4: One day overlap, intervals different sizes. an = [mar 1, mar 3], an+1 = [mar 3, mar 5]. Expected: 1 day.
--SELECT "calculate_days_overlap"('1992-03-01'::date, '1992-03-03'::date, '1992-03-03'::date, '1992-03-05'::date);
--Test 5: One subsumes the other. an = [mar 1, mar 3] an+1 = [mar 1, mar 5].  Expected: 3
--SELECT "calculate_days_overlap"('1992-03-01'::date, '1992-03-03'::date, '1992-03-01'::date, '1992-03-05'::date);
--Test 6: Overlap spans a leap year day. an = [feb 26 1992, mar 1 1992], an+1 = [feb 27 1992, mar 1 1992]. Expected: 4
--SELECT "calculate_days_overlap"('1992-02-26'::date, '1992-03-01'::date, '1992-02-27'::date, '1992-03-01'::date);


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION calculate_days_overlap
 * -------------------------------
 * Description
 * -----------
 * Determines the number of days from the combined overlap of each of the start date delta and the
 * end date delta with a life stage.
 * 
 * Consider the following example, where "start_date_delta" is abbreviated by "sd" and 
 * end_date_delta" is abbreviated by "ed". 
 * 
 * a1 had a change in end date because of an overlap
 * a2 had no changes so there will be no overlap 
 * a3 was changed to fill a gap and the start date delta partially overlaps with T2
 * a4 has an adjusted end date but it does not overlap at all with T2
 *
 * T2:                [                                                ]
 * a1:                                                 [ed_1, ed_2]
 * a2:                                                                
 * a3:              [sd_1, sd_2]
 * a4:                                                                    [ed_1, ed_2]
 * a5: [sd_1, sd_2]
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION get_total_contention_days(
	life_stage_start_date DATE,
	life_stage_end_date DATE,
	start_date_delta1 DATE,
	start_date_delta2 DATE,
	end_date_delta1 DATE,
	end_date_delta2 DATE)

	RETURNS INT AS 
$$
DECLARE
	total_start_contention_days INT;
	total_end_contention_days INT;
	total_contention_days INT;
	life_stage_period TSRANGE;
	overall_start_date_range TSRANGE;
	overall_end_date_range TSRANGE;
BEGIN

	total_start_contention_days := 0;
	total_end_contention_days := 0;
	total_contention_days := 0;


	IF (life_stage_start_date IS NULL OR life_stage_end_date IS NULL) THEN
		RETURN 0;
	END IF;

	IF (life_stage_end_date < life_stage_start_date) THEN
		--This is very rare but could happen for extremely premature babies
		RAISE NOTICE 'ERROR: get_contention_days with illegal range for life stage.  May describe premature births';
		RETURN  -999999;			
	END IF;	
	
	life_stage_period 
		:= tsrange(life_stage_start_date, life_stage_end_date);

	IF start_date_delta1 IS NOT NULL AND start_date_delta2 IS NOT NULL THEN
	
		IF (start_date_delta2 < start_date_delta1) THEN
			RAISE NOTICE 'ERROR: get total contention days - start date delta has improper range';
			RETURN -999999;
		END IF;
		
		overall_start_date_range 
			:= tsrange(start_date_delta1, start_date_delta2, '[]');

		overall_start_date_range := life_stage_period * overall_start_date_range;
		
		total_start_contention_days :=
			date_trunc('day', upper(overall_start_date_range))::date - 
			date_trunc('day', lower(overall_start_date_range))::date + 1;				
	END IF;

	IF end_date_delta1 IS NOT NULL AND end_date_delta2 IS NOT NULL THEN
		-- program responded to an overlap. This address period was shortened.

		IF (end_date_delta2 < end_date_delta1) THEN
			RAISE NOTICE 'ERROR: get total contention days - end date delta has improper range';
			RETURN -999999;
		END IF;
		
		overall_end_date_range 
			:= tsrange(end_date_delta1, end_date_delta2, '[]');

		overall_end_date_range := life_stage_period * overall_end_date_range;

		total_end_contention_days :=
			date_trunc('day', upper(overall_end_date_range))::date - 
			date_trunc('day', lower(overall_end_date_range))::date + 1;					
	END IF;

	total_contention_days := total_start_contention_days + total_end_contention_days;

	RETURN COALESCE(total_contention_days, 0);


END;
$$   LANGUAGE plpgsql;

/*
--Test Case 1
--Expected: 5 days of contention
SELECT "get_total_contention_days"(
	'1993-02-26'::date, -- start date of development stage of interest
	'1993-03-09'::date, -- end date of development stage of interest
	null::date, -- the lower bound of the interval of change for start date adjustment
	null::date, -- the upper bound of the interval of change for start date adjustment
	'1993-03-05'::date, -- the lower bound of the interval of change for end date adjustment
	'1993-03-07'::date); -- the upper ound of the interval of change for end date adjustment
*/
/*
--Test Case 2
--Expected: 3 days of contention
SELECT "get_total_contention_days"(
	'1993-02-26'::date, -- start date of development stage of interest
	'1993-03-05'::date, -- end date of development stage of interest
	null::date, -- the lower bound of the interval of change for start date adjustment
	null::date, -- the upper bound of the interval of change for start date adjustment
	'1993-03-05'::date, -- the lower bound of the interval of change for end date adjustment
	'1993-03-07'::date); -- the upper ound of the interval of change for end date adjustment
*/
/*
--Test Case 3
--Expected: 3 days of contention
SELECT "get_total_contention_days"(
	'1993-02-26'::date, -- start date of development stage of interest
	'1993-03-09'::date, -- end date of development stage of interest
	'1993-03-01'::date, -- the lower bound of the interval of change for start date adjustment
	'1993-03-03'::date, -- the upper bound of the interval of change for start date adjustment
	'1993-03-07'::date, -- the lower bound of the interval of change for end date adjustment
	'1993-03-08'::date); -- the upper ound of the interval of change for end date adjustment
*/

/*
SELECT
	get_total_uncontested_days(
		'1998-05-01'::date,
		'1998-05-10'::date,
		'1998-05-05'::date,
		'1998-05-10'::date,
		'1998-05-05'::date,
		'1998-05-06'::date);
*/



/* 
CREATE OR REPLACE FUNCTION get_total_uncontested_days(
	life_stage_start_date DATE,
	life_stage_end_date DATE,
	fin_adjusted_start_date DATE,
	fin_adjusted_end_date DATE,
	start_date_delta1 DATE,
	start_date_delta2 DATE)

	RETURNS INT AS 
$$
DECLARE
	adjusted_date_range TSRANGE;
	uncontested_date_range TSRANGE;
	uncontested_life_stage_overlap TSRANGE;
	total_uncontested_days INT;

BEGIN

	total_uncontested_days := 0;
	
	--this should not happen but it's a check
	IF (fin_adjusted_start_date IS NULL OR fin_adjusted_end_date IS NULL) THEN
		RETURN total_uncontested_days;	
	END IF;
	
	uncontested_date_range := 
		tsrange(fin_adjusted_start_date, fin_adjusted_end_date, '[]');
		
	IF (start_date_delta1 IS NOT NULL AND start_date_delta2 IS NOT NULL) THEN
		uncontested_date_range :=
			uncontested_date_range - tsrange(start_date_delta1, start_date_delta2, '[]');	
		uncontested_life_stage_overlap :=
			tsrange( LOWER(uncontested_life_stage_overlap) + INTERVAL '1 day', UPPER(uncontested_life_stage_overlap), '[]');					
	END IF;

	-- now find the overlap with the life stage
	uncontested_life_stage_overlap :=
		tsrange(life_stage_start_date, life_stage_end_date, '[]') *
		uncontested_date_range;

	total_uncontested_days :=
		date_trunc('day', upper(uncontested_life_stage_overlap))::date - 
		date_trunc('day', lower(uncontested_life_stage_overlap))::date + 1;				

	RETURN COALESCE(total_uncontested_days, 0);

END;
$$   LANGUAGE plpgsql;
*/
/*

--Expected: 1 day [apr 13]
SELECT "get_total_uncontested_days"(
	'1992-11-18'::date, -- start date of development stage of interest
	'1993-11-17'::date, -- end date of development stage of interest
	'1993-04-13'::date,
	'1993-04-15'::date,
	'1993-04-13'::date,
	'1993-04-13'::date);
*/

--Expected: 6 days [apr 20, apr 25] 
/*
SELECT "get_total_uncontested_days"(
	'1992-11-18'::date, -- start date of development stage of interest
	'1993-11-17'::date, -- end date of development stage of interest
	'1993-04-20'::date,
	'1993-04-25'::date,
	'1993-04-18'::date,
	'1993-04-25'::date);
*/

CREATE OR REPLACE FUNCTION do_periods_overlap(
	range1_start_date date,
	range1_end_date date,
	range2_start_date date,
	range2_end_date date)
	RETURNS BOOLEAN AS 
$$
DECLARE
	overlap_extent TSRANGE;
BEGIN

	IF (range1_start_date IS NULL OR
		range1_end_date IS NULL OR
		range2_start_date IS NULL OR
		range2_end_date IS NULL) THEN
		
		RETURN FALSE;
	END IF;

	IF (range1_end_date < range1_start_date) OR 
		(range2_end_date < range2_start_date) THEN

		RETURN FALSE;
	END IF;

	overlap_extent :=
		tsrange(range1_start_date, range1_end_date, '[]') * tsrange(range2_start_date, range2_end_date, '[]');
	IF UPPER(overlap_extent) IS NULL THEN
		RETURN FALSE;
	ELSE
		RETURN TRUE;
	END IF;

END;
$$   LANGUAGE plpgsql;

/*
SELECT "do_periods_overlap"(
	'1992-11-18'::date, -- start date of development stage of interest
	'1993-11-17'::date, -- end date of development stage of interest
	'1993-04-20'::date,
	'1993-04-25'::date);
*/

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION calc_percent_error
 * -------------------------------------
 * Description
 * -----------
 * Calculates the percentage difference between two values
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION calc_percent_error(
	exact_value DOUBLE PRECISION,
	approximate_value DOUBLE PRECISION)
	RETURNS DOUBLE PRECISION AS 
$$
DECLARE

BEGIN

	IF exact_value IS NULL OR 
		approximate_value IS NULL THEN
		RETURN NULL;
	END IF;
	
	IF (exact_value = 0) THEN
		RETURN -9999999999999.999;
	END IF;

	RETURN (ABS(approximate_value - exact_value) / exact_value) * 100.0;

/*
	IF (value1 = 0 or value2 = 0) THEN
		RETURN -9999999999999.999;
	END IF;

	RETURN ABS(value1 - value2) / ((value1 + value2)/2) * 100.0;
*/
END;
$$   LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION calc_percent_diff(
	first_value DOUBLE PRECISION,
	second_value DOUBLE PRECISION)
	RETURNS DOUBLE PRECISION AS 
$$
DECLARE

BEGIN

	IF first_value IS NULL OR 
		second_value IS NULL THEN
		RETURN NULL;
	END IF;
	
	IF first_value = 0 AND
		second_value = 0 THEN
		RETURN -9999999999999.999;
	END IF;

	RETURN ABS(first_value - second_value) / ((first_value + second_value)/2) * 100.0;

END;
$$   LANGUAGE plpgsql;



/**
 * Function: final_median
 * ----------------------
 * Apparently, PostgreSQL does not have a native median(...) function.  But 
 * I have used the code that was posted at: 
 * https://wiki.postgresql.org/wiki/Aggregate_Median
 *
 * Inputs: anyarray: a list of numbers
 * ------------------------------------------------------------------------------------------------ 
 */


CREATE OR REPLACE FUNCTION final_median(anyarray) RETURNS float8 AS
$$ 
DECLARE
	cnt INTEGER;
	
BEGIN
	cnt := (SELECT count(*) FROM unnest($1) val WHERE val IS NOT NULL);
	
	IF cnt = 0 THEN
		RETURN NULL;
	END IF;
	
		RETURN 
			(SELECT 
				AVG(tmp.val)::float8 
		 	 FROM 
		 		(SELECT 
		 			val 
		 	 	FROM 
		 	 		unnest($1) val
		 	 	WHERE 
		 	 		val IS NOT NULL 
		 	 	ORDER BY 
		 	 		1 
		 	 		LIMIT 2 - MOD(cnt, 2) 
		 	 	OFFSET 
		 	 		CEIL(cnt/ 2.0) - 1) AS tmp);
END
$$ LANGUAGE plpgsql;
 
CREATE AGGREGATE median(anyelement) (
	SFUNC = array_append,
	STYPE = anyarray,
	FINALFUNC = final_median,
	INITCOND = '{}'
);

/**
 * ================================================================================================= 
 * MODULE COMM_COV_LIFE_STAGE: Determine Spatial Coordinates
 * =================================================================================================
 * Description
 * -----------
 * This module contains code that tries to find geographic covariate data for the locations that
 * study members occupied at the start of each life stage. 
 *
 * This module contains code that is used to help epidemiologists and exposure scientists associate
 * study members with social deprivation scores based on how people move.  Moving is often regarded
 * as a type of stress to an individual's life.  The code here gathers geographical covariate data 
 * for the start date of every move.  It also captures the number of days between the start date of
 * a move and the conception date.  
 *
 * The table it produces has these fields:
 * person_id	start_days_from_conception	ed91	oa2001	coa2011
 * 
 * 
 * Assumptions
 * -----------
 * The routines in this module assume that the address periods have already been completely 
 * processed.
 *
 * Main Function
 * -------------
 *    comm_determine_life_stage_cov()
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
 * MAIN FUNCTION comm_determine_life_stage_cov
 * --------------------------------------------------
 * Description
 * -----------
 * Obtains the geocode associated with the start date of each life stage, then obtains the
 * geographic covariates that are associated with that geocode.
 *
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION comm_determine_life_stage_cov()
	RETURNS void AS 
$$
DECLARE

BEGIN

	--By the time we have created fin_cleaned_addr_periods, the address
	--periods should be temporally configuous.  Therefore, the start date of 
	--each life stage should be associated with exactly one address period.
	
	DROP TABLE IF EXISTS fin_life_stages_cov;
	CREATE TABLE fin_life_stages_cov AS
	WITH locations_on_life_stage_start_dates AS 
		(SELECT
			a.person_id,
			a.ith_life_stage,
			a.life_stage,
			b.geocode
		 FROM
		 	fin_general_life_stage_data a,
		 	fin_cleaned_addr_periods b
		 WHERE
		 	a.person_id = b.person_id AND
		 	a.start_date BETWEEN b.fin_adjusted_start_date AND b.fin_adjusted_end_date AND
		 	b.fit_type != 'D' AND --ignore deleted address periods
		 	b.is_fixed_invalid_geocode = 'N') --ignore periods with bad geocodes we "fixed"
	SELECT	
		c.person_id,
		c.ith_life_stage,
		c.life_stage,
		c.geocode,
		e.ed91,
		e.oa2001,
		e.coa2011
	FROM
		locations_on_life_stage_start_dates c,
		staging_geocode_data e
	WHERE
		c.geocode = e.geocode 
	ORDER BY
		c.person_id,
		c.ith_life_stage;

	ALTER TABLE fin_life_stages_cov ADD PRIMARY KEY (person_id, ith_life_stage);

END;
$$   LANGUAGE plpgsql;
--SELECT "comm_determine_life_stage_cov"();


/*
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION comm_determine_moves_cov
 * ------------------------------------------
 * Description
 * -----------
 * Establishes the geographical covariates for each move (2nd address period and onwards within the 
 * exposure time frame).  The sequence of moves is established by the number of days between when 
 * someone moved and that person's conception date.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION comm_determine_moves_cov()
	RETURNS void AS 
$$
DECLARE

BEGIN

	--Note: we use the ith_residence_type because ith_residence of 2 could
	--become the first viable address if it subsumes the one before it.
	DROP TABLE IF EXISTS fin_moves_cov;
	CREATE TABLE fin_moves_cov AS
	SELECT
		a.person_id,
		a.start_date_days_from_conception,
		b.geocode,
		b.ed91,
		b.oa2001,
		b.coa2011
	 FROM
	 	fin_cleaned_addr_periods a,
		staging_geocode_data b
	WHERE
		a.geocode = b.geocode AND 
		a.is_within_exposure_time_frame = 'Y' AND -- make sure period is relevant to exp time frame
		a.fit_type != 'D' AND -- ignore deleted periods
		a.is_fixed_invalid_geocode = 'N' AND --ignore periods that have been 'fixed' and omitted
		(a.ith_residence_type = 'middle' OR
		 a.ith_residence_type = 'last') -- moves begin with the start date of addr 2
	 ORDER BY 
	 	a.person_id;
	 	
	ALTER TABLE fin_moves_cov ADD PRIMARY KEY (person_id, start_date_days_from_conception);
	 	

END;
$$   LANGUAGE plpgsql;
--SELECT "comm_determine_moves_cov"();




/*
 * ================================================================================================= 
 * MODULE: COMMON_EXPOSURE_CALCULATIONS
 * ================================================================================================= 
 * Description
 * -----------
 * The exposure calculations for early and later life analyses are almost the same.
 * The main difference is that in the early life analysis, there is one more 
 * analysis, which is to assess the entire early life analysis using the addresses
 * the study members occupied at birth.
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
CREATE OR REPLACE FUNCTION common_calc_mob_cln_exp()
	RETURNS void AS 
$$
DECLARE

BEGIN

	-- Identify the contributions of each address period to each life stage
	-- of each person. Ignore address periods that have been marked deleted,
	-- or which have bad geocodes that were fixed.  Also ignore study members
	-- who had at least one bad, unfixed geocode within their exposure time frame.
	--
	-- Next, generate dates of year for every day of every life stage of every 
	-- person.
	--
	-- Now combine the two.  In the final cleaned address periods table, each day
	-- of each life stage should be occupied by exactly one address period.  Retain
	-- the geocode of the address period which overlaps with a given life stage day.
	DROP TABLE IF EXISTS tmp_common_exp_periods1; 
	CREATE TABLE tmp_common_exp_periods1 AS 
	WITH cleaned_addr AS
		(SELECT
			person_id,
			geocode,
			ith_residence,
			has_valid_geocode,
			has_name_exposures,
			has_nox_rd_exposures,
			has_pm10_gr_exposures,
			has_pm10_rd_exposures,
			has_pm10_tot_exposures,
			fin_adjusted_start_date,
			fin_adjusted_end_date
		FROM
			fin_cleaned_addr_periods
		WHERE
			fit_type != 'D' AND
			is_fixed_invalid_geocode = 'N')			
	SELECT
		c.person_id,
		d.ith_life_stage,
		d.life_stage,
		d.date_of_year,
		c.geocode,
		c.ith_residence,
		c.has_valid_geocode,
		c.has_name_exposures,
		c.has_nox_rd_exposures,
		c.has_pm10_gr_exposures,
		c.has_pm10_rd_exposures,
		c.has_pm10_tot_exposures
	FROM
		cleaned_addr c,
		tmp_life_stage_days d
	WHERE
		c.person_id = d.person_id AND
		d.date_of_year BETWEEN c.fin_adjusted_start_date AND c.fin_adjusted_end_date
	ORDER BY
		c.person_id,
		d.ith_life_stage;

   	ALTER TABLE tmp_common_exp_periods1 ADD PRIMARY KEY (person_id, date_of_year);
   	DROP INDEX IF EXISTS ind_tmp_common_exp_periods1;
	CREATE INDEX  ind_tmp_common_exp_periods1 ON staging_geocode_data(geocode);

	-- Later life stage data
	/*
	 * In this step we are including data quality indicators for each 
	 * day of exposure in the mobile cleaned assessment.  
	 *
	 *
	 *
	 *
	 *
	 *
	 */
	 		
	DROP TABLE IF EXISTS tmp_mob_cln_exp;
	CREATE TABLE tmp_mob_cln_exp AS 	
	WITH mob_cln_exp1 AS
		(SELECT
			a.person_id,
			a.ith_life_stage,
			a.life_stage,
			a.ith_residence,
			a.date_of_year,			
			CASE
				WHEN a.has_name_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS name_invalid_address_count,
			CASE
				WHEN a.has_name_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS name_oob_count,
			CASE
				WHEN a.has_name_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS name_poor_address_count,
			CASE
				WHEN a.has_name_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.name IS NULL THEN
					1
				ELSE 
					0
			END AS name_missing_exp_count,
			CASE
				WHEN a.has_name_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.name IS NOT NULL THEN
					1
				ELSE 
					0
			END AS name_good_address_count,
			b.name,
			CASE
				WHEN a.has_nox_rd_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS nox_rd_invalid_address_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS nox_rd_oob_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS nox_rd_poor_address_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.nox_rd IS NULL THEN
					1
				ELSE 
					0
			END AS nox_rd_missing_exp_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.nox_rd IS NOT NULL THEN
					1
				ELSE 
					0
			END AS nox_rd_good_address_count,
			b.nox_rd,
			CASE
				WHEN a.has_pm10_rd_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_rd_invalid_address_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS pm10_rd_oob_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_rd_poor_address_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_rd IS NULL THEN
					1
				ELSE 
					0
			END AS pm10_rd_missing_exp_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_rd IS NOT NULL THEN
					1
				ELSE 
					0
			END AS pm10_rd_good_address_count,
			b.pm10_rd,
			CASE
				WHEN a.has_pm10_gr_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_gr_invalid_address_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS pm10_gr_oob_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_gr_poor_address_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_gr IS NULL THEN
					1
				ELSE 
					0
			END AS pm10_gr_missing_exp_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_gr IS NOT NULL THEN
					1
				ELSE 
					0
			END AS pm10_gr_good_address_count,
			b.pm10_gr,
			CASE
				WHEN a.has_pm10_tot_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_tot_invalid_address_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS pm10_tot_oob_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_tot_poor_address_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_tot IS NULL THEN
					1
				ELSE 
					0
			END AS pm10_tot_missing_exp_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_tot IS NOT NULL THEN
					1
				ELSE 
					0
			END AS pm10_tot_good_address_count,
			b.pm10_tot
		FROM
			tmp_common_exp_periods1 a
		LEFT JOIN
			fin_daily_exposures b
		ON
			a.geocode = b.geocode AND
			a.date_of_year = b.date_of_year)
	SELECT
		person_id,
		ith_life_stage,
		life_stage,
		SUM(name_invalid_address_count) AS name_invalid_address_days,
		SUM(name_oob_count) AS name_oob_days,
		SUM(name_poor_address_count) AS name_poor_address_days,
		SUM(name_missing_exp_count) AS name_missing_exp_days,
		SUM(name_good_address_count) AS name_good_address_days,
		
		SUM(nox_rd_invalid_address_count) AS nox_rd_invalid_address_days,
		SUM(nox_rd_oob_count) AS nox_rd_oob_days,
		SUM(nox_rd_poor_address_count) AS nox_rd_poor_address_days,
		SUM(nox_rd_missing_exp_count) AS nox_rd_missing_exp_days,
		SUM(nox_rd_good_address_count) AS nox_rd_good_address_days,
		
		SUM(pm10_rd_invalid_address_count) AS pm10_rd_invalid_address_days,
		SUM(pm10_rd_oob_count) AS pm10_rd_oob_days,
		SUM(pm10_rd_poor_address_count) AS pm10_rd_poor_address_days,
		SUM(pm10_rd_missing_exp_count) AS pm10_rd_missing_exp_days,
		SUM(pm10_rd_good_address_count) AS pm10_rd_good_address_days,
		
		
		SUM(pm10_gr_invalid_address_count) AS pm10_gr_invalid_address_days,
		SUM(pm10_gr_oob_count) AS pm10_gr_oob_days,
		SUM(pm10_gr_poor_address_count) AS pm10_gr_poor_address_days,
		SUM(pm10_gr_missing_exp_count) AS pm10_gr_missing_exp_days,
		SUM(pm10_gr_good_address_count) AS pm10_gr_good_address_days,
		
		SUM(pm10_tot_invalid_address_count) AS pm10_tot_invalid_address_days,
		SUM(pm10_tot_oob_count) AS pm10_tot_oob_days,
		SUM(pm10_tot_poor_address_count) AS pm10_tot_poor_address_days,
		SUM(pm10_tot_missing_exp_count) AS pm10_tot_missing_exp_days,
		SUM(pm10_tot_good_address_count) AS pm10_tot_good_address_days,
		SUM(name) AS name_sum,
		AVG(name) AS name_avg,
		median(name) AS name_med,
		SUM(nox_rd) AS nox_rd_sum,
		AVG(nox_rd) AS nox_rd_avg,
		median(nox_rd) AS nox_rd_med,
		SUM(pm10_gr) AS pm10_gr_sum,
		AVG(pm10_gr) AS pm10_gr_avg,
		median(pm10_gr) AS pm10_gr_med,
		SUM(pm10_rd) AS pm10_rd_sum,
		AVG(pm10_rd) AS pm10_rd_avg,
		median(pm10_rd) AS pm10_rd_med,
		SUM(pm10_tot) AS pm10_tot_sum,
		AVG(pm10_tot) AS pm10_tot_avg,
		median(pm10_tot) AS pm10_tot_med
	FROM
		mob_cln_exp1
	GROUP BY
		person_id,
		ith_life_stage,
		life_stage
	ORDER BY
		person_id,
		ith_life_stage,
		life_stage;

	ALTER TABLE tmp_mob_cln_exp ADD PRIMARY KEY (person_id, ith_life_stage);

END;
$$   LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION common_calc_mob_cln_exp_err()
	RETURNS void AS 
$$
DECLARE

BEGIN
		
	-- Here we try to calculate error based on separate contributions of start date adjustments and end date adjustments
	-- The basic idea is that we create a date series of every day of every life stage of every study member and then
	-- figure out which of the adjustment ranges overlaps with that day.
	--
	-- So in the case of looking at adjustments due to gaps, we would determine which days of a life stage
	-- overlapped with the [start_date_delta1, start_date_delta2] of an address period.  When we identify any days
	-- that may be in contention, we capture the geocode assigned for that address period and what the alternative start
	-- date could have been, which would be the geocode of the previous address period.  Later on, we would obtain the 
	-- error values for each type of pollutant based on the absolute value of the difference in daily exposure values 
	-- between the daily exposure value that was used and the exposure value that *could have* been used.
	-- Therefore, the error is really the measure of opportunity cost of having used an assigned exposure instead of
	-- the alternative exposure value.
	-- 
	-- With start date contributions, we can guarantee that each day would overlap with at most one gap.
	
	-- However, overlaps require a slightly different approach.  Consider the following:
	--
	--  address period                            May 01    May 02    May 03    May 04    May 05    May 06
	--  a1 [01-MAY-94, 04-MAY-94]                 |-----------------------------------|
	--  a2 [02-MAY-94, 05-MAY-94]                           |----------------------------------|
	--  a3 [04-MAY-94, 06-MAY-94]                                               |-------------------------|
	--
	-- Here, 04-MAY-94 will overlap with both a1 and a2.  a3 will have the assigned value for 04-MAY-94 but
	-- we need to make sure that the opportunity cost that is used is from a2 and not a1.  If more than one
	-- end date adjustment overlaps with a given life stage day, we need to take the opportunity cost of 
	-- the closest overlapping period, which in this case would be a2.  Because of the ordering of the 
	-- address periods, we can rely on the maximum value of ith_residence for overlapping periods to serve as
	-- the period from which we get our opportunity cost geocode.  
	--
	-- When we encounter a life stage day that is associated with adjusted end dates from multiple address periods,
	-- we use the maximum ith residence to produce a 'keep' flag.  The flag is then used to ensure that each life
	-- stage date is associated with *at most* one overlap.
	
	
	-- In the query below, we use the following query steps
	-- "addr_with_error": find all the address periods that have something specified for [start_date_delta1, start_date_delta2]
	-- "tmp_life_stage_days": generate an in-memory table with each day of each life stage of each person
	-- "start_date_changes1": for those life stage days that do overlap with a start date delta, capture
	--    the assigned and opportunity cost geocode values
	-- Where there is a value for both assigned and opportunity geocodes, obtain exposure values for each type of pollutant.
	-- Obtain the difference and we'll call this the opportunity cost associated with filling gaps.
	-- 
	-- We should get a table that contains every day of every life stage of every person.  For each date, we will
	-- get a difference between exposures for assigned and opportunity geocodes, or zero if the date did not overlap with
	-- any change in start date.

	-- Assess errors due to gaps
	-- Determine opportunity cost error for days that overlap with a changed start date in
	-- one of the address periods
	
	-- note that we could get null exposure values if they are missing
	DROP TABLE IF EXISTS tmp_gap_errors1; 
	CREATE TABLE tmp_gap_errors1 AS
	WITH cleaned_addr_gap_data AS 
		(SELECT
			person_id,
			geocode,
			ith_residence,
			start_date_delta1,
			start_date_delta2,
			previous_geocode
	 	FROM
	 		fin_cleaned_addr_periods a
	 	WHERE 
	 		is_fixed_invalid_geocode = 'N' AND
	 		start_date_delta1 IS NOT NULL AND 
	 		start_date_delta2 IS NOT NULL),
	start_date_changes1 AS
		(SELECT
			c.person_id,
			c.ith_life_stage,
			c.life_stage,
			c.date_of_year,
			d.geocode AS assigned_geocode, -- even though this is a left join, this should not be null
			d.previous_geocode AS opportunity_geocode -- this will be null if no gap
		FROM
			tmp_life_stage_days c,
			cleaned_addr_gap_data d
		WHERE
			c.person_id = d.person_id AND
			c.date_of_year BETWEEN d.start_date_delta1 AND d.start_date_delta2)
	SELECT
		e.person_id,
		e.ith_life_stage,
		e.life_stage,
		e.date_of_year,
		e.assigned_geocode,
		e.opportunity_geocode,
		TRUE AS has_gap,
		ABS(f.name - g.name) AS name_start_err,
		ABS(f.nox_rd - g.nox_rd) AS nox_rd_start_err,
		ABS(f.pm10_gr - g.pm10_gr) AS pm10_gr_start_err,
		ABS(f.pm10_rd - g.pm10_rd) AS pm10_rd_start_err,
		ABS(f.pm10_tot - g.pm10_tot) AS pm10_tot_start_err
	FROM
		start_date_changes1 e
	LEFT JOIN fin_daily_exposures f ON 
	 	e.assigned_geocode = f.geocode AND 
	 	e.date_of_year = f.date_of_year
	LEFT JOIN fin_daily_exposures g ON 
	 	e.opportunity_geocode = g.geocode AND 
	 	e.date_of_year = g.date_of_year
	ORDER BY
		e.person_id,
		e.date_of_year;
	ALTER TABLE tmp_gap_errors1 ADD PRIMARY KEY (person_id, date_of_year);

	DROP TABLE IF EXISTS tmp_gap_errors2; 
	CREATE TABLE tmp_gap_errors2 AS
	SELECT
		a.person_id,
		a.ith_life_stage,
		a.life_stage,
		a.date_of_year,
		CASE
			WHEN b.has_gap IS NULL THEN
				FALSE
			ELSE
				TRUE
		END AS has_gap_contention,
		CASE
			WHEN b.has_gap IS NULL THEN
				0
			ELSE
				b.name_start_err
		END AS name_start_err,
		CASE
			WHEN b.has_gap IS NULL THEN
				0
			ELSE
				b.nox_rd_start_err
		END AS nox_rd_start_err,
		CASE
			WHEN b.has_gap IS NULL THEN
				0
			ELSE
				b.pm10_gr_start_err
		END AS pm10_gr_start_err,
		CASE
			WHEN b.has_gap IS NULL THEN
				0
			ELSE
				b.pm10_rd_start_err
		END AS pm10_rd_start_err,
		CASE
			WHEN b.has_gap IS NULL THEN
				0
			ELSE
				b.pm10_tot_start_err
		END AS pm10_tot_start_err
	FROM
		tmp_life_stage_days a
	LEFT JOIN
		tmp_gap_errors1	b
	ON
		a.person_id = b.person_id AND
		a.date_of_year = b.date_of_year
	ORDER BY
		a.person_id,
		a.date_of_year;

	ALTER TABLE tmp_gap_errors2 ADD PRIMARY KEY (person_id, date_of_year);
   	DROP INDEX IF EXISTS ind_tmp_gap_errors2;
	CREATE INDEX  ind_tmp_gap_errors2 ON tmp_gap_errors2(ith_life_stage);

	-- In the following query, we do most of the same things as we did to obtain errors that 
	-- owe to changes in start dates. The difference is that we have steps "max_end_date_changes" and
	-- "end_date_changes2" to ensure we pick at most one address period whose end date changes overlap
	-- with the date of year 

	DROP TABLE IF EXISTS tmp_over_lap_errors1;
	CREATE TABLE tmp_over_lap_errors1 AS
	WITH cleaned_addr_over_lap_data AS 
		(SELECT
			person_id,
			geocode,
			ith_residence,
			fin_adjusted_start_date,
			fin_adjusted_end_date,
			end_date_delta1,
			end_date_delta2,
			fit_type
	 	FROM
	 		fin_cleaned_addr_periods
	 	WHERE 
	 		is_fixed_invalid_geocode = 'N' AND
	 		end_date_delta1 IS NOT NULL AND end_date_delta2 IS NOT NULL),
	opportunity_exp1 AS		
		(SELECT
			e.person_id,
			e.ith_life_stage,
			e.life_stage,
			e.date_of_year,
			f.ith_residence,
			f.geocode AS opportunity_geocode
		FROM
			tmp_life_stage_days e,
			cleaned_addr_over_lap_data f
		WHERE
			e.person_id = f.person_id AND
			e.date_of_year BETWEEN f.end_date_delta1 AND f.end_date_delta2),
	closest_opportunity_exp AS
		(SELECT
			person_id,
			date_of_year,
			ith_life_stage,
			MAX(ith_residence) AS best_opportunity_choice
		 FROM
		 	opportunity_exp1
		 GROUP BY
		 	person_id,
		 	date_of_year,
		 	ith_life_stage),
	opportunity_exp2 AS
		(SELECT
			g.person_id,
			g.ith_life_stage,
			g.life_stage,
			g.date_of_year,
			g.opportunity_geocode
		 FROM
		 	opportunity_exp1 g,
		 	closest_opportunity_exp h
		 WHERE
		 	g.person_id = h.person_id AND
		 	g.date_of_year = h.date_of_year AND
		 	g.ith_residence = h.best_opportunity_choice)
	SELECT
		j.person_id,
		j.ith_life_stage,
		j.life_stage,
		j.date_of_year,
		j.geocode AS assigned_geocode,
		k.opportunity_geocode
	FROM
		tmp_common_exp_periods1 j
	LEFT JOIN
		opportunity_exp2 k
	ON
		j.person_id = k.person_id AND
		j.date_of_year = k.date_of_year;		

	ALTER TABLE tmp_over_lap_errors1 ADD PRIMARY KEY (person_id, date_of_year);

   	DROP INDEX IF EXISTS ind_tmp_over_lap_errors1;
	CREATE INDEX  ind_tmp_over_lap_errors1 ON tmp_over_lap_errors1(assigned_geocode);

   	DROP INDEX IF EXISTS ind_tmp_over_lap_errors2;
	CREATE INDEX  ind_tmp_over_lap_errors2 ON tmp_over_lap_errors1(opportunity_geocode);
	
	DROP TABLE IF EXISTS tmp_over_lap_errors2;
	CREATE TABLE tmp_over_lap_errors2 AS
	SELECT
		a.person_id,
		a.ith_life_stage,
		a.life_stage,
		a.date_of_year,
		a.assigned_geocode,
		a.opportunity_geocode,
		CASE
			WHEN a.opportunity_geocode IS NULL THEN
				FALSE
			ELSE
				TRUE
		END AS has_over_lap,
		ABS(b.name - c.name) AS name_end_err,
		ABS(b.nox_rd - c.nox_rd) AS nox_rd_end_err,
		ABS(b.pm10_gr - c.pm10_gr) AS pm10_gr_end_err,
		ABS(b.pm10_rd - c.pm10_rd) AS pm10_rd_end_err,
		ABS(b.pm10_tot - c.pm10_tot) AS pm10_tot_end_err
	FROM
		tmp_over_lap_errors1 a
	LEFT JOIN fin_daily_exposures b ON 
	 	a.assigned_geocode = b.geocode AND 
	 	a.date_of_year = b.date_of_year
	LEFT JOIN fin_daily_exposures c ON 
	 	a.opportunity_geocode = c.geocode AND 
	 	a.date_of_year = c.date_of_year
	ORDER BY
		a.person_id,
		a.date_of_year;

	ALTER TABLE tmp_over_lap_errors2 ADD PRIMARY KEY (person_id, date_of_year);

	DROP TABLE IF EXISTS tmp_over_lap_errors3;
	CREATE TABLE tmp_over_lap_errors3 AS
	SELECT
		a.person_id,
		a.ith_life_stage,
		a.life_stage,
		a.date_of_year,	
		b.has_over_lap AS has_over_lap_contention,
		CASE
			WHEN b.has_over_lap = FALSE THEN
				0
			ELSE
				b.name_end_err
		END AS name_end_err,
		CASE
			WHEN b.has_over_lap = FALSE THEN
				0
			ELSE
				b.nox_rd_end_err
		END AS nox_rd_end_err,
		CASE
			WHEN b.has_over_lap = FALSE THEN
				0
			ELSE
				b.pm10_gr_end_err
		END AS pm10_gr_end_err,
		CASE
			WHEN b.has_over_lap = FALSE THEN
				0
			ELSE
				b.pm10_rd_end_err
		END AS pm10_rd_end_err,
		CASE
			WHEN b.has_over_lap = FALSE THEN
				0
			ELSE
				b.pm10_tot_end_err
		END AS pm10_tot_end_err
	FROM
		tmp_life_stage_days a
	LEFT JOIN 
		tmp_over_lap_errors2 b
	ON
		a.person_id = b.person_id AND
		a.date_of_year = b.date_of_year
	ORDER BY
		a.person_id,
		a.date_of_year;
	ALTER TABLE tmp_over_lap_errors3 ADD PRIMARY KEY (person_id, date_of_year);

	-- We create this table and keep it around in order to help assess
	-- unimputed exposure values
	DROP TABLE IF EXISTS tmp_all_error_contributions_per_day;
	CREATE TABLE tmp_all_error_contributions_per_day AS
	WITH all_err_contributions_per_day AS
		(SELECT
			a.person_id,
			a.ith_life_stage,
			a.life_stage,
			a.date_of_year,
			a.has_gap_contention,
			b.has_over_lap_contention,
			a.name_start_err + b.name_end_err AS name_err,
			a.nox_rd_start_err + b.nox_rd_end_err AS nox_rd_err,
			a.pm10_gr_start_err + b.pm10_gr_end_err AS pm10_gr_err,
			a.pm10_rd_start_err + b.pm10_rd_end_err AS pm10_rd_err,
			a.pm10_tot_start_err + b.pm10_tot_end_err AS pm10_tot_err
		FROM
			tmp_gap_errors2 a,
			tmp_over_lap_errors3 b
		WHERE
			a.person_id = b.person_id AND
			a.date_of_year = b.date_of_year
		ORDER BY
			a.person_id,
			a.ith_life_stage,
			a.date_of_year)
	SELECT
			person_id,
			ith_life_stage,
			life_stage,
			date_of_year,
			name_err,
			nox_rd_err,
			pm10_gr_err,
			pm10_rd_err,
			pm10_tot_err,
			CASE
				WHEN has_gap_contention = TRUE OR 
					has_over_lap_contention = TRUE THEN
					TRUE
				ELSE
					FALSE
			END has_contention
	FROM
		all_err_contributions_per_day
	ORDER BY
		person_id,
		ith_life_stage;

   	ALTER TABLE tmp_all_error_contributions_per_day ADD PRIMARY KEY (person_id, date_of_year);
   	DROP INDEX IF EXISTS ind_tmp_all_error_contributions_per_day1;
	CREATE INDEX  ind_tmp_all_error_contributions_per_day1 ON tmp_all_error_contributions_per_day(life_stage);	
   	DROP INDEX IF EXISTS ind_tmp_all_error_contributions_per_day2;
	CREATE INDEX  ind_tmp_all_error_contributions_per_day2 ON tmp_all_error_contributions_per_day(ith_life_stage);	
   	DROP INDEX IF EXISTS ind_tmp_all_error_contributions_per_day3;
	CREATE INDEX  ind_tmp_all_error_contributions_per_day3 ON tmp_all_error_contributions_per_day(has_contention);	

	-- Now combine the tables together
	DROP TABLE IF EXISTS tmp_mob_cln_exp_err;
	CREATE TABLE tmp_mob_cln_exp_err AS
	SELECT
		person_id,
		ith_life_stage,
		life_stage,
		SUM(name_err) AS name_err_sum,
		AVG(name_err) AS name_err_avg,
		median(name_err) AS name_err_med,
		SUM(nox_rd_err) AS nox_rd_err_sum,
		AVG(nox_rd_err) AS nox_rd_err_avg,
		median(nox_rd_err) AS nox_rd_err_med,
		SUM(pm10_gr_err) AS pm10_gr_err_sum,
		AVG(pm10_gr_err) AS pm10_gr_err_avg,
		median(pm10_gr_err) AS pm10_gr_err_med,
		SUM(pm10_rd_err) AS pm10_rd_err_sum,
		AVG(pm10_rd_err) AS pm10_rd_err_avg,
		median(pm10_rd_err) AS pm10_rd_err_med,
		SUM(pm10_tot_err) AS pm10_tot_err_sum,
		AVG(pm10_tot_err) AS pm10_tot_err_avg,
		median(pm10_tot_err) AS pm10_tot_err_med
	FROM
		tmp_all_error_contributions_per_day
	GROUP BY
		person_id,
		ith_life_stage,
		life_stage
	ORDER BY
		person_id,
		ith_life_stage,
		life_stage;

END;
$$   LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION common_combine_mob_cln_exp_with_err()
	RETURNS void AS 
$$
DECLARE

BEGIN
	
	-- Will ensure that null values are supplied for study members who,
	-- for one reason or another, had no exposure values 
	DROP TABLE IF EXISTS fin_mob_cln_exp;
	CREATE TABLE fin_mob_cln_exp AS
	WITH study_members_with_exposures AS 
		(SELECT
			a.person_id,
			a.ith_life_stage,
			a.life_stage,
			COALESCE(a.name_invalid_address_days, 0) AS name_invalid_address_days,
			COALESCE(a.name_oob_days, 0) AS name_oob_days,
			COALESCE(a.name_poor_address_days, 0) AS name_poor_address_days,
			COALESCE(a.name_missing_exp_days, 0) AS name_missing_exp_days,
			COALESCE(a.name_good_address_days, 0) AS name_good_address_days,
			COALESCE(a.nox_rd_invalid_address_days, 0) AS nox_rd_invalid_address_days,
			COALESCE(a.nox_rd_oob_days, 0) AS nox_rd_oob_days,
			COALESCE(a.nox_rd_poor_address_days, 0) AS nox_rd_poor_address_days,
			COALESCE(a.nox_rd_missing_exp_days, 0) AS nox_rd_missing_exp_days,
			COALESCE(a.nox_rd_good_address_days, 0) AS nox_rd_good_address_days,
			COALESCE(a.pm10_rd_invalid_address_days, 0) AS pm10_rd_invalid_address_days,
			COALESCE(a.pm10_rd_oob_days, 0) AS pm10_rd_oob_days,
			COALESCE(a.pm10_rd_poor_address_days, 0) AS pm10_rd_poor_address_days,
			COALESCE(a.pm10_rd_missing_exp_days, 0) AS pm10_rd_missing_exp_days,
			COALESCE(a.pm10_rd_good_address_days, 0) AS pm10_rd_good_address_days,
			COALESCE(a.pm10_gr_invalid_address_days, 0) AS pm10_gr_invalid_address_days,
			COALESCE(a.pm10_gr_oob_days, 0) AS pm10_gr_oob_days,
			COALESCE(a.pm10_gr_poor_address_days, 0) AS pm10_gr_poor_address_days,
			COALESCE(a.pm10_gr_missing_exp_days, 0) AS pm10_gr_missing_exp_days,
			COALESCE(a.pm10_gr_good_address_days, 0) AS pm10_gr_good_address_days,
			COALESCE(a.pm10_tot_invalid_address_days, 0) AS pm10_tot_invalid_address_days,
			COALESCE(a.pm10_tot_oob_days, 0) AS pm10_tot_oob_days,
			COALESCE(a.pm10_tot_poor_address_days, 0) AS pm10_tot_poor_address_days,
			COALESCE(a.pm10_tot_missing_exp_days, 0) AS pm10_tot_missing_exp_days,
			COALESCE(a.pm10_tot_good_address_days, 0) AS pm10_tot_good_address_days,
			a.name_sum,
			b.name_err_sum,
			a.name_avg,
			b.name_err_avg,
			a.name_med,
			b.name_err_med,
			a.nox_rd_sum,
			b.nox_rd_err_sum,
			a.nox_rd_avg,
			b.nox_rd_err_avg,
			a.nox_rd_med,
			b.nox_rd_err_med,
			a.pm10_gr_sum,
			b.pm10_gr_err_sum,
			a.pm10_gr_avg,
			b.pm10_gr_err_avg,
			a.pm10_gr_med,
			b.pm10_gr_err_med,
			a.pm10_rd_sum,
			b.pm10_rd_err_sum,
			a.pm10_rd_avg,
			b.pm10_rd_err_avg,
			a.pm10_rd_med,
			b.pm10_rd_err_med,
			a.pm10_tot_sum,
			b.pm10_tot_err_sum,	
			a.pm10_tot_avg,
			b.pm10_tot_err_avg,
			a.pm10_tot_med,
			b.pm10_tot_err_med
		FROM
			tmp_mob_cln_exp a,
			tmp_mob_cln_exp_err b
		WHERE
			a.person_id = b.person_id AND
			a.ith_life_stage = b.ith_life_stage
		ORDER BY
			a.person_id,
			a.ith_life_stage)
	SELECT
		c.person_id,
		c.ith_life_stage,
		c.life_stage,
		c.life_stage_duration,
		d.name_invalid_address_days,
		d.name_oob_days,
		d.name_poor_address_days,
		d.name_missing_exp_days,
		d.name_good_address_days,	
		d.nox_rd_invalid_address_days,
		d.nox_rd_oob_days,
		d.nox_rd_poor_address_days,
		d.nox_rd_missing_exp_days,
		d.nox_rd_good_address_days,
		d.pm10_rd_invalid_address_days,
		d.pm10_rd_oob_days,
		d.pm10_rd_poor_address_days,
		d.pm10_rd_missing_exp_days,
		d.pm10_rd_good_address_days,
		d.pm10_gr_invalid_address_days,
		d.pm10_gr_oob_days,
		d.pm10_gr_poor_address_days,
		d.pm10_gr_missing_exp_days,
		d.pm10_gr_good_address_days,
		d.pm10_tot_invalid_address_days,
		d.pm10_tot_oob_days,
		d.pm10_tot_poor_address_days,
		d.pm10_tot_missing_exp_days,
		d.pm10_tot_good_address_days,
		d.name_sum,
		d.name_err_sum,
		d.name_avg,
		d.name_err_avg,
		d.name_med,
		d.name_err_med,
		d.nox_rd_sum,
		d.nox_rd_err_sum,
		d.nox_rd_avg,
		d.nox_rd_err_avg,
		d.nox_rd_med,
		d.nox_rd_err_med,
		d.pm10_gr_sum,
		d.pm10_gr_err_sum,
		d.pm10_gr_avg,
		d.pm10_gr_err_avg,
		d.pm10_gr_med,
		d.pm10_gr_err_med,
		d.pm10_rd_sum,
		d.pm10_rd_err_sum,
		d.pm10_rd_avg,
		d.pm10_rd_err_avg,
		d.pm10_rd_med,
		d.pm10_rd_err_med,
		d.pm10_tot_sum,
		d.pm10_tot_err_sum,	
		d.pm10_tot_avg,
		d.pm10_tot_err_avg,
		d.pm10_tot_med,
		d.pm10_tot_err_med
	FROM
		fin_general_life_stage_data c
	LEFT JOIN
		study_members_with_exposures d
	ON
		c.person_id = d.person_id AND
		c.ith_life_stage = d.ith_life_stage
	ORDER BY 
		c.person_id,
		c.ith_life_stage;

	ALTER TABLE fin_mob_cln_exp ADD PRIMARY KEY (person_id, ith_life_stage);

			
END;
$$   LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION common_calc_mob_uncln_exp()
	RETURNS void AS 
$$
DECLARE

BEGIN

	-- Gather up all the cleaned addresses that will be used to provide locations for
	-- days.  Generate a table that has a date entry for every life stage of every person
	-- and then one of the temporary tables for calculating errors to ignore dates that
	-- are involved with gaps and overlaps.  Of the dates that are left, determine
	-- which address period contributes a location for each day.  Link the locations
	-- to exposure values based on geocode and date of year.  Then aggregate exposure
	-- values.
	DROP TABLE IF EXISTS tmp_mob_uncln_exp1;
	CREATE TABLE tmp_mob_uncln_exp1 AS	
	WITH days_without_contention AS  -- only consider days that are not involved in gaps or overlaps
		(SELECT
			a.person_id,
			a.ith_life_stage,
			a.life_stage,
			a.date_of_year
		 FROM
		 	tmp_life_stage_days a,
		 	tmp_all_error_contributions_per_day b
		 WHERE
		 	a.person_id = b.person_id AND
		 	a.ith_life_stage = b.ith_life_stage AND
		 	a.date_of_year = b.date_of_year AND
		 	b.has_contention = 'N')
	SELECT
		c.person_id,
		c.ith_life_stage,
		c.life_stage,
		c.geocode,
		c.ith_residence,
		c.date_of_year,
		c.has_valid_geocode,
		c.has_name_exposures,
		c.has_nox_rd_exposures,
		c.has_pm10_gr_exposures,
		c.has_pm10_rd_exposures,
		c.has_pm10_tot_exposures		
	FROM
		tmp_common_exp_periods1 c,
		days_without_contention d
	WHERE
		c.person_id = d.person_id AND
		c.ith_life_stage = d.ith_life_stage AND
		c.life_stage = d.life_stage AND
		c.date_of_year = d.date_of_year
	ORDER BY
		c.person_id,
		c.ith_life_stage;

	ALTER TABLE tmp_mob_uncln_exp1 ADD PRIMARY KEY (person_id, date_of_year);

	DROP TABLE IF EXISTS tmp_mob_uncln_exp2;
	CREATE TABLE tmp_mob_uncln_exp2 AS 	
	WITH study_members_with_exposures AS
		(SELECT
			a.person_id,
			a.ith_life_stage,
			a.life_stage,
			a.ith_residence,
			a.date_of_year,
			CASE
				WHEN a.has_name_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS name_invalid_address_count,
			CASE
				WHEN a.has_name_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS name_oob_count,
			CASE
				WHEN a.has_name_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS name_poor_address_count,
			CASE
				WHEN a.has_name_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.name IS NULL THEN
					1
				ELSE 
					0
			END AS name_missing_exp_count,
			CASE
				WHEN a.has_name_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.name IS NOT NULL THEN
					1
				ELSE 
					0
			END AS name_good_address_count,
			b.name,
			CASE
				WHEN a.has_nox_rd_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS nox_rd_invalid_address_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS nox_rd_oob_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS nox_rd_poor_address_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.nox_rd IS NULL THEN
					1
				ELSE 
					0
			END AS nox_rd_missing_exp_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.nox_rd IS NOT NULL THEN
					1
				ELSE 
					0
			END AS nox_rd_good_address_count,
			b.nox_rd,
			CASE
				WHEN a.has_pm10_rd_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_rd_invalid_address_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS pm10_rd_oob_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_rd_poor_address_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_rd IS NULL THEN
					1
				ELSE 
					0
			END AS pm10_rd_missing_exp_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_rd IS NOT NULL THEN
					1
				ELSE 
					0
			END AS pm10_rd_good_address_count,
			b.pm10_rd,
			CASE
				WHEN a.has_pm10_gr_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_gr_invalid_address_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS pm10_gr_oob_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_gr_poor_address_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_gr IS NULL THEN
					1
				ELSE 
					0
			END AS pm10_gr_missing_exp_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_gr IS NOT NULL THEN
					1
				ELSE 
					0
			END AS pm10_gr_good_address_count,
			b.pm10_gr,
			CASE
				WHEN a.has_pm10_tot_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_tot_invalid_address_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS pm10_tot_oob_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_tot_poor_address_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_tot IS NULL THEN
					1
				ELSE 
					0
			END AS pm10_tot_missing_exp_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_tot IS NOT NULL THEN
					1
				ELSE 
					0
			END AS pm10_tot_good_address_count,
			b.pm10_tot
		FROM
			tmp_mob_uncln_exp1 a
		LEFT JOIN
			fin_daily_exposures b
		ON
			a.geocode = b.geocode AND
			a.date_of_year = b.date_of_year)
	SELECT
		person_id,
		ith_life_stage,
		life_stage,
		SUM(name_invalid_address_count) AS name_invalid_address_days,
		SUM(name_oob_count) AS name_oob_days,
		SUM(name_poor_address_count) AS name_poor_address_days,
		SUM(name_missing_exp_count) AS name_missing_exp_days,
		SUM(name_good_address_count) AS name_good_address_days,
		
		SUM(nox_rd_invalid_address_count) AS nox_rd_invalid_address_days,
		SUM(nox_rd_oob_count) AS nox_rd_oob_days,
		SUM(nox_rd_poor_address_count) AS nox_rd_poor_address_days,
		SUM(nox_rd_missing_exp_count) AS nox_rd_missing_exp_days,
		SUM(nox_rd_good_address_count) AS nox_rd_good_address_days,
		
		SUM(pm10_rd_invalid_address_count) AS pm10_rd_invalid_address_days,
		SUM(pm10_rd_oob_count) AS pm10_rd_oob_days,
		SUM(pm10_rd_poor_address_count) AS pm10_rd_poor_address_days,
		SUM(pm10_rd_missing_exp_count) AS pm10_rd_missing_exp_days,
		SUM(pm10_rd_good_address_count) AS pm10_rd_good_address_days,
				
		SUM(pm10_gr_invalid_address_count) AS pm10_gr_invalid_address_days,
		SUM(pm10_gr_oob_count) AS pm10_gr_oob_days,
		SUM(pm10_gr_poor_address_count) AS pm10_gr_poor_address_days,
		SUM(pm10_gr_missing_exp_count) AS pm10_gr_missing_exp_days,
		SUM(pm10_gr_good_address_count) AS pm10_gr_good_address_days,
		
		SUM(pm10_tot_invalid_address_count) AS pm10_tot_invalid_address_days,
		SUM(pm10_tot_oob_count) AS pm10_tot_oob_days,
		SUM(pm10_tot_poor_address_count) AS pm10_tot_poor_address_days,
		SUM(pm10_tot_missing_exp_count) AS pm10_tot_missing_exp_days,
		SUM(pm10_tot_good_address_count) AS pm10_tot_good_address_days,

		SUM(name) AS name_sum,
		AVG(name) AS name_avg,
		median(name) AS name_med,
		SUM(nox_rd) AS nox_rd_sum,
		AVG(nox_rd) AS nox_rd_avg,
		median(nox_rd) AS nox_rd_med,
		SUM(pm10_gr) AS pm10_gr_sum,
		AVG(pm10_gr) AS pm10_gr_avg,
		median(pm10_gr) AS pm10_gr_med,
		SUM(pm10_rd) AS pm10_rd_sum,
		AVG(pm10_rd) AS pm10_rd_avg,
		median(pm10_rd) AS pm10_rd_med,
		SUM(pm10_tot) AS pm10_tot_sum,
		AVG(pm10_tot) AS pm10_tot_avg,
		median(pm10_tot) AS pm10_tot_med
	FROM
		study_members_with_exposures
	GROUP BY
		person_id,
		ith_life_stage,
		life_stage
	ORDER BY
		person_id,
		ith_life_stage,
		life_stage;

	ALTER TABLE tmp_mob_uncln_exp2 ADD PRIMARY KEY (person_id, ith_life_stage);


	DROP TABLE IF EXISTS fin_mob_uncln_exp;
	CREATE TABLE fin_mob_uncln_exp AS 	
	SELECT
		c.person_id,
		c.ith_life_stage,
		c.life_stage,
		c.life_stage_duration,
		COALESCE(d.name_invalid_address_days, 0) AS name_invalid_address_days,
		COALESCE(d.name_oob_days, 0) AS name_oob_days,
		COALESCE(d.name_poor_address_days, 0) AS name_poor_address_days,
		COALESCE(d.name_missing_exp_days, 0) AS name_missing_exp_days,
		COALESCE(d.name_good_address_days, 0) AS name_good_address_days,
		COALESCE(d.nox_rd_invalid_address_days, 0) AS nox_rd_invalid_address_days,
		COALESCE(d.nox_rd_oob_days, 0) AS nox_rd_oob_days,
		COALESCE(d.nox_rd_poor_address_days, 0) AS nox_rd_poor_address_days,
		COALESCE(d.nox_rd_missing_exp_days, 0) AS nox_rd_missing_exp_days,
		COALESCE(d.nox_rd_good_address_days, 0) AS nox_rd_good_address_days,
		COALESCE(d.pm10_rd_invalid_address_days, 0) AS pm10_rd_invalid_address_days,
		COALESCE(d.pm10_rd_oob_days, 0) AS pm10_rd_oob_days,
		COALESCE(d.pm10_rd_poor_address_days, 0) AS pm10_rd_poor_address_days,
		COALESCE(d.pm10_rd_missing_exp_days, 0) AS pm10_rd_missing_exp_days,
		COALESCE(d.pm10_rd_good_address_days, 0) AS pm10_rd_good_address_days,
		COALESCE(d.pm10_gr_invalid_address_days, 0) AS pm10_gr_invalid_address_days,
		COALESCE(d.pm10_gr_oob_days, 0) AS pm10_gr_oob_days,
		COALESCE(d.pm10_gr_poor_address_days, 0) AS pm10_gr_poor_address_days,
		COALESCE(d.pm10_gr_missing_exp_days, 0) AS pm10_gr_missing_exp_days,
		COALESCE(d.pm10_gr_good_address_days, 0) AS pm10_gr_good_address_days,
		COALESCE(d.pm10_tot_invalid_address_days, 0) AS pm10_tot_invalid_address_days,
		COALESCE(d.pm10_tot_oob_days, 0) AS pm10_tot_oob_days,
		COALESCE(d.pm10_tot_poor_address_days, 0) AS pm10_tot_poor_address_days,
		COALESCE(d.pm10_tot_missing_exp_days, 0) AS pm10_tot_missing_exp_days,
		COALESCE(d.pm10_tot_good_address_days, 0) AS pm10_tot_good_address_days,
		d.name_sum,
		d.name_avg,
		d.name_med,
		d.nox_rd_sum,
		d.nox_rd_avg,
		d.nox_rd_med,
		d.pm10_gr_sum,
		d.pm10_gr_avg,
		d.pm10_gr_med,
		d.pm10_rd_sum,
		d.pm10_rd_avg,
		d.pm10_rd_med,
		d.pm10_tot_sum,
		d.pm10_tot_avg,
		d.pm10_tot_med
	FROM
			fin_general_life_stage_data c
	LEFT JOIN
		tmp_mob_uncln_exp2 d
	ON
		c.person_id = d.person_id AND
		c.ith_life_stage = d.ith_life_stage
	ORDER BY
		c.person_id,
		c.ith_life_stage,
		c.life_stage;

	ALTER TABLE fin_mob_uncln_exp ADD PRIMARY KEY (person_id, ith_life_stage);

END;
$$   LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION common_calc_stg_mob_exp()
	RETURNS void AS 
$$
DECLARE

BEGIN

	-- Determine locations at the start of each life stage
	DROP TABLE IF EXISTS tmp_stg_mob_exp1;
	CREATE TABLE tmp_stg_mob_exp1 AS	
	WITH cleaned_addr AS 
		(SELECT
			person_id,
			geocode,
			ith_residence,
			has_valid_geocode,
			has_name_exposures,
			has_nox_rd_exposures,
			has_pm10_gr_exposures,
			has_pm10_rd_exposures,
			has_pm10_tot_exposures,			
			fin_adjusted_start_date,
			fin_adjusted_end_date
		 FROM
	 		fin_cleaned_addr_periods
		 WHERE
			fit_type != 'D' AND
			is_fixed_invalid_geocode = 'N'),
	start_life_stage_addr AS
		(SELECT
			c.person_id,
			d.ith_life_stage,
			d.life_stage,
			c.geocode,
			d.start_date,
			d.end_date,
			c.has_valid_geocode,
			c.has_name_exposures,
			c.has_nox_rd_exposures,
			c.has_pm10_gr_exposures,
			c.has_pm10_rd_exposures,
			c.has_pm10_tot_exposures
		FROM
			cleaned_addr c,
			fin_general_life_stage_data d
		WHERE
			c.person_id = d.person_id AND
			d.start_date BETWEEN c.fin_adjusted_start_date AND c.fin_adjusted_end_date)
	SELECT
		e.person_id,
		e.ith_life_stage,
		e.life_stage,
		e.date_of_year,
		f.geocode,
		f.has_valid_geocode,
		f.has_name_exposures,
		f.has_nox_rd_exposures,
		f.has_pm10_gr_exposures,
		f.has_pm10_rd_exposures,
		f.has_pm10_tot_exposures
	FROM
		tmp_life_stage_days e,
		start_life_stage_addr f
	WHERE
		e.person_id = f.person_id AND
		e.ith_life_stage = f.ith_life_stage
	ORDER BY
		e.person_id,
		e.date_of_year;

	ALTER TABLE tmp_stg_mob_exp1 ADD PRIMARY KEY (person_id, date_of_year);

	DROP TABLE IF EXISTS fin_stg_mob_exp;
	CREATE TABLE fin_stg_mob_exp AS	
	WITH study_members_with_exposures1 AS 
		(SELECT	
			a.person_id,
			a.ith_life_stage,
			a.life_stage,
			CASE
				WHEN a.has_name_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS name_invalid_address_count,
			CASE
				WHEN a.has_name_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS name_oob_count,
			CASE
				WHEN a.has_name_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS name_poor_address_count,
			CASE
				WHEN a.has_name_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.name IS NULL THEN
					1
				ELSE 
					0
			END AS name_missing_exp_count,
			CASE
				WHEN a.has_name_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.name IS NOT NULL THEN
					1
				ELSE 
					0
			END AS name_good_address_count,
			b.name,
			CASE
				WHEN a.has_nox_rd_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS nox_rd_invalid_address_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS nox_rd_oob_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS nox_rd_poor_address_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.nox_rd IS NULL THEN
					1
				ELSE 
					0
			END AS nox_rd_missing_exp_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.nox_rd IS NOT NULL THEN
					1
				ELSE 
					0
			END AS nox_rd_good_address_count,
			b.nox_rd,
			CASE
				WHEN a.has_pm10_rd_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_rd_invalid_address_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS pm10_rd_oob_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_rd_poor_address_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_rd IS NULL THEN
					1
				ELSE 
					0
			END AS pm10_rd_missing_exp_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_rd IS NOT NULL THEN
					1
				ELSE 
					0
			END AS pm10_rd_good_address_count,
			b.pm10_rd,
			CASE
				WHEN a.has_pm10_gr_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_gr_invalid_address_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS pm10_gr_oob_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_gr_poor_address_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_gr IS NULL THEN
					1
				ELSE 
					0
			END AS pm10_gr_missing_exp_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_gr IS NOT NULL THEN
					1
				ELSE 
					0
			END AS pm10_gr_good_address_count,
			b.pm10_gr,
			CASE
				WHEN a.has_pm10_tot_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_tot_invalid_address_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS pm10_tot_oob_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_tot_poor_address_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_tot IS NULL THEN
					1
				ELSE 
					0
			END AS pm10_tot_missing_exp_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_tot IS NOT NULL THEN
					1
				ELSE 
					0
			END AS pm10_tot_good_address_count,
			b.pm10_tot	
		FROM
			tmp_stg_mob_exp1 a
		LEFT JOIN
			fin_daily_exposures b
		ON
			a.geocode = b.geocode AND
			a.date_of_year = b.date_of_year),
	study_members_with_exposures2 AS
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			
			SUM(name_invalid_address_count) AS name_invalid_address_days,
			SUM(name_oob_count) AS name_oob_days,
			SUM(name_poor_address_count) AS name_poor_address_days,
			SUM(name_missing_exp_count) AS name_missing_exp_days,
			SUM(name_good_address_count) AS name_good_address_days,
		
			SUM(nox_rd_invalid_address_count) AS nox_rd_invalid_address_days,
			SUM(nox_rd_oob_count) AS nox_rd_oob_days,
			SUM(nox_rd_poor_address_count) AS nox_rd_poor_address_days,
			SUM(nox_rd_missing_exp_count) AS nox_rd_missing_exp_days,
			SUM(nox_rd_good_address_count) AS nox_rd_good_address_days,
		
			SUM(pm10_rd_invalid_address_count) AS pm10_rd_invalid_address_days,
			SUM(pm10_rd_oob_count) AS pm10_rd_oob_days,
			SUM(pm10_rd_poor_address_count) AS pm10_rd_poor_address_days,
			SUM(pm10_rd_missing_exp_count) AS pm10_rd_missing_exp_days,
			SUM(pm10_rd_good_address_count) AS pm10_rd_good_address_days,
				
			SUM(pm10_gr_invalid_address_count) AS pm10_gr_invalid_address_days,
			SUM(pm10_gr_oob_count) AS pm10_gr_oob_days,
			SUM(pm10_gr_poor_address_count) AS pm10_gr_poor_address_days,
			SUM(pm10_gr_missing_exp_count) AS pm10_gr_missing_exp_days,
			SUM(pm10_gr_good_address_count) AS pm10_gr_good_address_days,
		
			SUM(pm10_tot_invalid_address_count) AS pm10_tot_invalid_address_days,
			SUM(pm10_tot_oob_count) AS pm10_tot_oob_days,
			SUM(pm10_tot_poor_address_count) AS pm10_tot_poor_address_days,
			SUM(pm10_tot_missing_exp_count) AS pm10_tot_missing_exp_days,
			SUM(pm10_tot_good_address_count) AS pm10_tot_good_address_days,

			SUM(name) AS name_sum,
			AVG(name) AS name_avg,
			median(name) AS name_med,
			SUM(nox_rd) AS nox_rd_sum,
			AVG(nox_rd) AS nox_rd_avg,
			median(nox_rd) AS nox_rd_med,
			SUM(pm10_gr) AS pm10_gr_sum,
			AVG(pm10_gr) AS pm10_gr_avg,
			median(pm10_gr) AS pm10_gr_med,
			SUM(pm10_rd) AS pm10_rd_sum,
			AVG(pm10_rd) AS pm10_rd_avg,
			median(pm10_rd) AS pm10_rd_med,
			SUM(pm10_tot) AS pm10_tot_sum,
			AVG(pm10_tot) AS pm10_tot_avg,
			median(pm10_tot) AS pm10_tot_med
		FROM
			study_members_with_exposures1
		GROUP BY
			person_id,
			ith_life_stage,
			life_stage)
	SELECT
		c.person_id,
		c.ith_life_stage,
		c.life_stage,
		c.life_stage_duration,
		COALESCE(d.name_invalid_address_days, 0) AS name_invalid_address_days,
		COALESCE(d.name_oob_days, 0) AS name_oob_days,
		COALESCE(d.name_poor_address_days, 0) AS name_poor_address_days,
		COALESCE(d.name_missing_exp_days, 0) AS name_missing_exp_days,
		COALESCE(d.name_good_address_days, 0) AS name_good_address_days,
		COALESCE(d.nox_rd_invalid_address_days, 0) AS nox_rd_invalid_address_days,
		COALESCE(d.nox_rd_oob_days, 0) AS nox_rd_oob_days,
		COALESCE(d.nox_rd_poor_address_days, 0) AS nox_rd_poor_address_days,
		COALESCE(d.nox_rd_missing_exp_days, 0) AS nox_rd_missing_exp_days,
		COALESCE(d.nox_rd_good_address_days, 0) AS nox_rd_good_address_days,
		COALESCE(d.pm10_rd_invalid_address_days, 0) AS pm10_rd_invalid_address_days,
		COALESCE(d.pm10_rd_oob_days, 0) AS pm10_rd_oob_days,
		COALESCE(d.pm10_rd_poor_address_days, 0) AS pm10_rd_poor_address_days,
		COALESCE(d.pm10_rd_missing_exp_days, 0) AS pm10_rd_missing_exp_days,
		COALESCE(d.pm10_rd_good_address_days, 0) AS pm10_rd_good_address_days,
		COALESCE(d.pm10_gr_invalid_address_days, 0) AS pm10_gr_invalid_address_days,
		COALESCE(d.pm10_gr_oob_days, 0) AS pm10_gr_oob_days,
		COALESCE(d.pm10_gr_poor_address_days, 0) AS pm10_gr_poor_address_days,
		COALESCE(d.pm10_gr_missing_exp_days, 0) AS pm10_gr_missing_exp_days,
		COALESCE(d.pm10_gr_good_address_days, 0) AS pm10_gr_good_address_days,
		COALESCE(d.pm10_tot_invalid_address_days, 0) AS pm10_tot_invalid_address_days,
		COALESCE(d.pm10_tot_oob_days, 0) AS pm10_tot_oob_days,
		COALESCE(d.pm10_tot_poor_address_days, 0) AS pm10_tot_poor_address_days,
		COALESCE(d.pm10_tot_missing_exp_days, 0) AS pm10_tot_missing_exp_days,
		COALESCE(d.pm10_tot_good_address_days, 0) AS pm10_tot_good_address_days,
		d.name_sum,
		d.name_avg,
		d.name_med,
		d.nox_rd_sum,
		d.nox_rd_avg,
		d.nox_rd_med,
		d.pm10_gr_sum,
		d.pm10_gr_avg,
		d.pm10_gr_med,
		d.pm10_rd_sum,
		d.pm10_rd_avg,
		d.pm10_rd_med,
		d.pm10_tot_sum,
		d.pm10_tot_avg,
		d.pm10_tot_med
	FROM
		fin_general_life_stage_data c
	LEFT JOIN
		study_members_with_exposures2 d
	ON
		c.person_id = d.person_id AND
		c.ith_life_stage = d.ith_life_stage
	ORDER BY 
		c.person_id,
		c.ith_life_stage;

	ALTER TABLE fin_stg_mob_exp ADD PRIMARY KEY (person_id, ith_life_stage);

END;
$$   LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION common_assess_approach_differences()
	RETURNS void AS 
$$
DECLARE

BEGIN

	DROP TABLE IF EXISTS fin_mob_cln_vs_unimp;
	CREATE TABLE fin_mob_cln_vs_unimp AS 
	SELECT
		imputed.person_id,
		imputed.ith_life_stage,
		imputed.life_stage,
		calc_percent_error(
			imputed.name_sum, 
			unimputed.name_sum) AS name_sum_percent_err,			
		calc_percent_error(
			imputed.name_avg, 
			unimputed.name_avg) AS name_avg_percent_err,
		calc_percent_error(
			imputed.name_med, 
			unimputed.name_med) AS name_med_percent_err,
		calc_percent_error(
			imputed.nox_rd_sum, 
			unimputed.nox_rd_sum) AS nox_rd_sum_percent_err,
		calc_percent_error(
			imputed.nox_rd_avg, 
			unimputed.nox_rd_avg) AS nox_rd_avg_percent_err,			
		calc_percent_error(
			imputed.nox_rd_med, 
			unimputed.nox_rd_med) AS nox_rd_med_percent_err,
		calc_percent_error(
			imputed.pm10_gr_sum, 
			unimputed.pm10_gr_sum) AS pm10_gr_sum_percent_err,
		calc_percent_error(
			imputed.pm10_gr_avg, 
			unimputed.pm10_gr_avg) AS pm10_gr_avg_percent_err,
		calc_percent_error(
			imputed.pm10_gr_med, 
			unimputed.pm10_gr_med) AS pm10_gr_med_percent_err,
		calc_percent_error(
			imputed.pm10_rd_sum, 
			unimputed.pm10_rd_sum) AS pm10_rd_sum_percent_err,		
		calc_percent_error(
			imputed.pm10_rd_avg, 
			unimputed.pm10_rd_avg) AS pm10_rd_avg_percent_err,
		calc_percent_error(
			imputed.pm10_rd_med, 
			unimputed.pm10_rd_med) AS pm10_rd_med_percent_err,
		calc_percent_error(
			imputed.pm10_tot_sum, 
			unimputed.pm10_tot_sum) AS pm10_tot_sum_percent_err,
		calc_percent_error(
			imputed.pm10_tot_avg, 
			unimputed.pm10_tot_avg) AS pm10_tot_avg_percent_err,
		calc_percent_error(
			imputed.pm10_tot_med, 
			unimputed.pm10_tot_med) AS pm10_tot_med_percent_err
	FROM
		fin_mob_cln_exp imputed,
		fin_mob_uncln_exp unimputed
	WHERE
		imputed.person_id = unimputed.person_id AND
		imputed.ith_life_stage = unimputed.ith_life_stage
	ORDER BY
		imputed.person_id,
		imputed.ith_life_stage;

	-- compare exposure values 
	DROP TABLE IF EXISTS fin_mob_cln_vs_no_mob;
	CREATE TABLE fin_mob_cln_vs_no_mob AS 
	SELECT
		imputed.person_id,
		imputed.ith_life_stage,
		imputed.life_stage,
		calc_percent_error(
			imputed.name_sum, 
			no_mobility.name_sum) AS name_sum_percent_err,			
		calc_percent_error(
			imputed.name_avg, 
			no_mobility.name_avg) AS name_avg_percent_err,
		calc_percent_error(
			imputed.name_med, 
			no_mobility.name_med) AS name_med_percent_err,
		calc_percent_error(
			imputed.nox_rd_sum, 
			no_mobility.nox_rd_sum) AS nox_rd_sum_percent_err,
		calc_percent_error(
			imputed.nox_rd_avg, 
			no_mobility.nox_rd_avg) AS nox_rd_avg_percent_err,			
		calc_percent_error(
			imputed.nox_rd_med, 
			no_mobility.nox_rd_med) AS nox_rd_med_percent_err,
		calc_percent_error(
			imputed.pm10_gr_sum, 
			no_mobility.pm10_gr_sum) AS pm10_gr_sum_percent_err,
		calc_percent_error(
			imputed.pm10_gr_avg, 
			no_mobility.pm10_gr_avg) AS pm10_gr_avg_percent_err,
		calc_percent_error(
			imputed.pm10_gr_med, 
			no_mobility.pm10_gr_med) AS pm10_gr_med_percent_err,
		calc_percent_error(
			imputed.pm10_rd_sum, 
			no_mobility.pm10_rd_sum) AS pm10_rd_sum_percent_err,		
		calc_percent_error(
			imputed.pm10_rd_avg, 
			no_mobility.pm10_rd_avg) AS pm10_rd_avg_percent_err,
		calc_percent_error(
			imputed.pm10_rd_med, 
			no_mobility.pm10_rd_med) AS pm10_rd_med_percent_err,
		calc_percent_error(
			imputed.pm10_tot_sum, 
			no_mobility.pm10_tot_sum) AS pm10_tot_sum_percent_err,
		calc_percent_error(
			imputed.pm10_tot_avg, 
			no_mobility.pm10_tot_avg) AS pm10_tot_avg_percent_err,
		calc_percent_error(
			imputed.pm10_tot_med, 
			no_mobility.pm10_tot_med) AS pm10_tot_med_percent_err
	FROM
		fin_mob_cln_exp imputed,
		fin_stg_mob_exp no_mobility
	WHERE
		imputed.person_id = no_mobility.person_id AND
		imputed.ith_life_stage = no_mobility.ith_life_stage
	ORDER BY
		imputed.person_id,
		imputed.ith_life_stage;

	DROP TABLE IF EXISTS fin_uncln_vs_no_mob;
	CREATE TABLE fin_uncln_vs_no_mob AS 
	SELECT
		unimputed.person_id,
		unimputed.ith_life_stage,
		unimputed.life_stage,
		calc_percent_error(
			unimputed.name_sum, 
			no_mobility.name_sum) AS name_sum_percent_err,			
		calc_percent_error(
			unimputed.name_avg, 
			no_mobility.name_avg) AS name_avg_percent_err,
		calc_percent_error(
			unimputed.name_med, 
			no_mobility.name_med) AS name_med_percent_err,
		calc_percent_error(
			unimputed.nox_rd_sum, 
			no_mobility.nox_rd_sum) AS nox_rd_sum_percent_err,
		calc_percent_error(
			unimputed.nox_rd_avg, 
			no_mobility.nox_rd_avg) AS nox_rd_avg_percent_err,			
		calc_percent_error(
			unimputed.nox_rd_med, 
			no_mobility.nox_rd_med) AS nox_rd_med_percent_err,
		calc_percent_error(
			unimputed.pm10_gr_sum, 
			no_mobility.pm10_gr_sum) AS pm10_gr_sum_percent_err,
		calc_percent_error(
			unimputed.pm10_gr_avg, 
			no_mobility.pm10_gr_avg) AS pm10_gr_avg_percent_err,
		calc_percent_error(
			unimputed.pm10_gr_med, 
			no_mobility.pm10_gr_med) AS pm10_gr_med_percent_err,
		calc_percent_error(
			unimputed.pm10_rd_sum, 
			no_mobility.pm10_rd_sum) AS pm10_rd_sum_percent_err,		
		calc_percent_error(
			unimputed.pm10_rd_avg, 
			no_mobility.pm10_rd_avg) AS pm10_rd_avg_percent_err,
		calc_percent_error(
			unimputed.pm10_rd_med, 
			no_mobility.pm10_rd_med) AS pm10_rd_med_percent_err,
		calc_percent_error(
			unimputed.pm10_tot_sum, 
			no_mobility.pm10_tot_sum) AS pm10_tot_sum_percent_err,
		calc_percent_error(
			unimputed.pm10_tot_avg, 
			no_mobility.pm10_tot_avg) AS pm10_tot_avg_percent_err,
		calc_percent_error(
			unimputed.pm10_tot_med, 
			no_mobility.pm10_tot_med) AS pm10_tot_med_percent_err
	FROM
		fin_mob_uncln_exp unimputed,
		fin_stg_mob_exp no_mobility
	WHERE
		unimputed.person_id = no_mobility.person_id AND
		unimputed.ith_life_stage = no_mobility.ith_life_stage
	ORDER BY
		unimputed.person_id,
		unimputed.ith_life_stage;



END;
$$   LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION common_cleanup_exposures()
	RETURNS void AS 
$$
DECLARE

BEGIN
/*
	DROP TABLE IF EXISTS tmp_over_lap_errors1;
	DROP TABLE IF EXISTS tmp_over_lap_errors2;
	DROP TABLE IF EXISTS tmp_gap_errors1;
	DROP TABLE IF EXISTS tmp_gap_errors2;
	DROP TABLE IF EXISTS tmp_life_stage_days;
	DROP TABLE IF EXISTS tmp_mob_cln_exp_err;
	DROP TABLE IF EXISTS tmp_mob_cln_exp;
	DROP TABLE IF EXISTS tmp_stg_mob_exp1;
	DROP TABLE IF EXISTS tmp_mob_uncln_exp1;
	DROP TABLE IF EXISTS tmp_mob_uncln_exp2;
*/	
END;
$$   LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION common_calc_exposures()
	RETURNS void AS 
$$
DECLARE

BEGIN
	
	
	DROP TABLE IF EXISTS tmp_life_stage_days; 
	CREATE TABLE tmp_life_stage_days AS 
	SELECT
		person_id,
		ith_life_stage,
		life_stage,
		life_stage_duration,
		generate_series(start_date, end_date, '1 day'::interval)::date AS date_of_year
	FROM
		fin_general_life_stage_data
	ORDER BY
		person_id,
		date_of_year;	
	
	ALTER TABLE tmp_life_stage_days ADD PRIMARY KEY (person_id, date_of_year);
	DROP INDEX IF EXISTS ind_tmp_life_stage_days1;
	CREATE INDEX ind_tmp_life_stage_days1 ON tmp_life_stage_days(ith_life_stage);	
	
	-- --------------------------------------------------------------------
	-- Part I: Calculate exposures using imputed, unimputed and no_mobility 
	-- approaches
	-- --------------------------------------------------------------------
	
	-- These three routines are used to calculate exposures that 
	-- consider imputations and exposure measurement errors
	PERFORM common_calc_mob_cln_exp();
	PERFORM common_calc_mob_cln_exp_err();
	PERFORM common_combine_mob_cln_exp_with_err();

	-- Calculate exposures without imputing.  In other words, calculate
	-- exposures by ignoring contributions of days that are involved with
	-- gaps and overlaps
	PERFORM common_calc_mob_uncln_exp();

	-- Calculate exposures with no mobility.  Use the exposure value on
	-- the first day of each life stage to assess the exposure for the
	-- entire life stage
	PERFORM common_calc_stg_mob_exp();
	
	-- --------------------------------------------------------------------
	-- Part II: Compare results of different approaches
	-- --------------------------------------------------------------------
	PERFORM common_assess_approach_differences();
	
	PERFORM common_cleanup_exposures();
	
END;
$$   LANGUAGE plpgsql;


/**
 * ================================================================================================= 
 * MODULE COMM_INIT: Initialise Global Constants
 * =================================================================================================
 * Description
 * -----------
 * This module contains code that manages the global settings that apply when the analyses are
 * running.
 * 
 * NOTE: Apart from the setting the default gestational age at birth,
 * the other global constants appear to be something that gets developed more in the
 * future.  
 * 
 * 
 *
 * Main Function
 * -------------
 *    setup_scripts
 *
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


/*
 * #DESIGN_FOR_REUSE:
 * setup_scripts is where you can make changes to the default gestation age.  For example, if your
 * study wants to use a value other than "38" to impute blank gestation age at birth values, change
 * specify the number of weeks in the first parameter.  The other constants will be developed more 
 * in the future.
*/


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION setup_scripts
 * ----------------------
 * Description
 * -----------
 * Initialises global constants that are used throughout the protocol.
 * NOTE: It seems that most of these 
 * 
 * Parameters
 * ----------
 * (1) f_default_gestation_age: The gestation age which will be used to impute
 *     life stage records which have a missing value for it.  Birth date and
 *     gestation age cannot be left blank because the fields are used to 
 *     calculate the start and stop boundaries of life stages such as 
 *     first trimester, second trimester, third trimester and first year of 
 *     life.  Default value: 38 weeks.
 * (2) f_instrument_error: the absolute measurement error that is associated
 *     with pollution monitors which recorded the concentrations of air 
 *     pollutants.  We realise that different types of pollutants may have 
 *     different detectors.  For our study we have assumed that the value
 *     is a constant +/- error value that can be applied to all types of 
 *     pollution values such as those for PM10, NOx, etc.
 *     Default value: 0.
 * (3) f_exp_model_error: the absolute value of +/- error of exposure
 *     values that are due to the kind of exposure model that is being used
 *     to generate results.  Models make use of historical pollution values
 *     which are usually generated by various types of sensors (see 
 *     instrument error above).  The model itself may generate errors when
 *     it attempts to interpolate the location of residential addresses
 *     amongst multiple known pollution values.  Default value: 0.
 * (4) f_perfect_match_score: the field value that is used to indicate that
 *     a geocode represents the result of a perfect match with some residential
 *     address.  Note that the kinds of values that appear for the flag can
 *     vary depending on the kind of address matching software that is used.
 *
 * Returns
 * -------
 * Nothing.  
 * Example Invocation
 * ------------------
 * SELECT setup_scripts(null, null, null, null); 
 * SELECT "setup_scripts"(38, 0.002, 0.005, 'M')
 * or
 * SELECT "setup_scripts"(NULL, NULL, NULL, NULL)
 * 
 * For each parameter, we can either express an explicit value or 
 * NULL.  If a parameter value is NULL then a default value will be
 * used in the scripts.  Note that you must pass *some* value for each
 * of the expected parameter 
 * Dependencies
 * ------------
 * None.  Users should call this method first
 *
 * General Notes on the Code
 * -------------------------
 * In the function, we try to use similar names to describe similar variable concepts.  In the 
 * header of the function, we use "f_" to denote formal parameters, which define what variables 
 * are called by the code that uses the function setup_scripts(...).  The values that are passed 
 * in each of these parameters are called actual parameters and are held in variables in the 
 * DECLARE section beginning with "a_".  The names of formal and actual parameters are reused 
 * again to define the names of field names in database tables that are used by the script.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION setup_scripts(
	f_default_gestation_age INT,
	f_instrument_error DOUBLE PRECISION,
	f_exp_model_error DOUBLE PRECISION,
	f_perfect_match_score VARCHAR(5))
	RETURNS void AS 
$$
DECLARE
	a_default_gestation_age INT;	
	a_instrument_error DOUBLE PRECISION;
	a_exp_model_error DOUBLE PRECISION;
	a_perfect_match_score VARCHAR(5);
BEGIN

 	-- Part I: Validate Parameters
 	
 	/*
 	 * #CHANGE_DEFAULT_GESTATION_AGE
 	 * If a study member's gestation age at birth field value in the
 	 * study_member_data table, then it will be filled in with 38 weeks
 	 */
	IF f_default_gestation_age IS NULL THEN
		a_default_gestation_age := 38;
	ELSE
		a_default_gestation_age := f_default_gestation_age;
	END IF;
		
	IF f_instrument_error IS NULL THEN
		a_instrument_error := 0;
	ELSE
		a_instrument_error := f_instrument_error;
	END IF;

	IF f_exp_model_error IS NULL THEN
		a_exp_model_error := 0;
	ELSE
		a_exp_model_error := f_exp_model_error;
	END IF;

	IF f_perfect_match_score IS NULL THEN
		a_perfect_match_score := 'M';
	ELSE
		a_perfect_match_score := f_perfect_match_score;
	END IF;


	SET DATESTYLE TO EUROPEAN;

	-- Part II: Create and populate table containing script constants
	DROP TABLE IF EXISTS global_script_constants;
	CREATE TABLE global_script_constants
	(
		default_gestation_age INT NOT NULL,
		minimum_conception_date DATE DEFAULT NULL,
		maximum_el_end_date DATE DEFAULT NULL,
		total_study_members INT,
		total_study_members_with_addresses INT,
		total_valid_geocodes INT,	
		instrument_error DOUBLE PRECISION NOT NULL,
		exposure_model_error DOUBLE PRECISION NOT NULL,
		perfect_match_score VARCHAR(5) NOT NULL,
		empty_geocode_value VARCHAR(15) NOT NULL
	);

	INSERT INTO global_script_constants(
		default_gestation_age,
		instrument_error, 
		exposure_model_error,
		perfect_match_score,
		empty_geocode_value) 
	VALUES (
		a_default_gestation_age, 
		a_instrument_error,
		a_exp_model_error,
		a_perfect_match_score,
		'empty_geocode');

END;
$$   LANGUAGE plpgsql;

--SELECT setup_scripts(null, 3, null, null, null);

/**
 * ================================================================================================= 
 * MODULE COMM_PRELIM_CHECKS: Preliminary System Checks
 * =================================================================================================
 * Description
 * -----------
 * This module contains code which is used to verify the integrity of staging tables before they 
 * are used by the rest of the program. Most of the code tries to add new properties to staging
 * tables to ensure that field values are not null.
 *
 * Assumptions
 * -----------
 * The code here assumes that the method common_preprocess_staging_tables() has already been
 * executed.  When common_perform_prelim_system_checks(), we assume that staging tables
 * have been created from their respective original data tables.  For example, a cohort group
 * may create a table original_study_member_data, but the ALGAE code will create a table called
 * staging_study_member_data, which attempts to address issues of formatting data and streamlining
 * different ways to express variables (eg: changing 'Yes', 'yes', 'true' to a standard 'Y'.
 *
 * Main Function
 * -------------
 *    comm_perform_prelim_system_checks
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
 * MAIN FUNCTION common_perform_prelim_system_checks
 * ------------------------------------------------------
 * Description
 * -----------
 * Ensures that certain fields in the staging tables are guaranteed not to have null values.
 * If any of the ALTER TABLE statements fail, the protocol will abort.  The method exists in order
 * to help minimise sources of bugs from influencing results in later steps.
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION comm_perform_prelim_system_checks()
	RETURNS void AS 
$$
DECLARE

BEGIN	

	--check if basic tables have been created
	perform check_table_exists('staging_study_member_data');	
	perform check_table_exists('staging_addr_history_data');	
	perform check_table_exists('staging_geocode_data');	
	perform check_table_exists('staging_exp_data');	

	--apply database constraints to identify problems in the data

    --Apply constraints to study member data
	ALTER TABLE staging_study_member_data ADD PRIMARY KEY(person_id);	
	ALTER TABLE staging_study_member_data ALTER COLUMN birth_date SET NOT NULL;
	--note that the field 'estimated_gestation_age' could be blank.	
	ALTER TABLE staging_study_member_data ALTER COLUMN absent_during_exp_period SET NOT NULL;
	ALTER TABLE staging_study_member_data ALTER COLUMN at_1st_addr_conception SET NOT NULL;
	
	--Apply constraints to address history data
	ALTER TABLE staging_addr_history_data ALTER COLUMN person_id SET NOT NULL;

    --Apply constraints to geocode and covariate data
	ALTER TABLE staging_geocode_data ALTER COLUMN geocode SET NOT NULL;
	ALTER TABLE staging_geocode_data ALTER COLUMN has_valid_geocode SET NOT NULL;

   	DROP INDEX IF EXISTS ind_staging_geocode_data;
	CREATE INDEX  ind_staging_geocode_data ON staging_geocode_data(geocode);
	
	--Apply constraints to exposure data
	ALTER TABLE staging_exp_data ALTER COLUMN geocode SET NOT NULL;
	ALTER TABLE staging_exp_data ALTER COLUMN date_of_year SET NOT NULL;
   	ALTER TABLE staging_exp_data ADD PRIMARY KEY (geocode, date_of_year);
   

   	
   
   
END;
$$   LANGUAGE plpgsql;	

--SELECT "common_perform_prelim_system_checks"();

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
 *    comm_preprocess_staging_tables
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
 * MAIN FUNCTION comm_preprocess_staging_tables
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

	-- #CHANGE_LIFE_STAGES
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

CREATE OR REPLACE FUNCTION common_calc_no_mob_birth_addr_exp()
	RETURNS void AS 
$$
DECLARE

BEGIN

	-- Determine locations at the start of each life stage
	DROP TABLE IF EXISTS tmp_no_mob_birth_addr_exp1;
	CREATE TABLE tmp_no_mob_birth_addr_exp1 AS	
	WITH cleaned_addr AS 
		(SELECT
			a.person_id,
			a.geocode,			
			a.ith_residence,
			has_valid_geocode,
			has_name_exposures,
			has_nox_rd_exposures,
			has_pm10_gr_exposures,
			has_pm10_rd_exposures,
			has_pm10_tot_exposures,			
			a.fin_adjusted_start_date,
			a.fin_adjusted_end_date
		FROM
			fin_cleaned_addr_periods a
		WHERE			
			fit_type != 'D' AND
			is_fixed_invalid_geocode = 'N'),
	birth_addr AS
		(SELECT
			d.person_id,
			c.geocode,
			c.has_valid_geocode,
			c.has_name_exposures,
			c.has_nox_rd_exposures,
			c.has_pm10_gr_exposures,
			c.has_pm10_rd_exposures,
			c.has_pm10_tot_exposures
		 FROM
			cleaned_addr c,
			staging_study_member_data d
		 WHERE
			c.person_id = d.person_id AND
			d.birth_date BETWEEN c.fin_adjusted_start_date AND c.fin_adjusted_end_date)
	SELECT
		e.person_id,
		f.ith_life_stage,
		f.life_stage,
		e.geocode,
		e.has_valid_geocode,
		e.has_name_exposures,
		e.has_nox_rd_exposures,
		e.has_pm10_gr_exposures,
		e.has_pm10_rd_exposures,
		e.has_pm10_tot_exposures,
		generate_series(
			f.start_date, 
			f.end_date, 
			'1 day'::interval)::date AS date_of_year
	FROM
		birth_addr e,
		fin_general_life_stage_data f
	WHERE
		e.person_id = f.person_id
	ORDER BY
		e.person_id;


	ALTER TABLE tmp_no_mob_birth_addr_exp1 ADD PRIMARY KEY (person_id, date_of_year);
	
	DROP TABLE IF EXISTS fin_no_mob_birth_addr_exp;
	CREATE TABLE fin_no_mob_birth_addr_exp AS	
	WITH study_members_with_exposure_data1 AS	
		(SELECT	
			a.person_id,
			a.ith_life_stage,
			a.life_stage,
			CASE
				WHEN a.has_name_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS name_invalid_address_count,
			CASE
				WHEN a.has_name_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS name_oob_count,
			CASE
				WHEN a.has_name_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS name_poor_address_count,
			CASE
				WHEN a.has_name_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.name IS NULL THEN
					1
				ELSE 
					0
			END AS name_missing_exp_count,
			CASE
				WHEN a.has_name_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.name IS NOT NULL THEN
					1
				ELSE 
					0
			END AS name_good_address_count,
			b.name,
			CASE
				WHEN a.has_nox_rd_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS nox_rd_invalid_address_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS nox_rd_oob_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS nox_rd_poor_address_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.nox_rd IS NULL THEN
					1
				ELSE 
					0
			END AS nox_rd_missing_exp_count,
			CASE
				WHEN a.has_nox_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.nox_rd IS NOT NULL THEN
					1
				ELSE 
					0
			END AS nox_rd_good_address_count,
			b.nox_rd,
			CASE
				WHEN a.has_pm10_rd_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_rd_invalid_address_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS pm10_rd_oob_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_rd_poor_address_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_rd IS NULL THEN
					1
				ELSE 
					0
			END AS pm10_rd_missing_exp_count,
			CASE
				WHEN a.has_pm10_rd_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_rd IS NOT NULL THEN
					1
				ELSE 
					0
			END AS pm10_rd_good_address_count,
			b.pm10_rd,
			CASE
				WHEN a.has_pm10_gr_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_gr_invalid_address_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS pm10_gr_oob_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_gr_poor_address_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_gr IS NULL THEN
					1
				ELSE 
					0
			END AS pm10_gr_missing_exp_count,
			CASE
				WHEN a.has_pm10_gr_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_gr IS NOT NULL THEN
					1
				ELSE 
					0
			END AS pm10_gr_good_address_count,
			b.pm10_gr,
			CASE
				WHEN a.has_pm10_tot_exposures = 'N' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_tot_invalid_address_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'N' AND a.has_valid_geocode = 'Y' THEN
					1
				ELSE 
					0
			END AS pm10_tot_oob_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'Y' AND a.has_valid_geocode = 'N' THEN
					1
				ELSE 
					0
			END AS pm10_tot_poor_address_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_tot IS NULL THEN
					1
				ELSE 
					0
			END AS pm10_tot_missing_exp_count,
			CASE
				WHEN a.has_pm10_tot_exposures = 'Y' AND 
					 a.has_valid_geocode = 'Y' AND
					 b.pm10_tot IS NOT NULL THEN
					1
				ELSE 
					0
			END AS pm10_tot_good_address_count,
			b.pm10_tot			
		FROM
			tmp_no_mob_birth_addr_exp1 a
		LEFT JOIN
			fin_daily_exposures b
		ON
			a.geocode = b.geocode AND
			a.date_of_year = b.date_of_year),		
	study_members_with_exposure_data2 AS
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			SUM(name_invalid_address_count) AS name_invalid_address_days,
			SUM(name_oob_count) AS name_oob_days,
			SUM(name_poor_address_count) AS name_poor_address_days,
			SUM(name_missing_exp_count) AS name_missing_exp_days,
			SUM(name_good_address_count) AS name_good_address_days,		
			SUM(nox_rd_invalid_address_count) AS nox_rd_invalid_address_days,
			SUM(nox_rd_oob_count) AS nox_rd_oob_days,
			SUM(nox_rd_poor_address_count) AS nox_rd_poor_address_days,
			SUM(nox_rd_missing_exp_count) AS nox_rd_missing_exp_days,
			SUM(nox_rd_good_address_count) AS nox_rd_good_address_days,	
			SUM(pm10_rd_invalid_address_count) AS pm10_rd_invalid_address_days,
			SUM(pm10_rd_oob_count) AS pm10_rd_oob_days,
			SUM(pm10_rd_poor_address_count) AS pm10_rd_poor_address_days,
			SUM(pm10_rd_missing_exp_count) AS pm10_rd_missing_exp_days,
			SUM(pm10_rd_good_address_count) AS pm10_rd_good_address_days,		
			SUM(pm10_gr_invalid_address_count) AS pm10_gr_invalid_address_days,
			SUM(pm10_gr_oob_count) AS pm10_gr_oob_days,
			SUM(pm10_gr_poor_address_count) AS pm10_gr_poor_address_days,
			SUM(pm10_gr_missing_exp_count) AS pm10_gr_missing_exp_days,
			SUM(pm10_gr_good_address_count) AS pm10_gr_good_address_days,	
			SUM(pm10_tot_invalid_address_count) AS pm10_tot_invalid_address_days,
			SUM(pm10_tot_oob_count) AS pm10_tot_oob_days,
			SUM(pm10_tot_poor_address_count) AS pm10_tot_poor_address_days,
			SUM(pm10_tot_missing_exp_count) AS pm10_tot_missing_exp_days,
			SUM(pm10_tot_good_address_count) AS pm10_tot_good_address_days,
			SUM(name) AS name_sum,
			AVG(name) AS name_avg,
			median(name) AS name_med,
			SUM(nox_rd) AS nox_rd_sum,
			AVG(nox_rd) AS nox_rd_avg,
			median(nox_rd) AS nox_rd_med,
			SUM(pm10_gr) AS pm10_gr_sum,
			AVG(pm10_gr) AS pm10_gr_avg,
			median(pm10_gr) AS pm10_gr_med,
			SUM(pm10_rd) AS pm10_rd_sum,
			AVG(pm10_rd) AS pm10_rd_avg,
			median(pm10_rd) AS pm10_rd_med,
			SUM(pm10_tot) AS pm10_tot_sum,
			AVG(pm10_tot) AS pm10_tot_avg,
			median(pm10_tot) AS pm10_tot_med
		FROM
			study_members_with_exposure_data1
		GROUP BY
			person_id,
			ith_life_stage,
			life_stage)
	SELECT
		c.person_id,
		c.ith_life_stage,
		c.life_stage,
		c.life_stage_duration,
		COALESCE(d.name_invalid_address_days, 0) AS name_invalid_address_days,
		COALESCE(d.name_oob_days, 0) AS name_oob_days,
		COALESCE(d.name_poor_address_days, 0) AS name_poor_address_days,
		COALESCE(d.name_missing_exp_days, 0) AS name_missing_exp_days,
		COALESCE(d.name_good_address_days, 0) AS name_good_address_days,		
		COALESCE(d.nox_rd_invalid_address_days, 0) AS nox_rd_invalid_address_days,
		COALESCE(d.nox_rd_oob_days, 0) AS nox_rd_oob_days,
		COALESCE(d.nox_rd_poor_address_days, 0) AS nox_rd_poor_address_days,
		COALESCE(d.nox_rd_missing_exp_days, 0) AS nox_rd_missing_exp_days,
		COALESCE(d.nox_rd_good_address_days, 0) AS nox_rd_good_address_days,
		COALESCE(d.pm10_rd_invalid_address_days, 0) AS pm10_rd_invalid_address_days,
		COALESCE(d.pm10_rd_oob_days, 0) AS pm10_rd_oob_days,
		COALESCE(d.pm10_rd_poor_address_days, 0) AS pm10_rd_poor_address_days,
		COALESCE(d.pm10_rd_missing_exp_days, 0) AS pm10_rd_missing_exp_days,
		COALESCE(d.pm10_rd_good_address_days, 0) AS pm10_rd_good_address_days,
		COALESCE(d.pm10_gr_invalid_address_days, 0) AS pm10_gr_invalid_address_days,
		COALESCE(d.pm10_gr_oob_days, 0) AS pm10_gr_oob_days,
		COALESCE(d.pm10_gr_poor_address_days, 0) AS pm10_gr_poor_address_days,
		COALESCE(d.pm10_gr_missing_exp_days, 0) AS pm10_gr_missing_exp_days,
		COALESCE(d.pm10_gr_good_address_days, 0) AS pm10_gr_good_address_days,
		COALESCE(d.pm10_tot_invalid_address_days, 0) AS pm10_tot_invalid_address_days,
		COALESCE(d.pm10_tot_oob_days, 0) AS pm10_tot_oob_days,
		COALESCE(d.pm10_tot_poor_address_days, 0) AS pm10_tot_poor_address_days,
		COALESCE(d.pm10_tot_missing_exp_days, 0) AS pm10_tot_missing_exp_days,
		COALESCE(d.pm10_tot_good_address_days, 0) AS pm10_tot_good_address_days,
		d.name_sum,
		d.name_avg,
		d.name_med,
		d.nox_rd_sum,
		d.nox_rd_avg,
		d.nox_rd_med,
		d.pm10_gr_sum,
		d.pm10_gr_avg,
		d.pm10_gr_med,
		d.pm10_rd_sum,
		d.pm10_rd_avg,
		d.pm10_rd_med,
		d.pm10_tot_sum,
		d.pm10_tot_avg,
		d.pm10_tot_med
	FROM
		fin_general_life_stage_data c
	LEFT JOIN
		study_members_with_exposure_data2 d
	ON
		c.person_id = d.person_id AND
		c.ith_life_stage = d.ith_life_stage
	ORDER BY 
		c.person_id,
		c.ith_life_stage;		

END;
$$   LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION common_early_assess_approach_differences()
	RETURNS void AS 
$$
DECLARE

BEGIN

	DROP TABLE IF EXISTS fin_mob_cln_vs_stg_mob_birth;
	CREATE TABLE fin_mob_cln_vs_stg_mob_birth AS 
	SELECT
		imputed.person_id,
		imputed.ith_life_stage,
		imputed.life_stage,
		calc_percent_error(
			imputed.name_sum, 
			no_mob_birth_addr.name_sum) AS name_sum_percent_err,			
		calc_percent_error(
			imputed.name_avg, 
			no_mob_birth_addr.name_avg) AS name_avg_percent_err,
		calc_percent_error(
			imputed.name_med, 
			no_mob_birth_addr.name_med) AS name_med_percent_err,
		calc_percent_error(
			imputed.nox_rd_sum, 
			no_mob_birth_addr.nox_rd_sum) AS nox_rd_sum_percent_err,
		calc_percent_error(
			imputed.nox_rd_avg, 
			no_mob_birth_addr.nox_rd_avg) AS nox_rd_avg_percent_err,			
		calc_percent_error(
			imputed.nox_rd_med, 
			no_mob_birth_addr.nox_rd_med) AS nox_rd_med_percent_err,
		calc_percent_error(
			imputed.pm10_gr_sum, 
			no_mob_birth_addr.pm10_gr_sum) AS pm10_gr_sum_percent_err,
		calc_percent_error(
			imputed.pm10_gr_avg, 
			no_mob_birth_addr.pm10_gr_avg) AS pm10_gr_avg_percent_err,
		calc_percent_error(
			imputed.pm10_gr_med, 
			no_mob_birth_addr.pm10_gr_med) AS pm10_gr_med_percent_err,
		calc_percent_error(
			imputed.pm10_rd_sum, 
			no_mob_birth_addr.pm10_rd_sum) AS pm10_rd_sum_percent_err,		
		calc_percent_error(
			imputed.pm10_rd_avg, 
			no_mob_birth_addr.pm10_rd_avg) AS pm10_rd_avg_percent_err,
		calc_percent_error(
			imputed.pm10_rd_med, 
			no_mob_birth_addr.pm10_rd_med) AS pm10_rd_med_percent_err,
		calc_percent_error(
			imputed.pm10_tot_sum, 
			no_mob_birth_addr.pm10_tot_sum) AS pm10_tot_sum_percent_err,
		calc_percent_error(
			imputed.pm10_tot_avg, 
			no_mob_birth_addr.pm10_tot_avg) AS pm10_tot_avg_percent_err,
		calc_percent_error(
			imputed.pm10_tot_med, 
			no_mob_birth_addr.pm10_tot_med) AS pm10_tot_med_percent_err
	FROM
		fin_mob_cln_exp imputed,
		fin_no_mob_birth_addr_exp no_mob_birth_addr
	WHERE
		imputed.person_id = no_mob_birth_addr.person_id AND
		imputed.ith_life_stage = no_mob_birth_addr.ith_life_stage
	ORDER BY
		imputed.person_id,
		imputed.ith_life_stage;

	DROP TABLE IF EXISTS fin_mob_uncln_vs_stg_mob_birth;
	CREATE TABLE fin_mob_uncln_vs_stg_mob_birth AS 
	SELECT
		unimputed.person_id,
		unimputed.ith_life_stage,
		unimputed.life_stage,
		calc_percent_error(
			unimputed.name_sum, 
			no_mob_birth_addr.name_sum) AS name_sum_percent_err,			
		calc_percent_error(
			unimputed.name_avg, 
			no_mob_birth_addr.name_avg) AS name_avg_percent_err,
		calc_percent_error(
			unimputed.name_med, 
			no_mob_birth_addr.name_med) AS name_med_percent_err,
		calc_percent_error(
			unimputed.nox_rd_sum, 
			no_mob_birth_addr.nox_rd_sum) AS nox_rd_sum_percent_err,
		calc_percent_error(
			unimputed.nox_rd_avg, 
			no_mob_birth_addr.nox_rd_avg) AS nox_rd_avg_percent_err,			
		calc_percent_error(
			unimputed.nox_rd_med, 
			no_mob_birth_addr.nox_rd_med) AS nox_rd_med_percent_err,
		calc_percent_error(
			unimputed.pm10_gr_sum, 
			no_mob_birth_addr.pm10_gr_sum) AS pm10_gr_sum_percent_err,
		calc_percent_error(
			unimputed.pm10_gr_avg, 
			no_mob_birth_addr.pm10_gr_avg) AS pm10_gr_avg_percent_err,
		calc_percent_error(
			unimputed.pm10_gr_med, 
			no_mob_birth_addr.pm10_gr_med) AS pm10_gr_med_percent_err,
		calc_percent_error(
			unimputed.pm10_rd_sum, 
			no_mob_birth_addr.pm10_rd_sum) AS pm10_rd_sum_percent_err,		
		calc_percent_error(
			unimputed.pm10_rd_avg, 
			no_mob_birth_addr.pm10_rd_avg) AS pm10_rd_avg_percent_err,
		calc_percent_error(
			unimputed.pm10_rd_med, 
			no_mob_birth_addr.pm10_rd_med) AS pm10_rd_med_percent_err,
		calc_percent_error(
			unimputed.pm10_tot_sum, 
			no_mob_birth_addr.pm10_tot_sum) AS pm10_tot_sum_percent_err,
		calc_percent_error(
			unimputed.pm10_tot_avg, 
			no_mob_birth_addr.pm10_tot_avg) AS pm10_tot_avg_percent_err,
		calc_percent_error(
			unimputed.pm10_tot_med, 
			no_mob_birth_addr.pm10_tot_med) AS pm10_tot_med_percent_err
	FROM
		fin_mob_uncln_exp unimputed,
		fin_no_mob_birth_addr_exp no_mob_birth_addr
	WHERE
		unimputed.person_id = no_mob_birth_addr.person_id AND
		unimputed.ith_life_stage = no_mob_birth_addr.ith_life_stage
	ORDER BY
		unimputed.person_id,
		unimputed.ith_life_stage;

	DROP TABLE IF EXISTS fin_stg_mob_vs_no_mob_birth;
	CREATE TABLE fin_stg_mob_vs_no_mob_birth AS 
	SELECT
		no_mob.person_id,
		no_mob.ith_life_stage,
		no_mob.life_stage,
		calc_percent_error(
			no_mob.name_sum, 
			no_mob_birth_addr.name_sum) AS name_sum_percent_err,			
		calc_percent_error(
			no_mob.name_avg, 
			no_mob_birth_addr.name_avg) AS name_avg_percent_err,
		calc_percent_error(
			no_mob.name_med, 
			no_mob_birth_addr.name_med) AS name_med_percent_err,
		calc_percent_error(
			no_mob.nox_rd_sum, 
			no_mob_birth_addr.nox_rd_sum) AS nox_rd_sum_percent_err,
		calc_percent_error(
			no_mob.nox_rd_avg, 
			no_mob_birth_addr.nox_rd_avg) AS nox_rd_avg_percent_err,			
		calc_percent_error(
			no_mob.nox_rd_med, 
			no_mob_birth_addr.nox_rd_med) AS nox_rd_med_percent_err,
		calc_percent_error(
			no_mob.pm10_gr_sum, 
			no_mob_birth_addr.pm10_gr_sum) AS pm10_gr_sum_percent_err,
		calc_percent_error(
			no_mob.pm10_gr_avg, 
			no_mob_birth_addr.pm10_gr_avg) AS pm10_gr_avg_percent_err,
		calc_percent_error(
			no_mob.pm10_gr_med, 
			no_mob_birth_addr.pm10_gr_med) AS pm10_gr_med_percent_err,
		calc_percent_error(
			no_mob.pm10_rd_sum, 
			no_mob_birth_addr.pm10_rd_sum) AS pm10_rd_sum_percent_err,		
		calc_percent_error(
			no_mob.pm10_rd_avg, 
			no_mob_birth_addr.pm10_rd_avg) AS pm10_rd_avg_percent_err,
		calc_percent_error(
			no_mob.pm10_rd_med, 
			no_mob_birth_addr.pm10_rd_med) AS pm10_rd_med_percent_err,
		calc_percent_error(
			no_mob.pm10_tot_sum, 
			no_mob_birth_addr.pm10_tot_sum) AS pm10_tot_sum_percent_err,
		calc_percent_error(
			no_mob.pm10_tot_avg, 
			no_mob_birth_addr.pm10_tot_avg) AS pm10_tot_avg_percent_err,
		calc_percent_error(
			no_mob.pm10_tot_med, 
			no_mob_birth_addr.pm10_tot_med) AS pm10_tot_med_percent_err
	FROM
		fin_stg_mob_exp no_mob,
		fin_no_mob_birth_addr_exp no_mob_birth_addr
	WHERE
		no_mob.person_id = no_mob_birth_addr.person_id AND
		no_mob.ith_life_stage = no_mob_birth_addr.ith_life_stage
	ORDER BY
		no_mob.person_id,
		no_mob.ith_life_stage;
		
END;
$$   LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION early_calc_exposures()
	RETURNS void AS 
$$
DECLARE

BEGIN

	PERFORM common_calc_no_mob_birth_addr_exp();
	PERFORM common_early_assess_approach_differences();
		
END;
$$   LANGUAGE plpgsql;

/**
 * ================================================================================================= 
 * MODULE COMM_EARLY_REP: Backup and Generate Early Life Reports
 * =================================================================================================
 * Description
 * -----------
 * This module contains routines that are meant to help finish the analyses.  It covers two 
 * important tasks:
 * (1) backs up staging tables
 * (2) generates result tables and writes them to CSV files.
 *
 * Some of the staging tables can be expensive to create.  For example, in the early life analysis,
 * the staging_exp_data can contain millions of rows and take a long time to load.  If the late 
 * analysis is run after the early analysis, it will drop and reconsititute the staging tables, but
 * will often include different data.
 *
 * The reports provide a way to filter what tables should be written to CSV files.  It is not really
 * meant to act as a security measure, but it does represent the place where cohort projects can decide
 * whether they are comfortable with extracting results.
 *
 * This is also where variables are renamed for how they will be referenced in publications.  Here,
 * we prefix most of them with "algae_"
 *
 * Main Functions
 * --------------
 * (1) early_life_create_result_tables_and_backup
 * (2) early_life_create_reports 
 *
 * Assumptions
 * ----------- 
 * These two methods should be called at the very end of the analysis and the backup routine should
 * be called before reports are generated.
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
 * FUNCTION early_life_create_result_tables_and_backup
 * -----------------------------------------------------
 * Description
 * -----------
 * Backs up the staging tables and creates result tables.  Early and late life analyses often share
 * commonly named tables.  In this method, a copies of tables are made to capture the output of
 * one analysis or the other.  This is also the routine in the early life analysis where variables
 * are renamed for the official variable names that will be cited by other studies that use ALGAE
 * variables.  
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION early_life_create_result_tables_and_backup()
	RETURNS void AS 
$$
DECLARE
	comments TEXT;
BEGIN


	DROP TABLE IF EXISTS bak_early_staging_exp_data;
	CREATE TABLE bak_early_staging_exp_data AS
	SELECT
		*
	FROM
		staging_exp_data;
	
	
	DROP TABLE IF EXISTS bak_early_staging_addr_history_data;
	CREATE TABLE bak_early_staging_addr_history_data AS
	SELECT
		*
	FROM
		staging_addr_history_data;
	
	DROP TABLE IF EXISTS bak_early_staging_geocode_data;
	CREATE TABLE bak_early_staging_geocode_data AS
	SELECT
		*
	FROM
		staging_geocode_data;
	

	DROP TABLE IF EXISTS bak_early_staging_study_member_data;
	CREATE TABLE bak_early_staging_study_member_data AS
	SELECT
		*
	FROM
		staging_study_member_data;

	
	
	-- -----------------------------------------------
	-- Generate life stage data file
	-- -----------------------------------------------	
	DROP TABLE IF EXISTS results_early_life_stages;
	CREATE TABLE results_early_life_stages AS
	SELECT
		person_id AS algae1100_person_id,
		ith_life_stage AS algae1101_ith_life_stage,
		life_stage AS algae1102_life_stage,
		life_stage_year AS algae1103_life_stage_year,
		start_date AS algae1104_start_date,
		end_date AS algae1105_end_date,
		life_stage_duration AS algae1106_life_stage_duration
	FROM
		fin_general_life_stage_data
	ORDER BY
		person_id,
		ith_life_stage;
	

	-- ------------------------------------
	-- Generate cleaned address period file
	-- ------------------------------------	
	
	DROP TABLE IF EXISTS results_early_cleaned_addr_periods;
	CREATE TABLE results_early_cleaned_addr_periods AS
	SELECT
		original_row_number AS algae2100_original_row_number,
		person_id AS algae2101_person_id,
		ith_residence AS algae2102_ith_residence,
		geocode AS algae2103_geocode,
		date_state AS algae2104_date_state,
		start_date AS algae2105_start_date,
		end_date AS algae2106_end_date,
		duration AS algae2107_duration,
		ith_residence_type AS algae2108_ith_residence_type,
		has_valid_geocode AS algae2109_has_valid_geocode,
		has_name_exposures AS algae2110_has_name_exposures,		
		has_nox_rd_exposures AS algae2111_has_nox_rd_exposures,
		has_pm10_gr_exposures AS algae2112_has_pm10_gr_exposures,
		has_pm10_rd_exposures AS algae2113_has_pm10_rd_exposures,
		has_pm10_tot_exposures AS algae2114_has_pm10_tot_exposures,		
		maximum_life_stage_overlap AS algae2115_max_life_stage_overlap,
		is_fixed_invalid_geocode AS algae2116_is_fixed_inv_geocode,
		fit_extent AS algae2117_fit_extent,
		adjusted_start_date AS algae2118_adj_start_date,
		adjusted_end_date AS algae2119_adj_end_date,
		days_changed AS algae2120_days_changed,
		fit_type AS algae2121_fit_type,
		start_date_delta1 AS algae2122_start_date_delta1,
		start_date_delta2 AS algae2123_start_date_delta2,
		end_date_delta1 AS algae2124_end_date_delta1,
		end_date_delta2 AS algae2125_end_date_delta2,
		previous_geocode AS algae2126_previous_geocode,
		next_geocode AS algae2127_next_geocode,
		fin_adjusted_start_date AS algae2128_fin_adj_start_date,
		imputed_first_start AS algae2129_imputed_first_start,
		fin_adjusted_end_date AS algae2130_fin_adj_end_date,
		imputed_last_end AS algae2131_imputed_last_end,
		start_date_days_from_conception AS algae2132_start_date_days_from_concep,
		is_within_exposure_time_frame AS algae2133_is_within_exp
	FROM
		fin_cleaned_addr_periods
	ORDER BY
		person_id,
		ith_residence;
	

	-- ------------------------------------
	-- Generate exposure data files
	-- ------------------------------------	

	DROP TABLE IF EXISTS results_early_mob_cln_exp;
	CREATE TABLE results_early_mob_cln_exp AS
	SELECT
		person_id AS algae3100_person_id,
		life_stage AS algae3101_life_stage,		
		life_stage_duration AS algae3102_life_stage_duration,
		name_invalid_address_days AS algae3103_name_inv_addr_days,
		name_oob_days AS algae3104_name_oob_days,
		name_poor_address_days AS algae3105_name_poor_addr_days,
		name_missing_exp_days AS algae3106_name_missing_exp_days,
		name_good_address_days AS algae3107_name_good_addr_days,

		nox_rd_invalid_address_days AS algae3108_nox_rd_inv_addr_days,
		nox_rd_oob_days AS algae3109_nox_rd_oob_days,
		nox_rd_poor_address_days AS algae3110_nox_rd_poor_addr_days,
		nox_rd_missing_exp_days AS algae3111_nox_rd_missing_exp_days,
		nox_rd_good_address_days AS algae3112_nox_rd_good_addr_days,
		
		pm10_rd_invalid_address_days AS algae3113_pm10_rd_inv_addr_days,
		pm10_rd_oob_days AS algae3114_pm10_rd_oob_days,
		pm10_rd_poor_address_days AS algae3115_pm10_rd_poor_addr_days,
		pm10_rd_missing_exp_days AS algae3116_pm10_rd_missing_exp_days,
		pm10_rd_good_address_days AS algae3117_pm10_rd_good_addr_days,

		pm10_gr_invalid_address_days AS algae3118_pm10_gr_inv_addr_days,
		pm10_gr_oob_days AS algae3119_pm10_gr_oob_days,
		pm10_gr_poor_address_days AS algae3120_pm10_gr_poor_addr_days,
		pm10_gr_missing_exp_days AS algae3121_pm10_gr_missing_exp_days,
		pm10_gr_good_address_days AS algae3122_pm10_gr_good_addr_days,

		pm10_tot_invalid_address_days AS algae3123_pm10_tot_inv_addr_days,
		pm10_tot_oob_days AS algae3124_pm10_tot_oob_days,
		pm10_tot_poor_address_days AS algae3125_pm10_tot_poor_addr_days,
		pm10_tot_missing_exp_days AS algae3126_pm10_tot_missing_exp_days,
		pm10_tot_good_address_days AS algae3127_pm10_tot_good_addr_days,

		name_sum AS algae3128_name_sum,
		name_err_sum AS algae3129_name_err_sum,
		name_avg AS algae3130_name_avg,
		name_err_avg AS algae3131_name_err_avg,
		name_med AS algae3132_name_med,
		name_err_med AS algae3133_name_err_med,
		nox_rd_sum AS algae3134_nox_rd_sum,
		nox_rd_err_sum AS algae3135_nox_rd_err_sum,
		nox_rd_avg AS algae3136_nox_rd_avg,
		nox_rd_err_avg AS algae3137_nox_rd_err_avg,
		nox_rd_med AS algae3138_nox_rd_med,
		nox_rd_err_med AS algae3139_nox_rd_err_med,
		pm10_gr_sum AS algae3140_pm10_gr_sum,
		pm10_gr_err_sum AS algae3141_pm10_gr_err_sum,
		pm10_gr_avg AS algae3142_pm10_gr_avg,
		pm10_gr_err_avg AS algae3143_pm10_gr_err_avg,
		pm10_gr_med AS algae3144_pm10_gr_med,
		pm10_gr_err_med AS algae3145_pm10_gr_err_med,
		pm10_rd_sum AS algae3146_pm10_rd_sum,
		pm10_rd_err_sum AS algae3147_pm10_rd_err_sum,		
		pm10_rd_avg AS algae3148_pm10_rd_avg,
		pm10_rd_err_avg AS algae3149_pm10_rd_err_avg,
		pm10_rd_med AS algae3150_pm10_rd_med,
		pm10_rd_err_med AS algae3151_pm10_rd_err_med,
		pm10_tot_sum AS algae3152_pm10_tot_sum,
		pm10_tot_err_sum AS algae3153_pm10_tot_err_sum,		
		pm10_tot_avg AS algae3154_pm10_tot_avg,
		pm10_tot_err_avg AS algae3155_pm10_tot_err_avg,
		pm10_tot_med AS algae3156_pm10_tot_med,
		pm10_tot_err_med AS algae3157_pm10_tot_err_med
	FROM
		fin_mob_cln_exp
	ORDER BY
		person_id,
		ith_life_stage;
			
	DROP TABLE IF EXISTS results_early_mob_uncln_exp;
	CREATE TABLE results_early_mob_uncln_exp AS
	SELECT
		person_id AS algae3200_person_id,
		life_stage AS algae3201_life_stage,
		life_stage_duration AS algae3202_life_stage_duration,

		name_invalid_address_days AS algae3203_name_inv_addr_days,
		name_oob_days AS algae3204_name_oob_days,
		name_poor_address_days AS algae3205_name_poor_addr_days,
		name_missing_exp_days AS algae3206_name_missing_exp_days,
		name_good_address_days AS algae3207_name_good_addr_days,

		nox_rd_invalid_address_days AS algae3208_nox_rd_inv_addr_days,
		nox_rd_oob_days AS algae3209_nox_rd_oob_days,
		nox_rd_poor_address_days AS algae3210_nox_rd_poor_addr_days,
		nox_rd_missing_exp_days AS algae3211_nox_rd_missing_exp_days,
		nox_rd_good_address_days AS algae3212_nox_rd_good_addr_days,
		
		pm10_rd_invalid_address_days AS algae3213_pm10_rd_inv_addr_days,
		pm10_rd_oob_days AS algae3214_pm10_rd_oob_days,
		pm10_rd_poor_address_days AS algae3215_pm10_rd_poor_addr_days,
		pm10_rd_missing_exp_days AS algae3216_pm10_rd_missing_exp_days,
		pm10_rd_good_address_days AS algae3217_pm10_rd_good_addr_days,

		pm10_gr_invalid_address_days AS algae3218_pm10_gr_inv_addr_days,
		pm10_gr_oob_days AS algae3219_pm10_gr_oob_days,
		pm10_gr_poor_address_days AS algae3220_pm10_gr_poor_addr_days,
		pm10_gr_missing_exp_days AS algae3221_pm10_gr_missing_exp_days,
		pm10_gr_good_address_days AS algae3222_pm10_gr_good_addr_days,

		pm10_tot_invalid_address_days AS algae3223_pm10_tot_inv_addr_days,
		pm10_tot_oob_days AS algae3224_pm10_tot_oob_days,
		pm10_tot_poor_address_days AS algae3225_pm10_tot_poor_addr_days,
		pm10_tot_missing_exp_days AS algae3226_pm10_tot_missing_exp_days,
		pm10_tot_good_address_days AS algae3227_pm10_tot_good_addr_days,			

		name_sum AS algae3228_name_sum,
		name_avg AS algae3229_name_avg,
		name_med AS algae3230_name_med,
		nox_rd_sum AS algae3231_nox_rd_sum,
		nox_rd_avg AS algae3232_nox_rd_avg,
		nox_rd_med AS algae3233_nox_rd_med,
		pm10_gr_sum AS algae3234_pm10_gr_sum,
		pm10_gr_avg AS algae3235_pm10_gr_avg,
		pm10_gr_med AS algae3236_pm10_gr_med,
		pm10_rd_sum AS algae3237_pm10_rd_sum,
		pm10_rd_avg AS algae3238_pm10_rd_avg,
		pm10_rd_med AS algae3239_pm10_rd_med,
		pm10_tot_sum AS algae3240_pm10_tot_sum,
		pm10_tot_avg AS algae3241_pm10_tot_avg,
		pm10_tot_med AS algae3242_pm10_tot_med
	FROM
		fin_mob_uncln_exp
	ORDER BY
		person_id,
		ith_life_stage;	
	
	DROP TABLE IF EXISTS results_early_stg_mob_exp;
	CREATE TABLE results_early_stg_mob_exp AS
	SELECT
		person_id AS algae3300_person_id,
		life_stage AS algae3301_life_stage,
		life_stage_duration AS algae3302_life_stage_duration,

		name_invalid_address_days AS algae3303_name_inv_addr_days,
		name_oob_days AS algae3304_name_oob_days,
		name_poor_address_days AS algae3305_name_poor_addr_days,
		name_missing_exp_days AS algae3306_name_missing_exp_days,
		name_good_address_days AS algae3307_name_good_addr_days,

		nox_rd_invalid_address_days AS algae3308_nox_rd_inv_addr_days,
		nox_rd_oob_days AS algae3309_nox_rd_oob_days,
		nox_rd_poor_address_days AS algae3310_nox_rd_poor_addr_days,
		nox_rd_missing_exp_days AS algae3311_nox_rd_missing_exp_days,
		nox_rd_good_address_days AS algae3312_nox_rd_good_addr_days,

		pm10_rd_invalid_address_days AS algae3313_pm10_rd_inv_addr_days,
		pm10_rd_oob_days AS algae3314_pm10_rd_oob_days,
		pm10_rd_poor_address_days AS algae3315_pm10_rd_poor_addr_days,
		pm10_rd_missing_exp_days AS algae3316_pm10_rd_missing_exp_days,
		pm10_rd_good_address_days AS algae3317_pm10_rd_good_addr_days,

		pm10_gr_invalid_address_days AS algae3318_pm10_gr_inv_addr_days,
		pm10_gr_oob_days AS algae3319_pm10_gr_oob_days,
		pm10_gr_poor_address_days AS algae3320_pm10_gr_poor_addr_days,
		pm10_gr_missing_exp_days AS algae3321_pm10_gr_missing_exp_days,
		pm10_gr_good_address_days AS algae3322_pm10_gr_good_addr_days,

		pm10_tot_invalid_address_days AS algae3323_pm10_tot_inv_addr_days,
		pm10_tot_oob_days AS algae3324_pm10_tot_oob_days,
		pm10_tot_poor_address_days AS algae3325_pm10_tot_poor_addr_days,
		pm10_tot_missing_exp_days AS algae3326_pm10_tot_missing_exp_days,
		pm10_tot_good_address_days AS algae3327_pm10_tot_good_addr_days,		

		name_sum AS algae3328_name_sum,
		name_avg AS algae3329_name_avg,
		name_med AS algae3330_name_med,
		nox_rd_sum AS algae3331_nox_rd_sum,
		nox_rd_avg AS algae3332_nox_rd_avg,
		nox_rd_med AS algae3333_nox_rd_med,
		pm10_gr_sum AS algae3334_pm10_gr_sum,
		pm10_gr_avg AS algae3335_pm10_gr_avg,
		pm10_gr_med AS algae3336_pm10_gr_med,
		pm10_rd_sum AS algae3337_pm10_rd_sum,
		pm10_rd_avg AS algae3338_pm10_rd_avg,
		pm10_rd_med AS algae3339_pm10_rd_med,
		pm10_tot_sum AS algae3340_pm10_tot_sum,
		pm10_tot_avg AS algae3341_pm10_tot_avg,
		pm10_tot_med AS algae3342_pm10_tot_med
	FROM
		fin_stg_mob_exp
	ORDER BY
		person_id,
		ith_life_stage;		

	DROP TABLE IF EXISTS results_early_no_mob_birth_addr_exp;
	CREATE TABLE results_early_no_mob_birth_addr_exp AS
	SELECT
		person_id AS algae3400_person_id,
		life_stage AS algae3401_life_stage,
		life_stage_duration AS algae3402_life_stage_duration,		
		name_invalid_address_days AS algae3403_name_inv_addr_days,
		name_oob_days AS algae3404_name_oob_days,
		name_poor_address_days AS algae3405_name_poor_addr_days,
		name_missing_exp_days AS algae3406_name_missing_exp_days,
		name_good_address_days AS algae3407_name_good_addr_days,

		nox_rd_invalid_address_days AS algae3408_nox_rd_inv_addr_days,
		nox_rd_oob_days AS algae3409_nox_rd_oob_days,
		nox_rd_poor_address_days AS algae3410_nox_rd_poor_addr_days,
		nox_rd_missing_exp_days AS algae3411_nox_rd_missing_exp_days,
		nox_rd_good_address_days AS algae3412_nox_rd_good_addr_days,

		pm10_rd_invalid_address_days AS algae3413_pm10_rd_inv_addr_days,
		pm10_rd_oob_days AS algae3414_pm10_rd_oob_days,
		pm10_rd_poor_address_days AS algae3415_pm10_rd_poor_addr_days,
		pm10_rd_missing_exp_days AS algae3416_pm10_rd_missing_exp_days,
		pm10_rd_good_address_days AS algae3417_pm10_rd_good_addr_days,
		
		pm10_gr_invalid_address_days AS algae3418_pm10_gr_inv_addr_days,
		pm10_gr_oob_days AS algae3419_pm10_gr_oob_days,
		pm10_gr_poor_address_days AS algae3420_pm10_gr_poor_addr_days,
		pm10_gr_missing_exp_days AS algae3421_pm10_gr_missing_exp_days,
		pm10_gr_good_address_days AS algae3422_pm10_gr_good_addr_days,

		pm10_tot_invalid_address_days AS algae3423_pm10_tot_inv_addr_days,
		pm10_tot_oob_days AS algae3424_pm10_tot_oob_days,
		pm10_tot_poor_address_days AS algae3425_pm10_tot_poor_addr_days,
		pm10_tot_missing_exp_days AS algae3426_pm10_tot_missing_exp_days,
		pm10_tot_good_address_days AS algae3427_pm10_tot_good_addr_days,		

		name_sum AS algae3428_name_sum,
		name_avg AS algae3429_name_avg,
		name_med AS algae3430_name_med,
		nox_rd_sum AS algae3431_nox_rd_sum,
		nox_rd_avg AS algae3432_nox_rd_avg,
		nox_rd_med AS algae3433_nox_rd_med,
		pm10_gr_sum AS algae3434_pm10_gr_sum,
		pm10_gr_avg AS algae3435_pm10_gr_avg,
		pm10_gr_med AS algae3436_pm10_gr_med,
		pm10_rd_sum AS algae3437_pm10_rd_sum,
		pm10_rd_avg AS algae3438_pm10_rd_avg,
		pm10_rd_med AS algae3439_pm10_rd_med,
		pm10_tot_sum AS algae3440_pm10_tot_sum,
		pm10_tot_avg AS algae3441_pm10_tot_avg,
		pm10_tot_med AS algae3442_pm10_tot_med
	FROM
		fin_no_mob_birth_addr_exp
	ORDER BY
		person_id,
		ith_life_stage;		

	-- ---------------------------------------
	-- Generate exposure data difference files
	-- ---------------------------------------

	DROP TABLE IF EXISTS results_early_mob_cln_vs_unimp;
	CREATE TABLE results_early_mob_cln_vs_unimp AS
	SELECT
		person_id AS algae4100_person_id,
		life_stage AS algae4101_life_stage,
		name_sum_percent_err AS algae4102_name_sum_percent_err,			
		name_avg_percent_err AS algae4103_name_avg_percent_err,
		name_med_percent_err AS algae4104_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4105_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4106_nox_rd_avg_percent_err,			
		nox_rd_med_percent_err AS algae4107_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4108_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4109_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4110_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4111_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4112_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4113_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4114_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4115_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4116_pm10_tot_med_diff
	FROM
		fin_mob_cln_vs_unimp
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_early_mob_cln_vs_no_mob;
	CREATE TABLE results_early_mob_cln_vs_no_mob AS
	SELECT
		person_id AS algae4200_person_id,
		life_stage AS algae4201_life_stage,
		name_sum_percent_err AS algae4202_name_sum_percent_err,			
		name_avg_percent_err AS algae4203_name_avg_percent_err,
		name_med_percent_err AS algae4204_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4205_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4206_nox_rd_avg_percent_err,			
		nox_rd_med_percent_err AS algae4207_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4208_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4209_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4210_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4211_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4212_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4213_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4214_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4215_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4216_pm10_tot_med_diff
	FROM
		fin_mob_cln_vs_no_mob
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_early_uncln_vs_no_mob;
	CREATE TABLE results_early_uncln_vs_no_mob AS
	SELECT
		person_id AS algae4300_person_id,
		life_stage AS algae4301_life_stage,
		name_sum_percent_err AS algae4302_name_sum_percent_err,			
		name_avg_percent_err AS algae4303_name_avg_percent_err,
		name_med_percent_err AS algae4304_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4305_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4306_nox_rd_avg_percent_err,
		nox_rd_med_percent_err AS algae4307_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4308_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4309_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4310_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4311_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4312_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4313_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4314_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4315_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4316_pm10_tot_med_diff
	FROM
		fin_uncln_vs_no_mob
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_early_mob_cln_vs_stg_mob_birth;
	CREATE TABLE results_early_mob_cln_vs_stg_mob_birth AS
	SELECT
		person_id AS algae4400_person_id,
		life_stage AS algae4401_life_stage,
		name_sum_percent_err AS algae4402_name_sum_percent_err,			
		name_avg_percent_err AS algae4403_name_avg_percent_err,
		name_med_percent_err AS algae4404_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4405_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4406_nox_rd_avg_percent_err,
		nox_rd_med_percent_err AS algae4407_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4408_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4409_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4410_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4411_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4412_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4413_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4414_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4415_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4416_pm10_tot_med_diff
	FROM
		fin_mob_cln_vs_stg_mob_birth
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_early_mob_uncln_vs_stg_mob_birth;
	CREATE TABLE results_early_mob_uncln_vs_stg_mob_birth AS
	SELECT
		person_id AS algae4500_person_id,
		life_stage AS algae4501_life_stage,
		name_sum_percent_err AS algae4502_name_sum_percent_err,			
		name_avg_percent_err AS algae4503_name_avg_percent_err,
		name_med_percent_err AS algae4504_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4505_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4506_nox_rd_avg_percent_err,
		nox_rd_med_percent_err AS algae4507_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4508_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4509_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4510_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4511_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4512_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4513_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4514_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4515_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4516_pm10_tot_med_diff
	FROM
		fin_mob_uncln_vs_stg_mob_birth
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_early_stg_mob_vs_no_mob_birth;
	CREATE TABLE results_early_stg_mob_vs_no_mob_birth AS
	SELECT
		person_id AS algae4600_person_id,
		life_stage AS algae4601_life_stage,
		name_sum_percent_err AS algae4602_name_sum_percent_err,			
		name_avg_percent_err AS algae4603_name_avg_percent_err,
		name_med_percent_err AS algae4604_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4605_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4606_nox_rd_avg_percent_err,
		nox_rd_med_percent_err AS algae4607_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4608_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4609_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4610_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4611_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4612_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4613_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4614_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4615_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4616_pm10_tot_med_diff
	FROM
		fin_stg_mob_vs_no_mob_birth
	ORDER BY
		person_id,
		ith_life_stage;

	-- ---------------------------------------
	-- Generate covariate results
	-- ---------------------------------------

	-- Generating covariates
	DROP TABLE IF EXISTS results_early_life_stages_cov;	
	CREATE TABLE results_early_life_stages_cov AS
	SELECT
		person_id AS algae5100_person_id,
		life_stage AS algae5101_life_stage,
		ed91 AS algae5102_ed91,
		oa2001 AS algae5103_oa2001,
		coa2011 AS algae5104_coa2011
	FROM
		fin_life_stages_cov
	ORDER BY
		person_id,
		ith_life_stage;
	
	DROP TABLE IF EXISTS results_early_moves_cov;
	CREATE TABLE results_early_moves_cov AS
	SELECT
		person_id AS algae5200_person_id,
		start_date_days_from_conception AS algae5201_mv_to_concept_days,
		ed91 AS algae5202_ed91,
		oa2001 AS algae5203_oa2001,
		coa2011 AS algae5204_coa2011
	FROM
		fin_moves_cov
	ORDER BY
		person_id;
	

	-- ---------------------------------------
	-- Generate sensitivity results
	-- ---------------------------------------
	
	-- Generating sensitivity variables
	DROP TABLE IF EXISTS results_early_sens_variables;
	CREATE TABLE results_early_sens_variables AS 
	SELECT
		person_id AS algae6100_person_id,
		at_1st_addr_conception AS algae6101_at_1st_addr_concept,
		absent_during_exp_period AS algae6102_absent_in_exp,
		gestation_age_at_birth AS algae6103_gest_age,	
		is_gestation_age_imputed AS algae6104_is_gest_age_imp,
		total_addr_periods AS algae6105_total_addr,
		fixed_geocodes AS algae6106_fixed_geocodes,
		over_laps AS algae6107_over_laps,
		gaps AS algae6108_gaps,
		gap_and_overlap_same_period AS algae6109_gap_over_lap,
		deletions AS algae6110_deletions,
		imp_blank_start_dates AS algae6111_cln_blank_start_date,
		imp_blank_end_dates AS algae6112_cln_blank_end_date,
		imp_blank_both_dates AS algae6113_cln_blank_both_dates,
		imp_last_dates AS algae6114_cln_last_dates,
		days_changed AS algae6115_days_changed,
		total_contention_days AS algae6116_contention_days,
		no_exposure_data_days AS algae6117_no_exp_data_days
	FROM
		fin_sens_variables
	ORDER BY
		person_id;


	DROP TABLE IF EXISTS results_early_stage_sens_variables;
	CREATE TABLE results_early_stage_sens_variables AS
	SELECT
		person_id AS algae6200_person_id,
		life_stage AS algae6201_life_stage,
		contention_days AS algae6202_contention_days,
		distinct_addresses AS algae6203_distinct_addr,
		moves AS algae6204_moves
	FROM
		fin_life_stage_sensitivity_variables;
	

END;
$$   LANGUAGE plpgsql;


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION early_life_create_result_tables_and_backup
 * -----------------------------------------------------
 * Description
 * -----------
 * Dumps result tables for the early life analysis to CSV files.
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION early_life_create_reports(output_directory TEXT)
	RETURNS void AS 
$$
DECLARE

	results_early_life_stages_csv_file TEXT;
	results_early_mob_cln_exp_csv_file TEXT;
	results_early_mob_uncln_exp_csv_file TEXT;
	results_early_stg_mob_exp_csv_file TEXT;
	results_early_no_mob_birth_addr_exp_csv_file TEXT;
	results_early_life_stages_cov_csv_file TEXT;
	results_early_moves_cov_csv_file TEXT;
	results_early_sens_variables_csv_file TEXT;
	results_early_stage_sens_variables_csv_file TEXT;
	
	results_early_mob_cln_vs_uncln_csv_file TEXT;
	results_early_mob_cln_vs_stg_mob_csv_file TEXT;
	results_early_mob_cln_vs_stg_mob_birth_csv_file TEXT;

	results_early_mob_uncln_vs_stg_mob_birth_csv_file TEXT;
	results_early_mob_uncln_vs_stg_mob_csv_file TEXT;

	
	results_early_stg_mob_vs_no_mob_birth_csv_file TEXT;
	results_early_cleaned_addr_periods_csv_file TEXT;
	results_early_move_date_from_conception_csv_file TEXT;
	date_phrase TEXT;
	
BEGIN

	date_phrase :=
		(SELECT to_char(current_timestamp, 'YYYY-Mon-DD-HH24-MI'));


	-- -----------------------------------------------
	-- Generate life stage data file
	-- -----------------------------------------------

	results_early_life_stages_csv_file :=
		output_directory || 
		'\life_stage_data' ||
		'\results_early_life_stages_' || date_phrase || '.csv';
		
	RAISE NOTICE 'early life stages file==%==', results_early_life_stages_csv_file;
	EXECUTE format ('
	COPY results_early_life_stages
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_life_stages_csv_file);	


	-- ------------------------------------
	-- Generate cleaned address period file
	-- ------------------------------------
	results_early_cleaned_addr_periods_csv_file :=
		output_directory || 
		'\cleaned_address_history\res_early_cleaned_addr' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_cleaned_addr_periods
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_cleaned_addr_periods_csv_file);


	-- -----------------------------------------------
	-- Generate exposure result files
	-- -----------------------------------------------

	-- Generate result file for early mobility exposures with imputing
	results_early_mob_cln_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\mobility_clean\res_early_mob_cln_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_mob_cln_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_mob_cln_exp_csv_file);	

	-- Generate result file for early mobility exposures without imputing
	results_early_mob_uncln_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\mobility_unclean\res_early_mob_uncln_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_mob_uncln_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_mob_uncln_exp_csv_file);

	-- Generate result file for early exposures with no mobility
	-- Uses address at start of each life stage to represent entire
	-- life stage
	results_early_stg_mob_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\mobility_life_stage\res_early_stg_mob_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_stg_mob_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_stg_mob_exp_csv_file);

	-- Generate result file for early exposures with no mobility
	-- Uses address at birth to cover entire early life period
	results_early_no_mob_birth_addr_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\no_mobility_birth_address\res_early_no_mob_birth_addr_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_no_mob_birth_addr_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_no_mob_birth_addr_exp_csv_file);

	-- -----------------------------------------------
	-- Generate exposure difference files
	-- -----------------------------------------------

	results_early_mob_cln_vs_uncln_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_early_mob_cln_vs_uncln_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_mob_cln_vs_unimp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_mob_cln_vs_uncln_csv_file);

	results_early_mob_cln_vs_stg_mob_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_early_mob_cln_vs_stg_mob_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_mob_cln_vs_no_mob
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_mob_cln_vs_stg_mob_csv_file);

	results_early_mob_uncln_vs_stg_mob_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_early_mob_uncln_vs_stg_mob_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_mob_cln_vs_no_mob
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_mob_uncln_vs_stg_mob_csv_file);



	results_early_mob_cln_vs_stg_mob_birth_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_early_mob_cln_vs_stg_mob_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_mob_cln_vs_no_mob
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_mob_cln_vs_stg_mob_birth_csv_file);

	results_early_mob_uncln_vs_stg_mob_birth_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_early_mob_uncln_vs_no_mob_birth_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_mob_cln_vs_stg_mob_birth
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_mob_uncln_vs_stg_mob_birth_csv_file);


	results_early_stg_mob_vs_no_mob_birth_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_early_stg_mob_vs_stg_mob_birth_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_stg_mob_vs_no_mob_birth
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_stg_mob_vs_no_mob_birth_csv_file);














	-- -----------------------------------------------
	-- Generate covariate files
	-- -----------------------------------------------

	results_early_life_stages_cov_csv_file :=
		output_directory || 
		'\covariates\res_early_cov_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_life_stages_cov
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_life_stages_cov_csv_file);

	results_early_moves_cov_csv_file :=
		output_directory || 
		'\covariates\res_early_moves_cov_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_moves_cov
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_moves_cov_csv_file);

	-- -----------------------------------------------
	-- Generate sensitivity variable files
	-- -----------------------------------------------

	results_early_sens_variables_csv_file :=
		output_directory || 
		'\sensitivity_variables\res_early_sens_variables_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_sens_variables
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_sens_variables_csv_file);


	results_early_stage_sens_variables_csv_file :=
		output_directory || 
		'\sensitivity_variables\res_early_stage_sens_variables_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_early_stage_sens_variables
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_early_stage_sens_variables_csv_file);


END;
$$   LANGUAGE plpgsql;

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


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION run_early_study
 * ------------------------
 * Description
 * -----------
 * The routine for running an early life study.  Note that the key difference between this method
 * and run_later_study is the table fin_daily_exposures table is created.  In the early life analysis
 * it involves just making a copy of the staging table.  In the late life analysis, it means
 * converting annual exposures into a set of daily exposures
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION run_early_analysis(output_directory TEXT)
	RETURNS void AS 
$$
DECLARE

BEGIN

	PERFORM comm_preprocess_staging_tables();
	PERFORM comm_perform_prelim_system_checks();

	DROP TABLE IF EXISTS fin_daily_exposures;
	CREATE TABLE fin_daily_exposures AS 	
	SELECT
		*
	FROM
		staging_exp_data;
	ALTER TABLE fin_daily_exposures ADD PRIMARY KEY (geocode, date_of_year);
		
	PERFORM early_calc_life_stages();
	PERFORM comm_set_study_member_sensitivity_data();
	PERFORM comm_process_addr_histories();
	PERFORM comm_set_geocode_sensitivity_data();
	PERFORM comm_set_address_history_sensitivity_data();
	
	PERFORM common_calc_exposures();
		
	PERFORM early_calc_exposures();
	PERFORM comm_set_exp_sensitivity_data();
	PERFORM comm_set_stage_sensitivity_data();

	PERFORM comm_determine_life_stage_cov();
	PERFORM comm_determine_moves_cov();
	PERFORM early_life_create_result_tables_and_backup();
	PERFORM early_life_create_reports(output_directory);
	PERFORM comm_cleanup_sensitivity_variable_data();

	--PERFORM comm_cleanup();
END;

$$   LANGUAGE plpgsql;

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

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION run_early_study
 * ------------------------
 * Description
 * -----------
 * The routine for running an early life study.  Note that the key difference between this method
 * and run_later_study is the table fin_daily_exposures table is created.  In the early life analysis
 * it involves just making a copy of the staging table.  In the late life analysis, it means
 * converting annual exposures into a set of daily exposures
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION run_later_analysis(output_directory TEXT)
	RETURNS void AS 
$$
DECLARE

BEGIN


	PERFORM comm_preprocess_staging_tables();
	PERFORM comm_perform_prelim_system_checks();
	
	DROP TABLE IF EXISTS fin_daily_exposures;
	CREATE TABLE fin_daily_exposures AS
	WITH days_for_years1 AS
		(SELECT
			geocode,
			date_of_year AS start_date,
			(date_of_year + INTERVAL '1 year' - INTERVAL '1 day')::date AS end_date
		 FROM
		 	staging_exp_data),				
	days_for_years2 AS
		(SELECT
			geocode,
			(generate_series(start_date, end_date, '1 day'::interval))::date AS date_of_year
		 FROM
		 	days_for_years1)
	SELECT
		geocode,
		generate_series(
			date_of_year,
			(date_of_year + INTERVAL '1 year' - INTERVAL '1 day')::date,
			'1 day'::interval) AS date_of_year,
		name,
		nox_rd,
		pm10_gr,
		pm10_rd,
		pm10_tot
	FROM
		staging_exp_data
	ORDER BY
		geocode,
		date_of_year;	
			
	ALTER TABLE fin_daily_exposures ADD PRIMARY KEY (geocode, date_of_year);
		
	
	PERFORM late_calc_life_stages();
	PERFORM comm_set_study_member_sensitivity_data();
	PERFORM comm_process_addr_histories();
	PERFORM comm_set_geocode_sensitivity_data();
	PERFORM comm_set_address_history_sensitivity_data();
	
	PERFORM common_calc_exposures();
		
	PERFORM comm_set_exp_sensitivity_data();
	PERFORM comm_set_stage_sensitivity_data();

	PERFORM comm_determine_life_stage_cov();
	PERFORM comm_determine_moves_cov();
	PERFORM later_life_create_result_tables_and_backup();
	PERFORM later_life_create_reports(output_directory);
	PERFORM comm_cleanup_sensitivity_variable_data();

	--PERFORM comm_cleanup();
	
END;

$$   LANGUAGE plpgsql;

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

/**
 * ================================================================================================= 
 * MODULE COMM_EARLY_REP: Backup and Generate Late Life Reports
 * =================================================================================================
 * Description
 * -----------
 * This module contains routines that are meant to help finish the analyses.  It covers two 
 * important tasks:
 * (1) backs up staging tables
 * (2) generates result tables and writes them to CSV files.
 *
 * Some of the staging tables can be expensive to create.  For example, in the late life analysis,
 * the staging_exp_data can contain millions of rows and take a long time to load.  If the late 
 * analysis is run after the late analysis, it will drop and reconsititute the staging tables, but
 * will often include different data.
 *
 * The reports provide a way to filter what tables should be written to CSV files.  It is not really
 * meant to act as a security measure, but it does represent the place where cohort projects can decide
 * whether they are comfortable with extracting results.
 *
 * This is also where variables are renamed for how they will be referenced in publications.  Here,
 * we prefix most of them with "algae_"
 *
 * Main Functions
 * --------------
 * (1) later_life_create_result_tables_and_backup
 * (2) later_life_create_reports 
 *
 * Assumptions
 * ----------- 
 * These two methods should be called at the very end of the analysis and the backup routine should
 * be called before reports are generated.
 *
 * ------------------------------------------------------------------------------------------------
 * Copyright 2017 Imperial College London, developed by the Small Area
 * Health Statistics Unit in collaboration with the Avon Longitudinal Study of Parents
 * and Children (ALSPAC).
 * 
 * The code was originally authored by Kevin Garwood and reviewed by Olly Butters
 * and Iain Bickerstaffe.
 *
 * This file is part of the ALGAE (Algorithms for Assessing Late life exposures) project.
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
 * FUNCTION later_life_create_result_tables_and_backup
 * -----------------------------------------------------
 * Description
 * -----------
 * Backs up the staging tables and creates result tables.  Late and late life analyses often share
 * commonly named tables.  In this method, a copies of tables are made to capture the output of
 * one analysis or the other.  This is also the routine in the late life analysis where variables
 * are renamed for the official variable names that will be cited by other studies that use ALGAE
 * variables.  
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION later_life_create_result_tables_and_backup()
	RETURNS void AS 
$$
DECLARE
	comments TEXT;
BEGIN

/*
	DROP TABLE IF EXISTS bak_later_staging_exp_data;
	CREATE TABLE bak_later_staging_exp_data AS
	SELECT
		*
	FROM
		staging_exp_data;
	
	
	DROP TABLE IF EXISTS bak_later_staging_addr_history_data;
	CREATE TABLE bak_later_staging_addr_history_data AS
	SELECT
		*
	FROM
		staging_addr_history_data;
	
	DROP TABLE IF EXISTS bak_later_staging_geocode_data;
	CREATE TABLE bak_later_staging_geocode_data AS
	SELECT
		*
	FROM
		staging_geocode_data;
	

	DROP TABLE IF EXISTS bak_later_staging_study_member_data;
	CREATE TABLE bak_later_staging_study_member_data AS
	SELECT
		*
	FROM
		staging_study_member_data;
*/
	
	
	-- -----------------------------------------------
	-- Generate life stage data file
	-- -----------------------------------------------	
	DROP TABLE IF EXISTS results_later_life_stages;
	CREATE TABLE results_later_life_stages AS
	SELECT
		person_id AS algae1200_person_id,
		ith_life_stage AS algae1201_ith_life_stage,
		life_stage AS algae1202_life_stage,
		life_stage_year AS algae1203_life_stage_year,
		start_date AS algae1204_start_date,
		end_date AS algae1205_end_date,
		life_stage_duration AS algae1206_life_stage_duration
	FROM
		fin_general_life_stage_data
	ORDER BY
		person_id,
		ith_life_stage;
	

	-- ------------------------------------
	-- Generate cleaned address period file
	-- ------------------------------------	
	
	DROP TABLE IF EXISTS results_later_cleaned_addr_periods;
	CREATE TABLE results_later_cleaned_addr_periods AS
	SELECT
		original_row_number AS algae2200_original_row_number,
		person_id AS algae2201_person_id,
		ith_residence AS algae2202_ith_residence,
		geocode AS algae2203_geocode,
		date_state AS algae2204_date_state,
		start_date AS algae2205_start_date,
		end_date AS algae2206_end_date,
		duration AS algae2207_duration,
		ith_residence_type AS algae2208_ith_residence_type,
		has_valid_geocode AS algae2209_has_valid_geocode,
		has_name_exposures AS algae2210_name_exposures,
		has_nox_rd_exposures AS algae2211_nox_rd_exposures,
		has_pm10_gr_exposures AS algae2212_pm10_gr_exposures,
		has_pm10_rd_exposures AS algae2213_pm10_rd_exposures,
		has_pm10_tot_exposures AS algae2214_pm10_tot_exposures,		
		maximum_life_stage_overlap AS algae2215_max_life_stage_overlap,
		is_fixed_invalid_geocode AS algae2216_is_fixed_inv_geocode,
		fit_extent AS algae2217_fit_extent,
		adjusted_start_date AS algae2218_adj_start_date,
		adjusted_end_date AS algae2219_adj_end_date,
		days_changed AS algae2220_days_changed,
		fit_type AS algae2221_fit_type,
		start_date_delta1 AS algae2222_start_date_delta1,
		start_date_delta2 AS algae2223_start_date_delta2,
		end_date_delta1 AS algae2224_end_date_delta1,
		end_date_delta2 AS algae2225_end_date_delta2,
		previous_geocode AS algae2226_previous_geocode,
		next_geocode AS algae2227_next_geocode,
		fin_adjusted_start_date AS algae2228_fin_adj_start_date,
		imputed_first_start AS algae2229_imputed_first_start,
		fin_adjusted_end_date AS algae2230_fin_adj_end_date,
		imputed_last_end AS algae2231_imputed_last_end,
		start_date_days_from_conception AS algae2232_start_date_days_from_concep,
		is_within_exposure_time_frame AS algae2233_is_within_exp
	FROM
		fin_cleaned_addr_periods
	ORDER BY
		person_id,
		ith_residence;

	-- ------------------------------------
	-- Generate exposure data files
	-- ------------------------------------	

	DROP TABLE IF EXISTS results_later_mob_cln_exp;
	CREATE TABLE results_later_mob_cln_exp AS
	SELECT
		person_id AS algae3500_person_id,
		life_stage AS algae3501_life_stage,	
		life_stage_duration AS algae3502_life_stage_duration,
		name_invalid_address_days AS algae3503_name_inv_addr_days,
		name_oob_days AS algae3504_name_oob_days,
		name_poor_address_days AS algae3505_name_poor_addr_days,
		name_missing_exp_days AS algae3506_name_missing_exp_days,
		name_good_address_days AS algae3507_name_good_addr_days,

		nox_rd_invalid_address_days AS algae3508_nox_rd_inv_addr_days,
		nox_rd_oob_days AS algae3509_nox_rd_oob_days,
		nox_rd_poor_address_days AS algae3510_nox_rd_poor_addr_days,
		nox_rd_missing_exp_days AS algae3511_nox_rd_missing_exp_days,
		nox_rd_good_address_days AS algae3512_nox_rd_good_addr_days,

		pm10_rd_invalid_address_days AS algae3513_pm10_rd_inv_addr_days,
		pm10_rd_oob_days AS algae3514_pm10_rd_oob_days,
		pm10_rd_poor_address_days AS algae3515_pm10_rd_poor_addr_days,
		pm10_rd_missing_exp_days AS algae3516_pm10_rd_missing_exp_days,
		pm10_rd_good_address_days AS algae3517_pm10_rd_good_addr_days,

		pm10_gr_invalid_address_days AS algae3518_pm10_gr_inv_addr_days,
		pm10_gr_oob_days AS algae3519_pm10_gr_oob_days,
		pm10_gr_poor_address_days AS algae3520_pm10_gr_poor_addr_days,
		pm10_gr_missing_exp_days AS algae3521_pm10_gr_missing_exp_days,
		pm10_gr_good_address_days AS algae3522_pm10_gr_good_addr_days,

		pm10_tot_invalid_address_days AS algae3523_pm10_tot_inv_addr_days,
		pm10_tot_oob_days AS algae3524_pm10_tot_oob_days,
		pm10_tot_poor_address_days AS algae3525_pm10_tot_poor_addr_days,
		pm10_tot_missing_exp_days AS algae3526_pm10_tot_missing_exp_days,
		pm10_tot_good_address_days AS algae3527_pm10_tot_good_addr_days,	

		name_sum AS algae3528_name_sum,
		name_err_sum AS algae3529_name_err_sum,
		name_avg AS algae3530_name_avg,
		name_err_avg AS algae3531_name_err_avg,
		name_med AS algae3532_name_med,
		name_err_med AS algae3533_name_err_med,
		nox_rd_sum AS algae3534_nox_rd_sum,
		nox_rd_err_sum AS algae3535_nox_rd_err_sum,
		nox_rd_avg AS algae3536_nox_rd_avg,
		nox_rd_err_avg AS algae3537_nox_rd_err_avg,
		nox_rd_med AS algae3538_nox_rd_med,
		nox_rd_err_med AS algae3539_nox_rd_err_med,
		pm10_gr_sum AS algae3540_pm10_gr_sum,
		pm10_gr_err_sum AS algae3541_pm10_gr_err_sum,
		pm10_gr_avg AS algae3542_pm10_gr_avg,
		pm10_gr_err_avg AS algae3543_pm10_gr_err_avg,
		pm10_gr_med AS algae3544_pm10_gr_med,
		pm10_gr_err_med AS algae3545_pm10_gr_err_med,
		pm10_rd_sum AS algae3546_pm10_rd_sum,
		pm10_rd_err_sum AS algae3547_pm10_rd_err_sum,		
		pm10_rd_avg AS algae3548_pm10_rd_avg,
		pm10_rd_err_avg AS algae3549_pm10_rd_err_avg,
		pm10_rd_med AS algae3550_pm10_rd_med,
		pm10_rd_err_med AS algae3551_pm10_rd_err_med,
		pm10_tot_sum AS algae3552_pm10_tot_sum,
		pm10_tot_err_sum AS algae3553_pm10_tot_err_sum,		
		pm10_tot_avg AS algae3554_pm10_tot_avg,
		pm10_tot_err_avg AS algae3555_pm10_tot_err_avg,
		pm10_tot_med AS algae3556_pm10_tot_med,
		pm10_tot_err_med AS algae3557_pm10_tot_err_med
	FROM
		fin_mob_cln_exp
	ORDER BY
		person_id,
		ith_life_stage;
			
	DROP TABLE IF EXISTS results_later_mob_uncln_exp;
	CREATE TABLE results_later_mob_uncln_exp AS
	SELECT
		person_id AS algae3600_person_id,
		life_stage AS algae3601_life_stage,
		life_stage_duration AS algae3602_life_stage_duration,
		
		name_invalid_address_days AS algae3603_name_inv_addr_days,
		name_oob_days AS algae3604_name_oob_days,
		name_poor_address_days AS algae3605_name_poor_addr_days,
		name_missing_exp_days AS algae3606_name_missing_exp_days,
		name_good_address_days AS algae3607_name_good_addr_days,
		
		nox_rd_invalid_address_days AS algae3608_nox_rd_inv_addr_days,
		nox_rd_oob_days AS algae3609_nox_rd_oob_days,
		nox_rd_poor_address_days AS algae3610_nox_rd_poor_addr_days,
		nox_rd_missing_exp_days AS algae3611_nox_rd_missing_exp_days,
		nox_rd_good_address_days AS algae3612_nox_rd_good_addr_days,

		pm10_rd_invalid_address_days AS algae3613_pm10_rd_inv_addr_days,
		pm10_rd_oob_days AS algae3614_pm10_rd_oob_days,
		pm10_rd_poor_address_days AS algae3615_pm10_rd_poor_addr_days,
		pm10_rd_missing_exp_days AS algae3616_pm10_rd_missing_exp_days,
		pm10_rd_good_address_days AS algae3617_pm10_rd_good_addr_days,

		pm10_gr_invalid_address_days AS algae3618_pm10_gr_inv_addr_days,
		pm10_gr_oob_days AS algae3619_pm10_gr_oob_days,
		pm10_gr_poor_address_days AS algae3620_pm10_gr_poor_addr_days,
		pm10_gr_missing_exp_days AS algae3621_pm10_gr_missing_exp_days,
		pm10_gr_good_address_days AS algae3622_pm10_gr_good_addr_days,

		pm10_tot_invalid_address_days AS algae3623_pm10_tot_inv_addr_days,
		pm10_tot_oob_days AS algae3624_pm10_tot_oob_days,
		pm10_tot_poor_address_days AS algae3625_pm10_tot_poor_addr_days,
		pm10_tot_missing_exp_days AS algae3626_pm10_tot_missing_exp_days,
		pm10_tot_good_address_days AS algae3627_pm10_tot_good_addr_days,

		name_sum AS algae3628_name_sum,
		name_avg AS algae3629_name_avg,
		name_med AS algae3630_name_med,
		nox_rd_sum AS algae3631_nox_rd_sum,
		nox_rd_avg AS algae3632_nox_rd_avg,
		nox_rd_med AS algae3633_nox_rd_med,
		pm10_gr_sum AS algae3634_pm10_gr_sum,
		pm10_gr_avg AS algae3635_pm10_gr_avg,
		pm10_gr_med AS algae3636_pm10_gr_med,
		pm10_rd_sum AS algae3637_pm10_rd_sum,
		pm10_rd_avg AS algae3638_pm10_rd_avg,
		pm10_rd_med AS algae3639_pm10_rd_med,
		pm10_tot_sum AS algae3640_pm10_tot_sum,
		pm10_tot_avg AS algae3641_pm10_tot_avg,
		pm10_tot_med AS algae3642_pm10_tot_med
	FROM
		fin_mob_uncln_exp
	ORDER BY
		person_id,
		ith_life_stage;	
	
	DROP TABLE IF EXISTS results_later_stg_mob_exp;
	CREATE TABLE results_later_stg_mob_exp AS
	SELECT
		person_id AS algae3700_person_id,
		life_stage AS algae3701_life_stage,
		life_stage_duration AS algae3702_life_stage_duration,
		
		name_invalid_address_days AS algae3703_name_inv_addr_days,
		name_oob_days AS algae3704_name_oob_days,
		name_poor_address_days AS algae3705_name_poor_addr_days,
		name_missing_exp_days AS algae3706_name_missing_exp_days,
		name_good_address_days AS algae3707_name_good_addr_days,
		
		nox_rd_invalid_address_days AS algae3708_nox_rd_inv_addr_days,
		nox_rd_oob_days AS algae3709_nox_rd_oob_days,
		nox_rd_poor_address_days AS algae3710_nox_rd_poor_addr_days,
		nox_rd_missing_exp_days AS algae3711_nox_rd_missing_exp_days,
		nox_rd_good_address_days AS algae3712_nox_rd_good_addr_days,

		pm10_rd_invalid_address_days AS algae3713_pm10_rd_inv_addr_days,
		pm10_rd_oob_days AS algae3714_pm10_rd_oob_days,
		pm10_rd_poor_address_days AS algae3715_pm10_rd_poor_addr_days,
		pm10_rd_missing_exp_days AS algae3716_pm10_rd_missing_exp_days,
		pm10_rd_good_address_days AS algae3717_pm10_rd_good_addr_days,

		pm10_gr_invalid_address_days AS algae3718_pm10_gr_inv_addr_days,
		pm10_gr_oob_days AS algae3719_pm10_gr_oob_days,
		pm10_gr_poor_address_days AS algae3720_pm10_gr_poor_addr_days,
		pm10_gr_missing_exp_days AS algae3721_pm10_gr_missing_exp_days,
		pm10_gr_good_address_days AS algae3722_pm10_gr_good_addr_days,

		pm10_tot_invalid_address_days AS algae3723_pm10_tot_inv_addr_days,
		pm10_tot_oob_days AS algae3724_pm10_tot_oob_days,
		pm10_tot_poor_address_days AS algae3725_pm10_tot_poor_addr_days,
		pm10_tot_missing_exp_days AS algae3726_pm10_tot_missing_exp_days,
		pm10_tot_good_address_days AS algae3727_pm10_tot_good_addr_days,

		name_sum AS algae3728_name_sum,
		name_avg AS algae3729_name_avg,
		name_med AS algae3730_name_med,
		nox_rd_sum AS algae3731_nox_rd_sum,
		nox_rd_avg AS algae3732_nox_rd_avg,
		nox_rd_med AS algae3733_nox_rd_med,
		pm10_gr_sum AS algae3734_pm10_gr_sum,
		pm10_gr_avg AS algae3735_pm10_gr_avg,
		pm10_gr_med AS algae3736_pm10_gr_med,
		pm10_rd_sum AS algae3737_pm10_rd_sum,
		pm10_rd_avg AS algae3738_pm10_rd_avg,
		pm10_rd_med AS algae3739_pm10_rd_med,
		pm10_tot_sum AS algae3740_pm10_tot_sum,
		pm10_tot_avg AS algae3741_pm10_tot_avg,
		pm10_tot_med AS algae3742_pm10_tot_med
	FROM
		fin_stg_mob_exp
	ORDER BY
		person_id,
		ith_life_stage;		

	-- ---------------------------------------
	-- Generate exposure data difference files
	-- ---------------------------------------

	DROP TABLE IF EXISTS results_later_mob_cln_vs_unimp;
	CREATE TABLE results_later_mob_cln_vs_unimp AS
	SELECT
		person_id AS algae4700_person_id,
		life_stage AS algae4701_life_stage,		
		name_sum_percent_err AS algae4702_name_sum_percent_err,			
		name_avg_percent_err AS algae4703_name_avg_percent_err,
		name_med_percent_err AS algae4704_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4705_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4706_nox_rd_avg_percent_err,			
		nox_rd_med_percent_err AS algae4707_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4708_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4709_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4710_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4711_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4712_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4713_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4714_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4715_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4716_pm10_tot_med_diff
	FROM
		fin_mob_cln_vs_unimp
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_later_mob_cln_vs_no_mob;
	CREATE TABLE results_later_mob_cln_vs_no_mob AS
	SELECT
		person_id AS algae4800_person_id,
		life_stage AS algae4801_life_stage,
		name_sum_percent_err AS algae4802_name_sum_percent_err,			
		name_avg_percent_err AS algae4803_name_avg_percent_err,
		name_med_percent_err AS algae4804_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4805_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4806_nox_rd_avg_percent_err,			
		nox_rd_med_percent_err AS algae4807_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4808_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4809_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4810_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4811_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4812_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4813_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4814_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4815_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4816_pm10_tot_med_diff
	FROM
		fin_mob_cln_vs_no_mob
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_later_uncln_vs_no_mob;
	CREATE TABLE results_later_uncln_vs_no_mob AS
	SELECT
		person_id AS algae4900_person_id,
		life_stage AS algae4901_life_stage,
		name_sum_percent_err AS algae4902_name_sum_percent_err,			
		name_avg_percent_err AS algae4903_name_avg_percent_err,
		name_med_percent_err AS algae4904_name_med_percent_err,
		nox_rd_sum_percent_err AS algae4905_nox_rd_sum_percent_err,
		nox_rd_avg_percent_err AS algae4906_nox_rd_avg_percent_err,
		nox_rd_med_percent_err AS algae4907_nox_rd_med_percent_err,
		pm10_gr_sum_percent_err AS algae4908_pm10_gr_sum_percent_err,
		pm10_gr_avg_percent_err AS algae4909_pm10_gr_avg_percent_err,
		pm10_gr_med_percent_err AS algae4910_pm10_gr_med_percent_err,
		pm10_rd_sum_percent_err AS algae4911_pm10_rd_sum_percent_err,		
		pm10_rd_avg_percent_err AS algae4912_pm10_rd_avg_percent_err,
		pm10_rd_med_percent_err AS algae4913_pm10_rd_med_percent_err,
		pm10_tot_sum_percent_err AS algae4914_pm10_tot_sum_percent_err,
		pm10_tot_avg_percent_err AS algae4915_pm10_tot_avg_percent_err,
		pm10_tot_med_percent_err AS algae4916_pm10_tot_med_diff
	FROM
		fin_uncln_vs_no_mob
	ORDER BY
		person_id,
		ith_life_stage;

	-- ---------------------------------------
	-- Generate covariate results
	-- ---------------------------------------

	-- Generating covariates
	DROP TABLE IF EXISTS results_later_life_stages_cov;	
	CREATE TABLE results_later_life_stages_cov AS
	SELECT
		person_id AS algae5300_person_id,
		life_stage AS algae5301_life_stage,
		ed91 AS algae5302_ed91,
		oa2001 AS algae5303_oa2001,
		coa2011 AS algae5304_coa2011
	FROM
		fin_life_stages_cov
	ORDER BY
		person_id,
		ith_life_stage;
	
	DROP TABLE IF EXISTS results_later_moves_cov;
	CREATE TABLE results_later_moves_cov AS
	SELECT
		person_id AS algae5400_person_id,
		start_date_days_from_conception AS algae5401_mv_to_concept_days,
		ed91 AS algae5402_ed91,
		oa2001 AS algae5403_oa2001,
		coa2011 AS algae5404_coa2011
	FROM
		fin_moves_cov
	ORDER BY
		person_id;
	

	-- ---------------------------------------
	-- Generate sensitivity results
	-- ---------------------------------------

	DROP TABLE IF EXISTS results_later_sens_variables;
	CREATE TABLE results_later_sens_variables AS 
	SELECT
		person_id AS algae6300_person_id,
		at_1st_addr_conception AS algae6301_at_1st_addr_concept,
		absent_during_exp_period AS algae6302_absent_in_exp,
		gestation_age_at_birth AS algae6303_gest_age,	
		is_gestation_age_imputed AS algae6304_is_gest_age_imp,
		total_addr_periods AS algae6305_total_addr,
		fixed_geocodes AS algae6306_fixed_geocodes,
		over_laps AS algae6307_over_laps,
		gaps AS algae6308_gaps,
		gap_and_overlap_same_period AS algae6309_gap_over_lap,
		deletions AS algae6310_deletions,
		imp_blank_start_dates AS algae6311_cln_blank_start_date,
		imp_blank_end_dates AS algae6312_cln_blank_end_date,
		imp_blank_both_dates AS algae6313_cln_blank_both_dates,
		imp_last_dates AS algae6314_cln_last_dates,
		days_changed AS algae6315_days_changed,
		total_contention_days AS algae6316_contention_days,
		no_exposure_data_days AS algae6317_no_exp_data_days
	FROM
		fin_sens_variables
	ORDER BY
		person_id;

	DROP TABLE IF EXISTS results_later_stage_sens_variables;
	CREATE TABLE results_later_stage_sens_variables AS
	SELECT
		person_id AS algae6400_person_id,
		life_stage AS algae6401_life_stage,
		contention_days AS algae6402_contention_days,
		distinct_addresses AS algae6403_distinct_addr,
		moves AS algae6404_moves
	FROM
		fin_life_stage_sensitivity_variables;
	

END;
$$   LANGUAGE plpgsql;


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION later_life_create_result_tables_and_backup
 * -----------------------------------------------------
 * Description
 * -----------
 * Dumps result tables for the late life analysis to CSV files.
 * ------------------------------------------------------------------------------------------------ 
*/
CREATE OR REPLACE FUNCTION later_life_create_reports(output_directory TEXT)
	RETURNS void AS 
$$
DECLARE

	results_later_life_stages_csv_file TEXT;
	results_later_mob_cln_exp_csv_file TEXT;
	results_later_mob_uncln_exp_csv_file TEXT;
	results_later_stg_mob_exp_csv_file TEXT;
	results_later_no_mob_birth_addr_exp_csv_file TEXT;
	results_later_life_stages_cov_csv_file TEXT;
	results_later_moves_cov_csv_file TEXT;
	results_later_sens_variables_csv_file TEXT;
	results_later_stage_sens_variables_csv_file TEXT;
	
	results_later_mob_cln_vs_uncln_csv_file TEXT;
	results_later_mob_cln_vs_stg_mob_csv_file TEXT;
	results_later_mob_uncln_vs_stg_mob_csv_file TEXT;	
	results_later_cleaned_addr_periods_csv_file TEXT;
	results_later_move_date_from_conception_csv_file TEXT;
	date_phrase TEXT;
	
BEGIN

	date_phrase :=
		(SELECT to_char(current_timestamp, 'YYYY-Mon-DD-HH24-MI'));


	-- -----------------------------------------------
	-- Generate life stage data file
	-- -----------------------------------------------

	results_later_life_stages_csv_file :=
		output_directory || 
		'\life_stage_data' ||
		'\results_later_life_stages_' || date_phrase || '.csv';
		
	RAISE NOTICE 'late life stages file==%==', results_later_life_stages_csv_file;
	EXECUTE format ('
	COPY results_later_life_stages
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_life_stages_csv_file);	


	-- ------------------------------------
	-- Generate cleaned address period file
	-- ------------------------------------
	results_later_cleaned_addr_periods_csv_file :=
		output_directory || 
		'\cleaned_address_history\res_later_cleaned_addr' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_cleaned_addr_periods
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_cleaned_addr_periods_csv_file);


	-- -----------------------------------------------
	-- Generate exposure result files
	-- -----------------------------------------------

	-- Generate result file for late mobility exposures with imputing
	results_later_mob_cln_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\mobility_clean\res_later_mob_cln_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_mob_cln_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_mob_cln_exp_csv_file);	

	-- Generate result file for late mobility exposures without imputing
	results_later_mob_uncln_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\mobility_unclean\res_later_mob_uncln_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_mob_uncln_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_mob_uncln_exp_csv_file);

	-- Generate result file for late exposures with no mobility
	-- Uses address at start of each life stage to represent entire
	-- life stage
	results_later_stg_mob_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\mobility_life_stage\res_later_stg_mob_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_stg_mob_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_stg_mob_exp_csv_file);

	-- Generate result file for late exposures with no mobility
	-- Uses address at birth to cover entire late life period
	/*
	results_later_no_mob_birth_addr_exp_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\no_mobility\res_later_no_mob_birth_addr_exp_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_stg_mob_exp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_stg_mob_exp_csv_file);
	*/

	-- -----------------------------------------------
	-- Generate exposure difference files
	-- -----------------------------------------------

	results_later_mob_cln_vs_uncln_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_later_mob_cln_vs_uncln_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_mob_cln_vs_unimp
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_mob_cln_vs_uncln_csv_file);

	results_later_mob_cln_vs_stg_mob_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_later_mob_cln_vs_stg_mob_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_mob_cln_vs_no_mob
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_mob_cln_vs_stg_mob_csv_file);

	results_later_mob_uncln_vs_stg_mob_csv_file :=
		output_directory || 
		'\exposure_data' ||
		'\method_comparisons\res_later_mob_uncln_vs_stg_mob_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_mob_cln_vs_no_mob
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_mob_uncln_vs_stg_mob_csv_file);


	-- -----------------------------------------------
	-- Generate covariate files
	-- -----------------------------------------------

	results_later_life_stages_cov_csv_file :=
		output_directory || 
		'\covariates\res_later_cov_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_life_stages_cov
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_life_stages_cov_csv_file);

	results_later_moves_cov_csv_file :=
		output_directory || 
		'\covariates\res_later_moves_cov_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_moves_cov
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_moves_cov_csv_file);

	-- -----------------------------------------------
	-- Generate sensitivity variable files
	-- -----------------------------------------------

	results_later_sens_variables_csv_file :=
		output_directory || 
		'\sensitivity_variables\res_later_sens_variables_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_sens_variables
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_sens_variables_csv_file);


	results_later_stage_sens_variables_csv_file :=
		output_directory || 
		'\sensitivity_variables\res_later_stage_sens_variables_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_later_stage_sens_variables
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_later_stage_sens_variables_csv_file);


END;
$$   LANGUAGE plpgsql;

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

/**
 * ================================================================================================= 
 * MODULE TEST_SUITES: Test Suites
 * =================================================================================================
 * Description
 * -----------
 * This module contains code for running an automated test suite, which tests various areas of 
 * functionality in the ALGAE protocol.
 * Assumptions
 * -----------
 * Directories containing test data should already be installed before the test suite is run.
 *
 * Main Function
 * -------------
 *    run_test_suite
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
 * FUNCTION test_preprocessing
 * -----------------------------
 * Description
 * -----------
 * Verifies that variables found in the fin_addr_periods table and variables found in the
 * sensitivity variables table reflect correct behaviour.
 * ------------------------------------------------------------------------------------------------ 
 */

CREATE OR REPLACE FUNCTION test_preprocessing(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;
	expected_results_directory TEXT;

	expected_geocode_flags_file TEXT;
	expected_study_member_flags_file TEXT;

BEGIN

	test_suite_directory 
		:= test_directory || 
		'\preprocessing_data\early_life\input_data';
	PERFORM load_original_test_data(test_suite_directory);

	PERFORM comm_preprocess_staging_tables();
	PERFORM comm_perform_prelim_system_checks();

	-- we are only interested in looking at the staging tables to make sure
	-- that pre-processing has been done correctly.

	expected_results_directory
		:= test_directory ||
			'\preprocessing_data\early_life\expected_results';

	expected_geocode_flags_file 
		:= expected_results_directory || 
		'\expected_geocode_flags.csv';

	DROP TABLE IF EXISTS test_preprocessing_geocodes;
	CREATE TABLE test_preprocessing_geocodes (
		geocode TEXT,
		comments TEXT,
		has_valid_geocode TEXT
	);
	
	EXECUTE format ('
	COPY test_preprocessing_geocodes (	
		geocode,
		comments,
		has_valid_geocode) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_geocode_flags_file);

	
	expected_study_member_flags_file 
		:= expected_results_directory || 
		'\expected_study_member_flags.csv';

	DROP TABLE IF EXISTS test_preprocessing_study_member_data;
	CREATE TABLE test_preprocessing_study_member_data (
		person_id TEXT,
		comments TEXT,
		absent_during_exp_period TEXT,
		at_1st_addr_conception TEXT
	);
	
	EXECUTE format ('
	COPY test_preprocessing_study_member_data (	
		person_id,
		comments,
		absent_during_exp_period,
		at_1st_addr_conception) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_study_member_flags_file);

	DROP TABLE IF EXISTS test_preprocessing_expected1;
	CREATE TABLE test_preprocessing_expected1 AS 
	SELECT
		a.geocode,
		a.comments,
		CASE
			WHEN a.has_valid_geocode = b.has_valid_geocode THEN
				TRUE
			ELSE
				FALSE
		END is_has_valid_geocode_correct
	FROM
		test_preprocessing_geocodes a,
		staging_geocode_data b
	WHERE
		a.geocode = b.geocode
	ORDER BY
		a.geocode;

	INSERT INTO tmp_all_test_case_results 
	SELECT
		geocode AS test_case_name,
		'Preprocessing: Geocode Flags' AS test_area,		
		comments AS test_case_description,
		CASE
			WHEN is_has_valid_geocode_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'	
		END AS pass_or_fail
	FROM
		test_preprocessing_expected1
	ORDER BY
		geocode;

	DROP TABLE IF EXISTS test_preprocessing_expected2;
	CREATE TABLE test_preprocessing_expected2 AS 
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN a.absent_during_exp_period = b.absent_during_exp_period THEN
				TRUE
			ELSE
				FALSE
		END AS is_absent_during_exp_period_correct,
		CASE
			WHEN a.at_1st_addr_conception = b.at_1st_addr_conception THEN
				TRUE
			ELSE
				FALSE
		END AS is_at_1st_addr_conception_correct		
	FROM
		test_preprocessing_study_member_data a,
		staging_study_member_data b
	WHERE
		a.person_id = b.person_id
	ORDER BY
		a.person_id;
	
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Preprocessing: Study Member Flags' AS test_area,		
		comments AS test_case_description,
		CASE
			WHEN is_absent_during_exp_period_correct = FALSE OR
				is_at_1st_addr_conception_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'	
		END AS pass_or_fail
	FROM
		test_preprocessing_expected2
	ORDER BY
		person_id;

	-- Cleanup
	DROP TABLE test_preprocessing_geocodes;
	DROP TABLE test_preprocessing_study_member_data;
	DROP TABLE test_preprocessing_expected1;
	DROP TABLE test_preprocessing_expected2;	

END;

$$   LANGUAGE plpgsql;
--SELECT "test_preprocessing"('C:\algae_protocol\test_environment');

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION test_addr_periods
 * -----------------------------
 * Description
 * -----------
 * Verifies that variables found in the fin_addr_periods table and variables found in the
 * sensitivity variables table reflect correct behaviour.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION test_address_histories(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;
	expected_results_directory TEXT;
	expected_results_file TEXT;
	expected_sens_results_file TEXT;

	expected_stage_sens_results_file TEXT;
	
	number_of_mismatches INT;

BEGIN
	test_suite_directory := test_directory || '\address_history_data\early_life\input_data';
	output_directory := test_directory || '\address_history_data\early_life\results';

	PERFORM load_original_test_data(test_suite_directory);
	PERFORM run_early_analysis(output_directory);

	expected_results_directory := test_directory || '\address_history_data\early_life\expected_results';
	expected_results_file := expected_results_directory || '\expected_address_period_results.csv';
	expected_sens_results_file 
		:= expected_results_directory || 
			'\expected_address_periods_sens_results.csv';

	expected_stage_sens_results_file
		:= expected_results_directory || 
			'\expected_address_periods_stage_sens_results.csv';


	DROP TABLE IF EXISTS test_addr_period_fields_expected1;
	CREATE TABLE test_addr_period_fields_expected1 (
		person_id TEXT,
		ith_residence INT,
		comments TEXT, -- comment field, often used in testing
		date_state TEXT,
		start_date DATE,
		end_date TEXT,
		fin_adjusted_start_date DATE,
		fin_adjusted_end_date TEXT,
		fit_type TEXT,
		days_changed INT,
		geocode TEXT,
		start_date_contender TEXT,
		start_date_delta1 DATE,
		start_date_delta2 DATE,
		end_date_contender TEXT,
		end_date_delta1 DATE,
		end_date_delta2 DATE,
		imputed_first_start TEXT,
		imputed_last_end TEXT
	);

	EXECUTE format ('
	COPY test_addr_period_fields_expected1 (	
		person_id,
		ith_residence,
		comments, -- comment field, often used in testing
		date_state,
		start_date,
		end_date,
		fin_adjusted_start_date,
		fin_adjusted_end_date,
		fit_type,
		days_changed,
		geocode,
		start_date_contender,
		start_date_delta1,
		start_date_delta2,
		end_date_contender,
		end_date_delta1,
		end_date_delta2,
		imputed_first_start,
		imputed_last_end) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_results_file);

	DROP TABLE IF EXISTS test_addr_period_fields_expected2;
	CREATE TABLE test_addr_period_fields_expected2 AS 
	SELECT
		person_id,
		ith_residence,
		comments,
		date_state,
		start_date,
		CASE
			WHEN end_date = 'now' THEN
				NOW() -- this is a way making sure that expected value changes as time goes on
			ELSE
				end_date::date
		END AS end_date,
		fin_adjusted_start_date,
		CASE
			WHEN fin_adjusted_end_date = 'now' THEN
				NOW()::date 
			ELSE
				fin_adjusted_end_date::date
		END fin_adjusted_end_date,
		fit_type,
		days_changed,
		geocode,
		start_date_contender,
		start_date_delta1,
		start_date_delta2,
		end_date_contender,
		end_date_delta1,
		end_date_delta2,
		imputed_first_start,
		imputed_last_end		
	FROM
		test_addr_period_fields_expected1;

	DROP TABLE IF EXISTS results_test_addr_periods1;
	CREATE TABLE results_test_addr_periods1 AS
	SELECT
		a.person_id,
		a.comments,
		a.ith_residence,
		CASE
			WHEN COALESCE(a.date_state::text, 'null') = COALESCE(b.date_state::text, 'null') THEN
				TRUE
		ELSE
			FALSE
		END AS is_date_state_correct,	       
		CASE
			WHEN COALESCE(a.fin_adjusted_start_date::text, 'null') = COALESCE(b.fin_adjusted_start_date::text, 'null') THEN
		TRUE
		ELSE
			FALSE
		END AS is_fin_adj_start_date_correct,
		CASE
			WHEN COALESCE(a.fin_adjusted_end_date::text, 'null') = COALESCE(b.fin_adjusted_end_date::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_fin_adj_end_date_correct,
		CASE
			WHEN COALESCE(a.start_date_delta1::text, 'null') = COALESCE(b.start_date_delta1::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_start_date_delta1_correct,
		CASE
			WHEN COALESCE(a.start_date_delta2::text, 'null') = COALESCE(b.start_date_delta2::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_start_date_delta2_correct,
		CASE
			WHEN COALESCE(a.end_date_delta1::text, 'null') = COALESCE(b.end_date_delta1::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_end_date_delta1_correct,
		CASE
			WHEN COALESCE(a.end_date_delta2::text, 'null') = COALESCE(b.end_date_delta2::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_end_date_delta2_correct,
		CASE
			WHEN COALESCE(a.start_date_contender::text, 'null') = COALESCE(b.start_date_contender::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_start_date_contender_correct,
		CASE
			WHEN COALESCE(a.end_date_contender::text, 'null') = COALESCE(b.end_date_contender::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_end_date_contender_correct,
		CASE
			WHEN COALESCE(a.fit_type::text, 'null') = COALESCE(b.fit_type::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_fit_type_correct,
		CASE
			WHEN COALESCE(a.days_changed::text, 'null') = COALESCE(b.days_changed::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_days_changed_correct,
		CASE
			WHEN COALESCE(a.imputed_first_start::text, 'null') = COALESCE(b.imputed_first_start::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_imputed_first_start_correct,
		CASE
			WHEN COALESCE(a.imputed_last_end::text, 'null') = COALESCE(b.imputed_last_end::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_imputed_last_end_correct
	FROM
		test_addr_period_fields_expected2 a,
		fin_cleaned_addr_periods b
	WHERE
		a.person_id = b.person_id AND
		a.ith_residence = b.ith_residence
	ORDER BY
		a.person_id,
		a.ith_residence;
		
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Address History Data' AS test_area,		
		comments AS test_case_description,
		CASE
			WHEN is_date_state_correct = FALSE OR 
				is_fin_adj_start_date_correct = FALSE OR 
				is_fin_adj_end_date_correct = FALSE OR 
				is_start_date_delta1_correct = FALSE OR 
				is_start_date_delta2_correct = FALSE OR 
				is_end_date_delta1_correct = FALSE OR 
				is_end_date_delta2_correct = FALSE OR 
				is_start_date_contender_correct = FALSE OR 
				is_end_date_contender_correct = FALSE OR 
				is_fit_type_correct = FALSE OR 
				is_days_changed_correct = FALSE OR 
				is_imputed_first_start_correct = FALSE OR 
				is_imputed_last_end_correct = FALSE THEN

				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_addr_periods1
	ORDER BY
		person_id;

	

	DROP TABLE IF EXISTS test_early_addr_periods_sens_expected;
	CREATE TABLE test_early_addr_periods_sens_expected (
		person_id TEXT,
		comments TEXT,
		over_laps INT,
		gaps INT,
		gap_and_overlap_same_period INT,
		deletions INT,
		imp_blank_start_dates INT,
		imp_blank_end_dates INT,
		imp_blank_both_dates INT,
		imp_last_dates INT,
		days_changed INT,
		total_addr_periods INT,
		total_contention_days INT
	);

	EXECUTE format ('
	COPY test_early_addr_periods_sens_expected (	
		person_id,
		comments,
		over_laps,
		gaps,
		gap_and_overlap_same_period,
		deletions,
		imp_blank_start_dates,
		imp_blank_end_dates,
		imp_blank_both_dates,
		imp_last_dates,
		days_changed,
		total_addr_periods,
		total_contention_days)
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_sens_results_file);

	DROP TABLE IF EXISTS results_test_addr_periods2;
	CREATE TABLE results_test_addr_periods2 AS
	WITH expected_sensitivity_variables AS
		(SELECT
			person_id,
			comments,
			over_laps,
			gaps,
			gap_and_overlap_same_period,
			deletions,
			imp_blank_start_dates,
			imp_blank_end_dates,
			imp_blank_both_dates,
			imp_last_dates,
			days_changed,
			total_addr_periods,
			total_contention_days
		 FROM
		 	test_early_addr_periods_sens_expected),
	actual_sensitivity_variables AS
		(SELECT
			person_id,
			over_laps,
			gaps,
			gap_and_overlap_same_period,
			deletions,
			imp_blank_start_dates,
			imp_blank_end_dates,
			imp_blank_both_dates,
			imp_last_dates,
			days_changed,
			total_addr_periods,
			total_contention_days
		 FROM
		 	fin_sens_variables)	
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN COALESCE(a.over_laps::text, 'null') = COALESCE(b.over_laps::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_over_laps_correct,
		CASE
			WHEN COALESCE(a.gaps::text, 'null') = COALESCE(b.gaps::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_gaps_correct,
		CASE
			WHEN COALESCE(a.gap_and_overlap_same_period::text, 'null') = COALESCE(b.gap_and_overlap_same_period::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_gap_and_overlap_same_period_correct,
		CASE
			WHEN COALESCE(a.deletions::text, 'null') = COALESCE(b.deletions::text, 'null') THEN
				TRUE
			ELSE
				FALSE				
		END AS is_deletions_correct,
		CASE
			WHEN COALESCE(a.imp_blank_start_dates::text, 'null') = COALESCE(b.imp_blank_start_dates::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_imp_blank_start_dates_correct,
		CASE
			WHEN COALESCE(a.imp_blank_end_dates::text, 'null') = COALESCE(b.imp_blank_end_dates::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_imp_blank_end_dates_correct,			
		CASE
			WHEN COALESCE(a.imp_blank_both_dates::text, 'null') = COALESCE(b.imp_blank_both_dates::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_imp_blank_both_dates_correct,
		CASE
			WHEN COALESCE(a.imp_last_dates::text, 'null') = COALESCE(b.imp_last_dates::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_imp_last_dates_correct,
		CASE
			WHEN COALESCE(a.days_changed::text, 'null') = COALESCE(b.days_changed::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_days_changed_correct,
		CASE
			WHEN COALESCE(a.total_addr_periods::text, 'null') = COALESCE(b.total_addr_periods::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_total_addr_periods_correct,
		CASE
			WHEN COALESCE(a.total_contention_days::text, 'null') = COALESCE(b.total_contention_days::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_total_contention_days_correct				
	FROM
		 expected_sensitivity_variables a,
		 actual_sensitivity_variables b
	WHERE
		a.person_id = b.person_id;

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Address Cleaning Sensitivity Variables' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_total_addr_periods_correct = FALSE OR
				is_over_laps_correct = FALSE OR 
				is_gaps_correct = FALSE OR 
				is_gap_and_overlap_same_period_correct = FALSE OR 
				is_deletions_correct = FALSE OR 
				is_imp_blank_start_dates_correct = FALSE OR 
				is_imp_blank_start_dates_correct = FALSE OR 
				is_imp_blank_end_dates_correct = FALSE OR 
				is_imp_blank_both_dates_correct = FALSE OR 
				is_imp_last_dates_correct = FALSE OR 
				is_days_changed_correct = FALSE OR 
				is_total_addr_periods_correct = FALSE OR
				is_total_contention_days_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
		FROM
			results_test_addr_periods2
		ORDER BY
			person_id;
	
	
	DROP TABLE IF EXISTS test_early_addr_periods_stage_sens_expected;
	CREATE TABLE test_early_addr_periods_stage_sens_expected (
		person_id TEXT,
		comments TEXT,
		life_stage TEXT,
		contention_days INT,
		distinct_addresses INT,
		moves INT
	);

	EXECUTE format ('
	COPY test_early_addr_periods_stage_sens_expected (	
		person_id,
		comments,
		life_stage,
		contention_days,
		distinct_addresses,
		moves)
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_stage_sens_results_file);
	
	DROP TABLE IF EXISTS results_test_addr_periods3;
	CREATE TABLE results_test_addr_periods3 AS
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN COALESCE(a.contention_days::text, 'null') = COALESCE(b.contention_days::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_contention_days_correct,
		CASE
			WHEN COALESCE(a.distinct_addresses::text, 'null') = COALESCE(b.distinct_addresses::text, null) THEN
				TRUE
			ELSE
				FALSE
		END AS is_distinct_addresses_correct,
		CASE
			WHEN COALESCE(a.moves::text, 'null') = COALESCE(b.moves::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_moves_correct
	FROM
		test_early_addr_periods_stage_sens_expected a,
		fin_life_stage_sensitivity_variables b
	WHERE
		a.person_id = b.person_id AND
		a.life_stage = b.life_stage
	ORDER BY
		a.person_id;

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Address Cleaning Life Stage Sensitivity Variables' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_contention_days_correct = FALSE OR 
				is_distinct_addresses_correct = FALSE OR 
				is_moves_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
		FROM
			results_test_addr_periods3
		ORDER BY
			person_id;
	
	-- Cleanup
	--DROP TABLE test_addr_period_fields_expected1;
	--DROP TABLE test_addr_period_fields_expected2;
	--DROP TABLE test_early_addr_periods_sens_expected;	
	--DROP TABLE results_test_addr_periods1;
	--DROP TABLE results_test_addr_periods2;
	--DROP TABLE results_test_addr_periods3;
	
END;

$$   LANGUAGE plpgsql;
--SELECT "test_addr_periods"('C:\algae_protocol\test_environment');

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION test_geocode_data
 * --------------------------
 * Description
 * -----------
 * Verifies that variables found in the fin_addr_periods table and variables found in the
 * sensitivity variables table reflect correct behaviour.
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION test_geocode_data(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;
	expected_results_directory TEXT;
	expected_results_file TEXT;
	expected_sens_results_file TEXT;
	number_of_mismatches INT;

BEGIN
	test_suite_directory := 
		test_directory || '\geocode_data\early_life\input_data';
	output_directory :=
		test_directory || '\geocode_data\early_life\results';

	PERFORM load_original_test_data(test_suite_directory);
	PERFORM run_early_analysis(output_directory);

	expected_results_directory := test_directory || '\geocode_data\early_life\expected_results';
	expected_results_file := expected_results_directory || '\expected_results.csv';
	expected_sens_results_file := expected_results_directory || '\expected_sensitivity_variable_results.csv';

	DROP TABLE IF EXISTS test_early_bad_geocodes_expected;
	CREATE TABLE test_early_bad_geocodes_expected (
		person_id TEXT,
		ith_residence INT,
		comments TEXT, -- comment field, often used in testing
		geocode TEXT, --(x,y) coordinates. Typically "x-y" will be the actual geocode value
		start_date DATE,
		end_date DATE,
		early_is_fixed_invalid_geocode TEXT,
		ith_residence_type TEXT,
		fin_adjusted_start_date DATE,
		fin_adjusted_end_date DATE
	);

	EXECUTE format ('
	COPY test_early_bad_geocodes_expected (	
		person_id,
		ith_residence,
		comments,
		geocode,
		start_date,
		end_date,
		early_is_fixed_invalid_geocode,
		ith_residence_type,
		fin_adjusted_start_date,
		fin_adjusted_end_date) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_results_file);

		--has_valid_geocode,
		--has_exposures,
		--is_out_of_bounds,


	WITH expected_actual_comparisons AS
		(SELECT
			a.person_id,
			a.comments,
			a.ith_residence,
			CASE
				WHEN COALESCE(a.start_date::text, 'null') = COALESCE(b.start_date::text, 'null') THEN
					TRUE
				ELSE
					FALSE
			END AS is_start_date_correct,
			CASE
				WHEN COALESCE(a.end_date::text, 'null') = COALESCE(b.end_date::text, 'null') THEN
					TRUE
				ELSE
					FALSE
			END AS is_end_date_correct,
			CASE
				WHEN COALESCE(a.early_is_fixed_invalid_geocode, 'null') = COALESCE(b.is_fixed_invalid_geocode, 'null') THEN
					TRUE
				ELSE
					FALSE
			END AS is_early_is_fixed_bad_geocode_correct,
			CASE
				WHEN COALESCE(a.ith_residence_type, 'null') = COALESCE(b.ith_residence_type, 'null') THEN
					TRUE
				ELSE
					FALSE
			END AS is_ith_residence_type_correct,
			CASE
				WHEN COALESCE(a.fin_adjusted_start_date::text, 'null') = COALESCE(b.fin_adjusted_start_date::text, 'null') THEN
					TRUE
				ELSE
					FALSE
			END AS is_fin_adjusted_start_date_correct,
			CASE
				WHEN COALESCE(a.fin_adjusted_end_date::text, 'null') = COALESCE(b.fin_adjusted_end_date::text, 'null') THEN
					TRUE
				ELSE
					FALSE
			END AS is_fin_adjusted_end_date_correct	
	FROM
		test_early_bad_geocodes_expected a,
		fin_cleaned_addr_periods b
	WHERE
		a.person_id = b.person_id AND
		a.ith_residence = b.ith_residence
	ORDER BY
		a.person_id,
		a.ith_residence) 
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Geocode Data' AS test_area,		
		comments AS test_case_description,
		CASE
			WHEN is_start_date_correct = FALSE OR
				is_end_date_correct = FALSE OR
				is_early_is_fixed_bad_geocode_correct = FALSE OR
				is_ith_residence_type_correct = FALSE OR
				is_fin_adjusted_start_date_correct = FALSE OR
				is_fin_adjusted_end_date_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		expected_actual_comparisons
	ORDER BY
		person_id;



	DROP TABLE IF EXISTS test_early_bad_geocodes_sens_expected;
	CREATE TABLE test_early_bad_geocodes_sens_expected (
		person_id TEXT,
		comments TEXT, -- comment field, often used in testing
		total_addr_periods INT,
		invalid_geocodes INT,
		fixed_geocodes INT
	);

	EXECUTE format ('
	COPY test_early_bad_geocodes_sens_expected (	
		person_id,
		comments,
		total_addr_periods,
		invalid_geocodes,
		fixed_geocodes)
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_sens_results_file);

	DROP TABLE IF EXISTS results_test_bad_geocodes1;
	CREATE TABLE results_test_bad_geocodes1 AS
	WITH expected_sensitivity_variables AS
		(SELECT
			person_id,
			comments,
			total_addr_periods,
			invalid_geocodes,
			fixed_geocodes
		 FROM
		 	test_early_bad_geocodes_sens_expected),
	actual_sensitivity_variables AS
		(SELECT
			person_id,
			total_addr_periods,
			invalid_geocodes,
			fixed_geocodes
		 FROM
		 	fin_sens_variables)
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN a.total_addr_periods = b.total_addr_periods THEN
				TRUE
			ELSE
				FALSE
		END AS is_total_addr_periods_correct,
		CASE
			WHEN a.invalid_geocodes = b.invalid_geocodes THEN
				TRUE
			ELSE
				FALSE
		END AS is_invalid_geocodes_correct,
		CASE
			WHEN a.fixed_geocodes = b.fixed_geocodes THEN
				TRUE
			ELSE
				FALSE				
		END AS is_fixed_geocodes_correct
	 FROM
	 	expected_sensitivity_variables a,
	 	actual_sensitivity_variables b
	 WHERE
	 	a.person_id = b.person_id;
		 			 	
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Geocode Data' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_total_addr_periods_correct = FALSE OR
				is_invalid_geocodes_correct = FALSE OR
				is_fixed_geocodes_correct = FALSE THEN 
			'FAIL'
		ELSE
			'PASS'
		END AS pass_or_fail
	FROM
		results_test_bad_geocodes1
	ORDER BY
		person_id;
	
	-- Cleanup
	--DROP TABLE test_early_bad_geocodes_expected;	
	--DROP TABLE test_early_bad_geocodes_sens_expected;
	--DROP TABLE results_test_bad_geocodes1;
	
END;

$$   LANGUAGE plpgsql;

--SELECT "test_bad_geocodes"('C:\algae_protocol\test_environment');

/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION test_study_member_data
 * -------------------------------
 * Description
 * -----------
 * Verifies that variables related to life stage calculates behave correctly.
 *
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION test_study_member_data(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;
	
	expected_results_directory TEXT;
	expected_life_stage_results_file TEXT;
	expected_life_stage_sens_results_file TEXT;
	
	expected_study_member_flags_file TEXT;	
	
	
	number_of_mismatches INT;

BEGIN

	test_suite_directory := 
		test_directory || '\study_member_data\early_life\input_data';
	output_directory :=
		test_directory || '\study_member_data\early_life\results';

	PERFORM load_original_test_data(test_suite_directory);
	PERFORM run_early_analysis(output_directory);

	output_directory := test_directory || '\study_member_data\early_life\results';
	expected_results_directory := test_directory || '\study_member_data\early_life\expected_results';

	-- =================================================
	-- Test sensitivity variables
	-- =================================================	

	expected_life_stage_sens_results_file :=
		expected_results_directory || '\expected_life_stage_sens_results.csv';

	DROP TABLE IF EXISTS test_expected_life_stage_sens_results;
	CREATE TABLE test_expected_life_stage_sens_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   gestation_age_at_birth INT,
	   is_gestation_age_imputed TEXT,
	   absent_during_exp_period TEXT,
	   at_1st_addr_conception TEXT
	);

	EXECUTE format ('
	COPY test_expected_life_stage_sens_results (	
		person_id,
		comments, -- comment field, often used in testing
		gestation_age_at_birth,
		is_gestation_age_imputed,
		absent_during_exp_period,
		at_1st_addr_conception) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_life_stage_sens_results_file);

	-- now compare expected vs actual results	
	DROP TABLE IF EXISTS results_test_life_stages1;
	CREATE TABLE results_test_life_stages1 AS	
	WITH expected_sensitivity_results AS
		(SELECT
			person_id,
			comments,
			gestation_age_at_birth,
			is_gestation_age_imputed,
			absent_during_exp_period,
			at_1st_addr_conception
		 FROM
		 	test_expected_life_stage_sens_results),
	actual_sensitivity_results AS
		(SELECT
			person_id,
			gestation_age_at_birth,
			is_gestation_age_imputed,
			absent_during_exp_period,
			at_1st_addr_conception
		 FROM
		 	fin_sens_variables)
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN a.gestation_age_at_birth = b.gestation_age_at_birth THEN
				TRUE
			ELSE
				FALSE
		END AS is_gestation_age_at_birth_correct,
		CASE
			WHEN a.is_gestation_age_imputed = b.is_gestation_age_imputed THEN
				TRUE
			ELSE
				FALSE
		END AS is_gestation_age_flag_correct,
		CASE
			WHEN a.absent_during_exp_period = b.absent_during_exp_period THEN
				TRUE
			ELSE
				FALSE
		END AS is_absent_during_exp_period_correct,
		CASE
			WHEN a.at_1st_addr_conception = b.at_1st_addr_conception THEN
				TRUE
			ELSE
				FALSE
		END AS is_at_1st_addr_conception_correct			
	 FROM
	 	expected_sensitivity_results a,
	 	actual_sensitivity_results b
	 WHERE
	 	a.person_id = b.person_id;
	 		 	
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Study Member Data' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_gestation_age_at_birth_correct = FALSE OR
				is_gestation_age_flag_correct = FALSE OR 	      	 
			 	is_at_1st_addr_conception_correct = FALSE OR
			 	is_absent_during_exp_period_correct = FALSE OR 
			 	is_at_1st_addr_conception_correct = FALSE THEN 
			 
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_life_stages1
	ORDER BY
		person_id;

	-- =================================================
	-- Test life stage variables
	-- =================================================	
	expected_life_stage_results_file :=
		expected_results_directory || '\expected_life_stage_results.csv';

	DROP TABLE IF EXISTS test_expected_life_stage_results;
	CREATE TABLE test_expected_life_stage_results (
		person_id TEXT,
		comments TEXT, -- comment field, often used in testing
		life_stage TEXT,
		start_date DATE,
		end_date DATE,
		life_stage_duration INT);

	EXECUTE format ('
	COPY test_expected_life_stage_results (	
		person_id,
		comments,
		life_stage,
		start_date,
		end_date,
		life_stage_duration) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_life_stage_results_file);

	DROP TABLE IF EXISTS results_test_life_stages2;
	CREATE TABLE results_test_life_stages2 AS
	WITH expected_life_stage_results AS
		(SELECT
			person_id,
			comments,
			life_stage,
			start_date,
			end_date,
			life_stage_duration
		 FROM
		 	test_expected_life_stage_results),
	actual_life_stage_results AS
		(SELECT
			person_id,
			life_stage,
			start_date,
			end_date,
			life_stage_duration
		 FROM
		 	fin_general_life_stage_data)
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN COALESCE(a.start_date::text, 'null') = COALESCE(b.start_date::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_start_date_correct,
		CASE
			WHEN COALESCE(a.end_date::text, 'null') = COALESCE(b.end_date::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_end_date_correct,
		CASE
			WHEN COALESCE(a.life_stage_duration::text, 'null') = COALESCE(b.life_stage_duration::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_life_stage_duration_correct
	FROM
		expected_life_stage_results a,
		actual_life_stage_results b
	WHERE
		a.person_id = b.person_id AND
		a.life_stage = b.life_stage
	ORDER BY
		a.person_id,
		a.life_stage;
				
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Study Member Data' AS test_area,		
		comments AS test_case_description,
		CASE
			WHEN is_start_date_correct = FALSE OR
				is_end_date_correct = FALSE OR
				is_life_stage_duration_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'	
		END AS pass_or_fail
	FROM
		results_test_life_stages2
	ORDER BY
		person_id;


	expected_study_member_flags_file :=
		expected_results_directory || '\expected_study_member_flags.csv';

	DROP TABLE IF EXISTS test_expected_study_member_flag_results;
	CREATE TABLE test_expected_study_member_flag_results (
		person_id TEXT,
		comments TEXT, -- comment field, often used in testing
		absent_during_exp_period TEXT,
		at_1st_addr_conception TEXT);

	EXECUTE format ('
	COPY test_expected_study_member_flag_results (	
		person_id,
		comments,
		absent_during_exp_period,
		at_1st_addr_conception) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', expected_study_member_flags_file);

	DROP TABLE IF EXISTS results_test_life_stages3;
	CREATE TABLE results_test_life_stages3 AS
	WITH expected_life_stage_results AS
		(SELECT
			person_id,
			comments,
			absent_during_exp_period,
			at_1st_addr_conception
		 FROM
		 	test_expected_study_member_flag_results),
	actual_life_stage_results AS
		(SELECT
			person_id,
			absent_during_exp_period,
			at_1st_addr_conception
		 FROM
		 	fin_sens_variables)
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN COALESCE(a.absent_during_exp_period::text, 'null') = COALESCE(b.absent_during_exp_period::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_absent_during_exp_period_correct,
		CASE
			WHEN COALESCE(a.at_1st_addr_conception::text, 'null') = COALESCE(b.at_1st_addr_conception::text, 'null') THEN
				TRUE
			ELSE
				FALSE
		END AS is_at_1st_addr_conception_correct		
	FROM
		expected_life_stage_results a,
		actual_life_stage_results b
	WHERE
		a.person_id = b.person_id
	ORDER BY
		a.person_id;

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Study Member Data' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_absent_during_exp_period_correct = FALSE OR
				is_at_1st_addr_conception_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'	
		END AS pass_or_fail
	FROM
		results_test_life_stages3
	ORDER BY
		person_id;


	-- Cleanup
	DROP TABLE test_expected_life_stage_sens_results;
	DROP TABLE test_expected_life_stage_results;
	DROP TABLE test_expected_study_member_flag_results;
	-- DROP TABLE results_test_life_stages1;
	-- DROP TABLE results_test_life_stages2;
	-- DROP TABLE results_test_life_stages3;

END;

$$   LANGUAGE plpgsql;
--SELECT "test_life_stages"('C:\algae_protocol\test_environment');





CREATE OR REPLACE FUNCTION test_early_dropped_exposures(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;	
	expected_results_directory TEXT;

	early_cln_exp_dq_results_file TEXT;	
	early_uncln_exp_dq_results_file TEXT;
	early_stg_exp_dq_results_file TEXT;
	early_birth_addr_exp_dq_results_file TEXT;

	number_of_mismatches INT;

BEGIN

	test_suite_directory := 
		test_directory || '\dropped_exposures\early_life\input_data';
	output_directory :=
		test_directory || '\dropped_exposures\early_life\results';

	PERFORM load_original_test_data(test_suite_directory);
	PERFORM run_early_analysis(output_directory);

	-- =================================================
	-- Test sensitivity variables
	-- =================================================	
	
	expected_results_directory 
		:= test_directory || '\dropped_exposures\early_life\expected_results';


	early_cln_exp_dq_results_file :=
		expected_results_directory || '\early_cln_exp_dq_results.csv';

	DROP TABLE IF EXISTS test_early_cln_exp_dq_results;
	CREATE TABLE test_early_cln_exp_dq_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   inv_addr_days INT,
	   oob_addr_days INT,
	   poor_addr_days INT,
	   missing_days INT,
	   good_addr_days INT
	);

	EXECUTE format ('
	COPY test_early_cln_exp_dq_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		inv_addr_days,
		oob_addr_days,
		poor_addr_days,
		missing_days,
		good_addr_days) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_cln_exp_dq_results_file);


	early_uncln_exp_dq_results_file :=
		expected_results_directory || '\early_uncln_exp_dq_results.csv';

	DROP TABLE IF EXISTS test_early_uncln_exp_dq_results;
	CREATE TABLE test_early_uncln_exp_dq_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   inv_addr_days INT,
	   oob_addr_days INT,
	   poor_addr_days INT,
	   missing_days INT,
	   good_addr_days INT
	);

	EXECUTE format ('
	COPY test_early_uncln_exp_dq_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		inv_addr_days,
		oob_addr_days,
		poor_addr_days,
		missing_days,
		good_addr_days) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_uncln_exp_dq_results_file);

	early_stg_exp_dq_results_file :=
		expected_results_directory || '\early_stg_exp_dq_results.csv';

	DROP TABLE IF EXISTS test_early_stg_exp_dq_results;
	CREATE TABLE test_early_stg_exp_dq_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   inv_addr_days INT,
	   oob_addr_days INT,
	   poor_addr_days INT,
	   missing_days INT,
	   good_addr_days INT
	);

	EXECUTE format ('
	COPY test_early_stg_exp_dq_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		inv_addr_days,
		oob_addr_days,
		poor_addr_days,
		missing_days,
		good_addr_days) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_stg_exp_dq_results_file);


	early_birth_addr_exp_dq_results_file :=
		expected_results_directory || '\early_birth_addr_exp_dq_results.csv';

	DROP TABLE IF EXISTS test_early_birth_addr_exp_dq_results;
	CREATE TABLE test_early_birth_addr_exp_dq_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   inv_addr_days INT,
	   oob_addr_days INT,
	   poor_addr_days INT,
	   missing_days INT,
	   good_addr_days INT
	);

	EXECUTE format ('
	COPY test_early_birth_addr_exp_dq_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		inv_addr_days,
		oob_addr_days,
		poor_addr_days,
		missing_days,
		good_addr_days) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_birth_addr_exp_dq_results_file);

	DROP TABLE IF EXISTS results_early_uncln_exp_dq_results;
	CREATE TABLE results_early_uncln_exp_dq_results AS		 	
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			inv_addr_days AS pm10_tot_invalid_address_days,
			oob_addr_days AS pm10_tot_oob_days,
			poor_addr_days AS pm10_tot_poor_address_days,
			missing_days AS pm10_tot_missing_exp_days,
			good_addr_days AS pm10_tot_good_address_days
		 FROM
		 	test_early_uncln_exp_dq_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_invalid_address_days,
			pm10_tot_oob_days,
			pm10_tot_poor_address_days,
			pm10_tot_missing_exp_days,
			pm10_tot_good_address_days
		 FROM
		 	fin_mob_uncln_exp)
	SELECT
		a.person_id,
		a.comments,
		a.ith_life_stage,
		a.life_stage,		
		CASE
			WHEN a.pm10_tot_invalid_address_days = b.pm10_tot_invalid_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_invalid_count_correct,
		CASE
			WHEN a.pm10_tot_oob_days = b.pm10_tot_oob_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_oob_count_correct,
		CASE
			WHEN a.pm10_tot_poor_address_days = b.pm10_tot_poor_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_poor_count_correct,
		CASE
			WHEN a.pm10_tot_missing_exp_days = b.pm10_tot_missing_exp_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_missing_count_correct,
		CASE
			WHEN a.pm10_tot_good_address_days = b.pm10_tot_good_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_good_count_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 	
	
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Dropped Exposures: Uncleaned' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_invalid_count_correct = FALSE OR
				is_oob_count_correct = FALSE OR
				is_poor_count_correct = FALSE OR
				is_missing_count_correct = FALSE OR
				is_good_count_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_early_uncln_exp_dq_results
	ORDER BY
		person_id,
		ith_life_stage;


	DROP TABLE IF EXISTS results_early_stg_exp_dq_results;
	CREATE TABLE results_early_stg_exp_dq_results AS		 	
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			inv_addr_days AS pm10_tot_invalid_address_days,
			oob_addr_days AS pm10_tot_oob_days,
			poor_addr_days AS pm10_tot_poor_address_days,
			missing_days AS pm10_tot_missing_exp_days,
			good_addr_days AS pm10_tot_good_address_days
		 FROM
		 	test_early_stg_exp_dq_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_invalid_address_days,
			pm10_tot_oob_days,
			pm10_tot_poor_address_days,
			pm10_tot_missing_exp_days,
			pm10_tot_good_address_days
		 FROM
		 	fin_stg_mob_exp)
	SELECT
		a.person_id,
		a.comments,
		a.ith_life_stage,
		a.life_stage,		
		CASE
			WHEN a.pm10_tot_invalid_address_days = b.pm10_tot_invalid_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_invalid_count_correct,
		CASE
			WHEN a.pm10_tot_oob_days = b.pm10_tot_oob_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_oob_count_correct,
		CASE
			WHEN a.pm10_tot_poor_address_days = b.pm10_tot_poor_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_poor_count_correct,
		CASE
			WHEN a.pm10_tot_missing_exp_days = b.pm10_tot_missing_exp_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_missing_count_correct,
		CASE
			WHEN a.pm10_tot_good_address_days = b.pm10_tot_good_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_good_count_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 	
	
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Dropped Exposures: Life Stage' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_invalid_count_correct = FALSE OR
				is_oob_count_correct = FALSE OR
				is_poor_count_correct = FALSE OR
				is_missing_count_correct = FALSE OR
				is_good_count_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_early_stg_exp_dq_results
	ORDER BY
		person_id,
		ith_life_stage;




	DROP TABLE IF EXISTS results_early_birth_addr_exp_dq_results;
	CREATE TABLE results_early_birth_addr_exp_dq_results AS		 	
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			inv_addr_days AS pm10_tot_invalid_address_days,
			oob_addr_days AS pm10_tot_oob_days,
			poor_addr_days AS pm10_tot_poor_address_days,
			missing_days AS pm10_tot_missing_exp_days,
			good_addr_days AS pm10_tot_good_address_days
		 FROM
		 	test_early_birth_addr_exp_dq_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_invalid_address_days,
			pm10_tot_oob_days,
			pm10_tot_poor_address_days,
			pm10_tot_missing_exp_days,
			pm10_tot_good_address_days
		 FROM
		 	fin_no_mob_birth_addr_exp)
	SELECT
		a.person_id,
		a.comments,
		a.ith_life_stage,
		a.life_stage,		
		CASE
			WHEN a.pm10_tot_invalid_address_days = b.pm10_tot_invalid_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_invalid_count_correct,
		CASE
			WHEN a.pm10_tot_oob_days = b.pm10_tot_oob_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_oob_count_correct,
		CASE
			WHEN a.pm10_tot_poor_address_days = b.pm10_tot_poor_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_poor_count_correct,
		CASE
			WHEN a.pm10_tot_missing_exp_days = b.pm10_tot_missing_exp_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_missing_count_correct,
		CASE
			WHEN a.pm10_tot_good_address_days = b.pm10_tot_good_address_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_good_count_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 	
	
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Dropped Exposures: Birth Address' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_invalid_count_correct = FALSE OR
				is_oob_count_correct = FALSE OR
				is_poor_count_correct = FALSE OR
				is_missing_count_correct = FALSE OR
				is_good_count_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_early_birth_addr_exp_dq_results
	ORDER BY
		person_id,
		ith_life_stage;








	
END;

$$   LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test_early_exposure_data(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;
	
	expected_results_directory TEXT;

	early_cln_expected_results_file TEXT;
	early_uncln_expected_results_file TEXT;
	early_stg_mob_expected_results_file TEXT;
	early_no_mob_birth_addr_expected_results_file TEXT;
	early_exp_sens_expected_results_file TEXT;
	
	number_of_mismatches INT;

BEGIN

	test_suite_directory := 
		test_directory || '\exposure_data\early_life\input_data';
	output_directory :=
		test_directory || '\exposure_data\early_life\results';

	PERFORM load_original_test_data(test_suite_directory);
	PERFORM run_early_analysis(output_directory);

	-- =================================================
	-- Test sensitivity variables
	-- =================================================	

	expected_results_directory 
		:= test_directory || '\exposure_data\early_life\expected_results';

	early_cln_expected_results_file :=
		expected_results_directory || '\early_cln_expected_results.csv';

	DROP TABLE IF EXISTS test_early_cln_exp_expected_results;
	CREATE TABLE test_early_cln_exp_expected_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   pm10_tot_sum DOUBLE PRECISION,
	   pm10_tot_avg DOUBLE PRECISION,
	   pm10_tot_med DOUBLE PRECISION,
	   pm10_tot_err_sum DOUBLE PRECISION,
	   pm10_tot_err_med DOUBLE PRECISION,
	   pm10_tot_err_avg DOUBLE PRECISION   
	);

	EXECUTE format ('
	COPY test_early_cln_exp_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		pm10_tot_sum,
		pm10_tot_avg,
		pm10_tot_med,
		pm10_tot_err_sum,
		pm10_tot_err_med,
		pm10_tot_err_avg) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_cln_expected_results_file);


	early_uncln_expected_results_file :=
		expected_results_directory || '\early_uncln_expected_results.csv';

	DROP TABLE IF EXISTS test_early_uncln_exp_expected_results;
	CREATE TABLE test_early_uncln_exp_expected_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   pm10_tot_sum DOUBLE PRECISION,
	   pm10_tot_avg DOUBLE PRECISION,
	   pm10_tot_med DOUBLE PRECISION   
	);

	EXECUTE format ('
	COPY test_early_uncln_exp_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		pm10_tot_sum,
		pm10_tot_avg,
		pm10_tot_med) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_uncln_expected_results_file);

	early_stg_mob_expected_results_file :=
		expected_results_directory || '\early_stg_mob_expected_results.csv';

	DROP TABLE IF EXISTS test_early_stg_mob_exp_expected_results;
	CREATE TABLE test_early_stg_mob_exp_expected_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   pm10_tot_sum DOUBLE PRECISION,
	   pm10_tot_avg DOUBLE PRECISION,
	   pm10_tot_med DOUBLE PRECISION   
	);

	EXECUTE format ('
	COPY test_early_stg_mob_exp_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		pm10_tot_sum,
		pm10_tot_avg,
		pm10_tot_med) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_stg_mob_expected_results_file);

	early_no_mob_birth_addr_expected_results_file :=
		expected_results_directory || '\early_no_mob_birth_addr_expected_results.csv';

	DROP TABLE IF EXISTS test_early_stg_mob_birth_exp_expected_results;
	CREATE TABLE test_early_stg_mob_birth_exp_expected_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   pm10_tot_sum DOUBLE PRECISION,
	   pm10_tot_avg DOUBLE PRECISION,
	   pm10_tot_med DOUBLE PRECISION   
	);

	EXECUTE format ('
	COPY test_early_stg_mob_birth_exp_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		pm10_tot_sum,
		pm10_tot_avg,
		pm10_tot_med) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_no_mob_birth_addr_expected_results_file);



	early_exp_sens_expected_results_file :=
		expected_results_directory || '\early_exp_sens_expected_results.csv';

	DROP TABLE IF EXISTS test_early_exp_sens_expected_results;
	CREATE TABLE test_early_exp_sens_expected_results (
	   person_id TEXT,
	   comments TEXT,
	   no_exposure_data_days INT   
	);

	EXECUTE format ('
	COPY test_early_exp_sens_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		no_exposure_data_days) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_exp_sens_expected_results_file);

	DROP TABLE IF EXISTS results_test_early_cln_exp;
	CREATE TABLE results_test_early_cln_exp AS		 	
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med,
			pm10_tot_err_sum,
			pm10_tot_err_med,
			pm10_tot_err_avg
		 FROM
		 	test_early_cln_exp_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med,
			pm10_tot_err_sum,
			pm10_tot_err_med,
			pm10_tot_err_avg
		 FROM
		 	fin_mob_cln_exp)
	SELECT
		a.person_id,
		a.comments,
		a.ith_life_stage,
		a.life_stage,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_cum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_med_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_err_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_err_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_err_sum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_err_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_err_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_err_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_err_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_err_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_err_med_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Early Exposures: Cleaned' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_pm10_tot_cum_correct = FALSE OR
				is_pm10_tot_avg_correct = FALSE OR
				is_pm10_tot_med_correct = FALSE OR
				is_pm10_tot_err_sum_correct = FALSE  OR
				is_pm10_tot_err_avg_correct = FALSE OR
				is_pm10_tot_err_med_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_early_cln_exp
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_test_early_uncln_exp;
	CREATE TABLE results_test_early_uncln_exp AS
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	test_early_uncln_exp_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	fin_mob_uncln_exp)
	SELECT
		a.person_id,
		a.comments,
		a.ith_life_stage,
		a.life_stage,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_cum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_med_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Early Exposures: Uncleaned' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_pm10_tot_cum_correct = FALSE OR
				is_pm10_tot_avg_correct = FALSE OR
				is_pm10_tot_med_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_early_uncln_exp
	ORDER BY
		person_id,
		ith_life_stage;


	DROP TABLE IF EXISTS results_test_early_stg_mob_exp;
	CREATE TABLE results_test_early_stg_mob_exp AS
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	test_early_stg_mob_exp_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	fin_stg_mob_exp)
	SELECT
		a.person_id,
		a.ith_life_stage,
		a.life_stage,
		a.comments,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_cum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_med_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Early Exposures: Fixed Life Stage Mobility' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_pm10_tot_cum_correct = FALSE OR
				is_pm10_tot_avg_correct = FALSE OR
				is_pm10_tot_med_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_early_stg_mob_exp
	ORDER BY
		person_id,
		ith_life_stage;


	DROP TABLE IF EXISTS results_test_early_stg_mob_birth_exp;
	CREATE TABLE results_test_early_stg_mob_birth_exp AS
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	test_early_stg_mob_birth_exp_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	fin_no_mob_birth_addr_exp)
	SELECT
		a.person_id,
		a.ith_life_stage,
		a.life_stage,
		a.comments,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_cum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_med_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Early Exposures: Fixed Mobility Birth Address' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_pm10_tot_cum_correct = FALSE OR
				is_pm10_tot_avg_correct = FALSE OR
				is_pm10_tot_med_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_early_stg_mob_birth_exp
	ORDER BY
		person_id,
		ith_life_stage;


	DROP TABLE IF EXISTS results_test_early_exp_sens;
	CREATE TABLE results_test_early_exp_sens AS
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			no_exposure_data_days
		 FROM
		 	test_early_exp_sens_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			no_exposure_data_days
		 FROM
		 	fin_sens_variables)
	SELECT
		a.person_id,
		a.comments,
		CASE
			WHEN a.no_exposure_data_days = b.no_exposure_data_days THEN
				TRUE
			ELSE
				FALSE
		END AS is_no_exposure_data_days_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id
	ORDER BY
		a.person_id;
	
	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Exposure Sensitivity Variables' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_no_exposure_data_days_correct = FALSE THEN 
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_early_exp_sens
	ORDER BY
		person_id;

	-- Cleanup
	DROP TABLE test_early_cln_exp_expected_results;
	DROP TABLE test_early_uncln_exp_expected_results;
	DROP TABLE test_early_stg_mob_exp_expected_results;
	DROP TABLE test_early_stg_mob_birth_exp_expected_results;
	DROP TABLE test_early_exp_sens_expected_results;

	/*
	DROP TABLE results_test_early_cln_exp;
	DROP TABLE results_test_early_uncln_exp;
	DROP TABLE results_test_early_stg_mob_exp;
	DROP TABLE results_test_early_stg_mob_birth_exp;
	DROP TABLE results_test_early_exp_sens;
	*/
END;

$$   LANGUAGE plpgsql;





CREATE OR REPLACE FUNCTION test_later_exposure_data(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	output_directory TEXT;
	test_suite_directory TEXT;
	
	expected_results_directory TEXT;

	early_cln_expected_results_file TEXT;
	early_uncln_expected_results_file TEXT;
	early_stg_mob_expected_results_file TEXT;
	
	number_of_mismatches INT;

BEGIN

	test_suite_directory := 
		test_directory || '\exposure_data\later_life\input_data';
	output_directory :=
		test_directory || '\exposure_data\later_life\results';

	PERFORM load_original_test_data(test_suite_directory);
	PERFORM run_later_analysis(output_directory);

	-- =================================================
	-- Test sensitivity variables
	-- =================================================	
	expected_results_directory 
		:= test_directory || '\exposure_data\later_life\expected_results';

	early_cln_expected_results_file :=
		expected_results_directory || '\later_cln_expected_results.csv';

	DROP TABLE IF EXISTS test_later_cln_exp_expected_results;
	CREATE TABLE test_later_cln_exp_expected_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   pm10_tot_sum DOUBLE PRECISION,
	   pm10_tot_avg DOUBLE PRECISION,
	   pm10_tot_med DOUBLE PRECISION,
	   pm10_tot_err_sum DOUBLE PRECISION,
	   pm10_tot_err_med DOUBLE PRECISION,
	   pm10_tot_err_avg DOUBLE PRECISION   
	);

	EXECUTE format ('
	COPY test_later_cln_exp_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		pm10_tot_sum,
		pm10_tot_avg,
		pm10_tot_med,
		pm10_tot_err_sum,
		pm10_tot_err_med,
		pm10_tot_err_avg) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_cln_expected_results_file);


	early_uncln_expected_results_file :=
		expected_results_directory || '\later_uncln_expected_results.csv';

	DROP TABLE IF EXISTS test_later_uncln_exp_expected_results;
	CREATE TABLE test_later_uncln_exp_expected_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   pm10_tot_sum DOUBLE PRECISION,
	   pm10_tot_avg DOUBLE PRECISION,
	   pm10_tot_med DOUBLE PRECISION   
	);

	EXECUTE format ('
	COPY test_later_uncln_exp_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		pm10_tot_sum,
		pm10_tot_avg,
		pm10_tot_med) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_uncln_expected_results_file);

	early_stg_mob_expected_results_file :=
		expected_results_directory || '\later_stg_mob_expected_results.csv';

	DROP TABLE IF EXISTS test_later_stg_mob_exp_expected_results;
	CREATE TABLE test_later_stg_mob_exp_expected_results (
	   person_id TEXT,
	   comments TEXT, -- comment field, often used in testing
	   ith_life_stage INT,
	   life_stage TEXT,
	   pm10_tot_sum DOUBLE PRECISION,
	   pm10_tot_avg DOUBLE PRECISION,
	   pm10_tot_med DOUBLE PRECISION   
	);

	EXECUTE format ('
	COPY test_later_stg_mob_exp_expected_results (	
		person_id,
		comments, -- comment field, often used in testing
		ith_life_stage,
		life_stage,
		pm10_tot_sum,
		pm10_tot_avg,
		pm10_tot_med) 
	FROM 
		%L
	(FORMAT CSV, HEADER)', early_stg_mob_expected_results_file);


	DROP TABLE IF EXISTS results_test_later_cln_exp;
	CREATE TABLE results_test_later_cln_exp AS		 	
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med,
			pm10_tot_err_sum,
			pm10_tot_err_med,
			pm10_tot_err_avg
		 FROM
		 	test_later_cln_exp_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med,
			pm10_tot_err_sum,
			pm10_tot_err_med,
			pm10_tot_err_avg
		 FROM
		 	fin_mob_cln_exp)
	SELECT
		a.person_id,
		a.comments,
		a.ith_life_stage,
		a.life_stage,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_cum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_med_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_err_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_err_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_err_sum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_err_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_err_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_err_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_err_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_err_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_err_med_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 



	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Late Exposures: Cleaned' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_pm10_tot_cum_correct = FALSE OR
				is_pm10_tot_avg_correct = FALSE OR
				is_pm10_tot_med_correct = FALSE OR
				is_pm10_tot_err_sum_correct = FALSE  OR
				is_pm10_tot_err_avg_correct = FALSE OR
				is_pm10_tot_err_med_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_later_cln_exp
	ORDER BY
		person_id,
		ith_life_stage;

	DROP TABLE IF EXISTS results_test_later_uncln_exp;
	CREATE TABLE results_test_later_uncln_exp AS
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	test_later_uncln_exp_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	fin_mob_uncln_exp)
	SELECT
		a.person_id,
		a.comments,
		a.ith_life_stage,
		a.life_stage,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_cum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_med_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Later Exposures: Uncleaned' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_pm10_tot_cum_correct = FALSE OR
				is_pm10_tot_avg_correct = FALSE OR
				is_pm10_tot_med_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_later_uncln_exp
	ORDER BY
		person_id,
		ith_life_stage;


	DROP TABLE IF EXISTS results_test_later_stg_mob_exp;
	CREATE TABLE results_test_later_stg_mob_exp AS
	WITH expected_results AS
		(SELECT
			person_id,
			comments,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	test_later_stg_mob_exp_expected_results),
	actual_results AS 
		(SELECT
			person_id,
			ith_life_stage,
			life_stage,
			pm10_tot_sum,
			pm10_tot_avg,
			pm10_tot_med
		 FROM
		 	fin_stg_mob_exp)
	SELECT
		a.person_id,
		a.ith_life_stage,
		a.life_stage,
		a.comments,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_sum, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_sum, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_cum_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_avg, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_avg, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_avg_correct,
		CASE
			WHEN ROUND(COALESCE(a.pm10_tot_med, 0)::numeric, 3) = ROUND(COALESCE(b.pm10_tot_med, 0)::numeric, 3) THEN
				TRUE
			ELSE
				FALSE
		END AS is_pm10_tot_med_correct
	FROM
		expected_results a,
		actual_results b
	WHERE
		a.person_id = b.person_id AND
		a.ith_life_stage = b.ith_life_stage
	ORDER BY
		a.person_id,
		a.ith_life_stage; 

	INSERT INTO tmp_all_test_case_results 
	SELECT
		person_id AS test_case_name,
		'Later Exposures: Fixed Life Stage Mobility' AS test_area,
		comments AS test_case_description,
		CASE
			WHEN is_pm10_tot_cum_correct = FALSE OR
				is_pm10_tot_avg_correct = FALSE OR
				is_pm10_tot_med_correct = FALSE THEN
				'FAIL'
			ELSE
				'PASS'
		END AS pass_or_fail
	FROM
		results_test_later_stg_mob_exp
	ORDER BY
		person_id,
		ith_life_stage;

END;

$$   LANGUAGE plpgsql;


/**
 * ------------------------------------------------------------------------------------------------ 
 * FUNCTION run_test_suite
 * -------------------------
 * Description
 * -----------
 * Runs test suite comprising test case methods that cover different areas of ALGAE functionality
 * ------------------------------------------------------------------------------------------------ 
 */
CREATE OR REPLACE FUNCTION run_test_suite(test_directory TEXT)
	RETURNS void AS 
$$
DECLARE
	date_phrase TEXT;
	results_test_suite_results_csv_file TEXT;
	
BEGIN

	DROP TABLE IF EXISTS tmp_all_test_case_results;
	CREATE TABLE tmp_all_test_case_results (
		test_case_name TEXT,
		test_area TEXT,
		test_case_description TEXT,
		pass_or_fail TEXT);

	RAISE NOTICE 'Test directory ==%==', test_directory;
	
	PERFORM test_preprocessing(test_directory);
	PERFORM test_study_member_data(test_directory);
	PERFORM test_geocode_data(test_directory);
	PERFORM test_address_histories(test_directory);
	PERFORM test_early_dropped_exposures(test_directory);      
	PERFORM test_early_exposure_data(test_directory);
	PERFORM test_later_exposure_data(test_directory);
	
	DROP TABLE IF EXISTS results_test_suites;
	CREATE TABLE results_test_suites AS
	SELECT
		test_case_name,
		test_area,
		test_case_description,
		pass_or_fail
	FROM
		 tmp_all_test_case_results
	ORDER BY
		test_area,
		test_case_name,
		test_case_description;

	DROP TABLE tmp_all_test_case_results;
	
	
	-- Write test results to a csv file	
	date_phrase :=
		(SELECT to_char(current_timestamp, 'YYYY-Mon-DD-HH24-MI'));
	results_test_suite_results_csv_file :=
		test_directory || 
		'\res_test_suite_' || date_phrase || '.csv';
	EXECUTE format ('
	COPY results_test_suites
	TO
		%L
	(FORMAT CSV, HEADER)', 
	results_test_suite_results_csv_file);	
	
END;
$$   LANGUAGE plpgsql;


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

SELECT "run_test_suite"('C:\algae_protocol\test_environment');


