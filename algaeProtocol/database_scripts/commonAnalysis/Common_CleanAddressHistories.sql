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