
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
