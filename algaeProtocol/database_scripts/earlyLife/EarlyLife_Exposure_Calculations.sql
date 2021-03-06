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
