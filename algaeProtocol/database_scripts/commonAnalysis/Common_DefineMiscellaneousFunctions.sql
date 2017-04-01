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