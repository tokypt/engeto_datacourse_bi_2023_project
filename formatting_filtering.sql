-- filtering only relevant codes for my data
SELECT *
FROM `estat_geo_17.0.tsv` egt 
WHERE egt.CODE IN (SELECT eel.geo FROM estat_ei_lmjv_q_r2_filtered_en eel);
SELECT *
FROM `estat_nace_r2.csv` enrc 
WHERE enrc.CODE IN (SELECT eel.nace_r2 FROM estat_ei_lmjv_q_r2_filtered_en eel);

-- only seasonaly unadjusted and sizeclass of total number of employees
SELECT 
	nace_r2
	, geo
	, TIME_PERIOD
	, OBS_VALUE 
FROM estat_ei_lmjv_q_r2_filtered_en eelqrfe 
WHERE s_adj = 'NSA' AND sizeclas = 'TOTAL';


-- following code presented problems when I calculated in PowerBI visuals
-- instead I decided to do the following operations in DAX

-- quarters included
WITH base AS 
(
SELECT 
	nace_r2
	, geo
	, TIME_PERIOD
	, OBS_VALUE
	, LAG(OBS_VALUE) OVER 
		(PARTITION BY geo, nace_r2 ORDER BY TIME_PERIOD) AS lag_obs
FROM estat_ei_lmjv_q_r2_filtered_en eelqrfe 
WHERE 1=1
	AND s_adj = 'NSA' 
	AND sizeclas = 'TOTAL'
)
SELECT 
	*
	, OBS_VALUE - lag_obs AS diff
	, ROUND((OBS_VALUE - lag_obs) / lag_obs * 100, 2) AS growth_perc
FROM base
;		

-- just years
WITH years_only AS(
SELECT
	nace_r2
	, geo
	, LEFT(TIME_PERIOD, 4) AS years
	, AVG(OBS_VALUE) AS obs_value
FROM estat_ei_lmjv_q_r2_filtered_en
WHERE 1=1
	AND s_adj = 'NSA' 
	AND sizeclas = 'TOTAL'
GROUP BY nace_r2, geo, years
)
, base AS (
SELECT 
	nace_r2
	, geo
	, years
	, OBS_VALUE
	, LAG(OBS_VALUE) OVER 
		(PARTITION BY geo, nace_r2 ORDER BY years) AS lag_obs
FROM years_only
)
SELECT 
	*
	, OBS_VALUE - lag_obs AS diff
	, ROUND((OBS_VALUE - lag_obs) / lag_obs * 100, 2) AS growth_perc
FROM base
WHERE lag_obs IS NOT NULL
;	


/* WITH years_only AS(
SELECT
	nace_r2
	, geo
	, LEFT(TIME_PERIOD, 4) AS years
	, AVG(OBS_VALUE) AS obs_value
FROM estat_ei_lmjv_q_r2_filtered_en
WHERE 1=1
	AND s_adj = 'NSA' 
	AND sizeclas = 'TOTAL'
GROUP BY nace_r2, geo, years
)
, base AS (
SELECT 
	nace_r2
	, geo
	, years
	, OBS_VALUE
	, LAG(OBS_VALUE) OVER 
		(PARTITION BY geo, nace_r2 ORDER BY years) AS lag_obs
FROM years_only
)
SELECT 
	*
	, OBS_VALUE - lag_obs AS diff
	, OBS_VALUE / lag_obs - 1 AS growth_perc
FROM base
WHERE lag_obs IS NOT NULL AND OBS_VALUE IS NOT NULL
;
-- stary-novy/starym 
*/

