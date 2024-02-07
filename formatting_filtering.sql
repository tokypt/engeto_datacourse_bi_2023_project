SELECT *
FROM `estat_geo_17.0.tsv` egt 
WHERE egt.CODE IN (SELECT eel.geo FROM estat_ei_lmjv_q_r2_filtered_en eel);
SELECT *
FROM `estat_nace_r2.csv` enrc 
WHERE enrc.CODE IN (SELECT eel.nace_r2 FROM estat_ei_lmjv_q_r2_filtered_en eel);

SELECT 
	nace_r2
	, geo
	, TIME_PERIOD
	, OBS_VALUE 
FROM estat_ei_lmjv_q_r2_filtered_en eelqrfe 
WHERE s_adj = 'NSA' AND sizeclas = 'TOTAL';


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
	, lag_obs
	, OBS_VALUE - lag_obs AS diff
	, ROUND((OBS_VALUE - lag_obs) / lag_obs * 100, 2) AS growth_perc
FROM base
;		




