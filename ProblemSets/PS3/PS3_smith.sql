.mode csv
.import FL_insurance_sample.csv my_table
SELECT * FROM my_table LIMIT 10;
SELECT DISTINCT county FROM my_table;
SELECT AVG(tiv_2012 -tivV_2011) AS mean_difference FROM my_table;
SELECT construction, COUNT(*) as frequency FROM my_table GROUP BY construction;
SELECT construction, COUNT(*) * 1.0 / (SELECT COUNT(*) FROM my_table) as fraction FROM my_table GROUP BY construction;

