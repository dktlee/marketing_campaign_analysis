WITH riders_with_bonus AS (
  SELECT DISTINCT(wages.rider_id)
  FROM WAGES AS wages
  WHERE wages.wage_type = "referral_bonus"
),

riders_with_no_orders AS (
  SELECT
    riders.rider_id,
    riders.rider_first_name,
    riders.rider_last_name,
    riders.rider_email
  FROM RIDERS AS riders
  WHERE riders.num_deliveries_ltd = 0 OR riders.num_deliveries_ltd IS NULL
)

SELECT
  final.*
FROM
  riders_with_no_orders AS final
  INNER JOIN
  riders_with_bonus AS riders_with_bonus
  ON
  final.rider_id = riders_with_bonus.rider_id
ORDER BY final.rider_last_name, final.rider_first_name
