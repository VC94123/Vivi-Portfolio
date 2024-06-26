USE Wedding_database;

-- All Vendors data
SELECT 
    v.vendor_id, v.vendor_depart, v.vendor_sustainable, v.vendor_rating,
	p.price_unit, p.unit_vol, p.price_ce -- select the useful information
FROM 
    Vendors AS v
INNER JOIN 
    Products AS p ON p.vendor_id = v.vendor_id-- inner join two tables
GROUP BY 
    v.vendor_id, v.vendor_depart, v.vendor_sustainable, v.vendor_rating,
    p.price_unit, p.unit_vol, p.price_ce;  -- limit the data


-- Non_Sustainable Vendor data
SELECT v.vendor_id, v.vendor_depart,v.vendor_location, v.vendor_sustainable,v.vendor_rating,
       p.product_id,p.product_name, p.price_unit, p.unit_vol, p.price_ce -- select the useful information
FROM Vendors as v
INNER JOIN Products as p -- inner join two tables
ON p.vendor_id = v.vendor_id
WHERE v.vendor_sustainable = 1; -- pick vendor_sustainable = 1


-- Sustainable Vendor data
SELECT v.vendor_id, v.vendor_depart,v.vendor_location, v.vendor_sustainable,v.vendor_rating,
       p.product_id,p.product_name, p.price_unit, p.unit_vol, p.price_ce -- select the useful information
FROM Vendors as v
INNER JOIN Products as p
ON p.vendor_id = v.vendor_id -- inner join two tables
WHERE v.vendor_sustainable = 0; -- pick vendor_sustainable = 0

