--- 1. Overview of All Vendors 

SELECT * 
FROM Purchasing.Vendor;

--- Summary of the vendors, products purchasing and rejection.

SELECT 
    v.BusinessEntityID AS VendorID,
    v.Name AS VendorName,
    v.CreditRating,
    v.PreferredVendorStatus,
    v.ActiveFlag,
    COUNT(pv.ProductID) AS NumberOfProductsSupplied,
    SUM(pod.OrderQty) AS TotalProductsPurchased,
    SUM(pod.ReceivedQty) AS TotalProductsReceived,
    SUM(pod.RejectedQty) AS TotalProductsRejected,
    (SUM(pod.RejectedQty) * 100.0) / SUM(pod.ReceivedQty) AS RejectionRate
FROM 
    Purchasing.Vendor v
LEFT JOIN 
    Purchasing.ProductVendor pv ON v.BusinessEntityID = pv.BusinessEntityID
LEFT JOIN 
    Purchasing.PurchaseOrderHeader poh ON v.BusinessEntityID = poh.VendorID
LEFT JOIN 
    Purchasing.PurchaseOrderDetail pod ON poh.PurchaseOrderID = pod.PurchaseOrderID
GROUP BY 
    v.BusinessEntityID, v.Name, v.CreditRating, v.PreferredVendorStatus, v.ActiveFlag
ORDER BY 
    v.Name;


--- 2. How much has been spent on procurement from each vendor?

SELECT 
    poh.VendorID,
    v.Name AS VendorName,
    SUM(pod.OrderQty) AS TotalQuantityPurchased,
    SUM(pod.UnitPrice * pod.OrderQty) AS TotalProcurementCost
FROM 
    Purchasing.PurchaseOrderHeader poh
JOIN 
    Purchasing.PurchaseOrderDetail pod ON poh.PurchaseOrderID = pod.PurchaseOrderID
JOIN 
    Purchasing.Vendor v ON poh.VendorID = v.BusinessEntityID
GROUP BY 
    poh.VendorID, v.Name
ORDER BY 
    TotalProcurementCost DESC;

--- 3. What is the total number of products purchased from each vendor?

SELECT 
    poh.VendorID,
    v.Name AS VendorName,
    COUNT(DISTINCT pod.ProductID) AS TotalProductsPurchased
FROM 
    Purchasing.PurchaseOrderHeader poh
JOIN 
    Purchasing.PurchaseOrderDetail pod ON poh.PurchaseOrderID = pod.PurchaseOrderID
JOIN 
    Purchasing.Vendor v ON poh.VendorID = v.BusinessEntityID
GROUP BY 
    poh.VendorID, v.Name
ORDER BY 
    TotalProductsPurchased DESC;


--- 4. Who are the top 10 vendors in terms of total procurement volume 

SELECT Top(10)
    poh.VendorID,
    v.Name AS VendorName,
    SUM(pod.OrderQty) AS TotalProcurementVolume
FROM 
    Purchasing.PurchaseOrderHeader poh
JOIN 
    Purchasing.PurchaseOrderDetail pod ON poh.PurchaseOrderID = pod.PurchaseOrderID
JOIN 
    Purchasing.Vendor v ON poh.VendorID = v.BusinessEntityID
GROUP BY 
    poh.VendorID, v.Name
ORDER BY 
    TotalProcurementVolume DESC;


--- 5. Who are the top 10 vendors in terms of total procurement value

SELECT Top(10)
    poh.VendorID,
    v.Name AS VendorName,
    SUM(pod.UnitPrice * pod.OrderQty) AS TotalProcurementValue
FROM 
    Purchasing.PurchaseOrderHeader poh
JOIN 
    Purchasing.PurchaseOrderDetail pod ON poh.PurchaseOrderID = pod.PurchaseOrderID
JOIN 
    Purchasing.Vendor v ON poh.VendorID = v.BusinessEntityID
GROUP BY 
    poh.VendorID, v.Name
ORDER BY 
    TotalProcurementValue DESC;


--- 6. Which Vendors provide unique products

WITH ProductVendorCount AS (
    SELECT 
        pv.ProductID,
        COUNT(DISTINCT pv.BusinessEntityID) AS VendorCount
    FROM 
        Purchasing.ProductVendor pv
    GROUP BY 
        pv.ProductID
)
SELECT 
    v.BusinessEntityID AS VendorID,
    v.Name AS VendorName,
    p.ProductID,
    p.Name AS ProductName
FROM 
    Purchasing.Vendor v
JOIN 
    Purchasing.ProductVendor pv ON v.BusinessEntityID = pv.BusinessEntityID
JOIN 
    Production.Product p ON pv.ProductID = p.ProductID
JOIN 
    ProductVendorCount pvc ON pvc.ProductID = p.ProductID
WHERE 
    pvc.VendorCount = 1
ORDER BY 
    v.Name, p.Name;


--- 7. Which Vendors provide the same products

WITH ProductVendorCount AS (
    SELECT 
        pv.ProductID,
        COUNT(DISTINCT pv.BusinessEntityID) AS VendorCount
    FROM 
        Purchasing.ProductVendor pv
    GROUP BY 
        pv.ProductID
)
SELECT 
    p.ProductID,
    p.Name AS ProductName,
    STRING_AGG(v.Name, ', ') AS VendorNames
FROM 
    Purchasing.Vendor v
JOIN 
    Purchasing.ProductVendor pv ON v.BusinessEntityID = pv.BusinessEntityID
JOIN 
    Production.Product p ON pv.ProductID = p.ProductID
JOIN 
    ProductVendorCount pvc ON pvc.ProductID = p.ProductID
WHERE 
    pvc.VendorCount > 1
GROUP BY 
    p.ProductID, p.Name
ORDER BY 
    p.Name;


--- 8. What percentage of total procurement costs is attributed to each vendor?

WITH VendorProcurement AS (
    SELECT 
        poh.VendorID,
        v.Name AS VendorName,
        SUM(pod.UnitPrice * pod.OrderQty) AS VendorProcurementCost
    FROM 
        Purchasing.PurchaseOrderHeader poh
    JOIN 
        Purchasing.PurchaseOrderDetail pod ON poh.PurchaseOrderID = pod.PurchaseOrderID
    JOIN 
        Purchasing.Vendor v ON poh.VendorID = v.BusinessEntityID
    GROUP BY 
        poh.VendorID, v.Name
), TotalProcurementCost AS (
    SELECT 
        SUM(VendorProcurementCost) AS TotalCost
    FROM 
        VendorProcurement
)
SELECT 
    vp.VendorID,
    vp.VendorName,
    vp.VendorProcurementCost,
    (vp.VendorProcurementCost / tp.TotalCost) * 100 AS PercentageOfTotalCost
FROM 
    VendorProcurement vp,
    TotalProcurementCost tp
ORDER BY 
    PercentageOfTotalCost DESC;



--- 9. Who are the top vendors that have the lowest average lead time  

SELECT 
    v.BusinessEntityID AS VendorID,
    v.Name AS VendorName,
    p.ProductID,
    p.Name AS ProductName,
    AverageLeadTime AS AvgLeadTime_Days
FROM 
    Purchasing.ProductVendor pv
JOIN 
    Purchasing.Vendor v ON pv.BusinessEntityID = v.BusinessEntityID
JOIN 
    Production.Product p ON pv.ProductID = p.ProductID
GROUP BY 
    v.BusinessEntityID, v.Name, p.ProductID, p.Name, AverageLeadTime
ORDER BY 
    AvgLeadTime_Days ASC;


--- 10. Who are the top 10 vendors that have the highest average time-to-ship  

SELECT 
    v.BusinessEntityID AS VendorID,
    v.Name AS VendorName,
    p.ProductID,
    p.Name AS ProductName,
    AverageLeadTime AS AvgLeadTime_Days
FROM 
    Purchasing.ProductVendor pv
JOIN 
    Purchasing.Vendor v ON pv.BusinessEntityID = v.BusinessEntityID
JOIN 
    Production.Product p ON pv.ProductID = p.ProductID
GROUP BY 
    v.BusinessEntityID, v.Name, p.ProductID, p.Name, AverageLeadTime
ORDER BY 
    AvgLeadTime_Days DESC;


--- 11. Are there specific vendors with higher procurement costs for the same products offered by others?


WITH ProductVendorCount AS (
    -- Find the count of vendors supplying each product
    SELECT 
        pv.ProductID,
        COUNT(DISTINCT pv.BusinessEntityID) AS VendorCount
    FROM 
        Purchasing.ProductVendor pv
    GROUP BY 
        pv.ProductID
),
SameProductVendors AS (
    -- Get the list of vendors supplying the same products
    SELECT 
        p.ProductID,
        p.Name AS ProductName,
        v.BusinessEntityID AS VendorID,
        v.Name AS VendorName
    FROM 
        Purchasing.ProductVendor pv
    JOIN 
        Purchasing.Vendor v ON pv.BusinessEntityID = v.BusinessEntityID
    JOIN 
        Production.Product p ON pv.ProductID = p.ProductID
    JOIN 
        ProductVendorCount pvc ON p.ProductID = pvc.ProductID
    WHERE 
        pvc.VendorCount > 1 -- Only products supplied by multiple vendors
),
VendorProcurementValue AS (
    -- Calculate the total procurement value, total quantity, and defect rate for each vendor
    SELECT 
        poh.VendorID,
        SUM(pod.UnitPrice * pod.OrderQty) AS TotalProcurementValue,
        SUM(pod.OrderQty) AS TotalQuantityPurchased,
        SUM(pod.UnitPrice * pod.OrderQty) / SUM(pod.OrderQty) AS AvgProcurementCost,
        SUM(pod.ReceivedQty) AS TotalReceivedQty,
        SUM(pod.RejectedQty) AS TotalRejectedQty,
        CASE 
            WHEN SUM(pod.ReceivedQty) > 0 
            THEN (SUM(pod.RejectedQty) * 100.0) / SUM(pod.ReceivedQty) 
            ELSE 0 
        END AS DefectRate
    FROM 
        Purchasing.PurchaseOrderHeader poh
    JOIN 
        Purchasing.PurchaseOrderDetail pod ON poh.PurchaseOrderID = pod.PurchaseOrderID
    WHERE 
        poh.ShipDate IS NOT NULL  -- Exclude orders that haven't shipped yet
    GROUP BY 
        poh.VendorID
)
-- Join vendors with their average procurement values, defect rates, and average lead time for the same products
SELECT 
    spv.ProductID,
    spv.ProductName,
    spv.VendorName,
    vpv.TotalProcurementValue,
    vpv.TotalQuantityPurchased,
    ROUND(vpv.AvgProcurementCost, 2) AS AvgProcurementCost,
    ROUND(vpv.DefectRate, 2) AS DefectRate, -- Defect rate as a percentage
    pv.AverageLeadTime -- Average lead time from Purchasing.ProductVendor
FROM 
    SameProductVendors spv
LEFT JOIN 
    VendorProcurementValue vpv ON spv.VendorID = vpv.VendorID
JOIN 
    Purchasing.ProductVendor pv ON spv.VendorID = pv.BusinessEntityID AND spv.ProductID = pv.ProductID
ORDER BY 
    spv.ProductName, AvgProcurementCost DESC;



--- 12. Who are the top 10 vendors with lowest product defect rate 

WITH VendorDefectRate AS (
    -- Calculate the defect rate for each vendor
    SELECT 
        poh.VendorID,
        v.Name AS VendorName,
        SUM(pod.ReceivedQty) AS TotalReceivedQty,
        SUM(pod.RejectedQty) AS TotalRejectedQty,
        CASE 
            WHEN SUM(pod.ReceivedQty) > 0 
            THEN (SUM(pod.RejectedQty) * 100.0) / SUM(pod.ReceivedQty) 
            ELSE 0 
        END AS DefectRate
    FROM 
        Purchasing.PurchaseOrderHeader poh
    JOIN 
        Purchasing.PurchaseOrderDetail pod ON poh.PurchaseOrderID = pod.PurchaseOrderID
    JOIN 
        Purchasing.Vendor v ON poh.VendorID = v.BusinessEntityID
    WHERE 
        pod.ReceivedQty > 0  -- Ensure we only consider vendors with received quantities
    GROUP BY 
        poh.VendorID, v.Name
)
-- Select the top 10 vendors with the lowest defect rate
SELECT top (10)
    VendorID,
    VendorName,
    ROUND(DefectRate, 2) AS DefectRate
FROM 
    VendorDefectRate
ORDER BY 
    DefectRate ASC; -- Sort by lowest defect rate;


--- 13. Who are the top 10 vendors with highest product defect rate 


WITH VendorDefectRate AS (
    -- Calculate the defect rate for each vendor
    SELECT 
        poh.VendorID,
        v.Name AS VendorName,
        SUM(pod.ReceivedQty) AS TotalReceivedQty,
        SUM(pod.RejectedQty) AS TotalRejectedQty,
        CASE 
            WHEN SUM(pod.ReceivedQty) > 0 
            THEN (SUM(pod.RejectedQty) * 100.0) / SUM(pod.ReceivedQty) 
            ELSE 0 
        END AS DefectRate
    FROM 
        Purchasing.PurchaseOrderHeader poh
    JOIN 
        Purchasing.PurchaseOrderDetail pod ON poh.PurchaseOrderID = pod.PurchaseOrderID
    JOIN 
        Purchasing.Vendor v ON poh.VendorID = v.BusinessEntityID
    WHERE 
        pod.ReceivedQty > 0  -- Ensure we only consider vendors with received quantities
    GROUP BY 
        poh.VendorID, v.Name
)
-- Select the top 10 vendors with the lowest defect rate
SELECT top (10)
    VendorID,
    VendorName,
    ROUND(DefectRate, 2) AS DefectRate
FROM 
    VendorDefectRate
ORDER BY 
    DefectRate DESC; -- Sort by highest defect rate;


--- 14. What percentage of products received from each vendor are rejected due to defects?

SELECT 
    poh.VendorID,
    v.Name AS VendorName,
    SUM(pod.ReceivedQty) AS TotalReceivedQty,
    SUM(pod.RejectedQty) AS TotalRejectedQty,
    CASE 
        WHEN SUM(pod.ReceivedQty) > 0 
        THEN (SUM(pod.RejectedQty) * 100.0) / SUM(pod.ReceivedQty) 
        ELSE 0 
    END AS RejectionPercentage
FROM 
    Purchasing.PurchaseOrderHeader poh
JOIN 
    Purchasing.PurchaseOrderDetail pod ON poh.PurchaseOrderID = pod.PurchaseOrderID
JOIN 
    Purchasing.Vendor v ON poh.VendorID = v.BusinessEntityID
GROUP BY 
    poh.VendorID, v.Name
ORDER BY 
    RejectionPercentage DESC;


--- 15. What is the Profit margin for products supplied by each vendor  - order by highest profit 

WITH VendorProductProfit AS (
    -- Calculate revenue and procurement costs for each vendor
    SELECT 
        poh.VendorID,
        v.Name AS VendorName,
        sod.ProductID,
        SUM(sod.UnitPrice * sod.OrderQty) AS TotalRevenue,  -- Total selling price (revenue)
        SUM(pod.UnitPrice * sod.OrderQty) AS TotalCost,     -- Total procurement cost (matching the sold quantity)
        SUM(sod.UnitPrice * sod.OrderQty) - SUM(pod.UnitPrice * sod.OrderQty) AS TotalProfit,  -- Total profit
        CASE 
            WHEN SUM(sod.UnitPrice * sod.OrderQty) > 0 
            THEN ((SUM(sod.UnitPrice * sod.OrderQty) - SUM(pod.UnitPrice * sod.OrderQty)) * 100.0) / SUM(sod.UnitPrice * sod.OrderQty)
            ELSE 0 
        END AS ProfitMarginPercentage -- Profit margin as a percentage
    FROM 
        Sales.SalesOrderDetail sod
    JOIN 
        Production.Product p ON sod.ProductID = p.ProductID
    JOIN 
        Purchasing.ProductVendor pv ON p.ProductID = pv.ProductID
    JOIN 
        Purchasing.PurchaseOrderDetail pod ON pv.ProductID = pod.ProductID
    JOIN 
        Purchasing.PurchaseOrderHeader poh ON pod.PurchaseOrderID = poh.PurchaseOrderID
    JOIN 
        Purchasing.Vendor v ON poh.VendorID = v.BusinessEntityID
    WHERE 
        sod.OrderQty <= pod.OrderQty -- Ensures matching quantities between sold and purchased products
    GROUP BY 
        poh.VendorID, v.Name, sod.ProductID
)
-- Select the results and order by highest profit margin
SELECT 
    VendorID,
    VendorName,
    ROUND(TotalProfit, 2) AS TotalProfit,
    ROUND(ProfitMarginPercentage, 2) AS ProfitMarginPercentage
FROM 
    VendorProductProfit
ORDER BY 
    ProfitMarginPercentage DESC;





